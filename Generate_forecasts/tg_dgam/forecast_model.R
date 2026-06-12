# Dynamic GAM forecast model using mvgam with GEFS met drivers
# Clark & Wells 2022, Methods in Ecology and Evolution, 10.1111/2041-210X.13974

library(tidyverse)
library(lubridate)
library(arrow)
library(mvgam)
library(yaml)

# Reduced set of uncorrelated predictors to avoid identifiability issues in Stan
MET_VARS <- c(
  "air_temperature",
  "precipitation_flux",
  "surface_downwelling_shortwave_flux_in_air"
)

# Impute NA values in a numeric vector with column mean
impute_mean <- function(x) {
  m <- mean(x, na.rm = TRUE)
  x[is.na(x) | is.nan(x)] <- if (is.finite(m)) m else 0
  x
}

forecast_model <- function(site,
                           noaa_past_mean,
                           noaa_future_daily,
                           target_variable,
                           target,
                           horiz,
                           step,
                           theme,
                           forecast_date,
                           model_id) {

  message(paste0("Running dgam: site=", site, " var=", target_variable))

  is_hourly <- horiz > 30

  if (is.null(noaa_future_daily)) {
    message(paste0("No future met data for ", forecast_date, ". Skipping site ", site, "."))
    return(NULL)
  }
  if (is.null(noaa_past_mean)) {
    message(paste0("No historical met data. Skipping site ", site, "."))
    return(NULL)
  }

  # Aggregate daily target observations
  site_target <- target |>
    dplyr::select(datetime, site_id, variable, observation) |>
    dplyr::mutate(site_id = as.character(site_id)) |>
    dplyr::filter(
      variable == target_variable,
      site_id  == as.character(site),
      datetime <  as.POSIXct(forecast_date, tz = "UTC")
    ) |>
    dplyr::mutate(
      datetime    = as.Date(datetime),
      observation = suppressWarnings(as.numeric(observation))
    ) |>
    dplyr::group_by(datetime, site_id) |>
    dplyr::summarise(y = mean(observation, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(y = ifelse(is.nan(y), NA_real_, y)) |>
    dplyr::arrange(datetime)

  if (nrow(site_target) < 20) {
    message(paste0("Too few observations at site ", site,
                   " (", nrow(site_target), "). Skipping."))
    return(NULL)
  }

  # Join historical met
  site_data <- site_target |>
    dplyr::left_join(
      noaa_past_mean |>
        dplyr::filter(site_id == as.character(site)) |>
        dplyr::mutate(datetime = as.Date(datetime)),
      by = c("datetime", "site_id")
    )

  available_met <- MET_VARS[MET_VARS %in% names(site_data)]

  if (length(available_met) == 0) {
    message(paste0("No met variables found for site ", site, ". Skipping."))
    return(NULL)
  }

  complete_rows <- sum(
    !is.na(site_data$y) &
    rowSums(!is.na(site_data[, available_met, drop = FALSE])) > 0
  )
  if (complete_rows < 10) {
    message(paste0("Too few complete rows at site ", site,
                   " (", complete_rows, "). Skipping."))
    return(NULL)
  }

  # mvgam training data frame: requires time (integer), series (factor), y, covariates
  series_name <- paste0("s", gsub("[^a-zA-Z0-9]", "", as.character(site)))
  if (!grepl("^[a-zA-Z]", series_name)) series_name <- paste0("s", series_name)

  train_df <- site_data |>
    dplyr::select(y, dplyr::all_of(available_met)) |>
    dplyr::mutate(
      time   = seq_len(nrow(site_data)),
      series = factor(series_name, levels = series_name)
    ) |>
    as.data.frame()

  # Mean-impute then standardize met covariates (mean=0, SD=1).
  # HMC mixes poorly when predictors are on very different scales
  # (e.g. air_temperature ~280-310 K vs precipitation_flux ~0-0.0001 kg/m2/s).
  col_means <- list()
  col_sds   <- list()
  for (v in available_met) {
    col_means[[v]] <- mean(train_df[[v]], na.rm = TRUE)
    train_df[[v]]  <- impute_mean(train_df[[v]])
    col_sds[[v]]   <- sd(train_df[[v]], na.rm = TRUE)
    if (is.finite(col_sds[[v]]) && col_sds[[v]] > 0) {
      train_df[[v]] <- (train_df[[v]] - col_means[[v]]) / col_sds[[v]]
    }
  }

  # Build smooth terms; k=4 is conservative and stable for ~300 obs
  k_val         <- 4L
  smooth_terms  <- paste(paste0("s(", available_met, ", k=", k_val, ")"), collapse = " + ")
  model_formula <- as.formula(paste("y ~", smooth_terms))

  fit <- tryCatch({
    mvgam::mvgam(
      formula     = model_formula,
      family      = gaussian(),
      data        = train_df,
      trend_model = AR(),
      noncentred  = TRUE,
      chains      = 2L,
      iter        = 2000L,
      warmup      = 1000L,
      silent      = 0L
    )
  }, error = function(e) {
    message(paste0("mvgam fit failed at site ", site, ": ", conditionMessage(e)))
    NULL
  })

  if (is.null(fit)) return(NULL)

  max_rhat <- tryCatch({
    # cmdstanr backend
    max(fit$model_output$summary()$rhat, na.rm = TRUE)
  }, error = function(e) tryCatch({
    # rstan backend
    max(rstan::summary(fit$model_output)$summary[, "Rhat"], na.rm = TRUE)
  }, error = function(e2) tryCatch({
    # posterior package fallback
    max(posterior::summarise_draws(fit$model_output$draws(), "rhat")$rhat, na.rm = TRUE)
  }, error = function(e3) {
    message("rhat check failed: ", conditionMessage(e3))
    NA_real_
  })))

  if (!is.na(max_rhat) && is.finite(max_rhat)) {
    if (max_rhat < 1.05) {
      message(paste0("R-hat is good! (max = ", round(max_rhat, 3), ") Yay!"))
    } else {
      message(paste0("R-hat warning: max = ", round(max_rhat, 3), " (threshold 1.05)"))
    }
  }

  # Build future met data frame (mean across ensemble members)
  n_train <- nrow(train_df)

  noaa_future_filt <- noaa_future_daily |>
    dplyr::filter(site_id == as.character(site)) |>
    dplyr::mutate(datetime = as.Date(datetime))

  if (is_hourly) {
    # Model is fit on daily data; forecast 2 daily steps then expand to hourly.
    # Keeps AR time indices consistent with training resolution.
    future_df <- noaa_future_filt |>
      dplyr::group_by(datetime) |>
      dplyr::summarise(
        dplyr::across(dplyr::all_of(available_met), \(x) mean(x, na.rm = TRUE)),
        .groups = "drop"
      ) |>
      dplyr::filter(
        datetime >= as.Date(forecast_date),
        datetime <  as.Date(forecast_date) + ceiling(horiz / 24)
      ) |>
      dplyr::arrange(datetime)
  } else {
    future_df <- noaa_future_filt |>
      dplyr::group_by(datetime) |>
      dplyr::summarise(
        dplyr::across(dplyr::all_of(available_met), \(x) mean(x, na.rm = TRUE)),
        .groups = "drop"
      ) |>
      dplyr::filter(
        datetime >= as.Date(forecast_date),
        datetime <  as.Date(forecast_date) + horiz
      ) |>
      dplyr::arrange(datetime)
  }

  if (nrow(future_df) == 0) {
    message(paste0("No future met rows for site ", site, ". Skipping."))
    return(NULL)
  }

  actual_horiz <- nrow(future_df)

  new_df <- future_df |>
    dplyr::select(dplyr::all_of(available_met)) |>
    dplyr::mutate(
      time   = n_train + seq_len(actual_horiz),
      series = factor(series_name, levels = series_name),
      y      = NA_real_
    ) |>
    as.data.frame()

  # Impute future met NAs then apply same standardization as training data
  for (v in available_met) {
    fill_val    <- if (is.finite(col_means[[v]])) col_means[[v]] else 0
    vals        <- new_df[[v]]
    vals[is.na(vals) | is.nan(vals)] <- fill_val
    if (is.finite(col_sds[[v]]) && col_sds[[v]] > 0) {
      vals <- (vals - col_means[[v]]) / col_sds[[v]]
    }
    new_df[[v]] <- vals
  }

  fc <- tryCatch({
    mvgam::forecast(fit, newdata = new_df, type = "response")
  }, error = function(e) {
    message(paste0("mvgam forecast failed at site ", site, ": ", conditionMessage(e)))
    NULL
  })

  if (is.null(fc)) return(NULL)

  # fc$forecasts is a list (one element per series): matrix [n_draws x n_timesteps]
  draws_mat <- fc$forecasts[[1]]

  if (is.null(draws_mat) || !is.matrix(draws_mat) || ncol(draws_mat) == 0) {
    message(paste0("Empty forecast draws for site ", site, ". Skipping."))
    return(NULL)
  }

  n_steps    <- min(ncol(draws_mat), actual_horiz)
  draws_sub  <- draws_mat[, seq_len(n_steps), drop = FALSE]

  mu_vals    <- pmax(colMeans(draws_sub, na.rm = TRUE), 0)
  sigma_vals <- apply(draws_sub, 2, sd, na.rm = TRUE)
  sigma_vals <- pmax(sigma_vals, 1e-6)

  # Daily output dates from future_df
  daily_dts <- future_df$datetime[seq_len(n_steps)]

  if (is_hourly) {
    # Expand each daily mu/sigma to 24 hourly rows
    hourly_dts    <- as.POSIXct(rep(daily_dts, each = 24), tz = "UTC") +
                       lubridate::hours(rep(0:23, times = n_steps))
    hourly_dts    <- hourly_dts[
      hourly_dts >= as.POSIXct(forecast_date, tz = "UTC") &
      hourly_dts <  as.POSIXct(forecast_date, tz = "UTC") + lubridate::hours(horiz)
    ]
    n_hourly      <- length(hourly_dts)
    mu_vals    <- rep(mu_vals,    each = 24)[seq_len(n_hourly)]
    sigma_vals <- rep(sigma_vals, each = 24)[seq_len(n_hourly)]
    out_dts    <- hourly_dts
  } else {
    out_dts <- daily_dts
  }

  dplyr::bind_rows(
    dplyr::tibble(
      model_id           = model_id,
      datetime           = out_dts,
      reference_datetime = as.Date(forecast_date),
      site_id            = as.character(site),
      family             = "normal",
      parameter          = "mu",
      variable           = target_variable,
      prediction         = mu_vals
    ),
    dplyr::tibble(
      model_id           = model_id,
      datetime           = out_dts,
      reference_datetime = as.Date(forecast_date),
      site_id            = as.character(site),
      family             = "normal",
      parameter          = "sigma",
      variable           = target_variable,
      prediction         = sigma_vals
    )
  )
}
