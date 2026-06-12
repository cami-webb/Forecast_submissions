# Random forest forecast model for bu4cast coastal and urban sites.
# Trains on historical met (stage3 daily means) + target observations each run.
# Tunes mtry and min.node.size via ranger's free OOB error (no separate CV loop).
# Predicts on each GEFS stage2 ensemble member; PT1H vars get hourly expansion.
#
# Requires ranger — install once if not present:
#   install.packages("ranger", lib = "/projectnb/dietzelab/cwebb16/x86_64-pc-linux-gnu-library/4.5/")

library(tidyverse)
library(lubridate)
library(arrow)
library(ranger)
library(yaml)

source("./Generate_forecasts/R/load_met_gefs.R")
source("./Generate_forecasts/R/generate_tg_forecast.R")
source("./Generate_forecasts/R/run_all_vars.R")

MET_VARS <- c(
  "air_temperature", "air_pressure", "precipitation_flux",
  "relative_humidity", "surface_downwelling_shortwave_flux_in_air",
  "eastward_wind", "northward_wind",
  "surface_downwelling_longwave_flux_in_air"
)

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

  message("Running site: ", site)

  # PT1H variables (horiz = 48) require daily aggregation for training
  is_hourly <- horiz > 30

  if (is.null(noaa_future_daily)) {
    message("No future met data available for ", forecast_date, ". Skipping site ", site, ".")
    return(NULL)
  }
  if (is.null(noaa_past_mean)) {
    message("No historical met data available. Skipping site ", site, ".")
    return(NULL)
  }

  # Subset target to this site/variable; aggregate to daily to match met
  site_target <- target |>
    dplyr::select(datetime, site_id, variable, observation) |>
    dplyr::mutate(site_id = as.character(site_id)) |>
    dplyr::filter(variable == target_variable,
                  site_id  == as.character(site),
                  datetime  < forecast_date) |>
    dplyr::mutate(datetime    = as.Date(datetime),
                  observation = suppressWarnings(as.numeric(observation))) |>
    dplyr::group_by(datetime, site_id, variable) |>
    dplyr::summarise(observation = mean(observation, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(names_from = "variable", values_from = "observation")

  site_target <- site_target |>
    dplyr::left_join(
      noaa_past_mean |> dplyr::filter(site_id == site),
      by = c("datetime", "site_id")
    )

  if (!target_variable %in% names(site_target)) {
    message("No target observations at site ", site, ". Skipping.")
    return(NULL)
  }

  if (sum(!is.na(site_target$air_temperature) &
          !is.na(site_target[[target_variable]])) == 0) {
    message("No overlapping air_temperature + target at site ", site, ". Skipping.")
    return(NULL)
  }

  available_met <- MET_VARS[MET_VARS %in% names(site_target)]
  train_data <- site_target |>
    dplyr::select(dplyr::all_of(c(target_variable, available_met))) |>
    tidyr::drop_na()

  if (nrow(train_data) < 10) {
    message("Only ", nrow(train_data), " complete rows at site ", site, ". Skipping.")
    return(NULL)
  }

  # ---- Tune mtry and min.node.size via OOB error ----
  p          <- length(available_met)
  mtry_grid  <- unique(pmax(1L, c(floor(sqrt(p)), floor(p / 3L), floor(p / 2L))))
  min_n_grid <- c(5L, 10L, 20L)
  rf_formula <- as.formula(paste(target_variable, "~ ."))

  best_oob   <- Inf
  best_mtry  <- mtry_grid[1]
  best_min_n <- min_n_grid[1]

  for (m in mtry_grid) {
    for (mn in min_n_grid) {
      rf_tune <- ranger::ranger(
        formula       = rf_formula,
        data          = train_data,
        num.trees     = 200,
        mtry          = m,
        min.node.size = mn
      )
      if (rf_tune$prediction.error < best_oob) {
        best_oob   <- rf_tune$prediction.error
        best_mtry  <- m
        best_min_n <- mn
      }
    }
  }
  message("  tuned: mtry=", best_mtry, " min.node.size=", best_min_n,
          " OOB_MSE=", round(best_oob, 4), " n_train=", nrow(train_data))

  # ---- Fit final model ----
  rf_fit <- ranger::ranger(
    formula       = rf_formula,
    data          = train_data,
    num.trees     = 500,
    mtry          = best_mtry,
    min.node.size = best_min_n
  )

  # ---- Build future met frame (daily), then expand to hourly if needed ----
  noaa_future <- noaa_future_daily |>
    dplyr::filter(site_id == site) |>
    dplyr::select(dplyr::any_of(c("datetime", "parameter", available_met))) |>
    tidyr::drop_na()

  if (nrow(noaa_future) == 0) {
    message("No future met data at site ", site, ". Skipping.")
    return(NULL)
  }

  if (is_hourly) {
    noaa_future <- noaa_future |>
      dplyr::mutate(datetime = as.Date(datetime)) |>
      dplyr::group_by(datetime, parameter) |>
      dplyr::reframe(
        dplyr::across(dplyr::all_of(available_met)),
        hour = 0:23
      ) |>
      dplyr::mutate(
        datetime = as.POSIXct(datetime, tz = "UTC") + lubridate::hours(hour)
      ) |>
      dplyr::select(-hour) |>
      dplyr::filter(
        datetime >= as.POSIXct(forecast_date, tz = "UTC"),
        datetime <  as.POSIXct(forecast_date, tz = "UTC") + lubridate::hours(horiz)
      )
  }

  # ---- Predict on each ensemble member ----
  pred_input  <- noaa_future |> dplyr::select(dplyr::all_of(available_met))
  predictions <- pmax(0, predict(rf_fit, data = pred_input)$predictions)

  noaa_future |>
    dplyr::mutate(
      site_id            = site,
      prediction         = predictions,
      variable           = target_variable,
      reference_datetime = forecast_date,
      family             = "ensemble",
      model_id           = model_id
    ) |>
    dplyr::select(model_id, datetime, reference_datetime,
                  site_id, family, parameter, variable, prediction)
}
