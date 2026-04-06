generate_tg_forecast <- function(forecast_date,
                                 forecast_model,
                                 model_themes = "coastal",
                                 model_id = model_id,
                                 all_sites = FALSE,
                                 noaa = FALSE,
                                 vars_manual = NULL,
                                 target_path = NULL,
                                 source_mode = c("buoy", "modis", "cci"),
                                 nowcast_only = FALSE) {

  forecast_date <- as.Date(forecast_date)
  model_themes  <- as.character(model_themes)
  config <- yaml::read_yaml("Generate_forecasts/config.yaml")

  # Targets path (allow passing in for urban vs coastal)
  if (is.null(target_path)) {
    target_path <- paste0(
      config$targets_path, "/",
      model_themes[1],
      "-targets.csv"
    )
  }

  s3_read <- arrow::s3_bucket(
    config$s3_bucket_read,
    endpoint_override = config$endpoint,
    access_key = Sys.getenv("OSN_KEY"),
    secret_key = Sys.getenv("OSN_SECRET"),
    scheme = "https"
  )

  target_raw <- arrow::read_csv_arrow(s3_read$path(target_path))
  
  # NOAA drivers (not used for ARIMA; keeping in case I add them later!)
if (isTRUE(noaa)) {
    source("./Generate_forecasts/R/load_met_gefs.R")
    met <- load_met_gefs(
      sites         = unique(target_raw$site_id),
      forecast_date = forecast_date,
      config        = config
    )
    noaa_past_mean    <- met$noaa_past_mean
    noaa_future_daily <- met$noaa_future_daily
  } else {
    noaa_future_daily <- NULL
    noaa_past_mean    <- NULL
  }

  # Targets path (allow passing in for urban vs coastal)
  if (is.null(target_path)) {
    target_path <- paste0(
      config$targets_path, "/",
      model_themes[1],
      "-targets.csv"
    )
  }

  target <- target_raw |>
    dplyr::mutate(
      datetime = if (inherits(datetime[1], "POSIXct")) {
        datetime
      } else if (inherits(datetime[1], "Date")) {
        as.POSIXct(datetime, tz = "UTC")
      } else {
        as.POSIXct(lubridate::ymd(datetime), tz = "UTC")
      }
    ) |>
    dplyr::select(dplyr::any_of(c("datetime", "site_id", "variable", "duration", "observation")))

  if (!"duration" %in% names(target)) target$duration <- NA_character_

  s3_write <- arrow::s3_bucket(
    config$s3_bucket_write,
    endpoint_override = config$endpoint,
    access_key = Sys.getenv("OSN_KEY"),
    secret_key = Sys.getenv("OSN_SECRET"),
    scheme = "https"
  )

  theme_default_vars <- function(theme) {
    if (!is.null(vars_manual)) return(vars_manual)
    vars <- config$theme_vars[[theme]]
    if (is.null(vars)) stop("Unknown theme: ", theme)
    return(vars)
  }

  duration_settings <- function(duration) {
      if (isTRUE(nowcast_only)) return(list(horiz = 1, step = 1))
      duration <- as.character(duration)
      if (duration == "P1D")  return(list(horiz = config$horizon_P1D,  step = 1))
      if (duration == "PT1H") return(list(horiz = config$horizon_PT1H, step = 1))
      return(list(horiz = config$horizon_P1D, step = 1))
    }

  all_coastal_sites <- unique(target$site_id)

  for (theme in model_themes) {
    vars <- theme_default_vars(theme)

    if (identical(theme, "coastal")) {

      buoy_sites  <- unique(target$site_id[target$variable == "chlora_buoy"])
      modis_sites <- unique(target$site_id[target$variable == "chlora_modis"])
      cci_sites   <- unique(target$site_id[target$variable == "chlora_cci"])

      mode_var_sites <- list(
        chlora_buoy  = list(sites = buoy_sites,  var = "chlora_buoy"),
        chlora_modis = list(sites = modis_sites, var = "chlora_modis"),
        chlora_cci   = list(sites = cci_sites,   var = "chlora_cci")
      )

      mode_var_sites <- mode_var_sites[
        names(mode_var_sites) %in% paste0("chlora_", source_mode)
      ]

      forecast_all <- purrr::map_dfr(mode_var_sites, function(mv) {
        if (length(mv$sites) == 0) {
          message("No sites for variable ", mv$var, "; skipping.")
          return(NULL)
        }

        dur <- target |>
          dplyr::filter(.data$variable == mv$var) |>
          dplyr::pull(.data$duration)
        dur <- dur[!is.na(dur) & nzchar(dur)]
        dur <- if (length(dur) == 0) NA_character_ else dur[1]
        hs  <- duration_settings(dur)

        run_all_vars(
          var               = mv$var,
          sites             = mv$sites,
          forecast_model    = forecast_model,
          noaa_past_mean    = noaa_past_mean,
          noaa_future_daily = noaa_future_daily,
          target            = target,
          horiz             = hs$horiz,
          step              = hs$step,
          theme             = theme,
          forecast_date     = forecast_date,
          model_id          = model_id
        )
      })

      if (is.null(forecast_all) || nrow(forecast_all) == 0) {
        message("No coastal forecasts produced; skipping write.")
        next
      }

      forecast_out <- forecast_all |>
        dplyr::mutate(
          reference_datetime = as.Date(reference_datetime),
          depth              = 1,
          family             = as.character("normal"),
          parameter          = as.character(parameter),
          obs_flag           = 0L,
          variable           = as.character(variable),
          prediction         = as.numeric(prediction)
        ) |>
        dplyr::select(
          reference_datetime, datetime, depth, family,
          parameter, obs_flag, variable, prediction
        )

      forecast_key <- paste0(
        config$forecasts_path, "/",
        theme, "-", forecast_date, "-", model_id, ".csv"
      )

      arrow::write_csv_arrow(forecast_out, sink = s3_write$path(forecast_key))
      message("Wrote forecast to S3: ", forecast_key)

    } else if (identical(theme, "urban")) {

      forecast_all <- purrr::map_dfr(vars, function(v) {
        dur <- target |>
          dplyr::filter(.data$variable == v) |>
          dplyr::pull(.data$duration)
        dur <- dur[!is.na(dur) & nzchar(dur)]
        dur <- if (length(dur) == 0) NA_character_ else dur[1]
        hs  <- duration_settings(dur)

        run_all_vars(
          var               = v,
          sites             = unique(target$site_id),
          forecast_model    = forecast_model,
          noaa_past_mean    = noaa_past_mean,
          noaa_future_daily = noaa_future_daily,
          target            = target,
          horiz             = hs$horiz,
          step              = hs$step,
          theme             = theme,
          forecast_date     = forecast_date,
          model_id          = model_id
        )
      })

      if (is.null(forecast_all) || nrow(forecast_all) == 0) {
        message("No urban forecasts produced; skipping write.")
        next
      }

      forecast_out <- forecast_all |>
        dplyr::mutate(
          reference_datetime = as.Date(reference_datetime),
          depth              = 1,
          family             = as.character("normal"),
          parameter          = as.character(parameter),
          obs_flag           = 0L,
          variable           = as.character(variable),
          prediction         = as.numeric(prediction)
        ) |>
        dplyr::select(
          reference_datetime, datetime, depth, family,
          parameter, obs_flag, variable, prediction
        )

      forecast_key <- paste0(
        config$forecasts_path, "/",
        theme, "-", forecast_date, "-", model_id, ".csv"
      )

      arrow::write_csv_arrow(forecast_out, sink = s3_write$path(forecast_key))
      message("Wrote forecast to S3: ", forecast_key)

    } else {
      stop("Unknown theme: ", theme)
    }
  }

  invisible(NULL)
}
