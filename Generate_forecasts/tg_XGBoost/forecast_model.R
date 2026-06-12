# tg_XGBoost model
# written by ASL, 21 Jan 2023
# edited 2023-09-08 to consolidate and set up framework for filling in missed dates

library(tidyverse)
library(lubridate)
library(arrow)
library(caret)
library(xgboost)
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

#### Define the forecast model for a site
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

  message(paste0("Running site: ", site))

  # PT1H variables (horiz = 48) require daily aggregation for training
  is_hourly <- horiz > 30

  # Guard against NULL met (stage2/stage3 not available for this date)
  if (is.null(noaa_future_daily)) {
    message(paste0("No future met data available for ", forecast_date, ". Skipping site ", site, "."))
    return(NULL)
  }
  if (is.null(noaa_past_mean)) {
    message(paste0("No historical met data available. Skipping site ", site, "."))
    return(NULL)
  }

  # Subset target to this site and variable, truncate datetime to Date for met join.
  # Aggregate duplicates to daily mean before pivot to avoid list-cols.
  site_target <- target |>
    dplyr::select(datetime, site_id, variable, observation) |>
    dplyr::mutate(site_id = as.character(site_id)) |>
    dplyr::filter(variable %in% c(target_variable),
                  site_id == as.character(site),
                  datetime < forecast_date) |>
    dplyr::mutate(datetime    = as.Date(datetime),
                  observation = suppressWarnings(as.numeric(observation))) |>
    dplyr::group_by(datetime, site_id, variable) |>
    dplyr::summarise(observation = mean(observation, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(names_from = "variable", values_from = "observation")

  # Join with daily historical met (both are now Date)
  site_target <- site_target |>
    dplyr::left_join(
      noaa_past_mean |> dplyr::filter(site_id == site),
      by = c("datetime", "site_id")
    )

  if (!target_variable %in% names(site_target)) {
    message(paste0("No target observations at site ", site, ". Skipping."))
    return(NULL)
  }

  if (sum(!is.na(site_target$air_temperature) &
          !is.na(site_target[[target_variable]])) == 0) {
    message(paste0("No overlapping air_temperature + target at site ", site, ". Skipping."))
    return(NULL)
  }

  available_met <- MET_VARS[MET_VARS %in% names(site_target)]
  data <- site_target |>
    dplyr::select(dplyr::all_of(c(target_variable, available_met))) |>
    tidyr::drop_na()

  if (nrow(data) < 3) {
    message(paste0("Not enough complete rows at site ", site, " (", nrow(data), "). Skipping."))
    return(NULL)
  }

  mat    <- as.matrix(data |> dplyr::select(-dplyr::all_of(target_variable)))
  dtrain <- xgboost::xgb.DMatrix(data = mat, label = data[[target_variable]])

  fit <- xgboost::xgb.train(
    data    = dtrain,
    nrounds = 50L,
    params  = list(eval_metric = "rmse"),
    verbose = 0
  )

  # Build future met frame (daily)
  noaa_future <- noaa_future_daily |>
    dplyr::filter(site_id == site) |>
    dplyr::select(dplyr::any_of(c("datetime", "parameter", available_met))) |>
    tidyr::drop_na()

  if (nrow(noaa_future) == 0) {
    message(paste0("No future met data at site ", site, ". Skipping."))
    return(NULL)
  }

  # For PT1H variables expand each daily met row to 24 hourly rows
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

  forecast <- noaa_future |>
    dplyr::mutate(
      site_id    = site,
      prediction = pmax(0, predict(fit, xgboost::xgb.DMatrix(
                             data = as.matrix(noaa_future |>
                               dplyr::select(dplyr::all_of(available_met)))))),
      variable   = target_variable
    )

  forecast |>
    dplyr::mutate(
      reference_datetime = forecast_date,
      family             = "ensemble",
      model_id           = model_id
    ) |>
    dplyr::select(model_id, datetime, reference_datetime,
                  site_id, family, parameter, variable, prediction)
}
