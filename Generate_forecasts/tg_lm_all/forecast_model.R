# tg_lm_all model
# Multiple linear regression using all GEFS met drivers

library(tidyverse)
library(lubridate)
library(arrow)
library(yaml)

source("./Generate_forecasts/R/load_met_gefs.R")
source("./Generate_forecasts/R/generate_tg_forecast.R")
source("./Generate_forecasts/R/run_all_vars.R")

MET_VARS <- c("air_temperature", "air_pressure", "precipitation_flux",
              "relative_humidity",
              "surface_downwelling_shortwave_flux_in_air",
              "eastward_wind", "northward_wind",
              "surface_downwelling_longwave_flux_in_air")

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

  site_target <- target |>
    dplyr::select(datetime, site_id, variable, observation) |>
    dplyr::mutate(site_id = as.character(site_id)) |>
    dplyr::filter(variable %in% c(target_variable),
                  site_id == site,
                  datetime < forecast_date) |>
    tidyr::pivot_wider(names_from = "variable", values_from = "observation") |>
    dplyr::left_join(noaa_past_mean %>% filter(site_id == site),
                     by = c("datetime", "site_id"))

  if (!target_variable %in% names(site_target)) {
    message(paste0("No target observations at site ", site, ". Skipping."))
    return()
  }

  data <- site_target |>
    dplyr::select(dplyr::all_of(c(target_variable, MET_VARS))) |>
    na.omit()

  if (nrow(data) < 10) {
    message(paste0("Insufficient training data at site ", site, ". Skipping."))
    return()
  }

  fit <- lm(
    as.formula(paste(target_variable, "~", paste(MET_VARS, collapse = " + "))),
    data = data
  )

  noaa_future <- noaa_future_daily |>
    dplyr::filter(site_id == site) |>
    dplyr::select(dplyr::all_of(c("datetime", "parameter", MET_VARS))) |>
    na.omit()

  if (nrow(noaa_future) == 0) {
    message(paste0("No future GEFS data at site ", site, ". Skipping."))
    return()
  }

  forecast <- noaa_future |>
    dplyr::mutate(
      site_id    = site,
      prediction = pmax(0, predict(fit, dplyr::select(noaa_future, dplyr::all_of(MET_VARS)))),
      variable   = target_variable
    )

  forecast |>
    dplyr::mutate(
      reference_datetime = forecast_date,
      family   = "ensemble",
      model_id = model_id
    ) |>
    dplyr::select(model_id, datetime, reference_datetime,
                  site_id, family, parameter, variable, prediction)
}
