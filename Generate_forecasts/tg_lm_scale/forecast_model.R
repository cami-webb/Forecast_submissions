# tg_lm_scale model
# Multiple linear regression with ALL GEFS met drivers, standardized to mean=0 sd=1.
# Slopes are directly comparable across predictors as measures of relative importance.

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
    dplyr::left_join(noaa_past_mean |> dplyr::filter(site_id == site),
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

  # Standardize met predictors (mean=0, sd=1) using training data stats.
  # Store centers and scales so we can apply the same transformation to future data.
  met_scaled  <- scale(data[, MET_VARS])
  scl_centers <- attr(met_scaled, "scaled:center")
  scl_scales  <- attr(met_scaled, "scaled:scale")

  fit_data <- data
  fit_data[, MET_VARS] <- met_scaled

  fit <- lm(
    as.formula(paste(target_variable, "~", paste(MET_VARS, collapse = " + "))),
    data = fit_data
  )

  noaa_future <- noaa_future_daily |>
    dplyr::filter(site_id == site) |>
    dplyr::select(dplyr::all_of(c("datetime", "parameter", MET_VARS))) |>
    na.omit()

  if (nrow(noaa_future) == 0) {
    message(paste0("No future GEFS data at site ", site, ". Skipping."))
    return()
  }

  # Apply the SAME training-data scaling to future met (do not re-scale)
  future_met_scaled <- scale(
    noaa_future[, MET_VARS],
    center = scl_centers,
    scale  = scl_scales
  )

  forecast <- noaa_future |>
    dplyr::mutate(
      site_id    = site,
      prediction = pmax(0, predict(fit, as.data.frame(future_met_scaled))),
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
