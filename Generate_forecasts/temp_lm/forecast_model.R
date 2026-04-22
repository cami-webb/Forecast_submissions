# tg_temp_lm model
# written by ASL, 21 Jan 2023
# edited 2023-09-08 to consolidate and set up framework for filling in missed dates

library(tidyverse)
library(lubridate)
library(arrow)
library(yaml)

source("./Generate_forecasts/R/load_met_gefs.R")
source("./Generate_forecasts/R/generate_tg_forecast.R")
source("./Generate_forecasts/R/run_all_vars.R")

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

  # Merge in past NOAA data into the targets file, matching by date.
  site_target <- target |>
    dplyr::select(datetime, site_id, variable, observation) |>
    dplyr::mutate(site_id = as.character(site_id)) |>
    dplyr::filter(variable %in% c(target_variable),
                  site_id == site,
                  datetime < forecast_date) |>
    tidyr::pivot_wider(names_from = "variable", values_from = "observation") |>
    dplyr::left_join(noaa_past_mean %>%
                       filter(site_id == site),
                     by = c("datetime", "site_id"))
  if (!target_variable %in% names(site_target)) {
    message(paste0("No target observations at site ", site, ". Skipping forecasts at this site."))
    return()

  } else if (sum(!is.na(site_target$surface_downwelling_shortwave_flux_in_air) & !is.na(site_target[target_variable])) == 0) {
    message(paste0("No historical surface downwelling shortwave flux data that corresponds with target observations at site ", site, ". Skipping forecasts at this site."))
    
  } else {
    # Fit linear model based on past data: target = m * air temp + b
    fit <- lm(get(target_variable) ~ surface_downwelling_shortwave_flux_in_air, data = site_target)
    
    # Get 30-day predicted temp ensemble at the site
    noaa_future <- noaa_future_daily %>%
      filter(site_id == site)

    # use the linear model to forecast target variable for each ensemble member
    forecast <- noaa_future |>
      mutate(site_id = site,
             prediction = predict(fit, tibble(surface_downwelling_shortwave_flux_in_air)),
             variable = target_variable)

    # Format results to EFI standard
    forecast <- forecast |>
      mutate(reference_datetime = forecast_date,
             family = "ensemble",
             model_id = model_id) |>
      select(model_id, datetime, reference_datetime,
             site_id, family, parameter, variable, prediction)
  }
}
