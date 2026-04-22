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

  # Get wspd from target data
  wspd_data <- target |>
    dplyr::select(datetime, site_id, variable, observation) |>
    dplyr::mutate(site_id = as.character(site_id)) |>
    dplyr::filter(variable == "wspd",
                  site_id == site,
                  datetime < forecast_date) |>
    tidyr::pivot_wider(names_from = "variable", values_from = "observation")

  # Merge target variable with wspd
  site_target <- target |>
    dplyr::select(datetime, site_id, variable, observation) |>
    dplyr::mutate(site_id = as.character(site_id)) |>
    dplyr::filter(variable %in% c(target_variable),
                  site_id == site,
                  datetime < forecast_date) |>
    tidyr::pivot_wider(names_from = "variable", values_from = "observation") |>
    dplyr::left_join(wspd_data, by = c("datetime", "site_id"))

  if (!target_variable %in% names(site_target)) {
    message(paste0("No target observations at site ", site, ". Skipping forecasts at this site."))
    return()

  } else if (sum(!is.na(site_target$wspd) & !is.na(site_target[target_variable])) == 0) {
    message(paste0("No historical wspd data that corresponds with target observations at site ", site, ". Skipping forecasts at this site."))
    return()

  } else {
    # Fit linear model based on past data: target = m * wspd + b
    fit <- lm(get(target_variable) ~ wspd, data = site_target)

    # Get last known wspd value for persistence forecast
    last_wspd <- wspd_data |>
      dplyr::filter(!is.na(wspd)) |>
      dplyr::arrange(desc(datetime)) |>
      dplyr::slice(1) |>
      dplyr::pull(wspd)

    if (length(last_wspd) == 0 || is.na(last_wspd)) {
      message(paste0("No recent wspd data at site ", site, ". Skipping forecasts at this site."))
      return()
    }

    # Create future dates using persistence (same wspd for all 30 days)
    future_dates <- seq.Date(as.Date(forecast_date),
                             as.Date(forecast_date) + horiz - 1,
                             by = "day")

    forecast <- tibble::tibble(
      datetime  = future_dates,
      site_id   = as.character(site),
      wspd      = last_wspd,
      parameter = 1
    ) |>
      dplyr::mutate(
        prediction = predict(fit, tibble::tibble(wspd = last_wspd)),
        variable   = target_variable
      )

    # Format results to EFI standard
    forecast <- forecast |>
      mutate(reference_datetime = forecast_date,
             family   = "ensemble",
             model_id = model_id) |>
      select(model_id, datetime, reference_datetime,
             site_id, family, parameter, variable, prediction)
  }
}
