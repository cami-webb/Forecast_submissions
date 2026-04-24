library(yaml)
config <- yaml::read_yaml("Generate_forecasts/config.yaml")
source("./Generate_forecasts/ARIMA/forecast_model.R")
source("Generate_forecasts/R/generate_tg_forecast.R")
source("Generate_forecasts/R/run_all_vars.R")

daily_urban_vars <- c("O3", "PM2.5_P1D", "PM10_P1D")

july_dates    <- seq.Date(as.Date("2025-07-01"), as.Date("2025-07-31"), by = "day")
aug_nov_dates <- seq.Date(as.Date("2025-08-01"), as.Date("2025-11-30"), by = "day")

# July: both themes, all vars (normal)
for (d in as.character(july_dates)) {
  message("July: ", d)
  tryCatch(
    generate_tg_forecast(
      forecast_date  = d,
      forecast_model = forecast_model,
      model_themes   = c("coastal", "urban"),
      model_id       = config$models$tg_arima$model_id,
      noaa           = FALSE,
      source_mode    = c("buoy", "cci")
    ),
    error = function(e) message("ERROR ", d, ": ", e$message)
  )
}

# Aug-Nov: coastal normal, urban daily only (called separately)
for (d in as.character(aug_nov_dates)) {
  message("Aug-Nov coastal: ", d)
  tryCatch(
    generate_tg_forecast(
      forecast_date  = d,
      forecast_model = forecast_model,
      model_themes   = "coastal",
      model_id       = config$models$tg_arima$model_id,
      noaa           = FALSE,
      source_mode    = c("buoy", "cci")
    ),
    error = function(e) message("ERROR coastal ", d, ": ", e$message)
  )

  message("Aug-Nov urban (daily only): ", d)
  tryCatch(
    generate_tg_forecast(
      forecast_date  = d,
      forecast_model = forecast_model,
      model_themes   = "urban",
      model_id       = config$models$tg_arima$model_id,
      noaa           = FALSE,
      source_mode    = "urban",
      vars_manual    = daily_urban_vars
    ),
    error = function(e) message("ERROR urban ", d, ": ", e$message)
  )
}
