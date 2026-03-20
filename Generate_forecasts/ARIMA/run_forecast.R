library(yaml)
config <- yaml::read_yaml("Generate_forecasts/config.yaml")
source("./Generate_forecasts/ARIMA/forecast_model.R")
source("Generate_forecasts/R/generate_tg_forecast.R")
source("Generate_forecasts/R/run_all_vars.R")

tryCatch({
  for (theme in config$models$tg_arima$model_themes) {
    generate_tg_forecast(
      forecast_date  = Sys.Date(),
      forecast_model = forecast_model,
      model_themes   = theme,
      model_id       = config$models$tg_arima$model_id,
      noaa           = FALSE,
      source_mode    = config$source_modes[[theme]]
    )
  }
}, error = function(e) { cat("ERROR with forecast generation:\n", conditionMessage(e), "\n") })
