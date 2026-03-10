library(yaml)
config <- yaml::read_yaml("Generate_forecasts/ARIMA/arima_config.yaml")

source("./Generate_forecasts/ARIMA/forecast_model.R")
source("Generate_forecasts/R/generate_tg_forecast.R")
source("Generate_forecasts/R/run_all_vars.R")

tryCatch({
  for (theme in config$model_themes) {
    generate_tg_forecast(
      forecast_date  = Sys.Date(),
      forecast_model = forecast_model,
      model_themes   = theme,
      model_id       = config$model_id,
      noaa           = FALSE
    )
  }
}, error = function(e) { cat("ERROR with forecast generation:\n", conditionMessage(e), "\n") })
