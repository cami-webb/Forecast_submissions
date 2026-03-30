library(yaml)
library(lubridate)

config <- yaml::read_yaml("Generate_forecasts/config.yaml")

source("./Generate_forecasts/tg_XGBoost/forecast_model.R")
source("./Generate_forecasts/R/generate_tg_forecast.R")
source("./Generate_forecasts/R/run_all_vars.R")

tryCatch({
  for (th in config$models$tg_xgboost$model_themes) {
    generate_tg_forecast(
      forecast_date  = Sys.Date(),
      forecast_model = forecast_model,
      model_themes   = th,
      model_id       = config$models$tg_xgboost$model_id,
      noaa           = TRUE
    )
  }
}, error = function(e) { cat("ERROR with forecast generation:\n", conditionMessage(e), "\n") })
