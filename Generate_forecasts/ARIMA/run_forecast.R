model_id <- "tg_arima"
model_themes <- c("coastal", "urban")

source("./Generate_forecasts/ARIMA/forecast_model.R")
source("Generate_forecasts/R/generate_tg_forecast.R")
source("Generate_forecasts/R/run_all_vars.R")

tryCatch({
    generate_tg_forecast(
    forecast_date = Sys.Date(),
    forecast_model = forecast_model,
    model_themes = "coastal",
    model_id = model_id,
    noaa = FALSE
  )
  
  generate_tg_forecast(
    forecast_date = Sys.Date(),
    forecast_model = forecast_model,
    model_themes = "urban",
    model_id = model_id,
    noaa = FALSE
  )
}, error=function(e){cat("ERROR with forecast generation:\n",conditionMessage(e), "\n")})
