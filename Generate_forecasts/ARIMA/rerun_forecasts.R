library(lubridate)

source("./Generate_forecasts/ARIMA/forecast_model.R")
source("./Generate_forecasts/R/rerun_forecasts.R")

END <- lubridate::as_date("2006-01-01")

# Optional knobs for different themes (coastal/urban/etc)
# Leave NULL to use defaults inside generate_tg_forecast()
target_path <- NULL

rerun_forecasts(
  model_id = model_id,
  forecast_model = forecast_model,
  model_themes = model_themes,
  END = END,
  noaa = FALSE,
  all_sites = FALSE,
  source_modes = c("buoy", "modis", "cci")
)
