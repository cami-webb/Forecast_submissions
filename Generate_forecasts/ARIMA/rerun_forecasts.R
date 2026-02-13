source("./Generate_forecasts/ARIMA/forecast_model.R")
source("./Generate_forecasts/R/rerun_forecasts.R")

END <- as_date("2023-11-13") # Re-run if forecasts have not been re-run after Nov 13, fixing calibration issue

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
  source_modes = c("buoy", "modis")
)
