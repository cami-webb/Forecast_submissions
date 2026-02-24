library(lubridate)

source("./Generate_forecasts/ARIMA/forecast_model.R")
source("./Generate_forecasts/R/generate_tg_forecast.R")
source("./Generate_forecasts/R/rerun_forecasts.R")
source("./Generate_forecasts/R/run_all_vars.R")


# Optional knobs for different themes (coastal/urban/etc)
# Leave NULL to use defaults inside generate_tg_forecast()
target_path <- NULL

model_id <- Sys.getenv("MODEL_ID", unset = "tg_arima")
model_themes <- strsplit(Sys.getenv("MODEL_THEMES", unset = "coastal"), ",")[[1]]

rerun_forecasts(
  model_id = model_id,
  forecast_model = forecast_model,
  model_themes = model_themes,
  END = END,
  noaa = FALSE,
  all_sites = FALSE,
  source_modes = c("buoy", "modis", "cci")
)
