library(lubridate)
library(yaml)

config <- yaml::read_yaml("Generate_forecasts/config.yaml")

source("./Generate_forecasts/ARIMA/forecast_model.R")
source("./Generate_forecasts/R/generate_tg_forecast.R")
source("./Generate_forecasts/R/rerun_forecasts.R")
source("./Generate_forecasts/R/run_all_vars.R")

target_path <- NULL

for (th in config$models$tg_arima$model_themes) {
  source_modes_th <- config$source_modes[[th]]
  rerun_forecasts(
    model_id       = config$models$tg_arima$model_id,
    forecast_model = forecast_model,
    model_themes   = th,
    END            = END,
    noaa           = FALSE,
    all_sites      = FALSE,
    source_modes   = source_modes_th
  )
}
