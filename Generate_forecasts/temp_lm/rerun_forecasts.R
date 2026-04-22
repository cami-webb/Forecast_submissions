library(lubridate)
library(yaml)

config <- yaml::read_yaml("Generate_forecasts/config.yaml")

source("./Generate_forecasts/temp_lm/forecast_model.R")
source("./Generate_forecasts/R/load_met_gefs.R")
source("./Generate_forecasts/R/generate_tg_forecast.R")
source("./Generate_forecasts/R/rerun_forecasts.R")
source("./Generate_forecasts/R/run_all_vars.R")

for (th in config$models$tg_dswrf_lm$model_themes) {
  source_modes_th <- config$source_modes[[th]]
  rerun_forecasts(
    model_id       = config$models$tg_dswrf_lm$model_id,
    forecast_model = forecast_model,
    model_themes   = th,
    noaa           = FALSE,
    use_gefs       = TRUE,
    all_sites      = FALSE,
    source_modes   = source_modes_th
  )
}
