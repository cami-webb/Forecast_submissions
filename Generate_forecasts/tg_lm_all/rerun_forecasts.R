library(lubridate)
library(yaml)

config <- yaml::read_yaml("Generate_forecasts/config.yaml")

source("./Generate_forecasts/tg_lm_all/forecast_model.R")
source("./Generate_forecasts/R/load_met_gefs.R")
source("./Generate_forecasts/R/generate_tg_forecast.R")
source("./Generate_forecasts/R/rerun_forecasts.R")
source("./Generate_forecasts/R/run_all_vars.R")

for (th in config$models$tg_lm_all$model_themes) {
  source_modes_th <- config$source_modes[[th]]
  rerun_forecasts(
    model_id       = config$models$tg_lm_all$model_id,
    forecast_model = forecast_model,
    model_themes   = th,
    noaa           = TRUE,
    all_sites      = FALSE,
    source_modes   = source_modes_th,
    local_dir      = file.path(config$local_forecasts_dir, "LM_all")
  )
}
