library(lubridate)
library(yaml)

config <- yaml::read_yaml("Generate_forecasts/config.yaml")

source("./Generate_forecasts/tg_lm_single/forecast_model.R")
source("./Generate_forecasts/R/load_met_gefs.R")
source("./Generate_forecasts/R/generate_tg_forecast.R")
source("./Generate_forecasts/R/rerun_forecasts.R")
source("./Generate_forecasts/R/run_all_vars.R")

MODEL_ID <- Sys.getenv("MODEL_ID")
if (!nzchar(MODEL_ID)) stop("MODEL_ID environment variable is not set.")

message("Backfilling model: ", MODEL_ID)

for (th in config$models[[MODEL_ID]]$model_themes) {
  source_modes_th <- config$source_modes[[th]]
  message("=== Backfilling: ", MODEL_ID, " | ", th, " ===")
  rerun_forecasts(
    model_id       = MODEL_ID,
    forecast_model = forecast_model,
    model_themes   = th,
    noaa           = TRUE,
    all_sites      = FALSE,
    source_modes   = source_modes_th,
    local_dir      = file.path(config$local_forecasts_dir, "LM")
  )
}
