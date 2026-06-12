library(yaml)
library(lubridate)

# Optional date range passed from submit_dgam_parallel.sh via command-line args:
#   Rscript rerun_forecasts.R 2025-07-01 2025-08-30
args       <- commandArgs(trailingOnly = TRUE)
date_start <- if (length(args) >= 1 && nzchar(args[1])) as.Date(args[1]) else NULL
date_end   <- if (length(args) >= 2 && nzchar(args[2])) as.Date(args[2]) else NULL

if (!is.null(date_start)) message("Date range: ", date_start, " to ", date_end)

config <- yaml::read_yaml("Generate_forecasts/config.yaml")

source("./Generate_forecasts/tg_dgam/forecast_model.R")
source("./Generate_forecasts/R/generate_tg_forecast.R")
source("./Generate_forecasts/R/rerun_forecasts.R")
source("./Generate_forecasts/R/load_met_gefs.R")
source("./Generate_forecasts/R/run_all_vars.R")

LOCAL_FORECASTS_DIR <- "/projectnb/dietzelab/cwebb16/FRP/Forecasts/dGAM"

for (th in config$models$tg_dgam$model_themes) {
  rerun_forecasts(
    model_id       = config$models$tg_dgam$model_id,
    forecast_model = forecast_model,
    model_themes   = th,
    noaa           = TRUE,
    all_sites      = FALSE,
    source_modes   = if (th == "coastal") c("buoy", "cci_corrected") else config$source_modes[[th]],
    local_dir      = LOCAL_FORECASTS_DIR,
    date_start     = date_start,
    date_end       = date_end
  )
}
