library(lubridate)
library(yaml)

config <- yaml::read_yaml("Generate_forecasts/config.yaml")

source("./Generate_forecasts/tg_lm_single/forecast_model.R")
source("./Generate_forecasts/R/load_met_gefs.R")
source("./Generate_forecasts/R/generate_tg_forecast.R")
source("./Generate_forecasts/R/run_all_vars.R")

MODEL_ID <- Sys.getenv("MODEL_ID")
if (!nzchar(MODEL_ID)) stop("MODEL_ID environment variable is not set.")

message("Running model: ", MODEL_ID)

for (th in config$models[[MODEL_ID]]$model_themes) {
  source_modes_th <- config$source_modes[[th]]
  message("=== Running today's forecast: ", MODEL_ID, " | ", th, " ===")
  tryCatch({
    generate_tg_forecast(
      forecast_date  = Sys.Date(),
      forecast_model = forecast_model,
      model_themes   = th,
      model_id       = MODEL_ID,
      all_sites      = FALSE,
      noaa           = TRUE,
      source_mode    = source_modes_th
    )
  }, error = function(e) {
    cat("ERROR:", MODEL_ID, th, ":", conditionMessage(e), "\n")
  })
}
