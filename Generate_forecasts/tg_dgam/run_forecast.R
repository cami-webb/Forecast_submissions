library(yaml)
library(lubridate)

config <- yaml::read_yaml("Generate_forecasts/config.yaml")

source("./Generate_forecasts/tg_dgam/forecast_model.R")
source("./Generate_forecasts/R/generate_tg_forecast.R")
source("./Generate_forecasts/R/load_met_gefs.R")
source("./Generate_forecasts/R/run_all_vars.R")

tryCatch({
  for (th in config$models$tg_dgam$model_themes) {
    generate_tg_forecast(
      forecast_date  = Sys.Date(),
      forecast_model = forecast_model,
      model_themes   = th,
      model_id       = config$models$tg_dgam$model_id,
      noaa           = TRUE,
      source_mode    = if (th == "coastal") c("buoy", "cci_corrected") else config$source_modes[[th]]
    )
  }
}, error = function(e) { cat("ERROR with forecast generation:\n", conditionMessage(e), "\n") })
