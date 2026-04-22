library(lubridate)
library(yaml)

config <- yaml::read_yaml("Generate_forecasts/config.yaml")

source("./Generate_forecasts/temp_lm/forecast_model.R")
source("./Generate_forecasts/R/generate_tg_forecast.R")
source("./Generate_forecasts/R/run_all_vars.R")

for (th in config$models$tg_humidity_lm$model_themes) {
  source_modes_th <- config$source_modes[[th]]
  tryCatch({
    generate_tg_forecast(
      forecast_date  = Sys.Date(),
      forecast_model = forecast_model,
      model_themes   = th,
      model_id       = config$models$tg_humidity_lm$model_id,
      all_sites      = FALSE,
      noaa           = TRUE,
      source_mode    = source_modes_th
    )
  }, error = function(e) {
    cat("ERROR with forecast generation for theme", th, ":\n", conditionMessage(e), "\n")
  })
}
