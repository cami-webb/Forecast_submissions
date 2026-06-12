#!/usr/bin/env Rscript
# run_forecast_date.R
# Run ARIMA forecast for a single date and theme.
# Usage: Rscript run_forecast_date.R <YYYY-MM-DD> <theme>
# Example: Rscript run_forecast_date.R 2026-05-10 coastal

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Usage: Rscript run_forecast_date.R <YYYY-MM-DD> <theme>  (e.g. 2026-05-10 coastal)")
}

forecast_date <- tryCatch(as.Date(args[1]), error = function(e) stop("Invalid date: ", args[1]))
theme         <- args[2]

if (!theme %in% c("coastal", "urban")) {
  stop("theme must be 'coastal' or 'urban', got: ", theme)
}

cat(sprintf("[%s] Starting %s ARIMA forecast for %s\n", format(Sys.time()), theme, forecast_date))

library(yaml)
config <- yaml::read_yaml("Generate_forecasts/config.yaml")

source("./Generate_forecasts/ARIMA/forecast_model.R")
source("./Generate_forecasts/R/generate_tg_forecast.R")
source("./Generate_forecasts/R/run_all_vars.R")

tryCatch({
  generate_tg_forecast(
    forecast_date  = forecast_date,
    forecast_model = forecast_model,
    model_themes   = theme,
    model_id       = config$models$tg_arima$model_id,
    noaa           = FALSE,
    source_mode    = config$source_modes[[theme]]
  )
  cat(sprintf("[%s] Done: %s %s\n", format(Sys.time()), theme, forecast_date))
}, error = function(e) {
  cat(sprintf("[%s] ERROR %s %s: %s\n", format(Sys.time()), theme, forecast_date, conditionMessage(e)))
  quit(status = 1)
})
