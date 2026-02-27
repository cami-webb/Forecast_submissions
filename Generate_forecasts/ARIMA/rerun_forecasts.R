library(lubridate)

source("./Generate_forecasts/ARIMA/forecast_model.R")
source("./Generate_forecasts/R/generate_tg_forecast.R")
source("./Generate_forecasts/R/rerun_forecasts.R")
source("./Generate_forecasts/R/run_all_vars.R")


# Optional knobs for different themes (coastal/urban/etc)
# Leave NULL to use defaults inside generate_tg_forecast()
target_path <- NULL

model_id <- Sys.getenv("MODEL_ID", unset = "tg_arima")
model_themes <- strsplit(Sys.getenv("MODEL_THEMES", unset = "coastal,urban"), ",")[[1]]
model_themes <- trimws(model_themes)

for (th in model_themes) {

  source_modes_th <- if (identical(th, "coastal")) {
    c("buoy", "modis", "cci")
  } else {
    # urban: no buoy/modis/cci split
    "urban"
  }

  rerun_forecasts(
    model_id = model_id,
    forecast_model = forecast_model,
    model_themes = th,
    END = END,
    noaa = FALSE,
    all_sites = FALSE,
    source_modes = source_modes_th
  )
}
