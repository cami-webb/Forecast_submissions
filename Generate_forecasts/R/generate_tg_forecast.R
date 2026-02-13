generate_tg_forecast <- function(forecast_date,
                                 forecast_model,
                                 model_themes = "coastal",
                                 model_id = model_id,
                                 all_sites = FALSE,
                                 noaa = FALSE,
                                 vars_manual = NULL,
                                 target_path = NULL,
                                 source_mode = c("buoy_only", "modis_only")) {

  forecast_date <- as.Date(forecast_date)

  # NOAA drivers (not used for ARIMA; keeping in case I add them later!) 
  if (isTRUE(noaa)) {
    load_met(forecast_date)
    noaa_future_daily <- read.csv(paste0("./Generate_forecasts/noaa_downloads/noaa_future_daily_", forecast_date, ".csv")) |>
      dplyr::mutate(datetime = lubridate::as_date(datetime))
    noaa_past_mean <- read.csv(paste0("./Generate_forecasts/noaa_downloads/noaa_past_mean_", forecast_date, ".csv")) |>
      dplyr::mutate(datetime = lubridate::as_date(datetime))
  } else {
    noaa_future_daily <- NULL
    noaa_past_mean <- NULL
  }

  # Read coastal targets
  if (is.null(target_path)) {
    target_path <- "challenges/targets/project_id=bu4cast/coastal-targets.csv"
  }

  s3 <- arrow::s3_bucket(
    "bu4cast-ci-read",
    endpoint_override = "https://minio-s3.apps.shift.nerc.mghpcc.org",
    access_key = Sys.getenv("OSN_KEY"),
    secret_key = Sys.getenv("OSN_SECRET"),
    scheme = "https"
  )

  target <- arrow::read_csv_arrow(s3$path(target_path)) |>
    dplyr::mutate(datetime = lubridate::as_date(substr(datetime, 1, 10))) |>
    dplyr::select(datetime, site_id, variable, observation)

  # Target variables
  vars <- if (is.null(vars_manual)) "chlorophyll" else vars_manual

  # Forecast horizon (daily)
  horiz <- 30
  step <- 1

  # Assign site_ids as buoy vs modis
  buoy_sites  <- unique(target$site_id[grepl("^UNH_buoy_", target$site_id)])
  modis_sites <- unique(target$site_id[grepl("^MODIS_", target$site_id)])

  theme <- "coastal"

  # Loop over requested modes (default: both)
  for (mode in source_mode) {

    sites <- if (mode == "buoy_only") buoy_sites else modis_sites

    if (length(sites) == 0) {
      message("No sites found for ", mode, "; skipping.")
      next
    }

    forecast <- purrr::map_dfr(
      vars,
      run_all_vars,
      sites = sites,
      forecast_model = forecast_model,
      noaa_past_mean = noaa_past_mean,
      noaa_future_daily = noaa_future_daily,
      target = target,
      horiz = horiz,
      step = step,
      theme = theme,
      forecast_date = forecast_date
    )

    if (is.null(forecast) || nrow(forecast) == 0) {
      message("No forecasts produced for ", mode, " (likely no usable history); skipping write/submit.")
      next
    }

    forecast <- forecast |>
      dplyr::mutate(duration = "P1D")

    # Different filenames so buoy/modis don't overwrite each other
    forecast_file <- paste0(theme, "-", mode, "-", forecast_date, "-", model_id, ".csv.gz")

    readr::write_csv(
      forecast |> dplyr::mutate(project_id = "bu4cast"),
      forecast_file
    )

    neon4cast::submit(forecast_file = forecast_file, metadata = NULL, ask = FALSE)

    message("Wrote + submitted: ", forecast_file)
  }

  invisible(NULL)
}
