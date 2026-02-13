generate_tg_forecast <- function(forecast_date,
                                 forecast_model,
                                 model_themes = "coastal",
                                 model_id = model_id,
                                 all_sites = FALSE,
                                 noaa = FALSE,
                                 vars_manual = NULL,
                                 target_path = NULL,
                                 source_mode = c("buoy", "modis")) {

  forecast_date <- as.Date(forecast_date)
  source_mode <- match.arg(source_mode, several.ok = TRUE)

  # NOAA drivers (not used for ARIMA; keeping in case I add them later!)
  if (isTRUE(noaa)) {
    load_met(forecast_date)
    noaa_future_daily <- read.csv(
      paste0("./Generate_forecasts/noaa_downloads/noaa_future_daily_", forecast_date, ".csv")
    ) |>
      dplyr::mutate(datetime = lubridate::as_date(datetime))

    noaa_past_mean <- read.csv(
      paste0("./Generate_forecasts/noaa_downloads/noaa_past_mean_", forecast_date, ".csv")
    ) |>
      dplyr::mutate(datetime = lubridate::as_date(datetime))
  } else {
    noaa_future_daily <- NULL
    noaa_past_mean <- NULL
  }

  # Targets path (allow passing in for urban vs coastal)
  if (is.null(target_path)) {
    target_path <- paste0(
      "challenges/targets/project_id=bu4cast/",
      model_themes[1],
      "-targets.csv"
    )
  }

  # Read targets bucket
  s3_read <- arrow::s3_bucket(
    "bu4cast-ci-read",
    endpoint_override = "https://minio-s3.apps.shift.nerc.mghpcc.org",
    access_key = Sys.getenv("OSN_KEY"),
    secret_key = Sys.getenv("OSN_SECRET"),
    scheme = "https"
  )

  target <- arrow::read_csv_arrow(s3_read$path(target_path)) |>
    dplyr::mutate(datetime = lubridate::as_date(substr(datetime, 1, 10))) |>
    dplyr::select(datetime, site_id, variable, observation)

  # Write bucket
  s3_write <- arrow::s3_bucket(
    "bu4cast-ci-write",
    endpoint_override = "https://minio-s3.apps.shift.nerc.mghpcc.org",
    access_key = Sys.getenv("OSN_KEY"),
    secret_key = Sys.getenv("OSN_SECRET"),
    scheme = "https"
  )

  # Target variables
  vars <- if (is.null(vars_manual)) "chlorophyll" else vars_manual

  # Forecast horizon (daily)
  horiz <- 30
  step <- 1

  # Identify buoy vs modis site_ids
  buoy_sites  <- unique(target$site_id[grepl("^UNH_buoy_", target$site_id)])
  modis_sites <- unique(target$site_id[grepl("^MODIS_", target$site_id)])

  # Loop over themes
  for (theme in model_themes) {

    # Loop over requested source modes (default: both)
    for (mode in source_mode) {

      if (mode == "buoy") {
        sites <- buoy_sites
      } else if (mode == "modis") {
        sites <- modis_sites
      } else {
        stop("Unknown source_mode: ", mode)
      }

      if (length(sites) == 0) {
        message("No sites found for theme=", theme, " mode=", mode, "; skipping.")
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
        message("No forecasts produced for theme=", theme, " mode=", mode, "; skipping write.")
        next
      }

      forecast <- forecast |>
        dplyr::mutate(
          duration = "P1D",
          project_id = "bu4cast"
        )

      # Write to bucket
      forecast_key <- paste0(
        "challenges/forecasts/project_id=bu4cast/",
        theme, "-", mode, "-", forecast_date, "-", model_id, ".csv.gz"
      )

      # write_csv_arrow handles compression by filename extension
      arrow::write_csv_arrow(forecast, sink = s3_write$path(forecast_key))

      message("Wrote forecast to S3: ", forecast_key)
    }
  }

  invisible(NULL)
}
