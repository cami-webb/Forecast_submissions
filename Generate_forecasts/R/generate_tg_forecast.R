generate_tg_forecast <- function(forecast_date,
                                 forecast_model,
                                 model_themes = "coastal",
                                 model_id = model_id,
                                 all_sites = FALSE,
                                 noaa = FALSE,
                                 vars_manual = NULL,
                                 target_path = NULL,
                                 source_mode = c("buoy", "modis", "cci")) {

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

  target_raw <- arrow::read_csv_arrow(s3_read$path(target_path))

  target <- target_raw |>
    dplyr::mutate(datetime = lubridate::as_date(substr(datetime, 1, 10))) |>
    dplyr::select(dplyr::any_of(c("datetime", "site_id", "variable", "duration", "observation")))
  
  if (!"duration" %in% names(target)) target$duration <- NA_character_

  # Write bucket
  s3_write <- arrow::s3_bucket(
    "bu4cast-ci-write",
    endpoint_override = "https://minio-s3.apps.shift.nerc.mghpcc.org",
    access_key = Sys.getenv("OSN_KEY"),
    secret_key = Sys.getenv("OSN_SECRET"),
    scheme = "https"
  )

    var_slug <- function(x) {
    x |>
      tolower() |>
      gsub("[^a-z0-9]+", "_", x = _) |>
      gsub("^_|_$", "", x = _)
  }
  
  theme_default_vars <- function(theme) {
    if (!is.null(vars_manual)) return(vars_manual)
  
    if (identical(theme, "coastal")) return("chlorophyll")
  
    if (identical(theme, "urban")) {
      return(c(
        "NO2 - Hourly",
        "O3",
        "PM2.5 - Daily",
        "PM10 - Daily",
        "PM2.5 - Hourly",
        "PM10 - Hourly"
      ))
    }
  
    stop("Unknown theme: ", theme)
  }
  
  duration_settings <- function(duration) {
    duration <- as.character(duration)
    if (duration == "P1D")  return(list(horiz = 30, step = 1))
    if (duration == "PT1H") return(list(horiz = 48, step = 1))  # default: 48 hours
    return(list(horiz = 30, step = 1))
  }

  # Identify buoy vs modis site_ids
  buoy_sites  <- unique(target$site_id[target$site_id == "UNH_buoy"])
  modis_sites <- unique(target$site_id[target$site_id == "MODIS"])
  cci_sites   <- unique(target$site_id[target$site_id == "CCI"])

  # Loop over themes
  for (theme in model_themes) {
    vars <- theme_default_vars(theme)
    modes_to_run <- if (identical(theme, "coastal")) source_mode else "urban"
  
    # Loop over requested source modes (default: both)
    for (mode in modes_to_run) {

            if (identical(theme, "coastal")) {
        if (mode == "buoy") {
          sites <- buoy_sites
        } else if (mode == "modis") {
          sites <- modis_sites
        } else if (mode == "cci") {
          sites <- cci_sites
        } else {
          stop("Unknown source_mode: ", mode)
        }
      } else {
        sites <- unique(target$site_id)
      }

      if (length(sites) == 0) {
        message("No sites found for theme=", theme, " mode=", mode, "; skipping.")
        next
      }

      forecast <- purrr::map_dfr(vars, function(v) {

        dur <- target |>
          dplyr::filter(.data$variable == v) |>
          dplyr::pull(.data$duration)
      
        dur <- dur[!is.na(dur) & nzchar(dur)]
        dur <- if (length(dur) == 0) NA_character_ else dur[1]
      
        hs <- duration_settings(dur)
      
        run_all_vars(
          var = v,
          sites = sites,
          forecast_model = forecast_model,
          noaa_past_mean = noaa_past_mean,
          noaa_future_daily = noaa_future_daily,
          target = target,
          horiz = hs$horiz,
          step = hs$step,
          theme = theme,
          forecast_date = forecast_date,
          model_id = model_id
        )
      })

      if (is.null(forecast) || nrow(forecast) == 0) {
        message("No forecasts produced for theme=", theme, " mode=", mode, "; skipping write.")
        next
      }

            # Add required columns + rename variable to chlorophyll_<DATA>
      forecast_out <- forecast |>
        dplyr::mutate(
          reference_datetime = as.Date(reference_datetime),  # already is, but keep explicit
          datetime           = as.Date(datetime),
          depth              = 1,
          family             = as.character("normal"),
          parameter          = as.character(parameter),
          obs_flag           = 0L,
          variable           = as.character(variable),
          prediction         = as.numeric(prediction)
        ) |>
        # keep ONLY required cols, in required order
        dplyr::select(
          reference_datetime,
          datetime,
          depth,
          family,
          parameter,
          obs_flag,
          variable,
          prediction
        )

      # Write to bucket
        vars_in_file <- unique(forecast_out$variable)
        var_tag <- if (length(vars_in_file) == 1) paste0("-var=", var_slug(vars_in_file)) else "-var=multi"
        
        forecast_key <- paste0(
          "challenges/forecasts/project_id=bu4cast/",
          theme, "-", mode, var_tag, "-", forecast_date, "-", model_id, ".csv"
        )
      
      arrow::write_csv_arrow(forecast_out, sink = s3_write$path(forecast_key))
      message("Wrote forecast to S3: ", forecast_key)
    }
  }

  invisible(NULL)
}
