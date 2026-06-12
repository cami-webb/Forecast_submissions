# Map model_id to local SCC subfolder name
model_local_folder <- function(model_id) {
  if (grepl("lm_scale", model_id, ignore.case = TRUE)) return("LM_scale")
  if (grepl("lm_all",  model_id, ignore.case = TRUE)) return("LM_all")
  if (grepl("_lm",     model_id, ignore.case = TRUE)) return("LM")
  if (grepl("XGBoost", model_id, ignore.case = TRUE)) return("XGBoost")
  if (grepl("randfor", model_id, ignore.case = TRUE)) return("RF")
  if (grepl("dgam",    model_id, ignore.case = TRUE)) return("dGAM")
  if (grepl("arima",   model_id, ignore.case = TRUE)) return("ARIMA")
  return(model_id)
}

# Write forecast CSV locally; then upload the earliest local file to S3 /submissions
# if no file for this thrust+model exists in either /submissions or /raw-submissions.
write_local_upload <- function(forecast_out, theme, forecast_date, model_id, config) {
  local_dir <- file.path(config$local_forecasts_dir, model_local_folder(model_id))
  dir.create(local_dir, recursive = TRUE, showWarnings = FALSE)

  fname      <- paste0(theme, "-", forecast_date, "-", model_id, ".csv")
  local_path <- file.path(local_dir, fname)
  readr::write_csv(forecast_out, local_path)
  message("Wrote forecast to SCC: ", local_path)

  # Check S3 for existing file for this thrust+model
  tryCatch({
    Sys.setenv(AWS_ACCESS_KEY_ID     = Sys.getenv("OSN_KEY"),
               AWS_SECRET_ACCESS_KEY = Sys.getenv("OSN_SECRET"),
               AWS_S3_FORCE_PATH_STYLE = "true")
    ep      <- gsub("https://", "", config$endpoint)
    bucket  <- config$s3_bucket_write
    pattern <- paste0("^", theme, ".*", model_id)

    sub_keys <- tryCatch(
      aws.s3::get_bucket_df(bucket = bucket, base_url = ep, region = "",
                            prefix = paste0(config$forecasts_path, "/"), max = Inf)$Key,
      error = function(e) character(0)
    )
    raw_keys <- tryCatch(
      aws.s3::get_bucket_df(bucket = bucket, base_url = ep, region = "",
                            prefix = gsub("/submissions", "/raw-submissions", config$forecasts_path),
                            max = Inf)$Key,
      error = function(e) character(0)
    )

    already_exists <- any(grepl(pattern, basename(c(sub_keys, raw_keys))))

    if (already_exists) {
      message("S3 already has a file for ", theme, "/", model_id, " — skipping upload.")
    } else {
      # Upload earliest local file for this thrust+model
      all_local <- list.files(local_dir, pattern = paste0("^", theme, ".*", model_id, "\\.csv$"),
                              full.names = TRUE)
      if (length(all_local) == 0) {
        message("No local files found to upload for ", theme, "/", model_id)
      } else {
        earliest  <- sort(all_local)[1]
        s3_key    <- paste0(config$forecasts_path, "/", basename(earliest))
        aws.s3::put_object(file = earliest, object = s3_key, bucket = bucket,
                           base_url = ep, region = "")
        message("Uploaded to S3 /submissions: ", basename(earliest))
      }
    }
  }, error = function(e) {
    message("S3 check/upload failed: ", e$message)
  })
}

generate_tg_forecast <- function(forecast_date,
                                 forecast_model,
                                 model_themes = "coastal",
                                 model_id = model_id,
                                 all_sites = FALSE,
                                 noaa = FALSE,
                                 vars_manual = NULL,
                                 target_path = NULL,
                                 source_mode = c("buoy", "cci"),
                                 nowcast_only = FALSE) {

  forecast_date <- as.Date(forecast_date)
  model_themes  <- as.character(model_themes)
  config <- yaml::read_yaml("Generate_forecasts/config.yaml")

  # Targets path (allow passing in for urban vs coastal)
  if (is.null(target_path)) {
    target_path <- paste0(
      config$targets_path, "/",
      model_themes[1],
      "-targets.csv"
    )
  }

  s3_read <- arrow::s3_bucket(
    config$s3_bucket_read,
    endpoint_override = config$endpoint,
    access_key = Sys.getenv("OSN_KEY"),
    secret_key = Sys.getenv("OSN_SECRET"),
    scheme = "https"
  )

  target_raw <- arrow::read_csv_arrow(s3_read$path(target_path))
  
  # NOAA drivers (not used for ARIMA; keeping in case I add them later!)
if (isTRUE(noaa)) {
    source("./Generate_forecasts/R/load_met_gefs.R")
    met <- load_met_gefs(
      sites         = unique(target_raw$site_id),
      forecast_date = forecast_date,
      config        = config
    )
    noaa_past_mean    <- met$noaa_past_mean
    noaa_future_daily <- met$noaa_future_daily
    if (is.null(noaa_future_daily)) {
      message("No future met data available for ", forecast_date, " — skipping.")
      return(invisible(NULL))
    }
  } else {
    noaa_future_daily <- NULL
    noaa_past_mean    <- NULL
  }

  # Targets path (allow passing in for urban vs coastal)
  if (is.null(target_path)) {
    target_path <- paste0(
      config$targets_path, "/",
      model_themes[1],
      "-targets.csv"
    )
  }

  target <- target_raw |>
    dplyr::mutate(
      datetime = if (inherits(datetime[1], "POSIXct")) {
        datetime
      } else if (inherits(datetime[1], "Date")) {
        as.POSIXct(datetime, tz = "UTC")
      } else {
        as.POSIXct(lubridate::ymd(datetime), tz = "UTC")
      },
      site_id = as.character(site_id)
    ) |>
    dplyr::select(dplyr::any_of(c("datetime", "site_id", "variable", "duration", "observation")))

    if (!"duration" %in% names(target)) target$duration <- NA_character_

  # Load corrected targets if cci_corrected is in source_mode
  if ("cci_corrected" %in% source_mode && identical(model_themes[1], "coastal")) {
    corrected_raw <- tryCatch(
      arrow::read_csv_arrow(s3_read$path(config$targets_corrected_path)) |>
        dplyr::filter(variable == "chlora_cci_corrected"),
      error = function(e) { message("Could not load corrected targets: ", e$message); NULL }
    )
    if (!is.null(corrected_raw) && nrow(corrected_raw) > 0) {
      corrected_target <- corrected_raw |>
        dplyr::mutate(
          datetime = if (inherits(datetime[1], "POSIXct")) {
            datetime
          } else if (inherits(datetime[1], "Date")) {
            as.POSIXct(datetime, tz = "UTC")
          } else {
            as.POSIXct(lubridate::ymd(datetime), tz = "UTC")
          },
          site_id = as.character(site_id)
        ) |>
        dplyr::select(dplyr::any_of(c("datetime", "site_id", "variable", "duration", "observation")))
      if (!"duration" %in% names(corrected_target)) corrected_target$duration <- NA_character_
      target <- dplyr::bind_rows(target, corrected_target)
    }
  }


  s3_write <- arrow::s3_bucket(
    config$s3_bucket_write,
    endpoint_override = config$endpoint,
    access_key = Sys.getenv("OSN_KEY"),
    secret_key = Sys.getenv("OSN_SECRET"),
    scheme = "https"
  )

  theme_default_vars <- function(theme) {
    if (!is.null(vars_manual)) return(vars_manual)
    vars <- config$theme_vars[[theme]]
    if (is.null(vars)) stop("Unknown theme: ", theme)
    return(vars)
  }

  duration_settings <- function(duration) {
      if (isTRUE(nowcast_only)) return(list(horiz = 1, step = 1))
      duration <- as.character(duration)
      if (duration == "P1D")  return(list(horiz = config$horizon_P1D,  step = 1))
      if (duration == "PT1H") return(list(horiz = config$horizon_PT1H, step = 1))
      return(list(horiz = config$horizon_P1D, step = 1))
    }

  all_coastal_sites <- unique(target$site_id)

  for (theme in model_themes) {
    vars <- theme_default_vars(theme)

    if (identical(theme, "coastal")) {

      # Restrict to config$sites$coastal whitelist if provided; otherwise use all target sites
      coastal_site_whitelist <- config$sites$coastal

      buoy_sites           <- unique(target$site_id[target$variable == "chlora_buoy"])
      cci_sites            <- unique(target$site_id[target$variable == "chlora_cci"])
      cci_corrected_sites  <- unique(target$site_id[target$variable == "chlora_cci_corrected"])

      if (!is.null(coastal_site_whitelist) && length(coastal_site_whitelist) > 0) {
        buoy_sites          <- intersect(buoy_sites,          coastal_site_whitelist)
        cci_sites           <- intersect(cci_sites,           coastal_site_whitelist)
        cci_corrected_sites <- intersect(cci_corrected_sites, coastal_site_whitelist)
      }
      
      mode_var_sites <- list(
        chlora_buoy          = list(sites = buoy_sites,          var = "chlora_buoy"),
        chlora_cci           = list(sites = cci_sites,           var = "chlora_cci"),
        chlora_cci_corrected = list(sites = cci_corrected_sites, var = "chlora_cci_corrected")
      )
      
      mode_var_sites <- mode_var_sites[
        names(mode_var_sites) %in% paste0("chlora_", source_mode)
      ]

      forecast_all <- purrr::map_dfr(mode_var_sites, function(mv) {
        if (length(mv$sites) == 0) {
          message("No sites for variable ", mv$var, "; skipping.")
          return(NULL)
        }

        dur <- target |>
          dplyr::filter(.data$variable == mv$var) |>
          dplyr::pull(.data$duration)
        dur <- dur[!is.na(dur) & nzchar(dur)]
        dur <- if (length(dur) == 0) NA_character_ else dur[1]
        hs  <- duration_settings(dur)

        run_all_vars(
          var               = mv$var,
          sites             = mv$sites,
          forecast_model    = forecast_model,
          noaa_past_mean    = noaa_past_mean,
          noaa_future_daily = noaa_future_daily,
          target            = target,
          horiz             = hs$horiz,
          step              = hs$step,
          theme             = theme,
          forecast_date     = forecast_date,
          model_id          = model_id
        )
      })

      if (is.null(forecast_all) || nrow(forecast_all) == 0) {
        message("No coastal forecasts produced; skipping write.")
        next
      }

      forecast_out <- forecast_all |>
        dplyr::mutate(
          reference_datetime = as.Date(reference_datetime),
          depth              = 1,
          family             = as.character(family),
          parameter          = as.character(parameter),
          obs_flag           = 0L,
          variable           = as.character(variable),
          prediction         = as.numeric(prediction)
        ) |>
        dplyr::select(
          reference_datetime, datetime, site_id, depth, family,
          parameter, obs_flag, variable, prediction
        )

      write_local_upload(forecast_out, theme, forecast_date, model_id, config)

    } else if (identical(theme, "urban")) {

      forecast_all <- purrr::map_dfr(vars, function(v) {
        dur <- target |>
          dplyr::filter(.data$variable == v) |>
          dplyr::pull(.data$duration)
        dur <- dur[!is.na(dur) & nzchar(dur)]
        dur <- if (length(dur) == 0) NA_character_ else dur[1]
        hs  <- duration_settings(dur)

        run_all_vars(
          var               = v,
          sites             = unique(target$site_id),
          forecast_model    = forecast_model,
          noaa_past_mean    = noaa_past_mean,
          noaa_future_daily = noaa_future_daily,
          target            = target,
          horiz             = hs$horiz,
          step              = hs$step,
          theme             = theme,
          forecast_date     = forecast_date,
          model_id          = model_id
        )
      })

      if (is.null(forecast_all) || nrow(forecast_all) == 0) {
        message("No urban forecasts produced; skipping write.")
        next
      }

      forecast_out <- forecast_all |>
        dplyr::mutate(
          reference_datetime = as.Date(reference_datetime),
          depth              = 1,
          family             = as.character(family),
          parameter          = as.character(parameter),
          obs_flag           = 0L,
          variable           = as.character(variable),
          prediction         = as.numeric(prediction)
        ) |>
        dplyr::select(
          reference_datetime, datetime, site_id, depth, family,
          parameter, obs_flag, variable, prediction
        )

      write_local_upload(forecast_out, theme, forecast_date, model_id, config)

    } else {
      stop("Unknown theme: ", theme)
    }
  }

  invisible(NULL)
}
