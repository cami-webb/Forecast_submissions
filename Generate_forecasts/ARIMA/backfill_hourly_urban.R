library(tidyverse)
library(lubridate)
library(yaml)
library(arrow)

config <- yaml::read_yaml("Generate_forecasts/config.yaml")
source("./Generate_forecasts/ARIMA/forecast_model.R")
source("./Generate_forecasts/R/run_all_vars.R")

Sys.setenv("AWS_DEFAULT_REGION" = "")

LOCAL_DIR   <- file.path(config$local_forecasts_dir, "ARIMA")
HOURLY_VARS <- c("NO2_P1H", "O3", "PM10_P1H", "PM2.5_P1H")

# Load targets from S3 (read-only, needed for model fitting)
s3_read <- arrow::s3_bucket(
  config$s3_bucket_read,
  endpoint_override = config$endpoint,
  access_key        = Sys.getenv("OSN_KEY"),
  secret_key        = Sys.getenv("OSN_SECRET"),
  scheme            = "https"
)

targets_url <- paste0(config$endpoint, "/", config$s3_bucket_read, "/",
                      config$targets_path, "/urban-targets.csv")
target_raw <- readr::read_csv(targets_url, show_col_types = FALSE) |>
  dplyr::mutate(
    datetime = if (inherits(datetime[1], "POSIXct")) datetime
               else as.POSIXct(lubridate::ymd(datetime), tz = "UTC"),
    site_id = as.character(site_id)
  ) |>
  dplyr::select(dplyr::any_of(c("datetime", "site_id", "variable", "duration", "observation")))

if (!"duration" %in% names(target_raw)) target_raw$duration <- NA_character_

urban_sites <- unique(target_raw$site_id)

# Detect horizon for urban
dur_urban <- target_raw |>
  dplyr::filter(!is.na(duration) & nzchar(duration)) |>
  dplyr::pull(duration)
dur_urban <- if (length(dur_urban) == 0) NA_character_ else dur_urban[1]
horiz <- if (!is.na(dur_urban) && dur_urban == "PT1H") config$horizon_PT1H else config$horizon_P1D

# Find urban ARIMA files missing any hourly variable
arima_files <- list.files(LOCAL_DIR, pattern = "urban-.*-tg_arima\\.csv$", full.names = TRUE)
message("Scanning ", length(arima_files), " urban ARIMA files...")

incomplete <- purrr::map_dfr(arima_files, function(f) {
  dat <- tryCatch(
    readr::read_csv(f, show_col_types = FALSE,
                    col_types = readr::cols(parameter = readr::col_character())),
    error = function(e) NULL
  )
  if (is.null(dat) || nrow(dat) == 0) return(NULL)
  vars_present  <- unique(dat$variable)
  missing_hourly <- setdiff(HOURLY_VARS, vars_present)
  if (length(missing_hourly) == 0) return(NULL)
  tibble(
    file          = f,
    date          = as.Date(stringr::str_extract(basename(f), "\\d{4}-\\d{2}-\\d{2}")),
    missing_vars  = list(missing_hourly)
  )
})

if (nrow(incomplete) == 0) {
  message("All urban ARIMA files already have hourly variables — nothing to do.")
  quit(save = "no")
}

message(nrow(incomplete), " files need hourly vars added. Range: ",
        min(incomplete$date), " to ", max(incomplete$date))

for (i in seq_len(nrow(incomplete))) {
  fdate        <- incomplete$date[i]
  fpath        <- incomplete$file[i]
  missing_vars <- incomplete$missing_vars[[i]]

  message("\n[", i, "/", nrow(incomplete), "] ", fdate,
          " — adding: ", paste(missing_vars, collapse = ", "))

  new_rows <- purrr::map_dfr(missing_vars, function(v) {
    tryCatch(
      run_all_vars(
        var               = v,
        sites             = urban_sites,
        forecast_model    = forecast_model,
        noaa_past_mean    = NULL,
        noaa_future_daily = NULL,
        target            = target_raw,
        horiz             = horiz,
        step              = 1,
        theme             = "urban",
        forecast_date     = fdate,
        model_id          = config$models$tg_arima$model_id
      ),
      error = function(e) {
        message("  ERROR on var ", v, ": ", e$message)
        NULL
      }
    )
  })

  if (is.null(new_rows) || nrow(new_rows) == 0) {
    message("  No new rows produced — skipping.")
    next
  }

  # Format to match existing file columns
  new_rows <- new_rows |>
    dplyr::mutate(
      reference_datetime = as.Date(fdate),
      depth              = 1,
      obs_flag           = 0L,
      family             = as.character(family),
      parameter          = as.character(parameter),
      prediction         = as.numeric(prediction)
    ) |>
    dplyr::select(reference_datetime, datetime, site_id, depth, family,
                  parameter, obs_flag, variable, prediction)

  existing <- readr::read_csv(fpath, show_col_types = FALSE,
                               col_types = readr::cols(parameter = readr::col_character()))
  combined <- dplyr::bind_rows(existing, new_rows) |>
    dplyr::arrange(variable, site_id, parameter, datetime)

  readr::write_csv(combined, fpath)
  message("  Saved: ", basename(fpath), " (", nrow(combined), " rows total)")
}

message("\nHourly urban ARIMA backfill complete. Files saved to SCC only.")
