END <- as_date("2006-01-01") 

rerun_forecasts <- function(model_id,
                            forecast_model,
                            model_themes,
                            END,
                            noaa = FALSE,
                            all_sites = FALSE,
                            source_modes = c("buoy", "modis", "cci")) {

  # Dates of forecasts
  end_date <- paste(Sys.Date() - days(2), "00:00:00") # yesterday's forecasts might not be processed yet

  # Get all the submissions
  if (Sys.getenv("AWS_ACCESS_KEY_ID") == "" && Sys.getenv("OSN_KEY") != "") {
    Sys.setenv(
      AWS_ACCESS_KEY_ID = Sys.getenv("OSN_KEY"),
      AWS_SECRET_ACCESS_KEY = Sys.getenv("OSN_SECRET")
    )
  }

#  submissions <- aws.s3::get_bucket_df(
#    bucket = "bu4cast-ci-write",
#    prefix = "challenges/forecasts/", 
#    base_url = "minio-s3.apps.shift.nerc.mghpcc.org",
#    region = "us-east-1",
#    max = Inf
#  )
### ADDED
  # Ensure creds available (same pattern you use elsewhere)
if (Sys.getenv("AWS_ACCESS_KEY_ID") == "" && Sys.getenv("OSN_KEY") != "") {
  Sys.setenv(
    AWS_ACCESS_KEY_ID = Sys.getenv("OSN_KEY"),
    AWS_SECRET_ACCESS_KEY = Sys.getenv("OSN_SECRET")
  )
}

s3_write <- arrow::s3_bucket(
  "bu4cast-ci-write",
  endpoint_override = "https://minio-s3.apps.shift.nerc.mghpcc.org",
  access_key = Sys.getenv("OSN_KEY"),
  secret_key = Sys.getenv("OSN_SECRET"),
  scheme = "https"
)

# List existing forecast files under the prefix
submissions <- arrow::FileSystemDatasetFactory$create(
  s3_write$path("challenges/forecasts/")
)$Finish() %>%
  dplyr::collect() %>%
  dplyr::transmute(
    # arrow listing gives you file paths; standardize to Key-like strings
    Key = file,
    # Arrow doesn't provide LastModified consistently; you can treat "exists" as OK
    LastModified = as_datetime(NA)
  )
  # ADDED END
  
  # For each theme, check if file is in bucket
  this_year <- data.frame(
    date = as.character(paste0(
      seq.Date(as_date("2024-01-01"), to = as_date(end_date), by = "day"),
      " 00:00:00"
    ))
  )

  model_themes <- as.character(model_themes)
  source_modes <- as.character(source_modes)

  # For each theme + source_mode, check existence in bucket
  for (theme in model_themes) {
    for (sm in source_modes) {

      colname <- paste0(theme, "__", sm)
      this_year[[colname]] <- FALSE

      for (i in seq_len(nrow(this_year))) {

        # Must match generate_tg_forecast() naming
        forecast_file <- paste0(theme, "-", sm, "-", as_date(this_year$date[i]), "-", model_id, ".csv.gz")

        hit <- dplyr::filter(submissions, stringr::str_detect(Key, forecast_file))

        if (nrow(hit) == 0) {
          this_year[[colname]][i] <- FALSE
        } else {
          modified <- max(as_datetime(hit$LastModified))
          this_year[[colname]][i] <- (modified >= END)
        }
      }
    }
  }

  # Figure out which (date, theme) pairs are missing ANY required source_mode file
  check_cols <- unlist(lapply(model_themes, function(th) paste0(th, "__", source_modes)), use.names = FALSE)

  missed_dates <- this_year

  # Any day that does not have all required (theme x source_mode) files
  missed_dates <- missed_dates[
    rowSums(missed_dates[, check_cols, drop = FALSE]) != length(check_cols),
    ,
    drop = FALSE
  ]

  if (nrow(missed_dates) == 0) {
    message("No missed dates detected for the requested themes/source_modes.")
    return(invisible(NULL))
  }

  # For each missed date, compute which themes are missing anything, rerun the whole theme for that date
  missed_dates$themes <- lapply(seq_len(nrow(missed_dates)), function(i) {
    missing_themes <- character(0)

    for (theme in model_themes) {
      theme_cols <- paste0(theme, "__", source_modes)
      ok <- all(missed_dates[i, theme_cols, drop = TRUE])
      if (!ok) missing_themes <- c(missing_themes, theme)
    }

    missing_themes
  })

  for (i in seq_len(nrow(missed_dates))) {

    forecast_date <- as_date(missed_dates$date[[i]])  # tiny tweak vs as.Date()
    forecast_themes <- missed_dates$themes[[i]]

    if (length(forecast_themes) == 0) next

    message(
      "Running forecasts for: ", forecast_date,
      ".\nThemes: ", paste0(forecast_themes, collapse = ", "),
      ".\nSources: ", paste0(source_modes, collapse = ", "), "."
    )

    tryCatch({
      generate_tg_forecast(
        forecast_date = forecast_date,
        forecast_model = forecast_model,
        model_themes = forecast_themes,
        model_id = model_id,
        all_sites = all_sites,
        noaa = FALSE,                 # force NOAA off like you want
        source_mode = source_modes
      )
    }, error = function(e) {
      cat("ERROR with forecast generation:\n", conditionMessage(e), "\n")
    })
  }

  invisible(NULL)
}
