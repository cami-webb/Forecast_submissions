library(dplyr)
rerun_forecasts <- function(model_id,
                            forecast_model,
                            model_themes,
                            END,
                            noaa = FALSE,
                            all_sites = FALSE,
                            source_modes = c("buoy", "cci"),
                            nowcast_only = FALSE) {

  config <- yaml::read_yaml("Generate_forecasts/config.yaml")

  if (Sys.getenv("AWS_ACCESS_KEY_ID") == "" && Sys.getenv("OSN_KEY") != "") {
    Sys.setenv(
      AWS_ACCESS_KEY_ID     = Sys.getenv("OSN_KEY"),
      AWS_SECRET_ACCESS_KEY = Sys.getenv("OSN_SECRET")
    )
  }
  Sys.setenv("AWS_S3_FORCE_PATH_STYLE" = "true")

  submissions <- aws.s3::get_bucket_df(
    bucket   = config$s3_bucket_write,
    prefix   = paste0(config$forecasts_path, "/"),
    base_url = gsub("https://", "", config$endpoint),
    region   = "",
    max      = Inf
  )

  end_date   <- lubridate::as_date(Sys.Date() - lubridate::days(2))
  start_date <- end_date - lubridate::days(config$backfill_days)
  this_year  <- data.frame(date = seq.Date(start_date, end_date, by = "day"))

  model_themes <- as.character(model_themes)
  source_modes <- as.character(source_modes)

  # One file per theme per day; coastal and urban each produce theme-date-model_id.csv
  for (theme in model_themes) {
    colname <- theme
    this_year[[colname]] <- FALSE
    for (i in seq_len(nrow(this_year))) {
      date_str       <- lubridate::as_date(this_year$date[i])
      expected_file  <- paste0(theme, "-", date_str, "-", model_id, "-corrected.csv")
      this_year[[colname]][i] <- nrow(
        dplyr::filter(submissions, stringr::str_detect(Key, stringr::fixed(expected_file)))
      ) > 0
    }
  }

  # Any day missing a file for any theme
  missed_dates <- this_year[
    rowSums(this_year[, model_themes, drop = FALSE]) != length(model_themes),
    ,
    drop = FALSE
  ]

  if (nrow(missed_dates) == 0) {
    message("No missed dates detected for the requested themes.")
    return(invisible(NULL))
  }

  # Identify which themes are missing for each missed date
  missed_dates$themes <- lapply(seq_len(nrow(missed_dates)), function(i) {
    model_themes[!unlist(missed_dates[i, model_themes, drop = TRUE])]
  })

  for (i in seq_len(nrow(missed_dates))) {
    forecast_date   <- lubridate::as_date(missed_dates$date[[i]])
    forecast_themes <- missed_dates$themes[[i]]
    if (length(forecast_themes) == 0) next
    message(
      "Running forecasts for: ", forecast_date,
      ".\nThemes: ", paste0(forecast_themes, collapse = ", "), "."
    )
    tryCatch({
      generate_tg_forecast(
          forecast_date  = forecast_date,
          forecast_model = forecast_model,
          model_themes   = forecast_themes,
          model_id       = model_id,
          all_sites      = all_sites,
          noaa           = FALSE,
          source_mode    = source_modes,
          nowcast_only   = nowcast_only
            )
    }, error = function(e) {
      cat("ERROR with forecast generation:\n", conditionMessage(e), "\n")
    })
  }

  invisible(NULL)
}
