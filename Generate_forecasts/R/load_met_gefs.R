load_met_gefs <- function(sites, forecast_date, config) {
  
  s3_read <- arrow::s3_bucket(
    config$s3_bucket_read,
    endpoint_override = config$endpoint,
    access_key        = Sys.getenv("OSN_KEY"),
    secret_key        = Sys.getenv("OSN_SECRET"),
    scheme            = "https"
  )
  
  drivers_path <- paste0("challenges/project_id=", config$project_id, "/drivers")
  
  # Stage 3 = historical met, averaged across ensemble members for LM fitting
  # Load all sites in one scan instead of one at a time
  message("Loading stage3 met data for ", length(sites), " sites...")
  noaa_past_mean <- tryCatch({
    arrow::open_dataset(s3_read$path(paste0(drivers_path, "/stage3"))) |>
      dplyr::filter(site_id %in% sites) |>
      dplyr::collect() |>
      dplyr::mutate(datetime = as.Date(datetime)) |>
      dplyr::group_by(datetime, site_id, variable) |>
      dplyr::summarise(prediction = mean(prediction, na.rm = TRUE), .groups = "drop") |>
      tidyr::pivot_wider(names_from = variable, values_from = prediction)
  }, error = function(e) {
    message("Error loading stage3 met: ", conditionMessage(e))
    return(NULL)
  })
  message("Stage3 met loaded.")
  
  # Stage 2 = future forecast met
  message("Loading stage2 met data for ", length(sites), " sites...")
  noaa_future_daily <- tryCatch({
    
    # Try forecast_date, then walk back up to 30 days to find available stage2 data
    ds <- NULL
    attempt_date <- as.Date(forecast_date)
    for (days_back in 0:30) {
      attempt_date <- as.Date(forecast_date) - days_back
      candidate <- arrow::open_dataset(s3_read$path(paste0(drivers_path, "/stage2"))) |>
        dplyr::filter(
          site_id %in% sites,
          as.Date(datetime) >= attempt_date
        ) |>
        dplyr::collect()
      if (nrow(candidate) > 0) {
        ds <- candidate
        if (days_back > 0) message("No stage2 data for ", forecast_date, ", using data from ", attempt_date)
        break
      }
    }
    
    if (is.null(ds) || nrow(ds) == 0) return(NULL)
    
    latest_ref <- max(as.Date(ds$reference_datetime), na.rm = TRUE)
    
    ds |>
      dplyr::filter(as.Date(reference_datetime) == latest_ref) |>
      dplyr::mutate(datetime = as.Date(datetime)) |>
      dplyr::group_by(datetime, site_id, parameter, variable) |>
      dplyr::summarise(prediction = mean(prediction, na.rm = TRUE), .groups = "drop") |>
      tidyr::pivot_wider(names_from = variable, values_from = prediction)
      
  }, error = function(e) {
    message("Error loading stage2 met: ", conditionMessage(e))
    return(NULL)
  })
  message("Stage2 met loaded.")
  
  list(
    noaa_past_mean    = noaa_past_mean,
    noaa_future_daily = noaa_future_daily
  )
}
