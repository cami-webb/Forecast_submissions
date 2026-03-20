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
  noaa_past_mean <- purrr::map_dfr(sites, function(site) {
    tryCatch({
      arrow::open_dataset(s3_read$path(paste0(drivers_path, "/stage3"))) |>
        dplyr::filter(site_id == site) |>
        dplyr::collect() |>
        dplyr::mutate(datetime = as.Date(datetime)) |>
        dplyr::group_by(datetime, site_id, variable) |>
        dplyr::summarise(prediction = mean(prediction, na.rm = TRUE), .groups = "drop") |>
        tidyr::pivot_wider(names_from = variable, values_from = prediction)
    }, error = function(e) {
      message("No stage3 met for site ", site, ": ", conditionMessage(e))
      return(NULL)
    })
  })
  
  # Stage 2 = future forecast met, use most recent reference_datetime available
  # Keep ensemble members separate for ensemble forecasting
  noaa_future_daily <- purrr::map_dfr(sites, function(site) {
    tryCatch({
      ds <- arrow::open_dataset(s3_read$path(paste0(drivers_path, "/stage2"))) |>
        dplyr::filter(site_id == site) |>
        dplyr::collect()
      
      if (nrow(ds) == 0) return(NULL)
      
      # use most recent reference_datetime
      latest_ref <- max(as.Date(ds$reference_datetime), na.rm = TRUE)
      
      ds |>
        dplyr::filter(
          as.Date(reference_datetime) == latest_ref,
          as.Date(datetime) >= as.Date(forecast_date)
        ) |>
        dplyr::mutate(datetime = as.Date(datetime)) |>
        dplyr::group_by(datetime, site_id, parameter, variable) |>
        dplyr::summarise(prediction = mean(prediction, na.rm = TRUE), .groups = "drop") |>
        tidyr::pivot_wider(names_from = variable, values_from = prediction)
    }, error = function(e) {
      message("No stage2 met for site ", site, ": ", conditionMessage(e))
      return(NULL)
    })
  })
  
  list(
    noaa_past_mean    = noaa_past_mean,
    noaa_future_daily = noaa_future_daily
  )
}
