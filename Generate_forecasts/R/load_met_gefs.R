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

  message("Loading stage2 met data for ", length(sites), " sites...")
  noaa_future_daily <- tryCatch({

    ds <- arrow::open_dataset(s3_read$path(paste0(drivers_path, "/stage2"))) |>
      dplyr::filter(
        site_id %in% sites,
        as.Date(reference_datetime) == as.Date(forecast_date)
      ) |>
      dplyr::collect()

    if (is.null(ds) || nrow(ds) == 0) {
      NULL
    } else {
      ds |>
        dplyr::filter(as.Date(datetime) >= as.Date(forecast_date)) |>
        dplyr::mutate(datetime = as.Date(datetime)) |>
        dplyr::group_by(datetime, site_id, parameter, variable) |>
        dplyr::summarise(prediction = mean(prediction, na.rm = TRUE), .groups = "drop") |>
        tidyr::pivot_wider(names_from = variable, values_from = prediction)
    }

  }, error = function(e) {
    message("Error loading stage2 met: ", conditionMessage(e))
    NULL
  })

  if (is.null(noaa_future_daily) && !is.null(noaa_past_mean)) {
    has_later_stage2 <- tryCatch({
      arrow::open_dataset(s3_read$path(paste0(drivers_path, "/stage2"))) |>
        dplyr::filter(
          site_id == sites[1],
          as.Date(reference_datetime) > as.Date(forecast_date)
        ) |>
        dplyr::select(reference_datetime) |>
        head(1) |>
        dplyr::collect() |>
        nrow() > 0
    }, error = function(e) FALSE)

    if (has_later_stage2) {
      message("No stage2 data for ", forecast_date, " but later data exists - falling back to stage3 as pseudo-future met.")
      noaa_future_daily <- noaa_past_mean |>
        dplyr::filter(datetime >= as.Date(forecast_date)) |>
        dplyr::mutate(parameter = "0")
    } else {
      message("No stage2 data for ", forecast_date, " and no later data available - skipping (data not yet released).")
    }
  }

  message("Stage2 met loaded.")
  
  list(
    noaa_past_mean    = noaa_past_mean,
    noaa_future_daily = noaa_future_daily
  )
}
