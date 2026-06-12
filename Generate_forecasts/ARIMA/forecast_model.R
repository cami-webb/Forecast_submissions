forecast_model <- function(site,
                           noaa_past_mean,
                           noaa_future_daily,
                           target_variable,
                           target,
                           horiz,
                           step,
                           theme,
                           forecast_date,
                           model_id) {

  message("Running site: ", site)

  # Wide initial pull — we'll trim to the right lookback once we know the duration
  site_target_raw <- target |>
    dplyr::select(dplyr::any_of(c("datetime", "site_id", "variable", "duration", "observation"))) |>
    dplyr::filter(
      site_id == site,
      variable == target_variable,
      datetime < as.POSIXct(as.Date(forecast_date), tz = "UTC"),
      datetime >= as.POSIXct(as.Date(forecast_date) - 365*3, tz = "UTC")
    )

  # Detect duration early so we know whether to collapse to daily
  dur <- site_target_raw |> dplyr::pull(duration)
  dur <- dur[!is.na(dur) & nzchar(dur)]
  dur <- if (length(dur) == 0) NA_character_ else dur[1]
  is_hourly <- identical(dur, "PT1H")

  # Trim to appropriate lookback window:
  #   hourly → 90 days  (keeps series short enough for auto.arima to be fast)
  #   daily  → 365 days (one full seasonal cycle)
  lookback_days <- if (is_hourly) 90L else 365L
  site_target_raw <- site_target_raw |>
    dplyr::filter(
      datetime >= as.POSIXct(as.Date(forecast_date) - lookback_days, tz = "UTC")
    )
  
  if (is_hourly) {
    site_target <- site_target_raw |>
      dplyr::mutate(datetime = as.POSIXct(datetime, tz = "UTC")) |>
      dplyr::filter(!is.na(datetime)) |>
      dplyr::group_by(datetime) |>
      dplyr::summarise(observation = mean(observation, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(datetime)
  } else {
    site_target <- site_target_raw |>
      dplyr::mutate(datetime = as.Date(datetime)) |>
      dplyr::filter(!is.na(datetime)) |>
      dplyr::group_by(datetime) |>
      dplyr::summarise(observation = mean(observation, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(datetime)
  }

  if (nrow(site_target) == 0 || all(is.na(site_target$observation))) {
      message("No target observations at site ", site, " for ", target_variable, "; skipping.")
      return(NULL)
    }
    
    # Build a regular time grid
    if (is_hourly) {
      # Expect datetime to already represent an hourly timestamp.
      # If your targets only store dates (not datetimes), hourly won’t be possible without changing targets.
      site_target <- site_target |>
        dplyr::mutate(datetime = as.POSIXct(datetime, tz = "UTC")) |>
        dplyr::filter(!is.na(datetime)) |>
        tidyr::complete(datetime = seq.POSIXt(min(datetime), max(datetime), by = "hour")) |>
        dplyr::arrange(datetime)
    } else {
      site_target <- site_target |>
        dplyr::mutate(datetime = as.Date(datetime)) |>
        dplyr::filter(!is.na(datetime)) |>
        tidyr::complete(datetime = seq.Date(min(datetime), max(datetime), by = "day")) |>
        dplyr::arrange(datetime)
    }

  y <- as.numeric(site_target$observation)

  if (sum(is.finite(y)) < 5) {
    message("Not enough non-missing observations at site ", site, "; skipping.")
    return(NULL)
  }

  # Seasonal ARIMA (uncomment if doing seasonal)
    freq <- if (is_hourly) 24 else 365
    y_ts <- ts(y, frequency = freq)
  
  if (sum(y <= 0, na.rm = TRUE) > 0) {
    # fit <- forecast::auto.arima(y) NOT SEASONAL
    fit <- forecast::auto.arima(y_ts) 
  } else {
    # fit <- forecast::auto.arima(y, lambda = "auto") NOT SEASONAL
    fit <- forecast::auto.arima(y_ts, lambda = "auto") 
  }

  last_dt <- max(site_target$datetime[is.finite(y)], na.rm = TRUE)

    if (is_hourly) {
      # forecast_date is a Date; anchor it at midnight UTC for hourly
      ref_dt <- as.POSIXct(as.Date(forecast_date), tz = "UTC")
      h <- as.integer(difftime(ref_dt, last_dt, units = "hours")) + horiz
    } else {
      h <- as.integer(as.Date(forecast_date) - as.Date(last_dt)) + horiz
    }
  
  if (!is.finite(h) || h <= 0) {
    message("Computed forecast horizon <= 0 for site ", site, "; skipping.")
    return(NULL)
  }

  fc <- as.data.frame(forecast::forecast(fit, h = h, level = 0.68)) |>
    dplyr::mutate(sigma = `Hi 68` - `Point Forecast`)

    tibble::tibble(
    datetime = if (is_hourly) {
      seq.POSIXt(from = last_dt + 3600, by = "hour", length.out = h)
    } else {
      seq.Date(from = as.Date(last_dt) + 1, by = "day", length.out = h)
    },
    reference_datetime = as.Date(forecast_date),
    site_id = site,
    family = "normal",
    variable = target_variable,
    mu = pmax(0, as.numeric(fc$`Point Forecast`)),
    sigma = as.numeric(fc$sigma),
    model_id = model_id
  ) |>
    tidyr::pivot_longer(c(mu, sigma), names_to = "parameter", values_to = "prediction") |>
    dplyr::select(model_id, datetime, reference_datetime, site_id, family, parameter, variable, prediction)
}
