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

  site_target <- target |>
    dplyr::select(datetime, site_id, variable, observation) |>
    dplyr::filter(
      site_id == site,
      variable == target_variable,
      datetime < as.Date(forecast_date)
    ) |>
    dplyr::mutate(datetime = as.Date(datetime)) |>
    dplyr::filter(!is.na(datetime)) |>
    dplyr::group_by(datetime) |>
    dplyr::summarise(observation = mean(observation, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(datetime)

  if (nrow(site_target) == 0 || all(is.na(site_target$observation))) {
    message("No target observations at site ", site, " for ", target_variable, "; skipping.")
    return(NULL)
  }

  # Fill missing dates to daily grid
  site_target <- site_target |>
    tidyr::complete(datetime = seq.Date(min(datetime), max(datetime), by = "day")) |>
    dplyr::arrange(datetime)

  y <- as.numeric(site_target$observation)

  if (sum(is.finite(y)) < 5) {
    message("Not enough non-missing observations at site ", site, "; skipping.")
    return(NULL)
  }

  # Seasonal ARIMA (uncomment if doing seasonal)
  # y_ts <- ts(y, frequency = 365)

  if (sum(y < 0, na.rm = TRUE) > 0) {
    fit <- forecast::auto.arima(y)
    # fit <- forecast::auto.arima(y_ts) SEASONAL
  } else {
    fit <- forecast::auto.arima(y, lambda = "auto")
    # fit <- forecast::auto.arima(y_ts, lambda = "auto") SEASONAL
  }

  last_dt <- max(site_target$datetime[is.finite(y)], na.rm = TRUE)
  h <- as.integer(as.Date(forecast_date) - last_dt + horiz)

  if (!is.finite(h) || h <= 0) {
    message("Computed forecast horizon <= 0 for site ", site, "; skipping.")
    return(NULL)
  }

  fc <- as.data.frame(forecast::forecast(fit, h = h, level = 0.68)) |>
    dplyr::mutate(sigma = `Hi 68` - `Point Forecast`)

  tibble::tibble(
    datetime = seq.Date(from = last_dt + 1, by = "day", length.out = h),
    reference_datetime = as.Date(forecast_date),
    site_id = site,
    family = "normal",
    variable = target_variable,
    mu = as.numeric(fc$`Point Forecast`),
    sigma = as.numeric(fc$sigma),
    model_id = "tg_arima"
  ) |>
    tidyr::pivot_longer(c(mu, sigma), names_to = "parameter", values_to = "prediction") |>
    dplyr::select(model_id, datetime, reference_datetime, site_id, family, parameter, variable, prediction)
}
