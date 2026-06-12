# tg_lm_single: single GEFS-variable linear models
# Supports: tg_dlwrf_lm, tg_dswrf_lm, tg_temp_lm,
#           tg_ugrd_lm, tg_vgrd_lm, tg_wspd_lm, tg_humidity_lm
# pmax(0, ...) applied to all predictions

MET_VAR_MAP <- c(
  "dlwrf"    = "surface_downwelling_longwave_flux_in_air",
  "dswrf"    = "surface_downwelling_shortwave_flux_in_air",
  "temp"     = "air_temperature",
  "ugrd"     = "eastward_wind",
  "vgrd"     = "northward_wind",
  "wspd"     = "wind_speed",       # computed from eastward + northward wind
  "humidity" = "relative_humidity"
)

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

  # Determine met variable from model_id (e.g. "tg_dlwrf_lm" -> "dlwrf")
  key     <- gsub("^tg_|_lm$", "", model_id)
  met_var <- MET_VAR_MAP[[key]]
  if (is.null(met_var) || is.na(met_var)) {
    message("Unknown met variable for model_id: ", model_id, " — skipping.")
    return(NULL)
  }

  message(paste0("Running site: ", site, " | predictor: ", met_var))

  # Compute wind speed from components if needed
  if (met_var == "wind_speed") {
    wind_cols <- c("eastward_wind", "northward_wind")
    if (!is.null(noaa_past_mean) && all(wind_cols %in% names(noaa_past_mean))) {
      noaa_past_mean <- noaa_past_mean |>
        dplyr::mutate(wind_speed = sqrt(eastward_wind^2 + northward_wind^2))
    }
    if (!is.null(noaa_future_daily) && all(wind_cols %in% names(noaa_future_daily))) {
      noaa_future_daily <- noaa_future_daily |>
        dplyr::mutate(wind_speed = sqrt(eastward_wind^2 + northward_wind^2))
    }
  }

  # Join targets with historical met
  site_target <- target |>
    dplyr::select(datetime, site_id, variable, observation) |>
    dplyr::mutate(site_id = as.character(site_id)) |>
    dplyr::filter(variable %in% c(target_variable),
                  site_id == site,
                  datetime < forecast_date) |>
    tidyr::pivot_wider(names_from = "variable", values_from = "observation") |>
    dplyr::left_join(noaa_past_mean |> dplyr::filter(site_id == site),
                     by = c("datetime", "site_id"))

  if (!target_variable %in% names(site_target)) {
    message("No target observations at site ", site, ". Skipping.")
    return(NULL)
  }
  if (!met_var %in% names(site_target)) {
    message("Met variable '", met_var, "' not available at site ", site, ". Skipping.")
    return(NULL)
  }

  fit_data <- site_target |>
    dplyr::select(dplyr::all_of(c(target_variable, met_var))) |>
    na.omit()

  if (nrow(fit_data) < 10) {
    message("Fewer than 10 overlapping observations at site ", site, ". Skipping.")
    return(NULL)
  }

  # Fit linear model: target ~ met_var
  fit <- lm(as.formula(paste0("`", target_variable, "` ~ `", met_var, "`")),
            data = fit_data)

  # Get future GEFS ensemble at the site
  noaa_future <- noaa_future_daily |> dplyr::filter(site_id == site)

  if (!met_var %in% names(noaa_future)) {
    message("Met variable '", met_var, "' not in future data at site ", site, ". Skipping.")
    return(NULL)
  }

  # Predict, clip to non-negative
  pred_input <- noaa_future |> dplyr::select(dplyr::all_of(met_var))
  forecast <- noaa_future |>
    dplyr::mutate(
      site_id    = site,
      prediction = pmax(0, predict(fit, pred_input)),
      variable   = target_variable
    )

  # Format to EFI standard
  forecast |>
    dplyr::mutate(
      reference_datetime = forecast_date,
      family             = "ensemble",
      model_id           = model_id
    ) |>
    dplyr::select(model_id, datetime, reference_datetime,
                  site_id, family, parameter, variable, prediction)
}
