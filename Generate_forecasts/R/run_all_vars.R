#Quick function to repeat for all variables
run_all_vars = function(var,
                        sites,
                        forecast_model,
                        noaa_past_mean,
                        noaa_future_daily,
                        target,
                        horiz,
                        step,
                        theme,
                        forecast_date,
                        model_id) {
  
message(
  paste0(
    "Running theme=", theme,
    " var=\"", var, "\"",
    " horiz=", horiz,
    " step=", step
  )
)

  purrr::map_dfr(
  sites,
  forecast_model,
  noaa_past_mean = noaa_past_mean,
  noaa_future_daily = noaa_future_daily,
  target_variable = var,
  target = target,
  horiz = horiz,
  step = step,
  theme = theme,
  forecast_date = forecast_date,
  model_id = model_id
) |>
  dplyr::mutate(variable = var)
}
