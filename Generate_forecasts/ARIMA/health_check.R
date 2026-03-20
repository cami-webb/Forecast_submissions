library(yaml)
config <- yaml::read_yaml("Generate_forecasts/config.yaml")

message("Pinging health check...")
tryCatch(
  RCurl::getURL(config$health_check_url),
  error = function(e) message("Health check ping failed: ", e$message)
)
message("ARIMA forecasts complete! :)")
