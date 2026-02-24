message("Pinging health check...")
tryCatch(
  RCurl::getURL("https://hc-ping.com/1088c2cf-3fb1-43da-9ff5-d2574bdac32b"),
  error = function(e) message("Health check ping failed: ", e$message)
)

message("ARIMA forecasts complete! :)")
