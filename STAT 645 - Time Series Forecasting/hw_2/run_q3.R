suppressPackageStartupMessages({
  library(fpp3)
  library(feasts)
})

cat("==============================================\n")
cat("QUESTION 3 - ACF plot for aus_airpassengers\n")
cat("==============================================\n")

png("acf_aus_airpassengers.png", width=800, height=500)
aus_airpassengers %>%
  ACF(Passengers) %>%
  autoplot() +
  labs(title = "ACF: aus_airpassengers",
       subtitle = "Slowly declining positive bars (strong trend, no seasonality)")
dev.off()

cat("Plot saved to acf_aus_airpassengers.png\n")
cat("Answer: Match the correlogram with slowly, smoothly declining positive bars.\n")
cat("        (Strong trend = high positive ACF that decays gradually)\n\n")
