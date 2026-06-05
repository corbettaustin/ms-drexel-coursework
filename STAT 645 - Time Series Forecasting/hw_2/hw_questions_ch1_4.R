# ============================================================
# STAT 645 - Homework: Chapters 1-4
# ============================================================
# Run this script section by section.
# Each question is clearly labeled with its answer output.
# ============================================================

library(fpp3)
library(USgas)
library(feasts)
library(dplyr)

cat("==============================================\n")
cat("QUESTION 1 - Frequency of 'prices' dataset\n")
cat("==============================================\n")

# Inspect the tsibble index to determine frequency
prices

# The index column is 'year' with [1Y] interval -> Annual
cat("Answer: The 'prices' dataset has ANNUAL frequency (index = year [1Y])\n\n")


cat("==============================================\n")
cat("QUESTION 2 - Largest ACF for California in us_total\n")
cat("==============================================\n")

# Convert us_total to tsibble and filter to California
ca_ts <- us_total %>%
  filter(state == "California") %>%
  as_tsibble(index = year, key = state)

# Compute ACF values
ca_acf <- ca_ts %>%
  ACF(y) %>%
  as_tibble()

# Find the largest autocorrelation
max_acf <- max(ca_acf$acf)
cat("All ACF values:\n")
print(ca_acf)
cat(sprintf("\nAnswer: Largest autocorrelation for California = %.3f\n\n", max_acf))


cat("==============================================\n")
cat("QUESTION 3 - ACF plot for aus_airpassengers\n")
cat("==============================================\n")

# aus_airpassengers is annual with a strong upward trend.
# The correlogram will show slowly declining positive bars (no scalloping).
aus_airpassengers %>%
  ACF(Passengers) %>%
  autoplot() +
  labs(title = "ACF: aus_airpassengers",
       subtitle = "Expect slowly declining positive bars (strong trend, no seasonality)")

cat("Answer: Match the correlogram with slowly, smoothly declining positive bars.\n")
cat("        (Strong trend = high positive ACF that decays gradually)\n\n")


cat("==============================================\n")
cat("QUESTION 4 - Smallest seasonal strength in us_employment\n")
cat("==============================================\n")

# Compute STL features for all series in us_employment
employment_features <- us_employment %>%
  features(Employed, feat_stl)

# Find the series with the SMALLEST seasonal strength
min_seasonal <- employment_features %>%
  filter(seasonal_strength_year == min(seasonal_strength_year, na.rm = TRUE)) %>%
  select(Series_ID, Title, seasonal_strength_year)

cat("Series with smallest seasonal strength:\n")
print(min_seasonal)
cat(sprintf("\nAnswer: Series_ID = %s\n\n", min_seasonal$Series_ID))


cat("==============================================\n")
cat("QUESTION 5 - Best Box-Cox lambda for canadian_gas Volume\n")
cat("==============================================\n")

# Use Guerrero method to find optimal lambda
lambda <- canadian_gas %>%
  features(Volume, features = guerrero) %>%
  pull(lambda_guerrero)

cat(sprintf("Answer: Best lambda (Box-Cox) for canadian_gas Volume = %.4f\n\n", lambda))

# Optional: visualize the transformation
canadian_gas %>%
  autoplot(box_cox(Volume, lambda)) +
  labs(title = sprintf("canadian_gas Volume after Box-Cox (lambda = %.4f)", lambda),
       y = "Transformed Volume")

cat("==============================================\n")
cat("All questions complete!\n")
cat("==============================================\n")
