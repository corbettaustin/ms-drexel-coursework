suppressPackageStartupMessages({
  library(fpp3)
  library(USgas)
  library(feasts)
  library(dplyr)
})

cat("==============================================\n")
cat("QUESTION 2 - Largest ACF for California in us_total\n")
cat("==============================================\n")

ca_ts <- us_total %>%
  filter(state == "California") %>%
  as_tsibble(index = year, key = state)

ca_acf <- ca_ts %>%
  ACF(y) %>%
  as_tibble()

max_acf <- max(ca_acf$acf)
cat("All ACF values:\n")
print(ca_acf)
cat(sprintf("\nAnswer: Largest autocorrelation for California = %.3f\n\n", max_acf))
