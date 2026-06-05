suppressPackageStartupMessages({
  library(fpp3)
  library(USgas)
  library(feasts)
  library(dplyr)
})

cat("==============================================\n")
cat("QUESTION 1 - Frequency of 'prices' dataset\n")
cat("==============================================\n")
print(prices)
cat("\nAnswer: The 'prices' dataset has ANNUAL frequency (index = year [1Y])\n\n")
