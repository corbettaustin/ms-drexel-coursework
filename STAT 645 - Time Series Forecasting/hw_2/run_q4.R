suppressPackageStartupMessages({
  library(fpp3)
  library(feasts)
  library(dplyr)
})

cat("==============================================\n")
cat("QUESTION 4 - Smallest seasonal strength in us_employment\n")
cat("==============================================\n")

employment_features <- us_employment %>%
  features(Employed, feat_stl)

cat("Column names in features output:\n")
print(names(employment_features))

min_seasonal <- employment_features %>%
  filter(seasonal_strength_year == min(seasonal_strength_year, na.rm = TRUE))

cat("\nSeries with smallest seasonal strength:\n")
print(min_seasonal)
cat(sprintf("\nAnswer: Series_ID = %s\n\n", min_seasonal$Series_ID))
