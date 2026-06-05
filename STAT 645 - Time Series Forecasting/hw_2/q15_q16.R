suppressPackageStartupMessages(library(fpp3))

# Q15 - PBS: highest seasonal strength for Scripts
pbs_features <- PBS |>
  features(Scripts, feat_stl)

cat("Q15 - Highest seasonal strength for Scripts:\n")
print(pbs_features |>
  dplyr::arrange(dplyr::desc(seasonal_strength_year)) |>
  dplyr::slice(1) |>
  dplyr::select(ATC1, ATC2, seasonal_strength_year))

# Q16 - PBS: smallest trend strength across all series
cat("Q16 - Smallest trend strength (FT) across all PBS Scripts series:\n")
print(PBS |>
  features(Scripts, feat_stl) |>
  dplyr::summarise(min_FT = min(trend_strength, na.rm = TRUE)))
