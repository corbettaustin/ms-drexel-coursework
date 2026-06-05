suppressPackageStartupMessages(library(fpp3))
suppressPackageStartupMessages(library(slider))
vic_elec_ma <- vic_elec |>
  dplyr::mutate(
    ma7   = slider::slide_dbl(Demand, mean, .before = 3, .after = 3, .complete = TRUE),
    ma3x7 = slider::slide_dbl(ma7, mean, .before = 1, .after = 1, .complete = TRUE)
  )
cat("Q10 - First non-NA value of 3x7-MA on vic_elec Demand:\n")
result <- vic_elec_ma |>
  dplyr::filter(!is.na(ma3x7)) |>
  utils::head(1) |>
  dplyr::pull(ma3x7)
print(result)
