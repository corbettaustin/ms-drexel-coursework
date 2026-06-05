suppressPackageStartupMessages(library(fpp3))

pbs_a11 <- PBS |>
  dplyr::filter(
    Concession == "General",
    Type == "Safety net",
    ATC2 == "A11"
  )

acf_scripts <- pbs_a11 |>
  ACF(Scripts, lag_max = 10)

cat("Q20 - ACF values for Scripts:\n")
print(acf_scripts)

vals <- acf_scripts |> dplyr::pull(acf)
cat("Q20 - Sum of squares of first 10 autocorrelations for Scripts:\n")
print(sum(vals^2))
