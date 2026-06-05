suppressPackageStartupMessages(library(fpp3))

pbs_a11 <- PBS |>
  dplyr::filter(
    Concession == "General",
    Type == "Safety net",
    ATC2 == "A11"
  )

# Q19 - Third autocorrelation for Cost
acf_cost <- pbs_a11 |>
  ACF(Cost, lag_max = 10)
cat("Q19 - ACF table for Cost:\n")
print(acf_cost)
cat("Q19 - Third autocorrelation (lag 3) for Cost:\n")
print(acf_cost |> dplyr::slice(3) |> dplyr::pull(acf))

# Q20 - Sum of squares of first 10 autocorrelations for Scripts
acf_scripts <- pbs_a11 |>
  ACF(Scripts, lag_max = 10)
cat("Q20 - Sum of squares of first 10 ACF values for Scripts:\n")
print(acf_scripts |> dplyr::summarise(sum_sq = sum(acf^2)))
