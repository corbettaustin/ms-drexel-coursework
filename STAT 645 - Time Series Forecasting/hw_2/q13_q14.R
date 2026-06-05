suppressPackageStartupMessages(library(fpp3))

# Q13 - STL decomp, canadian_gas, biggest seasonal component
cat("Q13 - Max seasonal component from STL (canadian_gas):\n")
result13 <- canadian_gas |>
  model(STL(Volume)) |>
  components() |>
  dplyr::pull(season_year) |>
  max()
print(result13)

# Q14 - X-13ARIMA-SEATS x11, check if seasonal package available
cat("Q14 - Checking if seasonal package is available:\n")
has_seasonal <- requireNamespace("seasonal", quietly = TRUE)
print(has_seasonal)
if (has_seasonal) {
  library(seasonal)
  x11_fit <- canadian_gas |>
    model(X_13ARIMA_SEATS(Volume ~ x11()))
  x11_components <- components(x11_fit)
  cat("Column names:\n")
  print(names(x11_components))
  result14 <- x11_components |>
    dplyr::filter(seasonal == min(seasonal, na.rm = TRUE)) |>
    dplyr::pull(Month)
  print(result14)
} else {
  cat("seasonal package not installed — skipping Q14\n")
}
