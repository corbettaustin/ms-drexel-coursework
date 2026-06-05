suppressPackageStartupMessages(library(fpp3))

# Q11 - Classical decomposition multiplicative, Gas, remainder Q2 2009
gas <- utils::tail(aus_production, 5*4) |> dplyr::select(Gas)
gas_dcmp <- gas |>
  model(classical_decomposition(Gas, type = "multiplicative")) |>
  components()
cat("Q11 - Column names in gas decomp:\n")
print(names(gas_dcmp))
cat("Q11 - Full decomp table:\n")
print(gas_dcmp)

# Q12 - Classical decomposition additive, Cement, season_adjust Q4 2007
cement_gas <- utils::tail(aus_production, 5*4) |> dplyr::select(Cement)
cement_dcmp <- cement_gas |>
  model(classical_decomposition(Cement, type = "additive")) |>
  components()
cat("Q12 - Column names in cement decomp:\n")
print(names(cement_dcmp))
cat("Q12 - Full decomp table:\n")
print(cement_dcmp)
