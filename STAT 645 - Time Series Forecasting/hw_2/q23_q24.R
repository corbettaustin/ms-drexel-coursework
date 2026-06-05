suppressPackageStartupMessages(library(fpp3))

cement <- aus_production |>
  dplyr::filter(!is.na(Cement)) |>
  dplyr::select(Quarter, Cement)

cement_train <- cement |> dplyr::filter(Quarter <= yearquarter("2006 Q3"))
cement_test  <- cement |> dplyr::filter(Quarter > yearquarter("2006 Q3"))

# Q23 - RW with drift RMSE on test set
cement_rwd_fc <- cement_train |>
  model(RW(Cement ~ drift())) |>
  forecast(h = nrow(cement_test))

cat("Q23 - RW with drift RMSE on test set (Cement):\n")
print(cement_rwd_fc |>
  accuracy(cement_test) |>
  dplyr::pull(RMSE))

# Q24 - SNAIVE RMSE on test set
cement_snaive_fc <- cement_train |>
  model(SNAIVE(Cement)) |>
  forecast(h = nrow(cement_test))

cat("Q24 - SNAIVE RMSE on test set (Cement):\n")
print(cement_snaive_fc |>
  accuracy(cement_test) |>
  dplyr::pull(RMSE))
