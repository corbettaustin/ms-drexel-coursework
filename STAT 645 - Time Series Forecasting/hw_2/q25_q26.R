suppressPackageStartupMessages(library(fpp3))

takeaway <- aus_retail |>
  dplyr::filter(Industry == "Takeaway food services") |>
  index_by(Month) |>
  dplyr::summarise(Turnover = sum(Turnover))

# Use yearmonth arithmetic for the split (last 4 years = 48 months)
max_month <- max(takeaway$Month)
split_month <- max_month - 48

takeaway_train <- takeaway |>
  dplyr::filter(Month <= split_month)

takeaway_test <- takeaway |>
  dplyr::filter(Month > split_month)

cat("Train ends:", format(max(takeaway_train$Month)), "| Test starts:", format(min(takeaway_test$Month)), "\n")

takeaway_fit <- takeaway_train |>
  model(
    Mean    = MEAN(Turnover),
    Naive   = NAIVE(Turnover),
    SNaive  = SNAIVE(Turnover),
    RWDrift = RW(Turnover ~ drift())
  )

takeaway_fc <- takeaway_fit |>
  forecast(h = nrow(takeaway_test))

takeaway_accuracy <- takeaway_fc |>
  accuracy(takeaway_test)

cat("Q25 - All model RMSEs (sorted):\n")
print(takeaway_accuracy |>
  dplyr::arrange(RMSE) |>
  dplyr::select(.model, RMSE))

cat("Q25 - Best model RMSE:\n")
print(takeaway_accuracy |>
  dplyr::slice_min(RMSE) |>
  dplyr::pull(RMSE))

best <- takeaway_accuracy |> dplyr::slice_min(RMSE) |> dplyr::pull(.model)
cat("Q25 - Best model:", best, "\n")
cat("Q26 - (Run gg_tsresiduals() interactively to assess white noise visually)\n")
