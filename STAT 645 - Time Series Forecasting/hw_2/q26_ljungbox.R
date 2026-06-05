suppressPackageStartupMessages(library(fpp3))

takeaway <- aus_retail |>
  dplyr::filter(Industry == "Takeaway food services") |>
  index_by(Month) |>
  dplyr::summarise(Turnover = sum(Turnover))

max_month <- max(takeaway$Month)
split_month <- max_month - 48

takeaway_train <- takeaway |> dplyr::filter(Month <= split_month)
takeaway_test  <- takeaway |> dplyr::filter(Month > split_month)

takeaway_fit <- takeaway_train |>
  model(
    Mean    = MEAN(Turnover),
    Naive   = NAIVE(Turnover),
    SNaive  = SNAIVE(Turnover),
    RWDrift = RW(Turnover ~ drift())
  )

# Best model is Naive — run Ljung-Box on its residuals
naive_resid <- takeaway_fit |>
  dplyr::select(Naive) |>
  residuals()

cat("Ljung-Box test on Naive residuals (lag=24):\n")
print(naive_resid |>
  features(.resid, ljung_box, lag = 24))

cat("\nFirst few residual ACF values:\n")
print(naive_resid |>
  ACF(.resid, lag_max = 24))
