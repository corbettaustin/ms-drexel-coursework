# ============================================================
# STAT 645 - Time Series Forecasting - Final Project
# Forecasting Median Listing Price in Rhode Island
# Source: FRED series MEDLISPRIRI (Zillow via St. Louis Fed)
# ============================================================
#
# This script reproduces the full analysis used in the slide
# deck: EDA, train/test split, candidate models, accuracy
# comparison, residual diagnostics, and the final 12-month
# forecast. All figures referenced by the deck are written to
# ./figures/*.png so the deck can be re-rendered after re-runs.
#
# Run with:   Rscript stat645_final_project.R
# Or knit:    stat645_final_project.Rmd

# ----- Packages ---------------------------------------------
suppressPackageStartupMessages({
  library(fpp3)          # tsibble + fable + feasts
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(knitr)
})

theme_set(theme_minimal(base_size = 12))

# Where to write figures
fig_dir <- "figures"
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

save_png <- function(plot, name, w = 9, h = 5, dpi = 150) {
  ggsave(file.path(fig_dir, name), plot = plot,
         width = w, height = h, dpi = dpi, bg = "white")
}

# ----- 1. Load data -----------------------------------------
raw <- read_excel("MEDLISPRIRI.xlsx", sheet = "Monthly")
names(raw) <- c("date", "price")

price_ts <- raw |>
  mutate(month = yearmonth(date)) |>
  as_tsibble(index = month) |>
  select(month, price)

cat("Observations:", nrow(price_ts), "\n")
cat("Range:", format(min(price_ts$month)), "to",
    format(max(price_ts$month)), "\n")
cat("Any gaps:", has_gaps(price_ts)$.gaps, "\n\n")

# ----- 2. EDA -----------------------------------------------

# Overview time-series plot
p_overview <- price_ts |>
  autoplot(price) +
  labs(title = "Median Listing Price - Rhode Island",
       subtitle = "Monthly, FRED series MEDLISPRIRI",
       x = NULL, y = "USD") +
  scale_y_continuous(labels = scales::label_dollar())
save_png(p_overview, "01_overview.png")

# Seasonal plot
p_season <- price_ts |>
  gg_season(price, labels = "both") +
  labs(title = "Seasonal plot - listing price by month",
       y = "USD") +
  scale_y_continuous(labels = scales::label_dollar())
save_png(p_season, "02_seasonality.png")

# STL decomposition
stl_fit <- price_ts |>
  model(STL(price ~ trend(window = 13) + season(window = "periodic"),
            robust = TRUE)) |>
  components()

p_stl <- autoplot(stl_fit) +
  labs(title = "STL decomposition - price")
save_png(p_stl, "03_stl.png", w = 9, h = 7)

# ACF for the raw series (very persistent due to trend)
p_acf <- price_ts |>
  ACF(price, lag_max = 36) |>
  autoplot() +
  labs(title = "ACF - raw price")
save_png(p_acf, "03b_acf.png", h = 4)

# Transformation is assessed on the TRAINING set only (after the split,
# below) to avoid leaking holdout information into the lambda estimate.

# ----- 3. Train / test split --------------------------------
# Hold out last 12 months (~10% of n=117)
h_test <- 12
cutoff <- max(price_ts$month) - h_test
train <- price_ts |> filter(month <= cutoff)
test  <- price_ts |> filter(month >  cutoff)

cat("Train: n =", nrow(train), "  ", format(min(train$month)),
    " -> ", format(max(train$month)), "\n")
cat("Test : n =", nrow(test),  "  ", format(min(test$month)),
    " -> ", format(max(test$month)), "\n\n")

# Variance-stabilising transform: Guerrero lambda on TRAIN only.
lambda_tr <- train |>
  features(price, features = guerrero) |>
  pull(lambda_guerrero)
cat(sprintf("Guerrero lambda (train only): %.3f\n", lambda_tr))
cat("This lambda is extreme and numerically unstable on a short series\n",
    "with a structural break, so the transformed ETS candidate uses a\n",
    "log scale (lambda = 0), which stabilises variance cleanly.\n\n", sep = "")

p_split <- ggplot() +
  geom_line(data = train, aes(month, price), colour = "steelblue") +
  geom_line(data = test,  aes(month, price), colour = "tomato") +
  geom_vline(xintercept = as.Date(cutoff), linetype = 2) +
  scale_y_continuous(labels = scales::label_dollar()) +
  labs(title = "Train (blue) vs holdout (red)",
       x = NULL, y = "USD")
save_png(p_split, "04_train_test.png")

# ----- 4. Candidate models ----------------------------------
# Fit all candidates in a single mable
fits <- train |>
  model(
    Mean       = MEAN(price),
    Naive      = NAIVE(price),
    SNaive     = SNAIVE(price ~ lag("year")),
    Drift      = RW(price ~ drift()),
    TSLM_seas  = TSLM(price ~ trend() + season()),
    TSLM_four  = TSLM(price ~ trend() + fourier(K = 2)),
    ETS_MAM    = ETS(price ~ error("M") + trend("A")  + season("M")),
    ETS_AAdA   = ETS(price ~ error("A") + trend("Ad") + season("A")),
    ETS_log    = ETS(log(price)),
    ARIMA      = ARIMA(price),
    ARIMA_man  = ARIMA(price ~ pdq(1, 1, 1) + PDQ(0, 1, 1)),
    STL_ETS    = decomposition_model(
                   STL(price ~ trend(window = 13) +
                                season(window = "periodic"),
                       robust = TRUE),
                   ETS(season_adjust ~ season("N"))
                 )
  )

# ETS() auto-selection independently lands on (M,A,M), confirming that the
# hand-specified ETS_MAM is the model the data prefers (so we report one,
# not a duplicate row). ARIMA() likewise auto-selects its orders.
cat("--- ETS() and ARIMA() auto-selected specifications ---\n")
print(train |> model(ETS_auto = ETS(price), ARIMA_auto = ARIMA(price)))

cat("\n--- In-sample summary (AICc) ---\n")
cat("Note: AICc is comparable WITHIN the ETS and ARIMA families but not\n")
cat("across families or vs TSLM; selection is made on holdout accuracy.\n")
# glance() doesn't work on decomposition_model; exclude STL_ETS here.
print(glance(fits |> select(-STL_ETS)) |>
        select(.model, AIC, AICc, BIC, sigma2) |>
        arrange(AICc), n = Inf)

# ----- 5. Forecast the holdout and compute accuracy --------
fc_test <- fits |> forecast(h = h_test)

acc <- fc_test |>
  accuracy(price_ts) |>
  select(.model, RMSE, MAE, MAPE, MASE, RMSSE) |>
  arrange(RMSE)
cat("\n--- Holdout accuracy (sorted by RMSE) ---\n")
print(acc, n = Inf)

# Save the accuracy table for the deck
write.csv(acc, file.path(fig_dir, "accuracy_table.csv"), row.names = FALSE)

# Plot all forecasts vs the actual holdout
p_compare <- fc_test |>
  autoplot(price_ts |> filter(month >= yearmonth("2022 Jan")),
           level = NULL) +
  labs(title = "Holdout forecasts - all candidate models",
       y = "USD") +
  scale_y_continuous(labels = scales::label_dollar()) +
  guides(colour = guide_legend(title = "Model"))
save_png(p_compare, "05_forecasts_compare.png", w = 10, h = 5)

# Zoom on the best three by RMSE
best3 <- acc$.model[1:3]
p_best3 <- fc_test |>
  filter(.model %in% best3) |>
  autoplot(price_ts |> filter(month >= yearmonth("2022 Jan"))) +
  labs(title = paste("Best 3 models on holdout:",
                     paste(best3, collapse = ", ")),
       y = "USD") +
  scale_y_continuous(labels = scales::label_dollar())
save_png(p_best3, "05b_best3_forecast.png", w = 10, h = 5)

# ----- 6. Residual diagnostics for the winner --------------
winner <- acc$.model[1]
cat("\nSelected model:", winner, "\n")

best_fit <- fits |> select(all_of(winner))

p_resid <- best_fit |> gg_tsresiduals()
save_png(p_resid, "06_residuals.png", w = 9, h = 6)

# Ljung-Box: tie the degrees of freedom to the chosen model's estimated
# parameters (smoothing params + initial states for ETS), not a hardcoded 0.
n_par <- nrow(tidy(best_fit))
lb_lag <- 24
lb <- augment(best_fit) |>
  features(.innov, ljung_box, lag = lb_lag, dof = n_par)
cat(sprintf("\nLjung-Box for %s  (lag = %d, dof = %d):\n",
            winner, lb_lag, n_par))
print(lb)

# Persist the test result so the slide deck can quote the actual number.
lb_out <- lb |>
  transmute(.model = winner, lag = lb_lag, dof = n_par,
            lb_stat = lb_stat, lb_pvalue = lb_pvalue)
write.csv(lb_out, file.path(fig_dir, "ljung_box.csv"), row.names = FALSE)

# ----- 7. Refit on full data and forecast 12 months -------
# Re-specify the chosen model by name so we can refit on the
# full series (train + test) before forecasting forward.
spec_lookup <- list(
  Mean      = quote(MEAN(price)),
  Naive     = quote(NAIVE(price)),
  SNaive    = quote(SNAIVE(price ~ lag("year"))),
  Drift     = quote(RW(price ~ drift())),
  TSLM_seas = quote(TSLM(price ~ trend() + season())),
  TSLM_four = quote(TSLM(price ~ trend() + fourier(K = 2))),
  ETS_MAM   = quote(ETS(price ~ error("M") + trend("A")  + season("M"))),
  ETS_AAdA  = quote(ETS(price ~ error("A") + trend("Ad") + season("A"))),
  ETS_log   = quote(ETS(log(price))),
  ARIMA     = quote(ARIMA(price)),
  ARIMA_man = quote(ARIMA(price ~ pdq(1, 1, 1) + PDQ(0, 1, 1))),
  STL_ETS   = quote(decomposition_model(
                     STL(price ~ trend(window = 13) +
                                  season(window = "periodic"),
                         robust = TRUE),
                     ETS(season_adjust ~ season("N"))))
)

final_fit <- price_ts |>
  model(final = eval(spec_lookup[[winner]]))

final_fc <- final_fit |> forecast(h = 12)

p_final <- final_fc |>
  autoplot(price_ts |> filter(month >= yearmonth("2018 Jan"))) +
  labs(title = paste0("Final forecast (", winner,
                     ") - next 12 months"),
       subtitle = "80% and 95% prediction intervals",
       y = "USD") +
  scale_y_continuous(labels = scales::label_dollar())
save_png(p_final, "07_final_forecast.png", w = 10, h = 5)

# Tabulate point forecasts + intervals
final_tbl <- final_fc |>
  hilo(level = c(80, 95)) |>
  as_tibble() |>
  mutate(
    point = .mean,
    lo80  = `80%`$lower,
    hi80  = `80%`$upper,
    lo95  = `95%`$lower,
    hi95  = `95%`$upper
  ) |>
  select(month, point, lo80, hi80, lo95, hi95)
write.csv(final_tbl, file.path(fig_dir, "final_forecast_table.csv"),
          row.names = FALSE)

cat("\n--- 12-month point forecasts + intervals ---\n")
print(final_tbl, n = Inf)

cat("\nDone. Figures written to ./figures/.\n")
