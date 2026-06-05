###############################################################################
# STAT 645 - Time Series Forecasting
# Homework: TSLM & ETS Models
# Organized by question with comments
###############################################################################

library(fpp3)
library(tidyverse)

###############################################################################
# QUESTIONS 1 & 2: Women's 5000M Olympic Running — TSLM with trend
###############################################################################

# Load and filter the olympic_running dataset for Women's 5000M
# Dataset only goes to 2016; extend with known 2020 and 2024 results
w5000 <- olympic_running %>%
  filter(Length == 5000, Sex == "women")

# Actual times from 2020 (Tokyo) and 2024 (Paris) Olympics:
#   2020: Sifan Hassan  — 14:36.79 = 876.79 seconds
#   2024: Beatrice Chebet — 14:28.56 = 868.56 seconds
w5000_extended <- w5000 %>%
  bind_rows(
    tibble(Year = 2020L, Length = 5000L, Sex = "women", Time = 14*60 + 36.79),
    tibble(Year = 2024L, Length = 5000L, Sex = "women", Time = 14*60 + 28.56)
  ) %>%
  as_tsibble(index = Year, key = c(Length, Sex))

cat("Extended Women's 5000M dataset (1996-2024):\n")
print(w5000_extended)

# Fit TSLM model with trend() on extended data (1996–2024)
fit_w5000 <- w5000_extended %>%
  model(TSLM(Time ~ trend()))

report(fit_w5000)

# --- QUESTION 1: Forecast error for 2024 ---
# Model is now fit through 2024, so re-fit on original data (through 2016)
# to forecast 2024 and compute the forecast error
fit_w5000_orig <- w5000 %>%
  model(TSLM(Time ~ trend()))

# h=1 → 2020, h=2 → 2024
fc_w5000_orig <- fit_w5000_orig %>%
  forecast(h = 2)

fc_w5000_orig %>% as_tibble() %>% select(Year, .mean)

# Actual 2024 time: Beatrice Chebet, Paris 2024 — 14:28.56 = 868.56 seconds
actual_2024_w5000 <- 14*60 + 28.56

forecast_2024 <- fc_w5000_orig %>%
  filter(Year == 2024) %>%
  pull(.mean)

forecast_2024  # check the point forecast

# Forecast error = Actual - Forecast
error_2024 <- actual_2024_w5000 - forecast_2024
cat("Q1 - Forecast error for 2024 Women's 5000M:", round(error_2024, 4), "\n")

# --- QUESTION 2: Lower bound of 93% prediction interval for 2028 ---
# Model fit on extended data (1996–2024); h=1 → 2028

fc_w5000 <- fit_w5000 %>%
  forecast(h = 1)

cat("Point forecast for 2028:\n")
print(fc_w5000 %>% as_tibble() %>% select(Year, .mean))

# Use hilo() to get 93% prediction interval
pi_w5000 <- fc_w5000 %>%
  hilo(level = 93)

pi_w5000

lower_2028 <- pi_w5000 %>%
  pull(`93%`) %>%
  .$lower

cat("Q2 - Lower bound of 93% PI for 2028 Women's 5000M:", round(lower_2028, 4), "\n")


###############################################################################
# QUESTION 3: Men's 800M Olympic Running — TSLM rate of change (slope)
###############################################################################

# Filter for Men's 800M
m800 <- olympic_running %>%
  filter(Length == 800, Sex == "men")

m800

# Fit TSLM with trend
fit_m800 <- m800 %>%
  model(TSLM(Time ~ trend()))

report(fit_m800)

# The slope (trend coefficient) = average rate of change in winning times per year
# (more precisely, per Olympic cycle unit — check the trend() indexing)
tidy_m800 <- tidy(fit_m800)
tidy_m800

slope_m800 <- tidy_m800 %>%
  filter(term == "trend()") %>%
  pull(estimate)

cat("Q3 - Average rate of change in Men's 800M winning times:", round(slope_m800, 3), "\n")


###############################################################################
# QUESTIONS 4, 5, 6: Pigs slaughtered in Victoria — Simple ETS (ANN)
###############################################################################

# Filter aus_livestock for pigs in Victoria
pigs <- aus_livestock %>%
  filter(Animal == "Pigs", State == "Victoria")

pigs

autoplot(pigs, Count) +
  labs(title = "Pigs slaughtered in Victoria")

# --- QUESTION 4: Optimal alpha from ETS(A,N,N) ---
fit_pigs <- pigs %>%
  model(ETS(Count ~ error("A") + trend("N") + season("N")))

report(fit_pigs)

alpha_pigs <- fit_pigs %>%
  tidy() %>%
  filter(term == "alpha") %>%
  pull(estimate)

cat("Q4 - Optimal alpha:", round(alpha_pigs, 4), "\n")

# --- QUESTION 5: Manual 90% PI using yhat ± z * sd(residuals) ---
# z for 90% PI: central 90% → 5% in each tail → z = 1.6449
z_90 <- qnorm(0.95)  # = 1.6449

# Get residuals
resid_pigs <- fit_pigs %>%
  augment() %>%
  pull(.resid)

s_pigs <- sd(resid_pigs, na.rm = TRUE)
cat("Residual SD:", s_pigs, "\n")

# One-step-ahead forecast
fc1_pigs <- fit_pigs %>%
  forecast(h = 1)

yhat_pigs <- fc1_pigs %>% pull(.mean)
cat("One-step-ahead point forecast:", yhat_pigs, "\n")

upper_manual <- yhat_pigs + z_90 * s_pigs
cat("Q5 - Upper bound of manual 90% PI:", round(upper_manual, 2), "\n")

# --- QUESTION 6: 90% PI using forecast() function ---
fc1_pigs_hilo <- fc1_pigs %>%
  hilo(level = 90)

fc1_pigs_hilo

upper_fcast <- fc1_pigs_hilo %>%
  pull(`90%`) %>%
  .$upper

cat("Q6 - Upper bound of 90% PI from forecast():", round(upper_fcast, 2), "\n")


###############################################################################
# QUESTIONS 7 & 8: Japan Exports — ETS(A,N,N) vs ETS(A,A,N)
###############################################################################

# Filter global_economy for Japan
japan <- global_economy %>%
  filter(Country == "Japan")

japan

autoplot(japan, Exports) +
  labs(title = "Japan Exports (% of GDP)")

# Fit both models
fit_japan <- japan %>%
  model(
    ANN = ETS(Exports ~ error("A") + trend("N") + season("N")),
    AAN = ETS(Exports ~ error("A") + trend("A") + season("N"))
  )

# --- QUESTION 7: AICc for best model ---
# Report both models individually to inspect AICc
report(fit_japan %>% select(ANN))
report(fit_japan %>% select(AAN))

# Compare AICc side by side — lower AICc = better fit
aicc_vals <- glance(fit_japan) %>%
  select(.model, AICc) %>%
  arrange(AICc)

aicc_vals

cat("Q7 - AICc values:\n")
cat(sprintf("  ANN  AICc = %.4f\n", aicc_vals %>% filter(.model == "ANN") %>% pull(AICc)))
cat(sprintf("  AAN  AICc = %.4f\n", aicc_vals %>% filter(.model == "AAN") %>% pull(AICc)))

best_model <- aicc_vals %>% slice(1)
cat("Q7 - Best model:", best_model$.model, "with AICc:", round(best_model$AICc, 4), "\n")

# --- QUESTION 8: Point forecast 4 steps ahead for both models ---
# The last observation year in Japan data
tail(japan)

fc_ann <- fit_japan %>% select(ANN) %>% forecast(h = 4)
fc_aan <- fit_japan %>% select(AAN) %>% forecast(h = 4)

ann_h4 <- fc_ann %>% slice_tail(n = 1) %>% pull(.mean)
aan_h4 <- fc_aan %>% slice_tail(n = 1) %>% pull(.mean)

cat("Q8 - h=4 point forecasts:\n")
cat(sprintf("  ANN: %.4f\n", ann_h4))
cat(sprintf("  AAN: %.4f\n", aan_h4))
cat("Q8 - Best model (", best_model$.model, ") h=4 forecast:",
    round(ifelse(best_model$.model == "ANN", ann_h4, aan_h4), 4), "\n")


###############################################################################
# QUESTIONS 9, 10, 11: Australia arrivals from New Zealand — Holt-Winters
###############################################################################

# Filter aus_arrivals for New Zealand
nz_arrivals <- aus_arrivals %>%
  filter(Origin == "NZ")

nz_arrivals

autoplot(nz_arrivals, Arrivals) +
  labs(title = "Quarterly arrivals to Australia from NZ")

# Data runs 1981 Q1 to 2012 Q3
# Withhold last 2 years = 8 quarters as test set
# Training: up to 2010 Q3; Test: 2010 Q4 to 2012 Q3

nz_train <- nz_arrivals %>%
  filter(Quarter <= yearquarter("2010 Q3"))

nz_test <- nz_arrivals %>%
  filter(Quarter > yearquarter("2010 Q3"))

cat("Training rows:", nrow(nz_train), "| Test rows:", nrow(nz_test), "\n")

# Why multiplicative seasonality?
# The seasonal variation GROWS with the level of the series,
# so multiplicative seasonality is more appropriate.
# Log transformation can linearize this.

# --- Fit four models on training set ---

fit_nz <- nz_train %>%
  model(
    # 1. ETS model (auto-selected, likely A,A,M or similar)
    ets = ETS(Arrivals),

    # 2. ETS on log-transformed series
    ets_log = ETS(log(Arrivals)),

    # 3. Seasonal naive
    snaive = SNAIVE(Arrivals),

    # 4. STL + ETS on log-transformed data
    stl_ets_log = decomposition_model(
      STL(log(Arrivals) ~ season(window = "periodic")),
      ETS(season_adjust)
    )
  )

fit_nz

# Forecast the 8-quarter test set (h = 8)
fc_nz <- fit_nz %>%
  forecast(h = 8)

# --- QUESTION 9: RMSE on test data (best method) ---
acc_nz <- fc_nz %>%
  accuracy(nz_arrivals)

acc_nz %>%
  select(.model, RMSE) %>%
  arrange(RMSE)

# Best model by RMSE
best_rmse_test <- acc_nz %>%
  arrange(RMSE) %>%
  slice(1)

cat("Q9 - Best model on test set:", best_rmse_test$.model,
    "| RMSE:", round(best_rmse_test$RMSE, 2), "\n")

# --- QUESTION 10: Which model is best? (same question, multiple choice) ---
acc_nz %>%
  select(.model, RMSE) %>%
  arrange(RMSE)
# Answer: whichever model has the lowest RMSE from above output

# --- QUESTION 11: Time series cross-validation ---
# Initial window = 36, step = 3

nz_cv <- nz_arrivals %>%
  stretch_tsibble(.init = 36, .step = 3)

cat("Number of CV windows:", max(nz_cv$.id), "\n")

fit_cv <- nz_cv %>%
  model(
    ets        = ETS(Arrivals),
    ets_log    = ETS(log(Arrivals)),
    snaive     = SNAIVE(Arrivals),
    stl_ets_log = decomposition_model(
      STL(log(Arrivals) ~ season(window = "periodic")),
      ETS(season_adjust)
    )
  )

fc_cv <- fit_cv %>%
  forecast(h = 8)

acc_cv <- fc_cv %>%
  accuracy(nz_arrivals)

acc_cv %>%
  select(.model, RMSE) %>%
  arrange(RMSE)

best_cv <- acc_cv %>%
  arrange(RMSE) %>%
  slice(1)

cat("Q11 - Best model by CV RMSE:", best_cv$.model,
    "| RMSE:", round(best_cv$RMSE, 2), "\n")

# Compare: do we reach the same conclusions as with training/test split?
# Print both for comparison
cat("\n--- Test set RMSE ---\n")
print(acc_nz %>% select(.model, RMSE) %>% arrange(RMSE))

cat("\n--- Cross-validation RMSE ---\n")
print(acc_cv %>% select(.model, RMSE) %>% arrange(RMSE))
