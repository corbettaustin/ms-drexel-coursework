# STAT 645 - HW3

library(fpp3)
library(tidyverse)

#Q1
#load and filter the olympic_running dataset for Women's 5000M
w5000 <- olympic_running %>%
  filter(Length == 5000, Sex == "W")

#inspect the data 
w5000

#fit TSLM model with trend() on historical data (1896–2016)
fit_w5000 <- w5000 %>%
  model(TSLM(Time ~ trend()))

report(fit_w5000)

#forecast for 2020 and 2024 

fc_w5000 <- fit_w5000 %>%
  forecast(h = 3)

fc_w5000

#point forecasts
fc_w5000 %>% select(.mean)

#2024 Women's 5000M Olympic winning time was 14:31.98 = 871.98 seconds
#(Beatrice Chebet, Paris 2024)
actual_2024_w5000 <- 14*60 + 31.98  # = 871.98 seconds

#the forecast for 2024 is h=2 (2016 → 2020 → 2024)
forecast_2024 <- fc_w5000 %>%
  filter(Year == 2024) %>%
  pull(.mean)

forecast_2024  #check the point forecast

#forecast error = Actual - Forecast
error_2024 <- actual_2024_w5000 - forecast_2024
cat("Q1 - Forecast error for 2024 Women's 5000M:", round(error_2024, 4), "\n")

#2@

#use hilo() to get prediction intervals at 93% level
pi_w5000 <- fc_w5000 %>%
  hilo(level = 93) %>%
  filter(Year == 2028)

pi_w5000

lower_2028 <- pi_w5000 %>%
  pull(`93%`) %>%
  .$lower

cat("Q2 - Lower bound of 93% PI for 2028 Women's 5000M:", round(lower_2028, 4), "\n")


#Q3
#filter for Men's 800M
m800 <- olympic_running %>%
  filter(Length == 800, Sex == "M")

m800

#fit TSLM with trend
fit_m800 <- m800 %>%
  model(TSLM(Time ~ trend()))

report(fit_m800)

# calculate slope (trend coefficient)
tidy_m800 <- tidy(fit_m800)
tidy_m800

slope_m800 <- tidy_m800 %>%
  filter(term == "trend()") %>%
  pull(estimate)

cat("Q3 - Average rate of change in Men's 800M winning times:", round(slope_m800, 3), "\n")


#Q4
#filter aus_livestock for pigs in Victoria
pigs <- aus_livestock %>%
  filter(Animal == "Pigs", State == "Victoria")

pigs

autoplot(pigs, Count) +
  labs(title = "Pigs slaughtered in Victoria")

#Q4
fit_pigs <- pigs %>%
  model(ETS(Count ~ error("A") + trend("N") + season("N")))

report(fit_pigs)

alpha_pigs <- fit_pigs %>%
  tidy() %>%
  filter(term == "alpha") %>%
  pull(estimate)

cat("Q4 - Optimal alpha:", round(alpha_pigs, 4), "\n")

#Q5 qnorm
z_90 <- qnorm(0.95)  # = 1.6449

#get residuals
resid_pigs <- fit_pigs %>%
  augment() %>%
  pull(.resid)

s_pigs <- sd(resid_pigs, na.rm = TRUE)
cat("Residual SD:", s_pigs, "\n")

#one-step-ahead forecast
fc1_pigs <- fit_pigs %>%
  forecast(h = 1)

yhat_pigs <- fc1_pigs %>% pull(.mean)
cat("One-step-ahead point forecast:", yhat_pigs, "\n")

upper_manual <- yhat_pigs + z_90 * s_pigs
cat("Q5 - Upper bound of manual 90% PI:", round(upper_manual, 2), "\n")

#Q6 90% PI using forecast() function ---
fc1_pigs_hilo <- fc1_pigs %>%
  hilo(level = 90)

fc1_pigs_hilo

upper_fcast <- fc1_pigs_hilo %>%
  pull(`90%`) %>%
  .$upper

cat("Q6 - Upper bound of 90% PI from forecast():", round(upper_fcast, 2), "\n")


#Q7
#filter global_economy for Japan
japan <- global_economy %>%
  filter(Country == "Japan")

japan

autoplot(japan, Exports) +
  labs(title = "Japan Exports (% of GDP)")

#fit both models
fit_japan <- japan %>%
  model(
    ANN = ETS(Exports ~ error("A") + trend("N") + season("N")),
    AAN = ETS(Exports ~ error("A") + trend("A") + season("N"))
  )

#Q7 AICc for best model
glance(fit_japan) %>%
  select(.model, AICc)

# Report each model to see AICc
report(fit_japan %>% select(ANN))
report(fit_japan %>% select(AAN))

aicc_vals <- glance(fit_japan) %>%
  select(.model, AICc) %>%
  arrange(AICc)

aicc_vals

best_model <- aicc_vals %>% slice(1)
cat("Q7 - Best model:", best_model$.model, "with AICc:", round(best_model$AICc, 4), "\n")

#Q8 point forecast 4 steps ahead for best model
fc_japan <- fit_japan %>%
  select(best_model$.model) %>%
  forecast(h = 4)

fc_japan

#last observation year in Japan data
tail(japan)

#h=4 forecast (4 years ahead)
fc4_japan <- fc_japan %>%
  slice_tail(n = 1) %>%
  pull(.mean)

cat("Q8 - Point forecast 4 steps ahead (best model):", round(fc4_japan, 4), "\n")


#Q9
#filter aus_arrivals for New Zealand
nz_arrivals <- aus_arrivals %>%
  filter(Origin == "NZ")

nz_arrivals

autoplot(nz_arrivals, Trips) +
  labs(title = "Quarterly arrivals to Australia from NZ")

#data runs 1981 Q1 to 2012 Q3
#withhold last 2 years = 8 quarters as test set
#training: up to 2010 Q3; Test: 2010 Q4 to 2012 Q3

nz_train <- nz_arrivals %>%
  filter(Quarter <= yearquarter("2010 Q3"))

nz_test <- nz_arrivals %>%
  filter(Quarter > yearquarter("2010 Q3"))

cat("Training rows:", nrow(nz_train), "| Test rows:", nrow(nz_test), "\n")

#fit four models on training set ---

fit_nz <- nz_train %>%
  model(
    #1. ETS model (auto-selected, likely A,A,M or similar)
    ets = ETS(Trips),

    #2. ETS on log-transformed series
    ets_log = ETS(log(Trips)),

    #3. Seasonal naive
    snaive = SNAIVE(Trips),

    #4. STL + ETS on log-transformed data
    stl_ets_log = decomposition_model(
      STL(log(Trips) ~ season(window = "periodic")),
      ETS(season_adjust)
    )
  )

fit_nz

#forecast the 8-quarter test set (h = 8)
fc_nz <- fit_nz %>%
  forecast(h = 8)

#RMSE on test data (best method)
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

#Q10 Which model is best?
acc_nz %>%
  select(.model, RMSE) %>%
  arrange(RMSE)
# Answer: whichever model has the lowest RMSE from above output

#Q11
#initial window = 36, step = 3

nz_cv <- nz_arrivals %>%
  stretch_tsibble(.init = 36, .step = 3)

cat("Number of CV windows:", max(nz_cv$.id), "\n")

fit_cv <- nz_cv %>%
  model(
    ets        = ETS(Trips),
    ets_log    = ETS(log(Trips)),
    snaive     = SNAIVE(Trips),
    stl_ets_log = decomposition_model(
      STL(log(Trips) ~ season(window = "periodic")),
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

#print both for comparison
cat("\n--- Test set RMSE ---\n")
print(acc_nz %>% select(.model, RMSE) %>% arrange(RMSE))

cat("\n--- Cross-validation RMSE ---\n")
print(acc_cv %>% select(.model, RMSE) %>% arrange(RMSE))
