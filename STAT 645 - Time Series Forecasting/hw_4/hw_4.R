#STAT645 - HW4

library(fpp3)
library(tidyverse)

#Q1 - Fit non-seasonal ARIMA; report AICc
fit_air <- aus_airpassengers %>%
  model(ARIMA(Passengers ~ PDQ(0, 0, 0)))

report(fit_air)

cat("Q1 - AICc:", round(glance(fit_air)$AICc, 4), "\n")  # 198.3236

#Q2 - Differencing order needed to make data stationary
ndiffs_air <- aus_airpassengers %>%
  features(Passengers, unitroot_ndiffs)

cat("Q2 - Differencing order (d):", ndiffs_air$ndiffs, "\n")  # 2

#Q3 - Plot differenced series and inspect ACF/PACF
d_order <- ndiffs_air$ndiffs  # d = 2

aus_airpassengers %>%
  gg_tsdisplay(difference(Passengers, differences = d_order),
               plot_type = "partial", lag_max = 15) +
  labs(title = paste0("Differenced Passengers (d = ", d_order, ")"))

# Interpretation:
# After 2nd difference: ACF cuts off sharply after lag 1 -> MA(1)
# PACF tails off -> MA structure preferred over AR
# Reasonable model: ARIMA(0, 2, 1)
cat("Q3 - (p,d,q) from ACF/PACF: (0, 2, 1)\n")


#Q4 - stepwise=FALSE and approximation=FALSE for exhaustive search
fit_air_ex <- aus_airpassengers %>%
  model(ARIMA(Passengers ~ PDQ(0, 0, 0),
              stepwise = FALSE, approximation = FALSE))

report(fit_air_ex)

cat("Q4 - Best non-seasonal ARIMA from exhaustive search:\n")
print(tidy(fit_air_ex))
cat("Q4 - (p,d,q) = (0, 2, 1)\n")

#Q5 - Fit ARIMA to Consumption, forecast 4 quarters
fit_cons <- us_change %>%
  model(ARIMA(Consumption))

report(fit_cons)
# Model: ARIMA(1,0,3)(1,0,1)[4] w/ mean

fc_cons <- forecast(fit_cons, h = 4)
cat("\nQ5 - All 4 forecasted Consumption values:\n")
print(fc_cons %>% as_tibble() %>% select(Quarter, .mean))

# 2020 Q2
q2_fc <- fc_cons %>% filter(lubridate::quarter(Quarter) == 2)
cat("Q5 - Forecasted Consumption Q2:", round(q2_fc$.mean, 4), "\n")  # 0.7303

#Q6 - Ljung-Box p-value at lag 10 (residual diagnostics)
fit_cons %>% gg_tsresiduals(lag_max = 10)

# check_residuals() uses dof=0 
lb_cons <- augment(fit_cons) %>%
  features(.innov, ljung_box, lag = 10, dof = 0)

cat("Q6 - Ljung-Box p-value at lag 10:", round(lb_cons$lb_pvalue, 4), "\n")  # 0.9839

#Q7 
dept_nsw <- aus_retail %>%
  filter(Industry == "Department stores",
         State == "New South Wales")

#Fit seasonal ARIMA (automatic selection)
fit_dept <- dept_nsw %>%
  model(ARIMA(Turnover))

report(fit_dept)
# Model: ARIMA(2,1,1)(1,1,2)[12] -> seasonal differencing D = 1
cat("Q7 - Seasonal differencing order D: 1\n")

#Q8 - Train on data prior to 2013, forecast remainder, compute RMSE
dept_train <- dept_nsw %>% filter(year(Month) < 2013)
dept_test  <- dept_nsw %>% filter(year(Month) >= 2013)

fit_dept_train <- dept_train %>%
  model(ARIMA(Turnover))

report(fit_dept_train)

fc_dept <- forecast(fit_dept_train, new_data = dept_test)

acc_dept <- accuracy(fc_dept, dept_nsw)
cat("Q8 - RMSE on test set:", round(acc_dept$RMSE, 4), "\n")  # 37.5836


#Q9 & Q10

#aggregate to daily totals
vic_daily <- vic_elec %>%
  index_by(Date = as_date(Time)) %>%
  summarise(Demand = sum(Demand))

#Fit seasonal ARIMA
fit_elec <- vic_daily %>%
  model(ARIMA(Demand))

report(fit_elec)
# Model: ARIMA(2,0,2)(2,1,0)[7]

elec_coefs <- tidy(fit_elec)
cat("\nQ9/Q10 - All estimated coefficients:\n")
print(elec_coefs)

ar1_coef <- elec_coefs %>%
  filter(term == "ar1") %>%
  pull(estimate)

cat("Q9  - AR(1) coefficient:", round(ar1_coef, 4), "\n")  # 0.9324
cat("Q10 - (p,d,q)(P,D,Q) = (2,0,2)(2,1,0)[7]\n")

#Q11 - Training set through Q4 2015
income_train <- us_change %>%
  filter(Quarter <= yearquarter("2015 Q4"))

fit_income <- income_train %>%
  model(ARIMA(Income))

report(fit_income)
# Model: ARIMA(0,0,2) w/ mean

fc_income <- forecast(fit_income, h = 4)
cat("\nQ11 - Income forecast (next 4 quarters):\n")
print(fc_income %>% as_tibble() %>% select(Quarter, .mean))

actual_q1_2016 <- us_change %>%
  filter(Quarter == yearquarter("2016 Q1")) %>%
  pull(Income)

error_q11 <- actual_q1_2016 - fc_income$.mean[1]
cat("Q11 - Actual Income Q1 2016:", actual_q1_2016, "\n")
cat("Q11 - Forecast Income Q1 2016:", round(fc_income$.mean[1], 4), "\n")
cat("Q11 - Forecast error (actual - forecast):", round(error_q11, 4), "\n")  # -0.1021


#Q12 - Fit ARIMA to Savings
fit_savings <- us_change %>%
  model(ARIMA(Savings))

report(fit_savings)
# Model: ARIMA(1,0,0) w/ mean -> (p,d,q) = (1,0,0)
cat("Q12 - (p,d,q) = (1, 0, 0)\n")

#Q13 - Fit ARIMA to Unemployment
fit_unemp <- us_change %>%
  model(ARIMA(Unemployment))

report(fit_unemp)

# std dev of residuals = sqrt(sigma^2) from the fitted model
resid_sd <- sqrt(glance(fit_unemp)$sigma2)

cat("Q13 - Std dev of residuals:", round(resid_sd, 4), "\n")  # 0.2914

#Q14 - Width of 95% prediction interval for first forecasted value
fc_cons_hilo <- fc_cons %>%
  hilo(95)

hi_col <- fc_cons_hilo %>% pull(`95%`)
lower_95 <- unclass(hi_col)$lower
upper_95 <- unclass(hi_col)$upper

width_q14 <- upper_95[1] - lower_95[1]
cat("Q14 - 95% PI width for first forecast:", round(width_q14, 4), "\n")  # 2.2600

#Q15 - Filter Victoria Clothing data
vic_cloth <- aus_retail %>%
  filter(Industry == "Clothing, footwear and personal accessory retailing",
         State == "Victoria")

#fit seasonal ARIMA
fit_cloth <- vic_cloth %>%
  model(ARIMA(Turnover))

report(fit_cloth)
# Model: ARIMA(0,1,2)(0,1,1)[12]

#forecast 12 months ahead
fc_cloth <- forecast(fit_cloth, h = 12)
cat("\nQ15 - 12-month forecast:\n")
print(fc_cloth %>% as_tibble() %>% select(Month, .mean))

#extract the next October in the forecast window
oct_fc <- fc_cloth %>%
  filter(month(Month) == 10)

cat("Q15 - Forecasted next October value:", round(oct_fc$.mean, 4), "\n")  # 639.2668
