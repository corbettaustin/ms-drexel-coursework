# Quiz 3 - Time Series Forecasting

#Q1
#The Naive method of forecasting is also known as random walk forecasting.
#ANSWER: TRUE
#The naive method (NAIVE()) sets every forecast equal to the last observed value, which is exactly what a random walk model implies. 

#Q2
#In order for a forecasting method to produce point forecasts, there must be an underlying probability model associated with that method.
#ANSWER: FALSE
#point forecasts are just single-value outputs from any algorithm, no probability model required. 
#prediction intervals require distributional assumptions, but point forecasts don't. 

#Q3
#Which of the following is NOT a true statement about forecast errors vs. residuals?
#ANSWER: Option C is the NOT-true statement.
#incorrectly frames residuals as measuring against some "true" underlying quantity rather than against the model's fitted value.

#Q4 and Q5

library(fpp3)  # loads tsibble, fable, feasts, and the us_gasoline dataset

#inspect data
glimpse(us_gasoline)

#build training set - data through end of 2005
gas_train <- us_gasoline |>
  filter(year(Week) <= 2005)

#identify (the test point) (actual value for week 01 of 2006)
gas_week1_2006 <- us_gasoline |>
  filter(year(Week) == 2006) |>
  slice(1)   # first row = first week of 2006

actual_value <- gas_week1_2006$Barrels
cat("Actual Barrels for week 01, 2006:", actual_value, "\n")

#fit harmonic regression models for K = 1 to 10
aicc_values <- numeric(10)  # store AICc for each K

for (k in 1:10) {
  fit_k <- gas_train |>
    model(TSLM(Barrels ~ trend() + fourier(K = k)))
  aicc_values[k] <- glance(fit_k)$AICc
}

#display AICc table 
aicc_table <- data.frame(K = 1:10, AICc = round(aicc_values, 4))
print(aicc_table)

#best K
best_K <- which.min(aicc_values)
cat("\nBest K (lowest AICc):", best_K,
    "| AICc =", round(min(aicc_values), 4), "\n")

#fit best model
best_fit <- gas_train |>
  model(TSLM(Barrels ~ trend() + fourier(K = best_K)))

#summary
report(best_fit)

#forecast one step ahead (week 01 of 2006)
fc <- forecast(best_fit, h = 1)

# Q4 ANSWER - the mean column holds point forecast (conditional mean)
point_forecast <- fc$.mean[1]
cat("\nQuestion 4 -- Point forecast for week 01 in 2006:", round(point_forecast, 4), "\n")

# Q5 ANSWER - forecast error = actual observed value minus point forecast
forecast_error <- actual_value - point_forecast
cat("Question 5 -- Forecast error for week 01 in 2006:", round(forecast_error, 4), "\n")
