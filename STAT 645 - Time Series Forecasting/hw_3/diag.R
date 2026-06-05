library(fpp3)

cat("========== Q2 DIAGNOSTICS ==========\n")
w5000 <- olympic_running %>%
  filter(Length == 5000, Sex == "women")

fit_w5000 <- w5000 %>% model(TSLM(Time ~ trend()))

fc_w5000 <- fit_w5000 %>% forecast(h = 3)

# Print full forecast with hilo at 93%
pi_all <- fc_w5000 %>% hilo(level = 93)
print(pi_all)

# Extract each bound individually
pi_2028 <- pi_all %>% filter(Year == 2028)
cat("Full 2028 PI row:\n")
print(pi_2028)
cat("Lower:", pi_2028 %>% pull(`93%`) %>% .$lower, "\n")
cat("Upper:", pi_2028 %>% pull(`93%`) %>% .$upper, "\n")

# Also check forecast point estimates precisely
cat("\nAll .mean forecasts:\n")
print(fc_w5000 %>% as_tibble() %>% select(Year, .mean))

cat("\n========== Q7 DIAGNOSTICS ==========\n")
japan <- global_economy %>% filter(Country == "Japan")

# Check for NAs in Exports
cat("NA count in Exports:", sum(is.na(japan$Exports)), "\n")
cat("Years with NA Exports:\n")
print(japan %>% filter(is.na(Exports)) %>% select(Year, Exports))
cat("Total rows in Japan data:", nrow(japan), "\n")

fit_japan <- japan %>%
  model(
    ANN = ETS(Exports ~ error("A") + trend("N") + season("N")),
    AAN = ETS(Exports ~ error("A") + trend("A") + season("N"))
  )

# Exact AICc values
aicc_exact <- glance(fit_japan) %>% select(.model, AIC, AICc, BIC)
cat("Exact AIC/AICc/BIC:\n")
print(aicc_exact, digits = 10)

cat("\n========== Q10 DIAGNOSTICS ==========\n")
nz_arrivals <- aus_arrivals %>% filter(Origin == "NZ")

nz_train <- nz_arrivals %>% filter(Quarter <= yearquarter("2010 Q3"))

fit_nz <- nz_train %>%
  model(
    ets         = ETS(Arrivals),
    ets_log     = ETS(log(Arrivals)),
    snaive      = SNAIVE(Arrivals),
    stl_ets_log = decomposition_model(
      STL(log(Arrivals) ~ season(window = "periodic")),
      ETS(season_adjust)
    )
  )

# Print which ETS models were selected
cat("Models selected:\n")
print(fit_nz)

fc_nz <- fit_nz %>% forecast(h = 8)
acc_nz <- fc_nz %>% accuracy(nz_arrivals)

# Print exact RMSE values
cat("\nExact RMSE values:\n")
print(acc_nz %>% select(.model, RMSE) %>% arrange(RMSE), digits = 10)
