library(fpp3)

cat("========== Q2: Full precision lower bound ==========\n")
w5000 <- olympic_running %>%
  filter(Length == 5000, Sex == "women")

fit_w5000 <- w5000 %>% model(TSLM(Time ~ trend()))

fc_w5000 <- fit_w5000 %>% forecast(h = 3)

# Show hilo for ALL 3 years
pi_all <- fc_w5000 %>% hilo(level = 93)

# Extract lower/upper for each year
for (yr in c(2020, 2024, 2028)) {
  row <- pi_all %>% filter(Year == yr)
  lo <- row %>% pull(`93%`) %>% .$lower
  hi <- row %>% pull(`93%`) %>% .$upper
  cat(sprintf("Year %d: lower = %.10f, upper = %.10f\n", yr, lo, hi))
}

# Also print the model tidy to see exact coefficients
cat("\nModel coefficients (full precision):\n")
tidy_w5000 <- tidy(fit_w5000)
print(tidy_w5000, digits = 15)

# Also check: what does glance give?
cat("\nGlance:\n")
print(glance(fit_w5000), digits = 15)

cat("\n========== Q7: Exact AICc values ==========\n")
japan <- global_economy %>% filter(Country == "Japan")
fit_japan <- japan %>%
  model(
    ANN = ETS(Exports ~ error("A") + trend("N") + season("N")),
    AAN = ETS(Exports ~ error("A") + trend("A") + season("N"))
  )

gl <- glance(fit_japan)
cat("ANN AICc:", sprintf("%.10f\n", gl %>% filter(.model=="ANN") %>% pull(AICc)))
cat("AAN AICc:", sprintf("%.10f\n", gl %>% filter(.model=="AAN") %>% pull(AICc)))
cat("ANN AIC: ", sprintf("%.10f\n", gl %>% filter(.model=="ANN") %>% pull(AIC)))
cat("AAN AIC: ", sprintf("%.10f\n", gl %>% filter(.model=="AAN") %>% pull(AIC)))

cat("\n========== Q10: Exact RMSE values ==========\n")
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

fc_nz <- fit_nz %>% forecast(h = 8)
acc_nz <- fc_nz %>% accuracy(nz_arrivals)

cat("Exact RMSE values:\n")
rmse_vals <- acc_nz %>% select(.model, RMSE) %>% arrange(RMSE)
for (i in seq_len(nrow(rmse_vals))) {
  cat(sprintf("  %-15s RMSE = %.10f\n", rmse_vals$.model[i], rmse_vals$RMSE[i]))
}
