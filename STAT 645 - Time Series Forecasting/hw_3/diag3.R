library(fpp3)

cat("========== Q2 REVISED: Add 2020 & 2024 to training, forecast 2028 ==========\n")

w5000 <- olympic_running %>%
  filter(Length == 5000, Sex == "women")

# Add 2020 (Sifan Hassan, 14:36.79) and 2024 (Beatrice Chebet, 14:28.56)
w5000_extended <- w5000 %>%
  bind_rows(
    tibble(Year = 2020L, Length = 5000L, Sex = "women", Time = 14*60 + 36.79),
    tibble(Year = 2024L, Length = 5000L, Sex = "women", Time = 14*60 + 28.56)
  ) %>%
  as_tsibble(index = Year, key = c(Length, Sex))

cat("Extended dataset:\n")
print(w5000_extended)

fit_extended <- w5000_extended %>%
  model(TSLM(Time ~ trend()))

report(fit_extended)

# Forecast h=1 → 2028
fc_extended <- fit_extended %>% forecast(h = 1)
cat("\nForecast for 2028:\n")
print(fc_extended %>% as_tibble() %>% select(Year, .mean))

pi_2028 <- fc_extended %>% hilo(level = 93)
lo <- pi_2028 %>% pull(`93%`) %>% .$lower
hi <- pi_2028 %>% pull(`93%`) %>% .$upper
cat(sprintf("Q2 revised lower bound (93%% PI, 2028): %.4f\n", lo))
cat(sprintf("Q2 revised upper bound (93%% PI, 2028): %.4f\n", hi))

cat("\n========== Q7 & Q8: Both ANN and AAN results ==========\n")
japan <- global_economy %>% filter(Country == "Japan")

fit_japan <- japan %>%
  model(
    ANN = ETS(Exports ~ error("A") + trend("N") + season("N")),
    AAN = ETS(Exports ~ error("A") + trend("A") + season("N"))
  )

cat("--- ANN ---\n")
report(fit_japan %>% select(ANN))

cat("--- AAN ---\n")
report(fit_japan %>% select(AAN))

cat("\nAICc summary:\n")
gl <- glance(fit_japan) %>% select(.model, AIC, AICc, BIC) %>% arrange(AICc)
cat(sprintf("  ANN  AICc = %.4f\n", gl %>% filter(.model=="ANN") %>% pull(AICc)))
cat(sprintf("  AAN  AICc = %.4f\n", gl %>% filter(.model=="AAN") %>% pull(AICc)))

cat("\nQ8 - 4-step forecast for each model:\n")
fc_ann <- fit_japan %>% select(ANN) %>% forecast(h = 4)
fc_aan <- fit_japan %>% select(AAN) %>% forecast(h = 4)

ann_h4 <- fc_ann %>% slice_tail(n = 1) %>% pull(.mean)
aan_h4 <- fc_aan %>% slice_tail(n = 1) %>% pull(.mean)

cat(sprintf("  ANN h=4 forecast: %.4f\n", ann_h4))
cat(sprintf("  AAN h=4 forecast: %.4f\n", aan_h4))
