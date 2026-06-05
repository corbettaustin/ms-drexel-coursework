# ============================================================
# Time Series Forecasting — HW 2
# Packages: fpp3, seasonal, feasts, fabletools
# All datasets are built into fpp3
# ============================================================

library(fpp3)
library(seasonal)   # needed for X-13ARIMA-SEATS (Q14)

# ============================================================
# Q1 & Q2 — global_economy: GDP per capita rankings
# ============================================================

# Calculate average GDP per capita for each country across all years
gdp_avg <- global_economy %>%
  arrange(Country, Year) %>%
  filter(!is.na(GDP), !is.na(Population), Population > 0) %>%
  mutate(GDP_per_capita = GDP / Population) %>%
  as_tibble() %>%                    # drop tsibble to avoid temporal warnings
  group_by(Country) %>%
  summarise(avg_gdppc = mean(GDP_per_capita, na.rm = TRUE)) %>%
  arrange(desc(avg_gdppc))

gdp_avg %>% slice(6) %>% pull(avg_gdppc)   # Q1
gdp_avg %>% slice(6) %>% pull(Country)     # Q2

# ============================================================
# Q3 — global_economy: Last recorded GDP per capita for Liechtenstein
# ============================================================

global_economy %>%
  filter(Country == "Liechtenstein") %>%
  mutate(GDP_per_capita = GDP / Population) %>%
  filter(!is.na(GDP_per_capita)) %>%
  slice_tail(n = 1) %>%
  pull(GDP_per_capita)

# ============================================================
# Q4 — global_economy: Guerrero lambda for Box-Cox (Mali)
# ============================================================

mali <- global_economy %>%
  filter(Country == "Mali") %>%
  arrange(Year) %>%
  mutate(GDP_per_capita = GDP / Population) %>%
  filter(!is.na(GDP_per_capita), !is.na(Population), Population > 0)

lambda_mali <- mali %>%
  as_tsibble(index = Year) %>%
  features(GDP_per_capita, features = guerrero)

lambda_mali
lambda_mali %>% print(digits = 6)
# ============================================================
# Q5 — aus_livestock: Box-Cox for Victorian Bulls, bullocks and steers
# (visual question — run the plots and compare)
# ============================================================

bulls <- aus_livestock %>%
  filter(
    Animal == "Bulls, bullocks and steers",
    State == "Victoria"
  )

# Find optimal lambda
lambda_bulls <- bulls %>%
  features(Count, features = guerrero) %>%
  pull(lambda_guerrero)

lambda_bulls  # use this to identify which plot is correct

# Plot original vs transformed to compare
bulls %>% autoplot(Count) +
  labs(title = "Original — Victorian Bulls")

bulls %>% autoplot(box_cox(Count, lambda_bulls)) +
  labs(title = paste("Box-Cox transformed (lambda =", round(lambda_bulls, 3), ")"))

# ============================================================
# Q6 — Moving averages: Middle weight of a 7-MA
# ============================================================

# A 7-MA has weights: each of the 7 terms gets weight 1/7
# The middle (4th) weight is simply 1/7
1/7   # answer to at least 3 decimal places

# ============================================================
# Q7 — Moving averages: Last weight of a 2x12-MA
# ============================================================

# A 2x12-MA: first apply 12-MA, then apply 2-MA to the result
# The resulting weights: endpoints get 1/24, all interior months get 2/24 = 1/12
# Last weight = 1/24
1/24   # answer to at least 4 decimal places

# ============================================================
# Q8 — Moving averages: Number of terms in a 3x7-MA
# ============================================================

# A 3x7-MA applies a 7-MA then a 3-MA to the result
# Total terms spanned: (7 - 1)/2 + (3 - 1)/2 on each side = 3 + 1 = 4 each side
# Total terms = 3 + 7 - 2 = ... use the formula: m1 + m2 - 1
# 3x7-MA: the combined order is 3*7 = 21, but unique terms = 7 + 3 - 1 = 9
# Actually: a k-MA of a m-MA uses (k-1)/2 + (m-1)/2 extra terms each side
# 3x7 spans: centre +/- (3-1)/2 + (7-1)/2 = 1 + 3 = 4 lags each side → 9 total terms
9

# ============================================================
# Q9 — Moving averages: Second to last weight in a 3x7-MA
# ============================================================

# Build the weights explicitly
# 7-MA weights: all 1/7
# Then apply 3-MA (weights 1/3 each) to the result
# Combined weights for 3x7-MA:
m <- 7
k <- 3
w7 <- rep(1/m, m)                  # 7 weights of 1/7
w3 <- rep(1/k, k)                  # 3 weights of 1/3
combined <- convolve(w3, rev(w7), type = "open")  # convolve to get combined weights
combined   # inspect all weights
# The second to last weight:
combined[length(combined) - 1]

# ============================================================
# Q10 — vic_elec: First non-NA value of 3x7-MA
# ============================================================

vic_elec_ma <- vic_elec %>%
  mutate(
    ma7  = slider::slide_dbl(Demand, mean, .before = 3, .after = 3, .complete = TRUE),
    ma3x7 = slider::slide_dbl(ma7,  mean, .before = 1, .after = 1, .complete = TRUE)
  )

# First non-NA 3x7-MA value
vic_elec_ma %>%
  filter(!is.na(ma3x7)) %>%
  head(1) %>%
  pull(ma3x7)

# ============================================================
# Q11 — aus_production: Classical decomposition (multiplicative), Gas
# Remainder for Q2 2009
# ============================================================

gas <- tail(aus_production, 5*4) %>% select(Gas)

gas_dcmp <- gas %>%
  model(classical_decomposition(Gas, type = "multiplicative")) %>%
  components()

gas_dcmp

# Filter for Quarter 2 of 2009
gas_dcmp %>%
  filter(Quarter == yearquarter("2009 Q2")) %>%
  pull(remainder)

# ============================================================
# Q12 — aus_production: Classical decomposition (additive), Cement
# Seasonally adjusted for Q4 2007
# ============================================================

cement_gas <- tail(aus_production, 5*4) %>% select(Cement)

cement_dcmp <- cement_gas %>%
  model(classical_decomposition(Cement, type = "additive")) %>%
  components()

cement_dcmp

# Seasonally adjusted = trend + remainder (or the season_adjust column)
cement_dcmp %>%
  filter(Quarter == yearquarter("2007 Q4")) %>%
  pull(season_adjust)

# ============================================================
# Q13 — canadian_gas: STL decomposition, biggest seasonal component
# ============================================================

canadian_gas %>%
  model(STL(Volume)) %>%
  components() %>%
  pull(season_year) %>%
  max()

# ============================================================
# Q14 — canadian_gas: X-13ARIMA-SEATS (x11), smallest seasonal component date
# ============================================================

x11_fit <- canadian_gas %>%
  model(X_13ARIMA_SEATS(Volume ~ x11()))

x11_components <- components(x11_fit)

# When is the smallest seasonal component?
x11_components %>%
  filter(seasonal == min(seasonal)) %>%
  pull(Month)

# ============================================================
# Q15 — PBS: Highest strength of seasonality (FS) for Scripts
# ============================================================

pbs_features <- PBS %>%
  features(Scripts, feat_stl)

pbs_features %>%
  arrange(desc(seasonal_strength_year)) %>%
  slice(1) %>%
  select(ATC1, ATC2, seasonal_strength_year)

# ============================================================
# Q16 — PBS: Smallest strength of trend (FT) across all series
# ============================================================

PBS %>%
  features(Scripts, feat_stl) %>%
  summarise(min_FT = min(trend_strength, na.rm = TRUE))

# ============================================================
# Q17 — Conceptual (no code needed)
# Answer: "The autocorrelation of a time series at various lags"
# (Time series features are summary statistics computed from the series itself)
# ============================================================

# Q17 answer: The autocorrelation of a time series at various lags

# ============================================================
# Q18 — Conceptual (no code needed)
# Answer: "autocorrelation function (ACF)"
# feasts::ACF() computes the autocorrelation at various lags
# ============================================================

# Q18 answer: autocorrelation function (ACF)

# ============================================================
# Q19 — PBS: Third autocorrelation for Cost
# General concession, Safety net, A11 ATC2
# ============================================================

pbs_a11 <- PBS %>%
  filter(
    Concession == "General",
    Type == "Safety net",
    ATC2 == "A11"
  )

acf_cost <- pbs_a11 %>%
  ACF(Cost, lag_max = 10)

acf_cost  # inspect — third row is lag 3
acf_cost %>% slice(3) %>% pull(acf)

# ============================================================
# Q20 — PBS: Sum of squares of first 10 autocorrelations for Scripts
# General concession, Safety net, A11 ATC2
# ============================================================

acf_scripts <- pbs_a11 %>%
  ACF(Scripts, lag_max = 10)

acf_scripts %>%
  summarise(sum_sq = sum(acf^2))

# ============================================================
# Q21 — aus_production: SNAIVE forecast for Bricks, Q1 2010
# ============================================================

bricks <- aus_production %>%
  filter(!is.na(Bricks)) %>%
  select(Quarter, Bricks)

bricks_snaive <- bricks %>%
  model(SNAIVE(Bricks)) %>%
  forecast(h = "5 years")

bricks_snaive %>%
  filter(Quarter == yearquarter("2010 Q1")) %>%
  pull(.mean)

# ============================================================
# Q22 — aus_production: Random Walk with drift for Bricks, Q1 2010
# ============================================================

bricks_rwd <- bricks %>%
  model(RW(Bricks ~ drift())) %>%
  forecast(h = "5 years")

bricks_rwd %>%
  filter(Quarter == yearquarter("2010 Q1")) %>%
  pull(.mean)

# ============================================================
# Q23 — aus_production: Cement, RW with drift, RMSE on test set
# Train: Q1 1980 – Q3 2006 | Test: remainder
# ============================================================

cement <- aus_production %>%
  select(Quarter, Cement) %>%
  filter(!is.na(Cement))

cement_train <- cement %>% filter(Quarter <= yearquarter("2006 Q3"))
cement_test  <- cement %>% filter(Quarter > yearquarter("2006 Q3"))

fit <- cement_train %>% model(RW(Cement ~ drift()))
fc  <- fit %>% forecast(h = nrow(cement_test))

fc %>% accuracy(cement_test) %>% pull(RMSE)

cement <- aus_production %>%
  select(Quarter, Cement) %>%
  filter(!is.na(Cement))

cement_train <- cement %>%
  filter(Quarter >= yearquarter("1980 Q1"),
         Quarter <= yearquarter("2006 Q3"))

cement_test <- cement %>%
  filter(Quarter > yearquarter("2006 Q3"))

fit <- cement_train %>% model(RW(Cement ~ drift()))
fc  <- fit %>% forecast(h = nrow(cement_test))

fc %>% accuracy(cement_test) %>% pull(RMSE)
# ============================================================
# Q24 — aus_production: Cement, SNAIVE, RMSE on test set
# Same train/test split as Q23
# ============================================================

cement_snaive_fit <- cement_train %>%
  model(SNAIVE(Cement))

cement_snaive_fc <- cement_snaive_fit %>%
  forecast(h = nrow(cement_test))

cement_snaive_fc %>%
  accuracy(cement_test) %>%
  pull(RMSE)

# ============================================================
# Q25 & Q26 — aus_retail: Australian takeaway food turnover
# Withhold last 4 years as test set
# Fit all benchmark methods, find best RMSE, check residuals
# ============================================================

# Filter to takeaway food
takeaway <- aus_retail %>%
  filter(Industry == "Takeaway food services") %>%
  summarise(Turnover = sum(Turnover))  # aggregate across states if needed

# Define train/test split (last 4 years = 48 months as test)
takeaway_train <- takeaway %>%
  filter(Month <= max(Month) %m-% months(48))

takeaway_test <- takeaway %>%
  filter(Month > max(Month) %m-% months(48))

# Fit all four benchmark methods
takeaway_fit <- takeaway_train %>%
  model(
    Mean   = MEAN(Turnover),
    Naive  = NAIVE(Turnover),
    SNaive = SNAIVE(Turnover),
    RWDrift = RW(Turnover ~ drift())
  )

# Forecast the test period
takeaway_fc <- takeaway_fit %>%
  forecast(h = nrow(takeaway_test))

# Q25: Accuracy on test set — find best (lowest) RMSE
takeaway_accuracy <- takeaway_fc %>%
  accuracy(takeaway_test)

takeaway_accuracy %>%
  arrange(RMSE) %>%
  select(.model, RMSE)

# Best RMSE:
takeaway_accuracy %>%
  slice_min(RMSE) %>%
  pull(RMSE)

# Q26: Do residuals for best model resemble white noise?
best_model_name <- takeaway_accuracy %>%
  slice_min(RMSE) %>%
  pull(.model)

takeaway_fit %>%
  select(all_of(best_model_name)) %>%
  gg_tsresiduals()
# Inspect the plot — if ACF bars are mostly within bounds and
# residuals look random, answer is TRUE. Otherwise FALSE.
