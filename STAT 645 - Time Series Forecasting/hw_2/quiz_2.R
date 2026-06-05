#STAT 645-901 Quiz 2
#Corbett Austin

#Q1 What is the frequency of the data set prices  in the fpp3 package?

library(fpp3)
#print
prices
#index column is 'year' with annual frequency [1Y]

#Q2 Using the dataset us_total found in the package USgas, what is the largest autocorrelation for the state of California? Enter your answer to three decimal places.

library(USgas)
library(tsibble)
library(feasts)
library(dplyr)

#filter and convert to tsibble
us_total_ts <- us_total %>%
  filter(state == "California") %>%
  as_tsibble(index = year, key = state)

#compute ACF
acf_vals <- us_total_ts %>%
  ACF(y) %>%
  as_tibble()

#find max
max(acf_vals$acf)

#Q3 Match the correct correlogram for the aus_airpassengers data in the fpp3 package.

aus_airpassengers %>% ACF(Passengers) %>% autoplot()
#strong trend, no seasonality. declines to zero around lag 16

#Q4 Using the us_employment dataset from the fpp3 package, what is the Series_ID for the series with the smallest seasonal strength measure? (copy and paste the 13 digit alpha-numeric value).

#compute STL decomposition features, filter 
us_employment %>%
  features(Employed, feat_stl) %>%
  filter(seasonal_strength_year == min(seasonal_strength_year)) %>%
  select(Series_ID, seasonal_strength_year)

#Q5 Using the canadian_gas dataset from the fpp3 package, find the best value of Lambda to tranform Volume (use a Box_Cox transformation). (Enter your answer to four decimal places).
#use guerrero method to find optimal lambda  
lambda <- canadian_gas %>%
  features(Volume, features = guerrero) %>%
  pull(lambda_guerrero)

round(lambda, 4)

