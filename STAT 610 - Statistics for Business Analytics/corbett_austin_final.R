## Stat 610 Fall 2025
## Final Exam solutions
## Corbett Austin

##Question 1.
##
#import data
trades <- read.csv("Trades(1).csv")
summary(trades)

#scatter plot
plot(trades$Calls, trades$Executions,
     xlab = "Number of Calls",
     ylab = "Number of Executions",
     main = "Trade Executions vs Phone Calls")


##Question 2.
##

#calculate correlation
correlation <- cor(trades$Calls, trades$Executions)
correlation
round(correlation, 3)

##Question 3.
##

#fit linear regression model
model <- lm(Executions ~ Calls, data = trades)

#extract b1 coefficient
b1 <- coef(model)[2]
round(b1, 3)

##Question 4.
##

#extract standard error
sigma <- summary(model)$sigma
round(sigma, 3)

##Question 5.
##

#extract t-value for Calls
t_value <- summary(model)$coefficients["Calls", "t value"]
round(t_value, 3)

##Question 6.
##

#calc 98% prediction interval for Calls = 1500
new_data <- data.frame(Calls = 1500)
prediction <- predict(model,
                      newdata = new_data,
                      interval = "prediction",
                      level = 0.98)

round(prediction[, "upr"], 3)

##Question 7.
##

#calc R^2 in a linear regression using correlation
r_squared <- correlation^2
round(r_squared, 4)

##Question 8.
##

#average**

##Question 9.
##

#A,B,E,F

##Question 10.
##

#given
n <- 29
xbar <- 80.9
s <- 26
df <- n - 1

#calc t-value
t_value <- qt(0.995, df)
upper_limit <- xbar + t_value * (s / sqrt(n))

round(upper_limit, 1)

##Question 11.
##

#given
n <- 301
x <- 58

#calc sample proportion
p_hat <- x / n

#find z critical value for 98% CI
Z_critical <- qnorm(.99)

#calc standard error
SE <- sqrt(p_hat * (1 - p_hat) / n)

#calc lower limit
lower_limit <- p_hat - Z_critical * SE

round(lower_limit, 3)

##Question 12.
##

#given
x <- c(0.29, 0.47, 0.28, 0.39, 0.11, 0.19, 0.13, 0.17, 
       0.28, 0.26, 0.16, 0.37, 0.12, 0.12, 0.31)

#calc MLE
n <- length(x)
theta_hat <- -n / sum(log(x))
round(theta_hat, 3)

##Question 13.
##

#given
sigma <- 100.1
E <- 24.8
conf_level <- 0.97

#calc z critical value
z_critical <- qnorm((1 + conf_level) / 2)

#calc sample size
n <- (z_critical * sigma / E)^2
n


##Question 14.
##

set.seed(123)
n <- 200
s <- 1
x1 <- rnorm(n, 2, 0.52)
x2 <- rnorm(n, -1, 0.12)
x3 <- x1*x2
e <- rnorm(n, 0, s)
y <- 5 + 0.9*x1 + 3*x2 + e  #true model
summary(y)

## Min. 1st Qu. Median Mean 3rd Qu. Max. 
## 0.8576 3.0472 3.9772 3.8429 4.5566 7.1697

##Question 15.
##

#fit model
model1 <- lm(y ~ x1 + x2 +x3)

#calculate r-squared
r_squared <- summary(model1)$r.squared
round(r_squared, 4)


##Question 16.
##

#fit model
model2 <- lm(y ~ x1 + x2)

#calculate r-squared
r_squared <- summary(model2)$r.squared
round(r_squared, 4)

##Question 17.
##

#fit model
model3 <- lm(y ~ x1 + x3)

#calculate r-squared
r_squared <- summary(model3)$r.squared
round(r_squared, 4)

##Question 18.
##

#model2 matches x1 and x2 in true model


##Question 19.
##

#fit models
model1 <- lm(y ~ x1 + x2 +x3)
model2 <- lm(y ~ x1 + x2)

#ANOVA comparison
anova_comparison <- anova(model2, model1)

#calc p-value
p_value <- anova_comparison[2, "Pr(>F)"]

round(p_value, 4)

##Question 20.
##
model1 <- lm(y ~ x1 + x2 +x3)
model2 <- lm(y ~ x1 + x2)
model3 <- lm(y ~ x1 + x3)
summary(model1)
summary(model2)
summary(model3)

anova(model3, model1)

##Question 21.
##

#import data
toyota <- read.csv("ToyotaCorolla(2).csv")
head(toyota)

#fit model
model1 <- lm(Price ~ Age + KM + Fuel_Type + HP + Automatic +
               cc + Doors + Gears + Quarterly_Tax + Weight,
             data = toyota)

#calc f-statistic
f_stat <- summary(model1)$fstatistic[1]
round(f_stat, 1)

##Question 22.
##

#check significance of each variable
summary_model1 <- summary(model1)
coef_table <- summary_model1$coefficients

#display p-values
cat("P-values for each predictor:\n")
print(round(coef_table[-1, "Pr(>|t|)"], 4))

#identify three non-significant variables (p > 0.05)
p_vals <- coef_table[-1, "Pr(>|t|)"]
non_sig_vars <- names(sort(p_vals[p_vals > 0.05], decreasing = TRUE))

cat("\nThree non-significant variables:\n")
print(non_sig_vars)

#remove and fit new model
model2 <- lm(Price ~ Age + KM + Fuel_Type + HP + Gears + Quarterly_Tax + Weight,
             data = toyota)

partial_f_test <- anova(model2, model1)
partial_f_test

partial_f <- partial_f_test$F[2]
round(partial_f, 4)

##Question 23.
##


##Question 24.
##
#fit models
model1 <- lm(Price ~ Age + KM + Fuel_Type + HP + Automatic +
               cc + Doors + Gears + Quarterly_Tax + Weight,
             data = toyota)
model2 <- lm(Price ~ Age + KM + Fuel_Type + HP + Gears + Quarterly_Tax + Weight,
             data = toyota)

#compare models
anova_comparison <- anova(model2, model1)
p_value_comparison <- anova_comparison$`Pr(>F)`[2]
anova_comparison
p_value_comparison

if(p_value_comparison > 0.05) {
  best_model <- model2
  cat("Model 2 (reduced) is better\n")
} else {
  best_model <- model1
  cat("Model 1 (full) is better\n")
}
best_model <- model2

#create new data point
new_car <- data.frame(
  Age = 57,
  KM = 45000,
  Fuel_Type = "Diesel",
  HP = 100,
  Gears = 5,
  Quarterly_Tax = 88,
  Weight = 1050
)
print(new_car)

#calc prediction interval
prediction <- predict(best_model,
                      newdata = new_car,
                      interval = "prediction",
                      level = 0.95)

#extract upper limit
upper_limit <- prediction[, "upr"]
round(upper_limit, 2)


