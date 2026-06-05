dat_train <- read.csv("HousingBoston_train.csv", stringsAsFactors = TRUE)
dat_test <- read.csv("HousingBoston_test.csv", stringsAsFactors = TRUE)

dat_train$mvalue <- 1*(dat_train$mvalue == "below")
dat_test$mvalue <- 1*(dat_test$mvalue == "below")

#Q1 
#plot average number of rooms per dwelling
plot(dat_train$rm, col = as.factor(dat_train$mvalue),
     ylab = "Average number of rooms per dwelling", xlab = "Observations")
legend('bottomright', legend = unique(as.factor(dat_train$mvalue)),
       pch = 1, col = unique(as.factor(dat_train$mvalue)))

q1 <- 2

#Q2
#build logisitic regression model
model_rm <- glm(mvalue ~ rm, data = dat_train, family = binomial)

# extract coefficient for rm (beta_1)
q2 <- coef(model_rm)["rm"]

#Q3
# predict probability for house with 5.5 rooms
new_data <- data.frame(rm = 5.5)
q3 <- predict(model_rm, newdata = new_data, type = "response")

#Q4
# predict probabilities on test set
pred_probs_rm <- predict(model_rm, newdata = dat_test, type = "response")

# apply threshold of 0.2
pred_class_rm_02 <- ifelse(pred_probs_rm >= 0.2, 1, 0)

# create confusion matrix
# correctly classified as BELOW median = True Positives
# (actual = 1, predicted = 1)
q4 <- sum(dat_test$mvalue == 1 & pred_class_rm_02 == 1)

#Q5
#misclassification error = (false positives + false negatives / total )
misclass_rm_02 <- mean(pred_class_rm_02 != dat_test$mvalue)
q5 <- misclass_rm_02

#Q6 initial choice is correct
q6 <- 1

#Q7
#build logistic regression model
model_age_rm <- glm(mvalue ~ age + rm, data = dat_train, family = binomial)

summary(model_age_rm)

#analyze coefficients
coefs <- coef(model_age_rm)

q7 <- c(2, 4)

#Q8
# predict probabilities on test set with age+rm model
pred_probs_age_rm <- predict(model_age_rm, newdata = dat_test, type = "response")

# apply threshold of 0.2
pred_class_age_rm_02 <- ifelse(pred_probs_age_rm >= 0.2, 1, 0)

# correctly classified as BELOW median = True Positives
q8 <- sum(dat_test$mvalue == 1 & pred_class_age_rm_02 == 1)

#Q9
#plot rm vs age with decision boundaries
plot(dat_train$age, dat_train$rm, col = as.factor(dat_train$mvalue),
     xlab = "Age", ylab = "Average number of rooms per dwelling",
     main = "Decision Boundaries at Different Thresholds")
legend('topright', legend = c("0 (above median)", "1 (below median)"),
       pch = 1, col = c("red", "black"))

b0 <- coefs["(Intercept)"]
b1 <- coefs["age"]
b2 <- coefs["rm"]

# calculate decision boundaries for different thresholds
age_range <- seq(min(dat_train$age), max(dat_train$age), length.out = 100)

# threshold 0.2
threshold_02 <- 0.2
rm_boundary_02 <- (log(threshold_02/(1-threshold_02)) - b0 - b1*age_range) / b2
lines(age_range, rm_boundary_02, col = "blue", lwd = 2)

# threshold 0.5
threshold_05 <- 0.5
rm_boundary_05 <- (log(threshold_05/(1-threshold_05)) - b0 - b1*age_range) / b2
lines(age_range, rm_boundary_05, col = "green", lwd = 2)

# threshold 0.8
threshold_08 <- 0.8
rm_boundary_08 <- (log(threshold_08/(1-threshold_08)) - b0 - b1*age_range) / b2
lines(age_range, rm_boundary_08, col = "purple", lwd = 2)

legend('bottomleft', legend = c("Threshold 0.2", "Threshold 0.5", "Threshold 0.8"),
       col = c("blue", "green", "purple"), lwd = 2)

q9 <- c(3, 4)

#Q10
model_full <- glm(mvalue ~ ., data = dat_train, family = binomial)

# predict probabilities on test set with full model
pred_probs_full <- predict(model_full, newdata = dat_test, type = "response")

# apply threshold of 0.2
pred_class_full_02 <- ifelse(pred_probs_full >= 0.2, 1, 0)

# Calculate misclassification error for full model
misclass_full_02 <- mean(pred_class_full_02 != dat_test$mvalue)

# calculate improvement: MisclError_full - MisclError_rm
# (negative value means full model is better, i.e., improvement)
q10 <- misclass_full_02 - misclass_rm_02


