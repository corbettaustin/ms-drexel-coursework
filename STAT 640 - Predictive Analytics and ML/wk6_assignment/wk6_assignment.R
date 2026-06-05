# Load train and test dataset
dat_train <- read.csv("HousingBoston_train.csv", stringsAsFactors = TRUE)
dat_test <- read.csv("HousingBoston_test.csv", stringsAsFactors = TRUE)

# change factors to binary with values of 0 and 1
dat_train$mvalue <- 1*(dat_train$mvalue == "below")
dat_test$mvalue <- 1*(dat_test$mvalue == "below")

# Create polynomials to the 6th degree
prm_train <- poly(dat_train$rm, 6)
colnames(prm_train) <- paste0("rmpoly", 1:6)
prm_test <- poly(dat_test$rm, 6)
colnames(prm_test) <- paste0("rmpoly", 1:6)

# Create discrte variables continuous form
# First, we create vectors of length
drm_train <- matrix(0, ncol = 3, nrow = nrow(dat_train))
colnames(drm_train) <- paste0("rmdis", 1:3)

# replace 0 with 1 if conditions is met
drm_train[which(dat_train$rm < 5), 1] <- 1
drm_train[which(dat_train$rm >= 5 & dat_train$rm < 6), 2] <- 1
drm_train[which(dat_train$rm >= 6 & dat_train$rm < 7), 3] <- 1

# repeat for test set
drm_test <- matrix(0, ncol = 3, nrow = nrow(dat_test))
colnames(drm_test) <- paste0("rmdis", 1:3)
drm_test[which(dat_test$rm < 5), 1] <- 1
drm_test[which(dat_test$rm >= 5 & dat_test$rm < 6), 2] <- 1
drm_test[which(dat_test$rm >= 6 & dat_test$rm < 7), 3] <- 1

# Add all new variables into the existing one
dat_train <- cbind(dat_train, prm_train, drm_train)
dat_test <- cbind(dat_test, prm_test, drm_test)

# Load the pROC
library(pROC)


#Q1 build polynomial models
log.p1 <- glm(mvalue ~ ., data = dat_train[c("mvalue","rmpoly1","age")],                                       family=binomial)
log.p2 <- glm(mvalue ~ ., data = dat_train[c("mvalue","rmpoly1","rmpoly2","age")],                             family=binomial)
log.p3 <- glm(mvalue ~ ., data = dat_train[c("mvalue","rmpoly1","rmpoly2","rmpoly3","age")],                   family=binomial)
log.p4 <- glm(mvalue ~ ., data = dat_train[c("mvalue","rmpoly1","rmpoly2","rmpoly3","rmpoly4","age")],         family=binomial)
log.p5 <- glm(mvalue ~ ., data = dat_train[c("mvalue","rmpoly1","rmpoly2","rmpoly3","rmpoly4","rmpoly5","age")], family=binomial)
log.p6 <- glm(mvalue ~ ., data = dat_train[c("mvalue","rmpoly1","rmpoly2","rmpoly3","rmpoly4","rmpoly5","rmpoly6","age")], family=binomial)

#display AIC and BIC for each
cat("Polynomial model AIC/BIC:\n")
for (k in 1:6) {
  m <- get(paste0("log.p", k))
  cat(sprintf("poly%d: AIC=%.4f, BIC=%.4f\n", k, AIC(m), BIC(m)))
}

q1 <- 2

#Q2 model definitions
log.p <- glm(mvalue ~ ., data = dat_train[c("mvalue","rmpoly1","rmpoly2","rmpoly3","age")], family=binomial)
log.d <- glm(mvalue ~ ., data = dat_train[c("mvalue","rmdis1","rmdis2","rmdis3","age")],   family=binomial)
log.n <- glm(mvalue ~ ., data = dat_train[c("mvalue","rm","age")],                          family=binomial)
log.a <- glm(mvalue ~ ., data = dat_train[c("mvalue","age")],                               family=binomial)

cat("\nTraining AIC/BIC for 4 models:\n")
cat(sprintf("age only:     AIC=%.4f, BIC=%.4f\n", AIC(log.a), BIC(log.a)))
cat(sprintf("age + rm:     AIC=%.4f, BIC=%.4f\n", AIC(log.n), BIC(log.n)))
cat(sprintf("age + rmdis:  AIC=%.4f, BIC=%.4f\n", AIC(log.d), BIC(log.d)))
cat(sprintf("age + rmpoly3:AIC=%.4f, BIC=%.4f\n", AIC(log.p), BIC(log.p)))

q2 <- 4

#Q3 predictions on test
pred.a <- predict(log.a, newdata = dat_test[c("age")],                              type="response")
pred.n <- predict(log.n, newdata = dat_test[c("rm","age")],                         type="response")
pred.d <- predict(log.d, newdata = dat_test[c("rmdis1","rmdis2","rmdis3","age")],   type="response")
pred.p <- predict(log.p, newdata = dat_test[c("rmpoly1","rmpoly2","rmpoly3","age")], type="response")

#score helper
f1_score <- function(actual, predicted_prob, threshold = 0.5) {
  pred_class <- as.integer(predicted_prob >= threshold)
  tp <- sum(pred_class == 1 & actual == 1)
  fp <- sum(pred_class == 1 & actual == 0)
  fn <- sum(pred_class == 0 & actual == 1)
  precision <- tp / (tp + fp)
  recall    <- tp / (tp + fn)
  f1 <- 2 * precision * recall / (precision + recall)
  return(f1)
}

cat("\nF1-scores on test set (threshold=0.5):\n")
cat(sprintf("age only:     F1=%.4f\n", f1_score(dat_test$mvalue, pred.a)))
cat(sprintf("age + rm:     F1=%.4f\n", f1_score(dat_test$mvalue, pred.n)))
cat(sprintf("age + rmdis:  F1=%.4f\n", f1_score(dat_test$mvalue, pred.d)))
cat(sprintf("age + rmpoly3:F1=%.4f\n", f1_score(dat_test$mvalue, pred.p)))

q3 <- 3

#Q4 compute roc objects
roc.a <- roc(dat_test$mvalue, pred.a)
roc.n <- roc(dat_test$mvalue, pred.n)
roc.d <- roc(dat_test$mvalue, pred.d)
roc.p <- roc(dat_test$mvalue, pred.p)

#plot roc curves
plot(roc.a, col="red",   main="ROC Curves - Test Set", legacy.axes=TRUE)
plot(roc.n, col="blue",  add=TRUE)
plot(roc.d, col="green", add=TRUE)
plot(roc.p, col="purple",add=TRUE)
abline(a=0, b=1, lty=2, col="gray")  # random classifier
legend("bottomright",
       legend=c("Age only","Age+rm","Age+rmdis","Age+rmpoly3","Random"),
       col=c("red","blue","green","purple","gray"), lty=c(1,1,1,1,2))

cat("\nAUC values:\n")
cat(sprintf("age only:     AUC=%.4f\n", auc(roc.a)))
cat(sprintf("age + rm:     AUC=%.4f\n", auc(roc.n)))
cat(sprintf("age + rmdis:  AUC=%.4f\n", auc(roc.d)))
cat(sprintf("age + rmpoly3:AUC=%.4f\n", auc(roc.p)))

q4 <- c(2, 4)

#Q5 proc stores sensitivities from 0 to 1

cat("\nQ5: Sensitivity at index 49:\n")
cat(sprintf("roc.p$sensitivities[49] = %.6f\n", roc.p$sensitivities[49]))
cat(sprintf("Corresponding threshold = %.6f\n", roc.p$thresholds[49]))

q5 <- roc.p$thresholds[49]

#Q6 distance from top left corner

dist_to_ideal <- sqrt((1 - roc.p$sensitivities)^2 + (1 - roc.p$specificities)^2)
opt_idx <- which.min(dist_to_ideal)

cat("\nQ6: Optimal sensitivity/specificity:\n")
cat(sprintf("Sensitivity = %.6f, Specificity = %.6f\n",
            roc.p$sensitivities[opt_idx], roc.p$specificities[opt_idx]))

q6 <- c

#Q7 no, yes, yes, yes
q7 <- c(2, 3, 4)

#Q8 AUC of the best performing model on test set
cat("\nQ8: Best model AUC (age+rm):\n")
cat(sprintf("AUC = %.6f\n", auc(roc.n)))

q8 <- as.numeric(auc(roc.n))

#Q9 build model with all 12 predictors
log.all <- glm(mvalue ~ ., data = dat_train[, c(1:13)], family = binomial)
pred.all <- predict(log.all, newdata = dat_test[, c(1:13)], type="response")
roc.all  <- roc(dat_test$mvalue, pred.all)

cat("\nQ9: AUC comparison poly3 vs full:\n")
cat(sprintf("AUC_poly3 = %.6f\n", auc(roc.p)))
cat(sprintf("AUC_full  = %.6f\n", auc(roc.all)))
cat(sprintf("AUC_poly3 - AUC_full = %.6f\n", auc(roc.p) - auc(roc.all)))

q9 <- as.numeric(auc(roc.p)) - as.numeric(auc(roc.all))

#Q10 full model with all variables outperforms
q10 <- 4