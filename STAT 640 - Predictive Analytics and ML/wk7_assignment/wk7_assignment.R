# Step 2: read dataset
dat <- read.csv("bank.csv", stringsAsFactors = TRUE)

# Step 3: binarization for categorical variables
library(fastDummies)
dat <- dummy_cols(dat, select_columns = c("job", "civil", "edu", "credit",
                                          "hloan", "ploan", "ctype", "month",
                                          "day", "presult"),
                  remove_first_dummy = TRUE, remove_selected_columns = TRUE)

# Step 4: make outcome binary
dat$outcome <- 1*(dat$outcome == "yes")
dat$y <- dat$outcome
dat$outcome <- NULL

# Step 5: remove perfectly correlated variable
dat$ploan_unknown <- NULL

# Step 6: Split data in training and test (70/30)
set.seed(42)
train <- sample(1:nrow(dat), nrow(dat)*0.7)
head(train, 10)

# Step 7 & 8: create training and test sets
datTrain <- dat[train, ]
datTest <- dat[-train, ]

cat("Train rows:", nrow(datTrain), "\n")
cat("Test rows:", nrow(datTest), "\n")

# Step 9: Load Rfunctions
source("Rfunctions.R")

# Load required packages
library(glmnet)
library(pROC)

#Q1
prop_yes <- mean(datTrain$y)
cat("\nQ1: Proportion of 'yes' (class 1) in training set:", prop_yes, "\n")
q1 <- 2  # Moderate

#Q2
q2 <- 2

#Q3
fit.full <- glm(y ~ ., data = datTrain, family = binomial)
sum_full <- summary(fit.full)
pvals <- coef(sum_full)[, "Pr(>|z|)"]
n_sig <- sum(pvals <= 0.05, na.rm = TRUE)
cat("\nQ3: Number of significant variables (p<=0.05) in full model:", n_sig, "\n")
# Subtract 1 for intercept if needed - question asks for variables
# Actually count all coefficients with p<=0.05 (including intercept)
q3 <- n_sig

#Q4
n_train <- nrow(datTrain)
fit.step <- step(glm(y ~ 1, data = datTrain, family = binomial),
                 scope = formula(fit.full),
                 direction = "forward",
                 k = log(n_train),
                 trace = 0)

n_vars_step <- length(coef(fit.step))
cat("\nQ4: Number of variables (including intercept) in stepwise BIC model:", n_vars_step, "\n")
q4 <- n_vars_step

#Q5
set.seed(42)
fit.cvstep <- stepCV(datTrain, response = "y", direction = "forward",
                     K = 10, perf = "auc")

n_vars_cvstep <- length(coef(fit.cvstep$BestModel))
cat("\nQ5: Number of variables (including intercept) in CV stepwise model:", n_vars_cvstep, "\n")
q5 <- n_vars_cvstep

#Q6
x_train <- as.matrix(datTrain[, !names(datTrain) %in% "y"])
y_train <- datTrain$y

fit.lasso <- glmnet(x_train, y_train, family = "binomial", alpha = 1)

coef_lasso <- as.matrix(fit.lasso$beta)

last_nonzero_lambda_idx <- apply(coef_lasso, 1, function(x) {
  nz <- which(x != 0)
  if (length(nz) == 0) return(0)
  return(max(nz))
})


cat("\nTop variables (last to shrink to 0):\n")
sorted_vars <- sort(last_nonzero_lambda_idx, decreasing = TRUE)
print(head(sorted_vars, 10))

# Check specific candidates mentioned in Q6
candidates <- c("employees", "cconf", "ctype_telephone", "presult_success")
for (v in candidates) {
  if (v %in% rownames(coef_lasso)) {
    cat(v, ": last nonzero at lambda idx =", last_nonzero_lambda_idx[v], "\n")
  }
}

# Find maximum nonzero lambda index
max_idx <- max(last_nonzero_lambda_idx)
last_vars <- names(last_nonzero_lambda_idx[last_nonzero_lambda_idx == max_idx])
cat("Last variable(s) to shrink to 0:", paste(last_vars, collapse=", "), "\n")

# Based on the options: employees(1), cconf(2), ctype_telephone(3), presult_success(4)
# Determine which of these are last
# Correct interpretation: 
# Variables that appear FIRST in the lasso path (nonzero even at highest lambda)
# are the LAST to shrink to 0 as lambda increases
first_nonzero <- apply(coef_lasso, 1, function(x) {
  nz <- which(x != 0)
  if (length(nz)==0) return(NA)
  return(min(nz))
})
min_first <- min(first_nonzero, na.rm=TRUE)
last_vars_correct <- names(first_nonzero[!is.na(first_nonzero) & first_nonzero == min_first])
cat("Corrected last to shrink:", paste(last_vars_correct, collapse=", "), "\n")

q6_options <- c()
if ("employees" %in% last_vars_correct) q6_options <- c(q6_options, 1)
if ("cconf" %in% last_vars_correct) q6_options <- c(q6_options, 2)
if ("ctype_telephone" %in% last_vars_correct) q6_options <- c(q6_options, 3)
if ("presult_success" %in% last_vars_correct) q6_options <- c(q6_options, 4)
if (length(q6_options) == 0) q6_options <- 5

q6 <- q6_options

#Q7
set.seed(42)
fit.lassoCv <- cv.glmnet(x_train, y_train, family = "binomial", alpha = 1,
                         nfolds = 10, type.measure = "auc")

# Use 1-SE rule
lambda_1se <- fit.lassoCv$lambda.1se
coef_lasso_1se <- coef(fit.lassoCv, s = "lambda.1se")
n_vars_lasso <- sum(coef_lasso_1se != 0)  # includes intercept
cat("\nQ7: Number of variables (including intercept) in CV Lasso (1-SE rule):", n_vars_lasso, "\n")
q7 <- n_vars_lasso

#Q8
set.seed(42)
fit.ridgeCv <- cv.glmnet(x_train, y_train, family = "binomial", alpha = 0,
                         nfolds = 10, type.measure = "auc")

coef_ridge <- coef(fit.ridgeCv, s = "lambda.1se")
ridge_coefs <- as.matrix(coef_ridge)
cat("\nRidge coefficients (sorted by absolute value):\n")
sorted_ridge <- sort(abs(ridge_coefs[,1]))
print(head(sorted_ridge, 10))

# Least important = smallest absolute coefficient (excluding intercept)
# Check the candidates: lcdays(1), Intercept(2), presult_success(3)
cat("lcdays:", ridge_coefs["lcdays", 1], "\n")
cat("(Intercept):", ridge_coefs["(Intercept)", 1], "\n")
if ("presult_success" %in% rownames(ridge_coefs)) {
  cat("presult_success:", ridge_coefs["presult_success", 1], "\n")
}

# Find the least important (min abs value, excluding intercept)
ridge_no_int <- ridge_coefs[rownames(ridge_coefs) != "(Intercept)", , drop=FALSE]
min_var <- rownames(ridge_no_int)[which.min(abs(ridge_no_int[,1]))]
cat("Least important variable:", min_var, "\n")

# Determine answer
if (min_var == "lcdays") {
  q8 <- 1
} else if (min_var == "(Intercept)") {
  q8 <- 2
} else if (min_var == "presult_success") {
  q8 <- 3
} else {
  q8 <- 4
}

#Q9
x_test <- as.matrix(datTest[, !names(datTest) %in% "y"])
y_test <- datTest$y

# Full model AUC
pred_full <- predict(fit.full, newdata = datTest, type = "response")
auc_full <- as.numeric(pROC::roc(y_test, pred_full, quiet=TRUE)$auc)

# Stepwise BIC model AUC
pred_step <- predict(fit.step, newdata = datTest, type = "response")
auc_step <- as.numeric(pROC::roc(y_test, pred_step, quiet=TRUE)$auc)

# CV Stepwise model AUC
pred_cvstep <- predict(fit.cvstep$BestModel, newdata = datTest, type = "response")
auc_cvstep <- as.numeric(pROC::roc(y_test, pred_cvstep, quiet=TRUE)$auc)

# Lasso CV model AUC
pred_lasso <- predict(fit.lassoCv, newx = x_test, s = "lambda.1se", type = "response")
auc_lasso <- as.numeric(pROC::roc(y_test, pred_lasso[,1], quiet=TRUE)$auc)

# Ridge CV model AUC
pred_ridge <- predict(fit.ridgeCv, newx = x_test, s = "lambda.1se", type = "response")
auc_ridge <- as.numeric(pROC::roc(y_test, pred_ridge[,1], quiet=TRUE)$auc)

cat("\nTest AUC Results:\n")
cat("Full model AUC:", auc_full, "\n")
cat("Stepwise BIC AUC:", auc_step, "\n")
cat("CV Stepwise AUC:", auc_cvstep, "\n")
cat("Lasso CV AUC:", auc_lasso, "\n")
cat("Ridge CV AUC:", auc_ridge, "\n")

aucs <- c(auc_full, auc_step, auc_cvstep, auc_lasso, auc_ridge)
best_model <- which.max(aucs)
cat("Best model (1=Full, 2=Step, 3=CVStep, 4=Lasso, 5=Ridge):", best_model, "\n")

q9 <- best_model

#Q10
q10 <- c(3, 4)