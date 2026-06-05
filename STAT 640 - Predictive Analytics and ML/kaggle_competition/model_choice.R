install.packages(c("randomForest", "xgboost", "caret"))
# =============================================================================
# KAGGLE COMPETITION: Product Return Prediction
# Step 2: Modeling Pipeline
# Requires: train_clean.csv, test_clean.csv (from preprocessing_pipeline.R)
# Produces:  submission_logistic.csv, submission_lasso.csv,
#            submission_rf.csv, submission_xgb.csv,
#            model_results.rds
# =============================================================================

library(dplyr)
library(glmnet)
library(pROC)
library(randomForest)
library(xgboost)
library(caret)

set.seed(42)

# =============================================================================
# 1. LOAD CLEAN DATA
# =============================================================================
train <- read.csv("train_clean.csv", stringsAsFactors = FALSE)
test  <- read.csv("test_clean.csv",  stringsAsFactors = FALSE)

# Separate IDs, target, features
train_ids <- train$transaction_id
test_ids  <- test$transaction_id

y <- as.factor(train$returned)   # factor for caret; numeric used where needed
y_num <- as.numeric(as.character(train$returned))

# Feature matrices (drop id and target)
drop_cols <- c("transaction_id", "returned")
X_train <- train[, !names(train) %in% drop_cols]
X_test  <- test[,  !names(test)  %in% drop_cols]

# Align columns (test may lack a dummy level that train has, or vice versa)
missing_in_test  <- setdiff(names(X_train), names(X_test))
missing_in_train <- setdiff(names(X_test),  names(X_train))
for (col in missing_in_test)  X_test[col]  <- 0
for (col in missing_in_train) X_train[col] <- 0
X_test <- X_test[, names(X_train)]   # enforce same column order

cat("X_train:", nrow(X_train), "x", ncol(X_train), "\n")
cat("X_test: ", nrow(X_test),  "x", ncol(X_test),  "\n")
cat("Class balance:", table(y_num), "\n")

# =============================================================================
# 2. CROSS-VALIDATION SETUP
# 5-fold CV; stratified to preserve class balance
# =============================================================================
cv_folds <- createFolds(y, k = 5, list = TRUE, returnTrain = FALSE)

# Helper: evaluate a model on held-out fold, return AUC
eval_fold_auc <- function(pred_probs, actual) {
  roc_obj <- roc(actual, pred_probs, quiet = TRUE)
  as.numeric(auc(roc_obj))
}

# =============================================================================
# 3. MODEL 1: LOGISTIC REGRESSION (baseline)
# =============================================================================
cat("\n--- Model 1: Logistic Regression ---\n")

X_mat <- as.matrix(X_train)

lr_aucs <- numeric(5)
for (i in seq_along(cv_folds)) {
  val_idx   <- cv_folds[[i]]
  train_idx <- setdiff(seq_len(nrow(X_train)), val_idx)
  
  # glm on fold
  df_fold <- as.data.frame(X_mat[train_idx, ])
  df_fold$y <- y_num[train_idx]
  df_val  <- as.data.frame(X_mat[val_idx, ])
  
  fit <- glm(y ~ ., data = df_fold, family = binomial, maxit = 100)
  pred <- predict(fit, newdata = df_val, type = "response")
  lr_aucs[i] <- eval_fold_auc(pred, y_num[val_idx])
}
cat("Logistic CV AUC:", round(mean(lr_aucs), 4),
    "± SD", round(sd(lr_aucs), 4), "\n")

# Fit on full training set for submission
df_full <- as.data.frame(X_mat)
df_full$y <- y_num
lr_final <- glm(y ~ ., data = df_full, family = binomial, maxit = 100)
lr_test_probs <- predict(lr_final, newdata = as.data.frame(as.matrix(X_test)),
                         type = "response")

# =============================================================================
# 4. MODEL 2: LASSO LOGISTIC REGRESSION (variable selection)
# =============================================================================
cat("\n--- Model 2: Lasso Logistic Regression ---\n")

lasso_aucs <- numeric(5)
for (i in seq_along(cv_folds)) {
  val_idx   <- cv_folds[[i]]
  train_idx <- setdiff(seq_len(nrow(X_train)), val_idx)
  
  cv_fit <- cv.glmnet(X_mat[train_idx, ], y_num[train_idx],
                      alpha = 1, family = "binomial",
                      type.measure = "auc", nfolds = 5)
  pred <- predict(cv_fit, newx = X_mat[val_idx, ],
                  s = "lambda.min", type = "response")
  lasso_aucs[i] <- eval_fold_auc(as.numeric(pred), y_num[val_idx])
}
cat("Lasso CV AUC:", round(mean(lasso_aucs), 4),
    "± SD", round(sd(lasso_aucs), 4), "\n")

# Fit on full training set
lasso_final <- cv.glmnet(X_mat, y_num, alpha = 1, family = "binomial",
                         type.measure = "auc", nfolds = 10)

# Print selected features
lasso_coefs <- coef(lasso_final, s = "lambda.min")
selected    <- rownames(lasso_coefs)[lasso_coefs[, 1] != 0]
cat("Lasso selected", length(selected) - 1, "features (excl. intercept)\n")
cat("Top features by |coef|:\n")
coef_df <- data.frame(
  feature = rownames(lasso_coefs)[-1],
  coef    = as.numeric(lasso_coefs[-1, 1])
)
coef_df <- coef_df[coef_df$coef != 0, ]
coef_df <- coef_df[order(abs(coef_df$coef), decreasing = TRUE), ]
print(head(coef_df, 15))

lasso_test_probs <- as.numeric(predict(lasso_final, newx = as.matrix(X_test),
                                       s = "lambda.min", type = "response"))

# =============================================================================
# 5. MODEL 3: RANDOM FOREST
# =============================================================================
cat("\n--- Model 3: Random Forest ---\n")

# Class weights to handle imbalance (~15% returns)
class_weights <- c("0" = 1, "1" = 5)

rf_aucs <- numeric(5)
for (i in seq_along(cv_folds)) {
  val_idx   <- cv_folds[[i]]
  train_idx <- setdiff(seq_len(nrow(X_train)), val_idx)
  
  rf_fit <- randomForest(
    x          = X_mat[train_idx, ],
    y          = as.factor(y_num[train_idx]),
    ntree      = 300,
    mtry       = floor(sqrt(ncol(X_mat))),
    classwt    = class_weights,
    importance = FALSE
  )
  pred <- predict(rf_fit, newdata = X_mat[val_idx, ], type = "prob")[, "1"]
  rf_aucs[i] <- eval_fold_auc(pred, y_num[val_idx])
}
cat("Random Forest CV AUC:", round(mean(rf_aucs), 4),
    "± SD", round(sd(rf_aucs), 4), "\n")

# Fit on full training set
rf_final <- randomForest(
  x          = X_mat,
  y          = as.factor(y_num),
  ntree      = 500,
  mtry       = floor(sqrt(ncol(X_mat))),
  classwt    = class_weights,
  importance = TRUE
)

# Variable importance
cat("\nTop 15 features by Mean Decrease Gini:\n")
imp <- importance(rf_final, type = 2)
imp_df <- data.frame(feature = rownames(imp), gini = imp[, 1])
imp_df <- imp_df[order(imp_df$gini, decreasing = TRUE), ]
print(head(imp_df, 15))

rf_test_probs <- predict(rf_final, newdata = as.matrix(X_test), type = "prob")[, "1"]

# =============================================================================
# 6. MODEL 4: XGBOOST (primary competition model)
# =============================================================================
cat("\n--- Model 4: XGBoost ---\n")

# DMatrix objects
dtrain_full <- xgb.DMatrix(data = X_mat, label = y_num)
dtest       <- xgb.DMatrix(data = as.matrix(X_test))

# Scale positive weight to handle class imbalance
scale_pos <- sum(y_num == 0) / sum(y_num == 1)
cat("scale_pos_weight:", round(scale_pos, 2), "\n")

# Initial params (strong starting point for tabular binary classification)
xgb_params <- list(
  objective        = "binary:logistic",
  eval_metric      = "auc",
  eta              = 0.05,           # learning rate: slow + steady
  max_depth        = 5,              # tree depth
  min_child_weight = 10,             # regularization: min obs per leaf
  subsample        = 0.8,            # row sampling per tree
  colsample_bytree = 0.7,            # col sampling per tree
  gamma            = 1,              # min loss reduction to split
  lambda           = 1,              # L2 regularization
  alpha            = 0.1,            # L1 regularization
  scale_pos_weight = scale_pos       # class imbalance correction
)

# CV to find optimal number of rounds
cat("Running XGBoost CV to find optimal nrounds...\n")
xgb_cv <- xgb.cv(
  params   = xgb_params,
  data     = dtrain_full,
  nrounds  = 1000,
  nfold    = 5,
  early_stopping_rounds = 50,
  verbose  = 0,
  stratified = TRUE
)

# Safe extraction: best_iteration can be NULL if early stopping never fired
best_nrounds <- xgb_cv$best_iteration
if (is.null(best_nrounds) || length(best_nrounds) == 0 || best_nrounds == 0) {
  # Fall back: pick the round with highest mean test AUC manually
  best_nrounds <- which.max(xgb_cv$evaluation_log$test_auc_mean)
  cat("Early stopping did not fire — using manual best round.\n")
}
best_cv_auc <- max(xgb_cv$evaluation_log$test_auc_mean)
cat("Best nrounds:", best_nrounds, "\n")
cat("XGBoost CV AUC:", round(best_cv_auc, 4), "\n")

# Train final model on full training data
xgb_final <- xgb.train(
  params  = xgb_params,
  data    = dtrain_full,
  nrounds = best_nrounds,
  verbose = 0
)

# Feature importance
cat("\nTop 15 features by XGBoost gain:\n")
xgb_imp <- xgb.importance(model = xgb_final)
print(head(xgb_imp[, c("Feature", "Gain", "Cover", "Frequency")], 15))

xgb_test_probs <- predict(xgb_final, dtest)

# =============================================================================
# 7. ENSEMBLE: Weighted average of RF + XGBoost
# (simple blend; weights favor XGBoost which typically has higher CV AUC)
# =============================================================================
cat("\n--- Model 5: Ensemble (RF + XGBoost blend) ---\n")

# Compute blend weights from CV AUC scores
rf_weight  <- mean(rf_aucs)
xgb_weight <- best_cv_auc
total_w    <- rf_weight + xgb_weight

ensemble_test_probs <- (rf_weight  / total_w) * rf_test_probs +
  (xgb_weight / total_w) * xgb_test_probs

cat("RF weight:",  round(rf_weight  / total_w, 3), "\n")
cat("XGB weight:", round(xgb_weight / total_w, 3), "\n")

# In-sample AUC for ensemble (approximate — not CV)
ensemble_train_probs <- (rf_weight  / total_w) *
  predict(rf_final, newdata = X_mat, type = "prob")[, "1"] +
  (xgb_weight / total_w) *
  predict(xgb_final, dtrain_full)
ensemble_train_auc <- as.numeric(auc(roc(y_num, ensemble_train_probs, quiet = TRUE)))
cat("Ensemble train AUC (in-sample, optimistic):", round(ensemble_train_auc, 4), "\n")

# =============================================================================
# 8. MODEL COMPARISON SUMMARY
# =============================================================================
cat("\n=== MODEL COMPARISON (5-fold CV AUC) ===\n")
results <- data.frame(
  Model  = c("Logistic Regression", "Lasso Logistic", "Random Forest",
             "XGBoost", "Ensemble (RF+XGB)"),
  CV_AUC = c(mean(lr_aucs), mean(lasso_aucs), mean(rf_aucs),
             best_cv_auc, NA),   # ensemble CV AUC requires separate pass
  CV_SD  = c(sd(lr_aucs), sd(lasso_aucs), sd(rf_aucs), NA, NA)
)
print(results)
cat("\nNote: XGBoost CV AUC from xgb.cv (built-in). Ensemble CV AUC not computed separately.\n")

# =============================================================================
# 9. GENERATE SUBMISSION FILES
# =============================================================================
make_submission <- function(probs, ids, filename) {
  sub <- data.frame(transaction_id = ids, returned = probs)
  write.csv(sub, filename, row.names = FALSE)
  cat("Saved:", filename, "\n")
}

make_submission(lr_test_probs,       test_ids, "submission_logistic.csv")
make_submission(lasso_test_probs,    test_ids, "submission_lasso.csv")
make_submission(rf_test_probs,       test_ids, "submission_rf.csv")
make_submission(xgb_test_probs,      test_ids, "submission_xgb.csv")
make_submission(ensemble_test_probs, test_ids, "submission_ensemble.csv")

# =============================================================================
# 10. SAVE MODEL OBJECTS (for slide deck / further tuning)
# =============================================================================
model_results <- list(
  logistic      = list(model = lr_final,    cv_auc = mean(lr_aucs),    cv_sd = sd(lr_aucs)),
  lasso         = list(model = lasso_final, cv_auc = mean(lasso_aucs), cv_sd = sd(lasso_aucs),
                       coefs = coef_df),
  random_forest = list(model = rf_final,    cv_auc = mean(rf_aucs),    cv_sd = sd(rf_aucs),
                       importance = imp_df),
  xgboost       = list(model = xgb_final,   cv_auc = best_cv_auc,
                       best_nrounds = best_nrounds,
                       importance = xgb_imp),
  ensemble      = list(rf_weight = rf_weight / total_w,
                       xgb_weight = xgb_weight / total_w)
)
saveRDS(model_results, "model_results.rds")
cat("\n✓ All models saved to model_results.rds\n")
cat("\n=== RECOMMENDED FIRST SUBMISSION: submission_xgb.csv ===\n")
cat("=== RECOMMENDED SECOND SUBMISSION (if XGB underperforms): submission_ensemble.csv ===\n")