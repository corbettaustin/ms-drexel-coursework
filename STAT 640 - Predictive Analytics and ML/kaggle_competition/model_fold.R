# =============================================================================
# KAGGLE COMPETITION: Product Return Prediction
# Step 4: Stacking Ensemble
# Requires: train_clean.csv, test_clean.csv, tuning_results.rds
# Produces:  submission_stack.csv, submission_stack_v2.csv
# =============================================================================

library(dplyr)
library(xgboost)
library(randomForest)
library(glmnet)
library(pROC)
library(caret)

set.seed(42)

# =============================================================================
# 1. LOAD DATA
# =============================================================================
train <- read.csv("train_clean.csv", stringsAsFactors = FALSE)
test  <- read.csv("test_clean.csv",  stringsAsFactors = FALSE)
tuning <- readRDS("tuning_results.rds")

test_ids <- test$transaction_id
y_num    <- as.numeric(as.character(train$returned))
scale_pos <- sum(y_num == 0) / sum(y_num == 1)

drop_cols <- c("transaction_id", "returned")
X_train   <- as.matrix(train[, !names(train) %in% drop_cols])
X_test    <- as.matrix(test[,  !names(test)  %in% drop_cols])

# Align columns
missing_in_test  <- setdiff(colnames(X_train), colnames(X_test))
for (col in missing_in_test) X_test <- cbind(X_test, setNames(data.frame(0), col))
X_test <- X_test[, colnames(X_train)]

# Re-apply engineered features from tuning pipeline
hour_col   <- which(colnames(X_train) == "hour_of_day")
formal_col <- which(colnames(X_train) == "product_subcategory_Formal")
night_col  <- which(colnames(X_train) == "time_bucket_night")
promo_col  <- which(colnames(X_train) == "promo_freeship")

X_train <- cbind(X_train,
                 hour_sin      = sin(2 * pi * X_train[, hour_col] / 24),
                 hour_cos      = cos(2 * pi * X_train[, hour_col] / 24),
                 formal_x_night = X_train[, formal_col] * X_train[, night_col],
                 promo_x_formal = X_train[, promo_col]  * X_train[, formal_col]
)
X_test <- cbind(X_test,
                hour_sin      = sin(2 * pi * X_test[, hour_col] / 24),
                hour_cos      = cos(2 * pi * X_test[, hour_col] / 24),
                formal_x_night = X_test[, formal_col] * X_test[, night_col],
                promo_x_formal = X_test[, promo_col]  * X_test[, formal_col]
)

dtrain_full <- xgb.DMatrix(data = X_train, label = y_num)
dtest_full  <- xgb.DMatrix(data = X_test)

# Best XGBoost params from tuning
best_params <- tuning$best_params

# =============================================================================
# 2. GENERATE OUT-OF-FOLD (OOF) PREDICTIONS — the key to stacking
#
# For each fold: train base models on the other 4 folds, predict on this fold.
# This gives unbiased predictions for every training row.
# The meta-learner trains on these OOF predictions.
# =============================================================================
cat("=== GENERATING OUT-OF-FOLD PREDICTIONS ===\n")

K <- 5
folds <- createFolds(y_num, k = K, list = TRUE, returnTrain = FALSE)

# Containers for OOF predictions
oof_xgb  <- numeric(nrow(X_train))
oof_rf   <- numeric(nrow(X_train))
oof_lasso <- numeric(nrow(X_train))

# Containers for test predictions (average across folds)
test_xgb_folds  <- matrix(0, nrow = nrow(X_test), ncol = K)
test_rf_folds   <- matrix(0, nrow = nrow(X_test), ncol = K)
test_lasso_folds <- matrix(0, nrow = nrow(X_test), ncol = K)

for (k in seq_len(K)) {
  cat(sprintf("\nFold %d/%d\n", k, K))
  val_idx   <- folds[[k]]
  train_idx <- setdiff(seq_len(nrow(X_train)), val_idx)
  
  X_tr <- X_train[train_idx, ];  y_tr <- y_num[train_idx]
  X_val <- X_train[val_idx, ];   y_val <- y_num[val_idx]
  
  dtr  <- xgb.DMatrix(data = X_tr,  label = y_tr)
  dval <- xgb.DMatrix(data = X_val)
  
  # --- Base model 1: XGBoost ---
  xgb_k <- xgb.train(
    params  = best_params,
    data    = dtr,
    nrounds = tuning$best_nrounds,
    verbose = 0
  )
  oof_xgb[val_idx]       <- predict(xgb_k, dval)
  test_xgb_folds[, k]    <- predict(xgb_k, dtest_full)
  
  xgb_auc <- as.numeric(auc(roc(y_val, oof_xgb[val_idx], quiet = TRUE)))
  cat(sprintf("  XGBoost  AUC: %.4f\n", xgb_auc))
  
  # --- Base model 2: Random Forest ---
  rf_k <- randomForest(
    x       = X_tr,
    y       = as.factor(y_tr),
    ntree   = 300,
    mtry    = floor(sqrt(ncol(X_tr))),
    classwt = c("0" = 1, "1" = 5)
  )
  oof_rf[val_idx]      <- predict(rf_k, X_val, type = "prob")[, "1"]
  test_rf_folds[, k]   <- predict(rf_k, X_test, type = "prob")[, "1"]
  
  rf_auc <- as.numeric(auc(roc(y_val, oof_rf[val_idx], quiet = TRUE)))
  cat(sprintf("  RF       AUC: %.4f\n", rf_auc))
  
  # --- Base model 3: Lasso ---
  lasso_k <- cv.glmnet(X_tr, y_tr, alpha = 1, family = "binomial",
                       type.measure = "auc", nfolds = 5)
  oof_lasso[val_idx]     <- as.numeric(predict(lasso_k, newx = X_val,
                                               s = "lambda.min", type = "response"))
  test_lasso_folds[, k]  <- as.numeric(predict(lasso_k, newx = X_test,
                                               s = "lambda.min", type = "response"))
  
  lasso_auc <- as.numeric(auc(roc(y_val, oof_lasso[val_idx], quiet = TRUE)))
  cat(sprintf("  Lasso    AUC: %.4f\n", lasso_auc))
}

# Average test predictions across folds
test_xgb_avg   <- rowMeans(test_xgb_folds)
test_rf_avg    <- rowMeans(test_rf_folds)
test_lasso_avg <- rowMeans(test_lasso_folds)

# OOF AUC for each base model
cat("\n=== OOF AUC (Base Models) ===\n")
cat("XGBoost OOF AUC: ", round(as.numeric(auc(roc(y_num, oof_xgb,   quiet=TRUE))), 4), "\n")
cat("RF      OOF AUC: ", round(as.numeric(auc(roc(y_num, oof_rf,    quiet=TRUE))), 4), "\n")
cat("Lasso   OOF AUC: ", round(as.numeric(auc(roc(y_num, oof_lasso, quiet=TRUE))), 4), "\n")

# =============================================================================
# 3. META-LEARNER: Lasso logistic on OOF predictions
#
# Train a simple Lasso on the 3 OOF columns.
# Lasso will automatically down-weight weak base models.
# =============================================================================
cat("\n=== TRAINING META-LEARNER ===\n")

meta_train <- cbind(oof_xgb, oof_rf, oof_lasso)
meta_test  <- cbind(test_xgb_avg, test_rf_avg, test_lasso_avg)

meta_cv <- cv.glmnet(meta_train, y_num, alpha = 1, family = "binomial",
                     type.measure = "auc", nfolds = 10)

stack_auc <- max(meta_cv$cvm)
cat("Stack CV AUC (meta-learner): ", round(stack_auc, 4), "\n")

meta_coefs <- coef(meta_cv, s = "lambda.min")
cat("Meta-learner coefficients:\n")
print(meta_coefs)

stack_probs <- as.numeric(predict(meta_cv, newx = meta_test,
                                  s = "lambda.min", type = "response"))

# =============================================================================
# 4. ALTERNATIVE: XGBoost meta-learner (sometimes outperforms Lasso meta)
# =============================================================================
cat("\n=== XGBoost META-LEARNER ===\n")

dmeta_train <- xgb.DMatrix(data = meta_train, label = y_num)
dmeta_test  <- xgb.DMatrix(data = meta_test)

meta_xgb_cv <- xgb.cv(
  params = list(
    objective        = "binary:logistic",
    eval_metric      = "auc",
    eta              = 0.05,
    max_depth        = 2,          # shallow — meta-learner should be simple
    min_child_weight = 5,
    subsample        = 0.8,
    colsample_bytree = 1.0,
    scale_pos_weight = scale_pos
  ),
  data       = dmeta_train,
  nrounds    = 500,
  nfold      = 10,
  early_stopping_rounds = 30,
  verbose    = 0,
  stratified = TRUE
)

meta_xgb_r <- meta_xgb_cv$best_iteration
if (is.null(meta_xgb_r) || length(meta_xgb_r) == 0 || meta_xgb_r == 0)
  meta_xgb_r <- which.max(meta_xgb_cv$evaluation_log$test_auc_mean)

meta_xgb_auc <- max(meta_xgb_cv$evaluation_log$test_auc_mean)
cat("XGBoost meta CV AUC:", round(meta_xgb_auc, 4), "\n")

meta_xgb_model <- xgb.train(
  params  = list(objective = "binary:logistic", eval_metric = "auc",
                 eta = 0.05, max_depth = 2, min_child_weight = 5,
                 subsample = 0.8, colsample_bytree = 1.0,
                 scale_pos_weight = scale_pos),
  data    = dmeta_train,
  nrounds = meta_xgb_r,
  verbose = 0
)
stack_xgb_probs <- predict(meta_xgb_model, dmeta_test)

# =============================================================================
# 5. SUMMARY & SUBMISSIONS
# =============================================================================
cat("\n=== FINAL COMPARISON ===\n")
cat("Baseline XGBoost CV AUC:         0.8196\n")
cat("Tuned XGBoost CV AUC:            0.8215\n")
cat("Stack (Lasso meta) CV AUC:      ", round(stack_auc,     4), "\n")
cat("Stack (XGBoost meta) CV AUC:    ", round(meta_xgb_auc,  4), "\n")

write.csv(data.frame(transaction_id = test_ids, returned = stack_probs),
          "submission_stack.csv", row.names = FALSE)
write.csv(data.frame(transaction_id = test_ids, returned = stack_xgb_probs),
          "submission_stack_xgb.csv", row.names = FALSE)

cat("\nSaved: submission_stack.csv\n")
cat("Saved: submission_stack_xgb.csv\n")

cat("\n=== SUBMISSION SLOTS RECOMMENDATION ===\n")
cat("Slot 1: Best of submission_stack.csv or submission_stack_xgb.csv\n")
cat("Slot 2: submission_xgb_tuned.csv (best single model as backup)\n")
cat("Select the 2 private leaderboard models based on public LB AUC.\n") 