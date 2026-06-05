# =============================================================================
# KAGGLE COMPETITION: Product Return Prediction
# Step 3: XGBoost Hyperparameter Tuning + Feature Engineering Refinement
# Requires: train_clean.csv, test_clean.csv
# Produces:  submission_xgb_tuned.csv, submission_xgb_v2.csv,
#            tuning_results.rds
# =============================================================================

library(dplyr)
library(xgboost)
library(pROC)
library(caret)

set.seed(42)

# =============================================================================
# 1. LOAD DATA
# =============================================================================
train <- read.csv("train_clean.csv", stringsAsFactors = FALSE)
test  <- read.csv("test_clean.csv",  stringsAsFactors = FALSE)

test_ids <- test$transaction_id
y_num    <- as.numeric(as.character(train$returned))

drop_cols <- c("transaction_id", "returned")
X_train   <- as.matrix(train[, !names(train) %in% drop_cols])
X_test    <- as.matrix(test[,  !names(test)  %in% drop_cols])

# Align columns
missing_in_test  <- setdiff(colnames(X_train), colnames(X_test))
missing_in_train <- setdiff(colnames(X_test),  colnames(X_train))
for (col in missing_in_test)  X_test  <- cbind(X_test,  setNames(data.frame(0), col))
for (col in missing_in_train) X_train <- cbind(X_train, setNames(data.frame(0), col))
X_test <- X_test[, colnames(X_train)]

dtrain <- xgb.DMatrix(data = X_train, label = y_num)
dtest  <- xgb.DMatrix(data = X_test)

scale_pos <- sum(y_num == 0) / sum(y_num == 1)

# =============================================================================
# 2. ADDITIONAL FEATURE ENGINEERING
# Based on importance output: hour_of_day and subcategory dominate.
# Add interaction and cyclical features to help XGBoost capture them better.
# =============================================================================

# -- 2a. Cyclical encoding of hour_of_day (captures 23->0 continuity) --
hour_col <- which(colnames(X_train) == "hour_of_day")
if (length(hour_col) > 0) {
  hour_sin_train <- sin(2 * pi * X_train[, hour_col] / 24)
  hour_cos_train <- cos(2 * pi * X_train[, hour_col] / 24)
  hour_sin_test  <- sin(2 * pi * X_test[,  hour_col] / 24)
  hour_cos_test  <- cos(2 * pi * X_test[,  hour_col] / 24)
  
  X_train <- cbind(X_train, hour_sin = hour_sin_train, hour_cos = hour_cos_train)
  X_test  <- cbind(X_test,  hour_sin = hour_sin_test,  hour_cos = hour_cos_test)
  cat("Added cyclical hour features.\n")
}

# -- 2b. Formal x night interaction (50% return + 23% return = highest risk combo) --
formal_col     <- which(colnames(X_train) == "product_subcategory_Formal")
night_col      <- which(colnames(X_train) == "time_bucket_night")
if (length(formal_col) > 0 & length(night_col) > 0) {
  X_train <- cbind(X_train, formal_x_night = X_train[, formal_col] * X_train[, night_col])
  X_test  <- cbind(X_test,  formal_x_night = X_test[,  formal_col] * X_test[,  night_col])
  cat("Added Formal x Night interaction.\n")
}

# -- 2c. Promo x Apparel interaction (promos lift return rate more in Apparel) --
promo_col   <- which(colnames(X_train) == "promo_freeship")
apparel_col <- which(colnames(X_train) == "product_subcategory_Formal")  # proxy
if (length(promo_col) > 0 & length(apparel_col) > 0) {
  X_train <- cbind(X_train, promo_x_formal = X_train[, promo_col] * X_train[, apparel_col])
  X_test  <- cbind(X_test,  promo_x_formal = X_test[,  promo_col] * X_test[,  apparel_col])
  cat("Added Promo x Formal interaction.\n")
}

dtrain_v2 <- xgb.DMatrix(data = X_train, label = y_num)
dtest_v2  <- xgb.DMatrix(data = X_test)

cat("Feature matrix after engineering:", ncol(X_train), "columns\n\n")

# =============================================================================
# 3. GRID SEARCH OVER KEY HYPERPARAMETERS
# Strategy: fix eta low, tune tree structure params, then regularization.
# Full grid would be too slow; this is a focused search around the best
# known starting point (eta=0.05, max_depth=5 from baseline run).
# =============================================================================
cat("=== GRID SEARCH ===\n")

param_grid <- expand.grid(
  max_depth        = c(4, 5, 6),
  min_child_weight = c(5, 10, 20),
  subsample        = c(0.7, 0.8),
  colsample_bytree = c(0.6, 0.7, 0.8),
  stringsAsFactors = FALSE
)
cat("Grid size:", nrow(param_grid), "combinations\n")

grid_results <- data.frame(
  max_depth        = integer(),
  min_child_weight = integer(),
  subsample        = numeric(),
  colsample_bytree = numeric(),
  cv_auc           = numeric(),
  best_nrounds     = integer()
)

for (i in seq_len(nrow(param_grid))) {
  p <- param_grid[i, ]
  
  params_i <- list(
    objective        = "binary:logistic",
    eval_metric      = "auc",
    eta              = 0.05,
    max_depth        = p$max_depth,
    min_child_weight = p$min_child_weight,
    subsample        = p$subsample,
    colsample_bytree = p$colsample_bytree,
    gamma            = 1,
    lambda           = 1,
    alpha            = 0.1,
    scale_pos_weight = scale_pos
  )
  
  cv_i <- xgb.cv(
    params    = params_i,
    data      = dtrain_v2,
    nrounds   = 800,
    nfold     = 5,
    early_stopping_rounds = 40,
    verbose   = 0,
    stratified = TRUE
  )
  
  best_r <- cv_i$best_iteration
  if (is.null(best_r) || length(best_r) == 0 || best_r == 0)
    best_r <- which.max(cv_i$evaluation_log$test_auc_mean)
  
  best_auc <- max(cv_i$evaluation_log$test_auc_mean)
  
  grid_results <- rbind(grid_results, data.frame(
    max_depth        = p$max_depth,
    min_child_weight = p$min_child_weight,
    subsample        = p$subsample,
    colsample_bytree = p$colsample_bytree,
    cv_auc           = best_auc,
    best_nrounds     = best_r
  ))
  
  cat(sprintf("[%02d/%02d] depth=%d mcw=%02d sub=%.1f col=%.1f | AUC=%.4f rounds=%d\n",
              i, nrow(param_grid),
              p$max_depth, p$min_child_weight,
              p$subsample, p$colsample_bytree,
              best_auc, best_r))
}

# Best configuration
best_row <- grid_results[which.max(grid_results$cv_auc), ]
cat("\n=== BEST PARAMS ===\n")
print(best_row)

# =============================================================================
# 4. TRAIN FINAL TUNED MODEL ON FULL DATA
# =============================================================================
cat("\nTraining final tuned XGBoost...\n")

best_params <- list(
  objective        = "binary:logistic",
  eval_metric      = "auc",
  eta              = 0.05,
  max_depth        = best_row$max_depth,
  min_child_weight = best_row$min_child_weight,
  subsample        = best_row$subsample,
  colsample_bytree = best_row$colsample_bytree,
  gamma            = 1,
  lambda           = 1,
  alpha            = 0.1,
  scale_pos_weight = scale_pos
)

xgb_tuned <- xgb.train(
  params  = best_params,
  data    = dtrain_v2,
  nrounds = best_row$best_nrounds,
  verbose = 0
)

tuned_probs <- predict(xgb_tuned, dtest_v2)

# =============================================================================
# 5. SLOWER ETA RUN (0.01) — often squeezes out final AUC points
# Uses best structural params from grid search, just slower learning rate
# =============================================================================
cat("\nTraining slow-eta model (eta=0.01)...\n")

slow_params <- best_params
slow_params$eta <- 0.01

slow_cv <- xgb.cv(
  params    = slow_params,
  data      = dtrain_v2,
  nrounds   = 5000,
  nfold     = 5,
  early_stopping_rounds = 100,
  verbose   = 0,
  stratified = TRUE
)

slow_best_r <- slow_cv$best_iteration
if (is.null(slow_best_r) || length(slow_best_r) == 0 || slow_best_r == 0)
  slow_best_r <- which.max(slow_cv$evaluation_log$test_auc_mean)

slow_best_auc <- max(slow_cv$evaluation_log$test_auc_mean)
cat("Slow-eta CV AUC:", round(slow_best_auc, 4), "| nrounds:", slow_best_r, "\n")

xgb_slow <- xgb.train(
  params  = slow_params,
  data    = dtrain_v2,
  nrounds = slow_best_r,
  verbose = 0
)

slow_probs <- predict(xgb_slow, dtest_v2)

# =============================================================================
# 6. FINAL COMPARISON & SUBMISSIONS
# =============================================================================
cat("\n=== FINAL MODEL COMPARISON ===\n")
cat("Baseline XGBoost CV AUC:    0.8196  (from modeling_pipeline.R)\n")
cat("Tuned XGBoost CV AUC:      ", round(best_row$cv_auc, 4), "\n")
cat("Slow-eta XGBoost CV AUC:   ", round(slow_best_auc, 4), "\n")

# Pick best of tuned vs slow-eta for primary submission
if (slow_best_auc >= best_row$cv_auc) {
  primary_probs   <- slow_probs
  primary_label   <- "slow_eta"
} else {
  primary_probs   <- tuned_probs
  primary_label   <- "tuned"
}
cat("Primary submission: XGBoost", primary_label, "\n")

write.csv(data.frame(transaction_id = test_ids, returned = tuned_probs),
          "submission_xgb_tuned.csv", row.names = FALSE)
write.csv(data.frame(transaction_id = test_ids, returned = slow_probs),
          "submission_xgb_slow_eta.csv", row.names = FALSE)

cat("Saved: submission_xgb_tuned.csv\n")
cat("Saved: submission_xgb_slow_eta.csv\n")

# =============================================================================
# 7. SAVE RESULTS
# =============================================================================
saveRDS(list(
  grid_results   = grid_results,
  best_params    = best_params,
  best_nrounds   = best_row$best_nrounds,
  tuned_cv_auc   = best_row$cv_auc,
  slow_cv_auc    = slow_best_auc,
  slow_nrounds   = slow_best_r,
  model_tuned    = xgb_tuned,
  model_slow     = xgb_slow
), "tuning_results.rds")

cat("\n✓ Saved: tuning_results.rds\n")
cat("\n=== SUBMISSION RECOMMENDATION ===\n")
cat("1st choice: submission_xgb_slow_eta.csv  (if slow_eta CV AUC >= tuned)\n")
cat("2nd choice: submission_xgb_tuned.csv\n")
cat("Keep submission_ensemble.csv from modeling_pipeline.R as backup slot.\n")