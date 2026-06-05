# =============================================================================
# KAGGLE COMPETITION: Ablation Test + nrounds Verification
# Confirms which features drive the AUC gain and that nrounds=131 is correct
# =============================================================================

library(xgboost)
library(pROC)
library(caret)

set.seed(42)

# =============================================================================
# 1. LOAD
# =============================================================================
train_v1  <- read.csv("train_clean.csv",    stringsAsFactors = FALSE)
train_v2  <- read.csv("train_clean_v2.csv", stringsAsFactors = FALSE)
test_v2   <- read.csv("test_clean_v2.csv",  stringsAsFactors = FALSE)
tuning    <- readRDS("tuning_results.rds")

test_ids  <- test_v2$transaction_id
y_num     <- as.numeric(as.character(train_v2$returned))
scale_pos <- sum(y_num == 0) / sum(y_num == 1)
best_params <- tuning$best_params
best_params$scale_pos_weight <- scale_pos

drop_cols <- c("transaction_id", "returned")

run_cv <- function(X, y, params, nrounds = 800) {
  dm <- xgb.DMatrix(data = as.matrix(X), label = y)
  cv <- xgb.cv(
    params    = params,
    data      = dm,
    nrounds   = nrounds,
    nfold     = 5,
    early_stopping_rounds = 50,
    verbose   = 0,
    stratified = TRUE
  )
  best_r <- cv$best_iteration
  if (is.null(best_r) || length(best_r) == 0 || best_r == 0)
    best_r <- which.max(cv$evaluation_log$test_auc_mean)
  list(
    auc        = max(cv$evaluation_log$test_auc_mean),
    nrounds    = best_r,
    eval_log   = cv$evaluation_log
  )
}

add_interactions <- function(X) {
  hour_col   <- which(colnames(X) == "hour_of_day")
  formal_col <- which(colnames(X) == "product_subcategory_Formal")
  night_col  <- which(colnames(X) == "time_bucket_night")
  promo_col  <- which(colnames(X) == "promo_freeship")
  cbind(X,
        hour_sin       = sin(2 * pi * X[, hour_col] / 24),
        hour_cos       = cos(2 * pi * X[, hour_col] / 24),
        formal_x_night = X[, formal_col] * X[, night_col],
        promo_x_formal = X[, promo_col]  * X[, formal_col]
  )
}

# =============================================================================
# 2. ABLATION: isolate contribution of each feature group
# =============================================================================
cat("=== ABLATION TEST ===\n")
cat("(Each run uses best params from tuning; 5-fold CV AUC)\n\n")

new_te_cols   <- c("te_subcategory", "te_zip", "te_hour")
new_cust_cols <- c("cust_hist_return_rate", "cust_hist_tx_count",
                   "cust_is_repeat", "cust_has_returned_before")

# --- A: Baseline v1 features + interactions (known: 0.8215) ---
cat("[A] v1 features + interactions (reference)...\n")
X_A <- add_interactions(as.matrix(train_v1[, !names(train_v1) %in% drop_cols]))
res_A <- run_cv(X_A, y_num, best_params)
cat(sprintf("    CV AUC: %.4f | nrounds: %d\n\n", res_A$auc, res_A$nrounds))

# --- B: v1 + target encoding only (no customer features) ---
cat("[B] v1 + target encoding only...\n")
train_B <- train_v2[, !names(train_v2) %in% c(drop_cols, new_cust_cols)]
X_B <- add_interactions(as.matrix(train_B))
res_B <- run_cv(X_B, y_num, best_params)
cat(sprintf("    CV AUC: %.4f | nrounds: %d\n\n", res_B$auc, res_B$nrounds))

# --- C: v1 + customer history only (no target encoding) ---
cat("[C] v1 + customer history only...\n")
train_C <- train_v2[, !names(train_v2) %in% c(drop_cols, new_te_cols)]
X_C <- add_interactions(as.matrix(train_C))
res_C <- run_cv(X_C, y_num, best_params)
cat(sprintf("    CV AUC: %.4f | nrounds: %d\n\n", res_C$auc, res_C$nrounds))

# --- D: Full v2 (all new features) ---
cat("[D] Full v2 (target encoding + customer history)...\n")
X_D <- add_interactions(as.matrix(train_v2[, !names(train_v2) %in% drop_cols]))
# Align test
X_test_D <- add_interactions(as.matrix(test_v2[, !names(test_v2) %in% drop_cols]))
for (col in setdiff(colnames(X_D), colnames(X_test_D)))
  X_test_D <- cbind(X_test_D, setNames(data.frame(0), col))
X_test_D <- X_test_D[, colnames(X_D)]
res_D <- run_cv(X_D, y_num, best_params)
cat(sprintf("    CV AUC: %.4f | nrounds: %d\n\n", res_D$auc, res_D$nrounds))

# =============================================================================
# 3. NROUNDS VERIFICATION
# Plot AUC curve for full v2 model to confirm 131 is the true optimum
# =============================================================================
cat("=== NROUNDS VERIFICATION ===\n")
cat("Running extended CV (up to 600 rounds, no early stopping)...\n")

dm_D <- xgb.DMatrix(data = X_D, label = y_num)
cv_extended <- xgb.cv(
  params     = best_params,
  data       = dm_D,
  nrounds    = 600,
  nfold      = 5,
  verbose    = 0,
  stratified = TRUE
)

log <- cv_extended$evaluation_log
peak_round <- which.max(log$test_auc_mean)
peak_auc   <- max(log$test_auc_mean)

cat(sprintf("Peak AUC %.4f at round %d\n", peak_auc, peak_round))
cat(sprintf("AUC at round 131: %.4f\n", log$test_auc_mean[131]))
cat(sprintf("AUC at round 200: %.4f\n", log$test_auc_mean[200]))
cat(sprintf("AUC at round 300: %.4f\n", log$test_auc_mean[300]))

# Check for overfitting: train AUC should keep rising while test plateaus
cat(sprintf("\nTrain AUC at peak round (%d): %.4f\n",
            peak_round, log$train_auc_mean[peak_round]))
cat(sprintf("Test  AUC at peak round (%d): %.4f\n",
            peak_round, log$test_auc_mean[peak_round]))
cat(sprintf("Train-test gap: %.4f  (>0.02 suggests overfitting)\n",
            log$train_auc_mean[peak_round] - log$test_auc_mean[peak_round]))

# =============================================================================
# 4. TRAIN FINAL MODEL AT TRUE PEAK AND SAVE SUBMISSION
# =============================================================================
cat("\nTraining final model at peak round", peak_round, "...\n")

xgb_final_v2 <- xgb.train(
  params  = best_params,
  data    = dm_D,
  nrounds = peak_round,
  verbose = 0
)

final_probs <- predict(xgb_final_v2, xgb.DMatrix(data = X_test_D))

write.csv(
  data.frame(transaction_id = test_ids, returned = final_probs),
  "submission_xgb_v2_final.csv",
  row.names = FALSE
)

# =============================================================================
# 5. SUMMARY
# =============================================================================
cat("\n=== ABLATION SUMMARY ===\n")
cat(sprintf("  [A] v1 baseline:                %.4f  (+0.0000)\n", res_A$auc))
cat(sprintf("  [B] + target encoding only:     %.4f  (%+.4f)\n",
            res_B$auc, res_B$auc - res_A$auc))
cat(sprintf("  [C] + customer history only:    %.4f  (%+.4f)\n",
            res_C$auc, res_C$auc - res_A$auc))
cat(sprintf("  [D] + both (full v2):           %.4f  (%+.4f)\n",
            res_D$auc, res_D$auc - res_A$auc))
cat(sprintf("\n  True peak AUC (extended CV):    %.4f at round %d\n",
            peak_auc, peak_round))
cat("\n✓ Saved: submission_xgb_v2_final.csv\n")