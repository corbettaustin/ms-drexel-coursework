# =============================================================================
# KAGGLE COMPETITION: Product Return Prediction
# Step 2c: XGBoost on v3 Features + Targeted Re-tune
# Requires: train_clean_v3.csv, test_clean_v3.csv, tuning_results.rds
# Produces:  submission_xgb_v3.csv, submission_xgb_v3_retune.csv
# =============================================================================

library(xgboost)
library(pROC)
library(caret)

set.seed(42)

# =============================================================================
# 1. LOAD
# =============================================================================
train   <- read.csv("train_clean_v3.csv", stringsAsFactors = FALSE)
test    <- read.csv("test_clean_v3.csv",  stringsAsFactors = FALSE)
tuning  <- readRDS("tuning_results.rds")

test_ids  <- test$transaction_id
y_num     <- as.numeric(as.character(train$returned))
scale_pos <- sum(y_num == 0) / sum(y_num == 1)

drop_cols <- c("transaction_id", "returned")
X_train   <- as.matrix(train[, !names(train) %in% drop_cols])
X_test    <- as.matrix(test[,  !names(test)  %in% drop_cols])

for (col in setdiff(colnames(X_train), colnames(X_test)))
  X_test <- cbind(X_test, setNames(data.frame(0), col))
X_test <- X_test[, colnames(X_train)]

# Re-apply interaction features from tuning pipeline
hour_col   <- which(colnames(X_train) == "hour_of_day")
formal_col <- which(colnames(X_train) == "product_subcategory_Formal")
night_col  <- which(colnames(X_train) == "time_bucket_night")
promo_col  <- which(colnames(X_train) == "promo_freeship")

X_train <- cbind(X_train,
                 hour_sin       = sin(2 * pi * X_train[, hour_col] / 24),
                 hour_cos       = cos(2 * pi * X_train[, hour_col] / 24),
                 formal_x_night = X_train[, formal_col] * X_train[, night_col],
                 promo_x_formal = X_train[, promo_col]  * X_train[, formal_col]
)
X_test <- cbind(X_test,
                hour_sin       = sin(2 * pi * X_test[, hour_col] / 24),
                hour_cos       = cos(2 * pi * X_test[, hour_col] / 24),
                formal_x_night = X_test[, formal_col] * X_test[, night_col],
                promo_x_formal = X_test[, promo_col]  * X_test[, formal_col]
)

cat("Features:", ncol(X_train), "\n")

dtrain <- xgb.DMatrix(data = X_train, label = y_num)
dtest  <- xgb.DMatrix(data = X_test)

best_params <- tuning$best_params
best_params$scale_pos_weight <- scale_pos

# =============================================================================
# 2. CV WITH EXISTING PARAMS (quick check — does v3 improve on v2?)
# =============================================================================
cat("\n--- Pass 1: v3 features, existing params ---\n")

cv1 <- xgb.cv(
  params    = best_params,
  data      = dtrain,
  nrounds   = 500,
  nfold     = 5,
  early_stopping_rounds = 50,
  verbose   = 0,
  stratified = TRUE
)
r1 <- cv1$best_iteration
if (is.null(r1) || length(r1) == 0 || r1 == 0)
  r1 <- which.max(cv1$evaluation_log$test_auc_mean)
auc1 <- max(cv1$evaluation_log$test_auc_mean)
cat(sprintf("v3 (existing params) CV AUC: %.4f | nrounds: %d\n", auc1, r1))
cat(sprintf("v2 baseline:                 0.8843\n"))
cat(sprintf("Delta:                       %+.4f\n", auc1 - 0.8843))

# =============================================================================
# 3. TARGETED RE-TUNE
#
# With strong continuous target-encoded features, shallower trees often
# generalize better — the encodings already capture nonlinearity, so deep
# trees just overfit. Test depths 3-5 with lower eta.
# =============================================================================
cat("\n--- Pass 2: Targeted re-tune (depth x eta) ---\n")

retune_grid <- expand.grid(
  max_depth = c(3, 4, 5, 6),
  eta       = c(0.02, 0.05),
  stringsAsFactors = FALSE
)

retune_results <- data.frame(
  max_depth = integer(), eta = numeric(),
  cv_auc = numeric(), nrounds = integer()
)

for (i in seq_len(nrow(retune_grid))) {
  p <- retune_grid[i, ]
  params_i <- best_params
  params_i$max_depth <- p$max_depth
  params_i$eta       <- p$eta
  
  cv_i <- xgb.cv(
    params    = params_i,
    data      = dtrain,
    nrounds   = ifelse(p$eta == 0.02, 1500, 800),
    nfold     = 5,
    early_stopping_rounds = 60,
    verbose   = 0,
    stratified = TRUE
  )
  best_r <- cv_i$best_iteration
  if (is.null(best_r) || length(best_r) == 0 || best_r == 0)
    best_r <- which.max(cv_i$evaluation_log$test_auc_mean)
  best_auc <- max(cv_i$evaluation_log$test_auc_mean)
  
  retune_results <- rbind(retune_results, data.frame(
    max_depth = p$max_depth, eta = p$eta,
    cv_auc = best_auc, nrounds = best_r
  ))
  cat(sprintf("  depth=%d eta=%.2f | AUC=%.4f rounds=%d\n",
              p$max_depth, p$eta, best_auc, best_r))
}

best_retune <- retune_results[which.max(retune_results$cv_auc), ]
cat("\nBest re-tune config:\n")
print(best_retune)

# =============================================================================
# 4. TRAIN BOTH MODELS AND SAVE SUBMISSIONS
# =============================================================================

# Model A: v3 features, existing params
cat("\nTraining Model A (v3 + existing params)...\n")
xgb_v3 <- xgb.train(
  params  = best_params,
  data    = dtrain,
  nrounds = r1,
  verbose = 0
)
probs_v3 <- predict(xgb_v3, dtest)

# Model B: v3 features, retuned params
cat("Training Model B (v3 + retuned params)...\n")
retuned_params <- best_params
retuned_params$max_depth <- best_retune$max_depth
retuned_params$eta       <- best_retune$eta

xgb_v3_retune <- xgb.train(
  params  = retuned_params,
  data    = dtrain,
  nrounds = best_retune$nrounds,
  verbose = 0
)
probs_v3_retune <- predict(xgb_v3_retune, dtest)

# Feature importance for best model
cat("\nTop 15 features by Gain:\n")
best_model <- if (best_retune$cv_auc >= auc1) xgb_v3_retune else xgb_v3
imp <- xgb.importance(model = best_model)
print(head(imp[, c("Feature", "Gain")], 15))

write.csv(data.frame(transaction_id = test_ids, returned = probs_v3),
          "submission_xgb_v3.csv", row.names = FALSE)
write.csv(data.frame(transaction_id = test_ids, returned = probs_v3_retune),
          "submission_xgb_v3_retune.csv", row.names = FALSE)

# =============================================================================
# 5. SUMMARY
# =============================================================================
cat("\n=== FINAL SUMMARY ===\n")
cat(sprintf("  v2 final (public LB):             0.8528\n"))
cat(sprintf("  v3 existing params  (CV):         %.4f  (%+.4f vs v2 CV 0.8843)\n",
            auc1, auc1 - 0.8843))
cat(sprintf("  v3 retuned          (CV):         %.4f  (%+.4f vs v2 CV 0.8843)\n",
            best_retune$cv_auc, best_retune$cv_auc - 0.8843))

best_cv <- max(auc1, best_retune$cv_auc)
primary <- if (best_retune$cv_auc >= auc1) "submission_xgb_v3_retune.csv" else "submission_xgb_v3.csv"
cat(sprintf("\n  Primary submission: %s (CV AUC %.4f)\n", primary, best_cv))
cat("  Backup: submission_xgb_v2_final.csv\n")