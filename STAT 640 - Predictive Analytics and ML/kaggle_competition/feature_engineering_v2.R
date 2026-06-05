# =============================================================================
# KAGGLE COMPETITION: Product Return Prediction
# Step 1b (FIXED): Advanced Feature Engineering
#
# FIX: Replaces LOO target encoding with 5-fold out-of-fold target encoding.
# LOO encoding creates systematic leakage: returned rows always get
# slightly lower encodings than non-returned rows within the same category,
# which XGBoost exploits to reconstruct the target (AUC -> 1.0).
#
# k-fold OOF encoding: each row's encoding is computed from the OTHER 4 folds,
# so its own label never influences its own feature value.
#
# Produces: train_clean_v2.csv, test_clean_v2.csv
# =============================================================================

library(dplyr)
library(caret)

set.seed(42)

# =============================================================================
# 1. LOAD DATA
# =============================================================================
train_raw <- read.csv("returns_train.csv", stringsAsFactors = FALSE)
test_raw  <- read.csv("returns_test.csv",  stringsAsFactors = FALSE)
train     <- read.csv("train_clean.csv",   stringsAsFactors = FALSE)
test      <- read.csv("test_clean.csv",    stringsAsFactors = FALSE)

global_rate <- mean(train_raw$returned)
cat("Global return rate:", round(global_rate, 4), "\n")
cat("Train rows:", nrow(train_raw), "| Test rows:", nrow(test_raw), "\n\n")

# =============================================================================
# 2. K-FOLD OUT-OF-FOLD TARGET ENCODING FUNCTION
#
# For training rows:
#   Split train into K folds. For each fold, compute category return rates
#   using ONLY the other K-1 folds, then encode this fold's rows with those
#   rates. No row's label ever influences its own encoding.
#
# For test rows:
#   Use full training set statistics (no leakage risk).
#
# Smoothing: encoded = (sum_returns + k * global_rate) / (count + k)
# =============================================================================

kfold_target_encode <- function(train_cat, train_target, test_cat,
                                K = 5, smooth_k = 10, global_rate) {
  n      <- length(train_cat)
  result <- numeric(n)
  
  folds <- createFolds(train_target, k = K, list = TRUE, returnTrain = FALSE)
  
  for (fold_idx in seq_len(K)) {
    val_rows   <- folds[[fold_idx]]
    train_rows <- setdiff(seq_len(n), val_rows)
    
    cats_tr  <- train_cat[train_rows]
    targs_tr <- train_target[train_rows]
    
    cat_sum   <- tapply(targs_tr, cats_tr, sum)
    cat_count <- tapply(targs_tr, cats_tr, length)
    
    for (i in val_rows) {
      cat_i <- train_cat[i]
      if (cat_i %in% names(cat_sum)) {
        result[i] <- (cat_sum[cat_i] + smooth_k * global_rate) /
          (cat_count[cat_i] + smooth_k)
      } else {
        result[i] <- global_rate
      }
    }
  }
  
  # Test: full training set encoding
  cat_sum_full   <- tapply(train_target, train_cat, sum)
  cat_count_full <- tapply(train_target, train_cat, length)
  
  test_result <- numeric(length(test_cat))
  for (i in seq_along(test_cat)) {
    cat_i <- test_cat[i]
    if (!is.na(cat_i) && cat_i %in% names(cat_sum_full)) {
      test_result[i] <- (cat_sum_full[cat_i] + smooth_k * global_rate) /
        (cat_count_full[cat_i] + smooth_k)
    } else {
      test_result[i] <- global_rate
    }
  }
  
  list(train_encoded = result, test_encoded = test_result)
}

# =============================================================================
# 3. APPLY TARGET ENCODING
# =============================================================================

cat("Encoding product_subcategory...\n")
te_sub <- kfold_target_encode(
  train_cat    = train_raw$product_subcategory,
  train_target = train_raw$returned,
  test_cat     = test_raw$product_subcategory,
  K = 5, smooth_k = 10, global_rate = global_rate
)
train$te_subcategory <- te_sub$train_encoded
test$te_subcategory  <- te_sub$test_encoded

# Leakage check: returned vs non-returned rows in Formal should have
# nearly identical mean encodings (difference should be < 0.001)
formal_rows <- which(train_raw$product_subcategory == "Formal")
ret1_enc <- mean(train$te_subcategory[formal_rows[train_raw$returned[formal_rows] == 1]])
ret0_enc <- mean(train$te_subcategory[formal_rows[train_raw$returned[formal_rows] == 0]])
cat(sprintf("  Formal encoding gap (ret=1 vs ret=0): %.6f", abs(ret1_enc - ret0_enc)))
cat(if (abs(ret1_enc - ret0_enc) < 0.002) "  ✓ clean\n" else "  ✗ WARNING\n")

cat("Encoding zip_code...\n")
te_zip <- kfold_target_encode(
  train_cat    = as.character(train_raw$zip_code),
  train_target = train_raw$returned,
  test_cat     = as.character(test_raw$zip_code),
  K = 5, smooth_k = 20, global_rate = global_rate
)
train$te_zip <- te_zip$train_encoded
test$te_zip  <- te_zip$test_encoded
cat("  Range:", round(range(train$te_zip), 3), "\n")

cat("Encoding hour_of_day...\n")
train_raw$hour <- as.integer(substr(train_raw$transaction_timestamp, 12, 13))
test_raw$hour  <- as.integer(substr(test_raw$transaction_timestamp,  12, 13))

te_hour <- kfold_target_encode(
  train_cat    = as.character(train_raw$hour),
  train_target = train_raw$returned,
  test_cat     = as.character(test_raw$hour),
  K = 5, smooth_k = 5, global_rate = global_rate
)
train$te_hour <- te_hour$train_encoded
test$te_hour  <- te_hour$test_encoded
cat("  Range:", round(range(train$te_hour), 3), "\n")

# =============================================================================
# 4. CUSTOMER HISTORY FEATURES
#
# Safe from target encoding leakage because we aggregate across a customer's
# OTHER transactions, not the current row's own label.
# Leave-current-out ensures the current transaction's label doesn't
# contaminate its own customer history feature.
# =============================================================================
cat("\nBuilding customer history features...\n")

train_raw$returned <- as.integer(train_raw$returned)

cust_stats <- train_raw %>%
  group_by(customer_id) %>%
  summarise(
    cust_total_tx      = n(),
    cust_total_returns = sum(returned),
    cust_avg_price     = mean(price),
    cust_avg_discount  = mean(discount_pct),
    .groups = "drop"
  )

train_raw_aug <- train_raw %>%
  left_join(cust_stats, by = "customer_id") %>%
  mutate(
    loo_tx      = cust_total_tx - 1,
    loo_returns = cust_total_returns - returned,
    cust_hist_return_rate    = ifelse(
      loo_tx > 0,
      (loo_returns + 3 * global_rate) / (loo_tx + 3),
      global_rate
    ),
    cust_hist_tx_count       = loo_tx,
    cust_is_repeat           = as.integer(loo_tx > 0),
    cust_has_returned_before = as.integer(loo_returns > 0)
  )

train$cust_hist_return_rate     <- train_raw_aug$cust_hist_return_rate
train$cust_hist_tx_count        <- train_raw_aug$cust_hist_tx_count
train$cust_is_repeat            <- train_raw_aug$cust_is_repeat
train$cust_has_returned_before  <- train_raw_aug$cust_has_returned_before

test_aug <- test_raw %>%
  left_join(cust_stats, by = "customer_id") %>%
  mutate(
    cust_hist_return_rate    = ifelse(
      !is.na(cust_total_tx),
      (cust_total_returns + 3 * global_rate) / (cust_total_tx + 3),
      global_rate
    ),
    cust_hist_tx_count       = ifelse(!is.na(cust_total_tx), cust_total_tx, 0),
    cust_is_repeat           = as.integer(!is.na(cust_total_tx)),
    cust_has_returned_before = ifelse(
      !is.na(cust_total_returns),
      as.integer(cust_total_returns > 0), 0L
    )
  )

test$cust_hist_return_rate     <- test_aug$cust_hist_return_rate
test$cust_hist_tx_count        <- test_aug$cust_hist_tx_count
test$cust_is_repeat            <- test_aug$cust_is_repeat
test$cust_has_returned_before  <- test_aug$cust_has_returned_before

cat("  Test customers with history:", sum(test$cust_is_repeat), "/", nrow(test),
    "(", round(100*mean(test$cust_is_repeat), 1), "%)\n")

# =============================================================================
# 5. SIGNAL CHECK
# =============================================================================
cat("\n=== SIGNAL CHECK (correlations with 'returned') ===\n")
new_features <- c("te_subcategory", "te_zip", "te_hour",
                  "cust_hist_return_rate", "cust_hist_tx_count",
                  "cust_is_repeat", "cust_has_returned_before")

for (f in new_features) {
  r <- cor(train[[f]], train$returned, use = "complete.obs")
  cat(sprintf("  %-30s r = %+.4f\n", f, r))
}

# =============================================================================
# 6. SAVE
# =============================================================================
write.csv(train, "train_clean_v2.csv", row.names = FALSE)
write.csv(test,  "test_clean_v2.csv",  row.names = FALSE)

cat("\n✓ Saved: train_clean_v2.csv (", ncol(train), "cols)\n")
cat("✓ Saved: test_clean_v2.csv  (", ncol(test),  "cols)\n")
cat("Next: run modeling_v2.R\n")