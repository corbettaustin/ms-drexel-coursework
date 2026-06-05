# =============================================================================
# KAGGLE COMPETITION: Product Return Prediction
# Step 1c: Feature Engineering v3
#
# Adds on top of v2:
#   1. te_promo_count  — target encode promo count (0/1/2): dose-response signal
#                        0 promos=10.9%, 1 promo=15.5%, 2 promos=20.2%
#   2. price_x_te_hour — price × te_hour interaction: high-price night purchases
#   3. te_subcategory_x_promo — subcategory rate × promo_count interaction
#
# NOT added (EDA ruled out):
#   - Month encoding: flat 14-17% across all months, no signal
#   - Day-of-week encoding: flat 14-16% across all days, no signal  
#   - Discount × subcategory: Formal return rate flat across all discount levels
#   - Price target encoding: price signal fully explained by subcategory composition
#
# Produces: train_clean_v3.csv, test_clean_v3.csv
# =============================================================================

library(dplyr)
library(caret)

set.seed(42)

# =============================================================================
# 1. LOAD
# =============================================================================
train_raw <- read.csv("returns_train.csv", stringsAsFactors = FALSE)
test_raw  <- read.csv("returns_test.csv",  stringsAsFactors = FALSE)
train     <- read.csv("train_clean_v2.csv", stringsAsFactors = FALSE)
test      <- read.csv("test_clean_v2.csv",  stringsAsFactors = FALSE)

global_rate <- mean(train_raw$returned)
cat("Loaded v2 features:", ncol(train), "cols\n")

# =============================================================================
# 2. K-FOLD OOF TARGET ENCODING FUNCTION (same as v2)
# =============================================================================
kfold_target_encode <- function(train_cat, train_target, test_cat,
                                K = 5, smooth_k = 10, global_rate) {
  n      <- length(train_cat)
  result <- numeric(n)
  folds  <- createFolds(train_target, k = K, list = TRUE, returnTrain = FALSE)
  
  for (fold_idx in seq_len(K)) {
    val_rows   <- folds[[fold_idx]]
    train_rows <- setdiff(seq_len(n), val_rows)
    cats_tr    <- train_cat[train_rows]
    targs_tr   <- train_target[train_rows]
    cat_sum    <- tapply(targs_tr, cats_tr, sum)
    cat_count  <- tapply(targs_tr, cats_tr, length)
    
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
  
  cat_sum_full   <- tapply(train_target, train_cat, sum)
  cat_count_full <- tapply(train_target, train_cat, length)
  test_result    <- numeric(length(test_cat))
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
# 3. PROMO COUNT TARGET ENCODING
#
# Raw promo_count already exists in v2 features (0/1/2).
# But encoding it as a target-encoded continuous feature gives the model
# a pre-computed return rate per promo level:
#   0 promos -> ~0.109, 1 promo -> ~0.155, 2 promos -> ~0.202
# This is a clean dose-response signal not fully captured by the binary flags.
# Low smoothing k=3: only 3 levels, each has ~10-14k observations.
# =============================================================================
cat("Encoding promo_count...\n")

train_raw$promo_count <- ifelse(
  train_raw$applied_promo_codes == "" | is.na(train_raw$applied_promo_codes),
  0L,
  lengths(regmatches(
    train_raw$applied_promo_codes,
    gregexpr(",", train_raw$applied_promo_codes)
  )) + 1L
)
test_raw$promo_count <- ifelse(
  test_raw$applied_promo_codes == "" | is.na(test_raw$applied_promo_codes),
  0L,
  lengths(regmatches(
    test_raw$applied_promo_codes,
    gregexpr(",", test_raw$applied_promo_codes)
  )) + 1L
)

te_promo <- kfold_target_encode(
  train_cat    = as.character(train_raw$promo_count),
  train_target = train_raw$returned,
  test_cat     = as.character(test_raw$promo_count),
  K = 5, smooth_k = 3, global_rate = global_rate
)
train$te_promo_count <- te_promo$train_encoded
test$te_promo_count  <- te_promo$test_encoded

cat("  Unique train values:", length(unique(train$te_promo_count)),
    "(should be ~15, 3 levels x 5 folds)\n")
cat("  Range:", round(range(train$te_promo_count), 4), "\n")

# =============================================================================
# 4. INTERACTION FEATURES
#
# te_subcategory × te_promo_count:
#   Captures "high-return-rate item bought with multiple promos" 
#   e.g. Formal (te=0.50) × 2 promos (te=0.20) = 0.10 (large value)
#   vs   Audio  (te=0.10) × 0 promos (te=0.11) = 0.011 (small value)
#   Gives the model a direct signal for the highest-risk combination.
#
# te_hour × te_subcategory:
#   Night purchase (te_hour~0.28) of Formal (te_sub~0.50) = 0.14
#   vs Daytime Electronics purchase = ~0.01
#   Finer-grained than the binary formal_x_night interaction from v2.
# =============================================================================
cat("Adding interaction features...\n")

train$te_sub_x_promo <- train$te_subcategory * train$te_promo_count
test$te_sub_x_promo  <- test$te_subcategory  * test$te_promo_count

train$te_hour_x_sub  <- train$te_hour * train$te_subcategory
test$te_hour_x_sub   <- test$te_hour  * test$te_subcategory

cat("  te_sub_x_promo range:", round(range(train$te_sub_x_promo), 4), "\n")
cat("  te_hour_x_sub range: ", round(range(train$te_hour_x_sub),  4), "\n")

# =============================================================================
# 5. SIGNAL CHECK
# =============================================================================
cat("\n=== SIGNAL CHECK (new v3 features vs returned) ===\n")
new_v3 <- c("te_promo_count", "te_sub_x_promo", "te_hour_x_sub")
for (f in new_v3) {
  r <- cor(train[[f]], train$returned, use = "complete.obs")
  cat(sprintf("  %-30s r = %+.4f\n", f, r))
}

# Compare to key v2 features for context
cat("\n  (v2 reference correlations)\n")
for (f in c("te_subcategory", "te_zip", "te_hour")) {
  r <- cor(train[[f]], train$returned, use = "complete.obs")
  cat(sprintf("  %-30s r = %+.4f\n", f, r))
}

# =============================================================================
# 6. SAVE
# =============================================================================
write.csv(train, "train_clean_v3.csv", row.names = FALSE)
write.csv(test,  "test_clean_v3.csv",  row.names = FALSE)

cat("\n✓ Saved: train_clean_v3.csv (", ncol(train), "cols)\n")
cat("✓ Saved: test_clean_v3.csv  (", ncol(test),  "cols)\n")
cat("New features added: te_promo_count, te_sub_x_promo, te_hour_x_sub\n")
cat("Next: run modeling_v3.R\n")