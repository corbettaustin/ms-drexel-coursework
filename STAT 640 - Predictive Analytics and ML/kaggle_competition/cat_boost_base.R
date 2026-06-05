# =============================================================================
# KAGGLE COMPETITION: Product Return Prediction
# CatBoost Pipeline in R
#
# Install catboost for R (run once):
#   install.packages('devtools')
#   devtools::install_url(
#     'https://github.com/catboost/catboost/releases/download/v1.2.7/catboost-R-Darwin-universal2-1.2.7.tgz',
#     INSTALL_opts = c("--no-multiarch", "--no-test-load")
#   )
#   # For Windows:
#   # devtools::install_url('https://github.com/catboost/catboost/releases/download/v1.2.7/catboost-R-Windows-x86_64-1.2.7.tgz', ...)
#   # For Linux:
#   # devtools::install_url('https://github.com/catboost/catboost/releases/download/v1.2.7/catboost-R-Linux-1.2.7.tgz', ...)
#
# Other packages: install.packages(c("dplyr", "pROC", "caret"))
# =============================================================================

library(catboost)
library(dplyr)
library(pROC)
library(caret)

set.seed(42)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

train <- read.csv("returns_train.csv", stringsAsFactors = FALSE)
test  <- read.csv("returns_test.csv",  stringsAsFactors = FALSE)

cat("Train shape:", nrow(train), "x", ncol(train), "\n")
cat("Return rate:", round(mean(train$returned), 4), "\n")

# =============================================================================
# 2. PREPROCESSING
# =============================================================================

preprocess <- function(df) {
  
  # --- Timestamp features ---
  df$ts          <- as.POSIXct(df$transaction_timestamp, format = "%Y-%m-%d %H:%M:%S")
  df$hour        <- as.integer(format(df$ts, "%H"))
  df$day_of_week <- as.integer(format(df$ts, "%u")) - 1L  # 0=Mon, 6=Sun
  df$month       <- as.integer(format(df$ts, "%m"))
  df$day_of_month <- as.integer(format(df$ts, "%d"))
  
  # --- Clean payment_method (messy casing/naming) ---
  df$payment_method_clean <- dplyr::case_when(
    df$payment_method %in% c("Visa", "visa", "Credit Card - Visa") ~ "visa",
    df$payment_method %in% c("MasterCard", "mastercard")           ~ "mastercard",
    df$payment_method %in% c("PayPal", "paypal")                   ~ "paypal",
    TRUE                                                            ~ "other"
  )
  
  # --- Promo features ---
  df$has_promo      <- as.integer(!is.na(df$applied_promo_codes))
  df$promo_codes_safe <- ifelse(is.na(df$applied_promo_codes), "", df$applied_promo_codes)
  df$promo_freeship <- as.integer(grepl("FREESHIP", df$promo_codes_safe))
  df$promo_winter   <- as.integer(grepl("WINTER",   df$promo_codes_safe))
  df$promo_count    <- ifelse(df$has_promo == 1,
                              nchar(gsub("[^,]", "", df$promo_codes_safe)) + 1L,
                              0L)
  
  # --- Derived numeric features ---
  df$discount_amount <- df$price * df$discount_pct
  df$log_price       <- log1p(df$price)
  df$guest_checkout  <- as.integer(df$guest_checkout)
  df$zip_code_str    <- as.character(df$zip_code)
  
  # --- CRITICAL: CatBoost requires NAs in categorical columns to be a string ---
  cat_cols <- c("marketing_channel", "product_category", "product_subcategory",
                "loyalty_tier", "payment_method_clean", "zip_code_str")
  for (col in cat_cols) {
    df[[col]] <- ifelse(is.na(df[[col]]), "MISSING", as.character(df[[col]]))
  }
  
  return(df)
}

train <- preprocess(train)
test  <- preprocess(test)

# =============================================================================
# 3. DEFINE FEATURE SETS
# =============================================================================

cat_features <- c(
  "marketing_channel", "product_category", "product_subcategory",
  "loyalty_tier", "payment_method_clean", "zip_code_str"
)

num_features <- c(
  "price", "log_price", "discount_pct", "discount_amount",
  "customer_age", "account_age_days",
  "hour", "day_of_week", "month", "day_of_month",
  "has_promo", "promo_freeship", "promo_winter", "promo_count",
  "guest_checkout"
)

all_features <- c(cat_features, num_features)
target       <- "returned"

# FIX: Convert cat columns to factors — CatBoost R auto-detects factors as
# categorical features. Passing string columns causes the "Dictionary size 0" error.
for (col in cat_features) {
  train[[col]] <- as.factor(train[[col]])
  test[[col]]  <- as.factor(test[[col]])
}

# Ensure numeric columns are numeric (not integer)
for (col in num_features) {
  train[[col]] <- as.numeric(train[[col]])
  test[[col]]  <- as.numeric(test[[col]])
}

X_train <- train[, all_features]
y_train <- train[[target]]
X_test  <- test[,  all_features]

cat("Features:", length(all_features), "\n")
cat("Categorical features (as factors):", length(cat_features), "\n")

# =============================================================================
# 4. 5-FOLD CROSS-VALIDATION
# =============================================================================

set.seed(42)
folds     <- createFolds(y_train, k = 5, list = TRUE, returnTrain = FALSE)
oof_preds <- numeric(nrow(X_train))
fold_aucs <- numeric(5)
models    <- list()

# CatBoost parameters (best config from tuning: depth=6, lr=0.03, l2=3)
cb_params <- list(
  iterations          = 1000,
  learning_rate       = 0.03,
  depth               = 6,
  l2_leaf_reg         = 3,
  eval_metric         = "AUC",
  random_seed         = 42,
  verbose             = 0,
  early_stopping_rounds = 50
)

cat("\n=== 5-Fold Cross-Validation ===\n")

for (fold in 1:5) {
  val_idx <- folds[[fold]]
  tr_idx  <- setdiff(seq_len(nrow(X_train)), val_idx)
  
  X_tr <- X_train[tr_idx, ]
  y_tr <- y_train[tr_idx]
  X_val <- X_train[val_idx, ]
  y_val <- y_train[val_idx]
  
  # Build CatBoost Pool objects (factor columns auto-detected as categorical)
  train_pool <- catboost.load_pool(
    data  = X_tr,
    label = y_tr
  )
  val_pool <- catboost.load_pool(
    data  = X_val,
    label = y_val
  )
  
  # Train model
  model <- catboost.train(
    learn_pool = train_pool,
    test_pool  = val_pool,
    params     = cb_params
  )
  
  # Out-of-fold predictions
  val_preds <- catboost.predict(model, val_pool, prediction_type = "Probability")
  oof_preds[val_idx] <- val_preds
  
  fold_auc <- as.numeric(auc(roc(y_val, val_preds, quiet = TRUE)))
  fold_aucs[fold] <- fold_auc
  models[[fold]]  <- model
  
  best_iter <- model$tree_count
  cat(sprintf("  Fold %d: AUC = %.4f  (iterations used: %d)\n", fold, fold_auc, best_iter))
}

oof_auc <- as.numeric(auc(roc(y_train, oof_preds, quiet = TRUE)))
cat(sprintf("\nOOF AUC:       %.4f\n", oof_auc))
cat(sprintf("Mean Fold AUC: %.4f +/- %.4f\n", mean(fold_aucs), sd(fold_aucs)))

# =============================================================================
# 5. LEAKAGE CHECK
# (Shuffle labels, refit 1 fold — clean model should drop to ~0.50)
# =============================================================================

cat("\n=== Leakage Check (1 fold, shuffled labels) ===\n")
set.seed(99)
y_shuffled <- sample(y_train)

val_idx_check <- folds[[1]]
tr_idx_check  <- setdiff(seq_len(nrow(X_train)), val_idx_check)

check_pool_tr  <- catboost.load_pool(X_train[tr_idx_check, ],  y_shuffled[tr_idx_check])
check_pool_val <- catboost.load_pool(X_train[val_idx_check, ], y_shuffled[val_idx_check])

check_params <- cb_params
check_params$iterations <- 200
check_params$early_stopping_rounds <- 30

check_model <- catboost.train(check_pool_tr, check_pool_val, params = check_params)
check_preds <- catboost.predict(check_model, check_pool_val, prediction_type = "Probability")
shuffled_auc <- as.numeric(auc(roc(y_shuffled[val_idx_check], check_preds, quiet = TRUE)))

cat(sprintf("  Real label AUC:     %.4f\n", fold_aucs[1]))
cat(sprintf("  Shuffled label AUC: %.4f\n", shuffled_auc))
if (shuffled_auc < 0.55) {
  cat("  CLEAN: No leakage detected.\n")
} else {
  cat("  WARNING: Shuffled AUC suspicious — check for leakage!\n")
}

# =============================================================================
# 6. FEATURE IMPORTANCE (averaged across folds)
# =============================================================================

cat("\n=== Top 15 Feature Importances (avg across folds) ===\n")

imp_matrix <- matrix(0, nrow = length(all_features), ncol = 5)
for (fold in 1:5) {
  full_pool <- catboost.load_pool(X_train, y_train)
  imp_matrix[, fold] <- catboost.get_feature_importance(models[[fold]], full_pool)
}
avg_importance <- rowMeans(imp_matrix)

imp_df <- data.frame(
  feature    = all_features,
  importance = avg_importance
) |> arrange(desc(importance))

print(head(imp_df, 15), row.names = FALSE)

# =============================================================================
# 7. GENERATE SUBMISSION (average predictions across all 5 fold models)
# =============================================================================

test_pool  <- catboost.load_pool(X_test)
test_preds <- numeric(nrow(X_test))

for (fold in 1:5) {
  test_preds <- test_preds + catboost.predict(models[[fold]], test_pool,
                                              prediction_type = "Probability")
}
test_preds <- test_preds / 5

cat(sprintf("\nTest prediction range: [%.4f, %.4f]\n", min(test_preds), max(test_preds)))

submission <- data.frame(
  transaction_id = test$transaction_id,
  returned       = test_preds
)
write.csv(submission, "submission_catboost.csv", row.names = FALSE)
cat("Submission saved to submission_catboost.csv\n")

# =============================================================================
# 8. SUMMARY
# =============================================================================

cat("\n=== MODEL PROGRESSION SUMMARY ===\n")
results <- data.frame(
  model = c("XGBoost baseline (prior work)", "XGBoost tuned (prior work)",
            "CatBoost baseline"),
  oof_auc = c(0.8196, 0.8215, oof_auc)
)
print(results, row.names = FALSE)