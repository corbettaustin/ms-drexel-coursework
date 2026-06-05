# =============================================================================
# KAGGLE COMPETITION: Product Return Prediction
# CatBoost + XGBoost Ensemble Blend
#
# What this script does:
#   1. Preprocesses train/test identically for both models
#   2. Builds OOF target encodings for XGBoost (CatBoost doesn't need them)
#   3. Runs 5-fold CV for both models, collecting OOF predictions
#   4. Searches for the optimal blend weight using OOF AUC
#   5. Saves: submission_catboost_final.csv
#             submission_blend_cb_xgb.csv
#             oof_predictions.csv
#
# Packages needed:
#   install.packages(c("dplyr", "pROC", "caret", "xgboost", "fastDummies"))
#   # CatBoost: see catboost_pipeline.R header for install instructions
# =============================================================================

library(catboost)
library(xgboost)
library(dplyr)
library(pROC)
library(caret)
library(fastDummies)

set.seed(42)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

train <- read.csv("returns_train.csv",  stringsAsFactors = FALSE)
test  <- read.csv("returns_test.csv",   stringsAsFactors = FALSE)

cat("Train:", nrow(train), "rows |",
    "Return rate:", round(mean(train$returned), 4), "\n")

# =============================================================================
# 2. SHARED PREPROCESSING
# =============================================================================

preprocess <- function(df) {
  df$ts           <- as.POSIXct(df$transaction_timestamp, format = "%Y-%m-%d %H:%M:%S")
  df$hour         <- as.integer(format(df$ts, "%H"))
  df$day_of_week  <- as.integer(format(df$ts, "%u")) - 1L
  df$month        <- as.integer(format(df$ts, "%m"))
  df$day_of_month <- as.integer(format(df$ts, "%d"))
  
  df$payment_method_clean <- dplyr::case_when(
    df$payment_method %in% c("Visa","visa","Credit Card - Visa") ~ "visa",
    df$payment_method %in% c("MasterCard","mastercard")          ~ "mastercard",
    df$payment_method %in% c("PayPal","paypal")                  ~ "paypal",
    TRUE                                                          ~ "other"
  )
  
  df$promo_safe    <- ifelse(is.na(df$applied_promo_codes), "", df$applied_promo_codes)
  df$has_promo     <- as.integer(!is.na(df$applied_promo_codes))
  df$promo_freeship <- as.integer(grepl("FREESHIP", df$promo_safe))
  df$promo_winter  <- as.integer(grepl("WINTER",   df$promo_safe))
  df$promo_count   <- ifelse(df$has_promo == 1,
                             nchar(gsub("[^,]","", df$promo_safe)) + 1L, 0L)
  
  df$discount_amount <- df$price * df$discount_pct
  df$log_price       <- log1p(df$price)
  df$guest_checkout  <- as.integer(df$guest_checkout)
  df$zip_code_str    <- as.character(df$zip_code)
  df$is_late_night   <- as.integer(df$hour %in% c(0,1,2,3,22,23))
  
  # Fill NAs in categorical columns with "MISSING"
  cat_cols <- c("marketing_channel","product_category","product_subcategory",
                "loyalty_tier","payment_method_clean","zip_code_str")
  for (col in cat_cols)
    df[[col]] <- ifelse(is.na(df[[col]]), "MISSING", as.character(df[[col]]))
  
  return(df)
}

train <- preprocess(train)
test  <- preprocess(test)

# =============================================================================
# 3. OOF TARGET ENCODINGS (for XGBoost only; CatBoost handles cats natively)
# =============================================================================

global_rate <- mean(train$returned)
skf_feat    <- createFolds(train$returned, k = 5, list = TRUE, returnTrain = FALSE)

oof_target_encode <- function(train_df, test_df, col, k = 10) {
  # Returns smoothed OOF encoding for train rows, full-data encoding for test rows
  out_train <- rep(global_rate, nrow(train_df))
  
  for (val_idx in skf_feat) {
    tr_idx    <- setdiff(seq_len(nrow(train_df)), val_idx)
    fold_data <- train_df[tr_idx, ]
    
    agg <- aggregate(returned ~ ., data = fold_data[, c(col, "returned")],
                     FUN = function(x) c(sum = sum(x), n = length(x)))
    # Rebuild as clean data frame
    agg_df <- data.frame(
      key  = agg[[1]],
      te   = (agg$returned[, "sum"] + k * global_rate) /
        (agg$returned[, "n"]   + k)
    )
    names(agg_df)[1] <- col
    
    val_df <- train_df[val_idx, col, drop = FALSE]
    val_df$row_idx <- val_idx
    val_df <- merge(val_df, agg_df, by = col, all.x = TRUE)
    val_df$te[is.na(val_df$te)] <- global_rate
    out_train[val_df$row_idx] <- val_df$te
  }
  
  # Test encoding: use full training data
  agg_full <- aggregate(returned ~ ., data = train_df[, c(col, "returned")],
                        FUN = function(x) c(sum = sum(x), n = length(x)))
  agg_full_df <- data.frame(
    key = agg_full[[1]],
    te  = (agg_full$returned[, "sum"] + k * global_rate) /
      (agg_full$returned[, "n"]   + k)
  )
  names(agg_full_df)[1] <- col
  
  test_mapped <- merge(test_df[, col, drop = FALSE], agg_full_df, by = col, all.x = TRUE)
  out_test <- ifelse(is.na(test_mapped$te), global_rate, test_mapped$te)
  
  return(list(train = out_train, test = out_test))
}

cat("Building OOF target encodings for XGBoost...\n")
te_subcat <- oof_target_encode(train, test, "product_subcategory")
te_zip    <- oof_target_encode(train, test, "zip_code_str")
te_hour   <- oof_target_encode(train, test, "hour")

train$te_subcat <- te_subcat$train
train$te_zip    <- te_zip$train
train$te_hour   <- te_hour$train
test$te_subcat  <- te_subcat$test
test$te_zip     <- te_zip$test
test$te_hour    <- te_hour$test
cat("Done.\n")

# =============================================================================
# 4. FEATURE SETS
# =============================================================================

cat_features <- c("marketing_channel","product_category","product_subcategory",
                  "loyalty_tier","payment_method_clean","zip_code_str")

# CatBoost feature set (categoricals as factors, rest numeric)
cb_num_features <- c("price","log_price","discount_pct","discount_amount",
                     "customer_age","account_age_days","hour","day_of_week",
                     "month","day_of_month","has_promo","promo_freeship",
                     "promo_winter","promo_count","guest_checkout","is_late_night")
cb_all_features <- c(cat_features, cb_num_features)

# Convert cat columns to factors for CatBoost
for (col in cat_features) {
  train[[col]] <- as.factor(train[[col]])
  test[[col]]  <- as.factor(test[[col]])
}
for (col in cb_num_features) {
  train[[col]] <- as.numeric(train[[col]])
  test[[col]]  <- as.numeric(test[[col]])
}

# XGBoost feature set (one-hot + target encodings, no native categoricals)
xgb_num_features <- c("price","log_price","discount_pct","discount_amount",
                      "customer_age","account_age_days","hour","day_of_week",
                      "month","day_of_month","has_promo","promo_freeship",
                      "promo_winter","promo_count","guest_checkout","is_late_night",
                      "te_subcat","te_zip","te_hour")

# One-hot encode categorical columns for XGBoost
cat_cols_char <- lapply(cat_features, function(col) as.character(train[[col]]))
names(cat_cols_char) <- cat_features
cat_df_train <- as.data.frame(cat_cols_char)
cat_df_test  <- as.data.frame(lapply(cat_features, function(col) as.character(test[[col]])))
names(cat_df_test) <- cat_features

dummies_train <- fastDummies::dummy_cols(cat_df_train, remove_first_dummy = FALSE,
                                         remove_selected_columns = TRUE)
dummies_test  <- fastDummies::dummy_cols(cat_df_test,  remove_first_dummy = FALSE,
                                         remove_selected_columns = TRUE)

# Align columns
missing_cols <- setdiff(names(dummies_train), names(dummies_test))
for (col in missing_cols) dummies_test[[col]] <- 0
dummies_test <- dummies_test[, names(dummies_train), drop = FALSE]

X_xgb_train <- cbind(train[, xgb_num_features], dummies_train)
X_xgb_test  <- cbind(test[,  xgb_num_features], dummies_test)

# Replace any remaining NAs with -999 (XGBoost handles these as missing)
X_xgb_train[is.na(X_xgb_train)] <- -999
X_xgb_test[is.na(X_xgb_test)]   <- -999

X_cb_train <- train[, cb_all_features]
X_cb_test  <- test[,  cb_all_features]
y          <- train$returned

cat("CatBoost features:", length(cb_all_features), "\n")
cat("XGBoost features:", ncol(X_xgb_train), "\n")

# =============================================================================
# 5. 5-FOLD OOF: CATBOOST + XGBOOST
# =============================================================================

set.seed(42)
folds <- createFolds(y, k = 5, list = TRUE, returnTrain = FALSE)

oof_cb  <- numeric(nrow(train))
oof_xgb <- numeric(nrow(train))
test_cb_folds  <- matrix(0, nrow = nrow(test), ncol = 5)
test_xgb_folds <- matrix(0, nrow = nrow(test), ncol = 5)

# CatBoost parameters (tuned: depth=6, lr=0.03, l2=3)
cb_params <- list(
  iterations           = 1000,
  learning_rate        = 0.03,
  depth                = 6,
  l2_leaf_reg          = 3,
  eval_metric          = "AUC",
  random_seed          = 42,
  verbose              = 0,
  early_stopping_rounds = 50
)

# XGBoost parameters
scale_pos <- sum(y == 0) / sum(y == 1)
xgb_params <- list(
  objective        = "binary:logistic",
  eval_metric      = "auc",
  eta              = 0.05,
  max_depth        = 6,
  min_child_weight = 3,
  subsample        = 0.8,
  colsample_bytree = 0.8,
  scale_pos_weight = scale_pos,
  seed             = 42
)

cat("\n=== 5-Fold OOF: CatBoost + XGBoost ===\n")

for (fold in seq_len(5)) {
  val_idx <- folds[[fold]]
  tr_idx  <- setdiff(seq_len(nrow(train)), val_idx)
  
  # ── CatBoost ────────────────────────────────────────────────────────────────
  train_pool <- catboost.load_pool(X_cb_train[tr_idx, ],  label = y[tr_idx])
  val_pool   <- catboost.load_pool(X_cb_train[val_idx, ], label = y[val_idx])
  test_pool  <- catboost.load_pool(X_cb_test)
  
  cb_model <- catboost.train(train_pool, val_pool, params = cb_params)
  
  oof_cb[val_idx]        <- catboost.predict(cb_model, val_pool,  prediction_type = "Probability")
  test_cb_folds[, fold]  <- catboost.predict(cb_model, test_pool, prediction_type = "Probability")
  
  auc_cb <- as.numeric(auc(roc(y[val_idx], oof_cb[val_idx], quiet = TRUE)))
  
  # ── XGBoost ─────────────────────────────────────────────────────────────────
  dtrain_f <- xgb.DMatrix(as.matrix(X_xgb_train[tr_idx, ]),  label = y[tr_idx])
  dval_f   <- xgb.DMatrix(as.matrix(X_xgb_train[val_idx, ]), label = y[val_idx])
  dtest_f  <- xgb.DMatrix(as.matrix(X_xgb_test))
  
  xgb_model <- xgb.train(
    params  = xgb_params,
    data    = dtrain_f,
    nrounds = 500,
    watchlist = list(val = dval_f),
    early_stopping_rounds = 40,
    verbose = 0
  )
  
  oof_xgb[val_idx]       <- predict(xgb_model, dval_f)
  test_xgb_folds[, fold] <- predict(xgb_model, dtest_f)
  
  auc_xgb <- as.numeric(auc(roc(y[val_idx], oof_xgb[val_idx], quiet = TRUE)))
  
  cat(sprintf("  Fold %d: CB=%.4f  XGB=%.4f\n", fold, auc_cb, auc_xgb))
}

test_cb_avg  <- rowMeans(test_cb_folds)
test_xgb_avg <- rowMeans(test_xgb_folds)

cb_oof_auc  <- as.numeric(auc(roc(y, oof_cb,  quiet = TRUE)))
xgb_oof_auc <- as.numeric(auc(roc(y, oof_xgb, quiet = TRUE)))
corr        <- cor(oof_cb, oof_xgb)

cat(sprintf("\nCatBoost OOF AUC: %.4f\n", cb_oof_auc))
cat(sprintf("XGBoost  OOF AUC: %.4f\n", xgb_oof_auc))
cat(sprintf("OOF correlation:  %.4f\n", corr))

# =============================================================================
# 6. BLEND WEIGHT SEARCH
# =============================================================================

cat("\n=== Blend Weight Search ===\n")
weights <- seq(0.50, 1.00, by = 0.05)
blend_results <- data.frame(cb_weight = weights, xgb_weight = 1 - weights, oof_auc = NA)

best_w   <- 1.0
best_auc <- 0.0

for (i in seq_along(weights)) {
  w    <- weights[i]
  bauc <- as.numeric(auc(roc(y, w * oof_cb + (1-w) * oof_xgb, quiet = TRUE)))
  blend_results$oof_auc[i] <- bauc
  marker <- if (bauc > best_auc) " <-- best" else ""
  cat(sprintf("  CB=%.2f XGB=%.2f: %.4f%s\n", w, 1-w, bauc, marker))
  if (bauc > best_auc) { best_auc <- bauc; best_w <- w }
}

cat(sprintf("\nBest blend: CB=%.2f, XGB=%.2f → OOF AUC = %.4f\n", best_w, 1-best_w, best_auc))
cat(sprintf("Gain over pure CatBoost: %+.4f\n", best_auc - cb_oof_auc))

# =============================================================================
# 7. SAVE SUBMISSIONS
# =============================================================================

sample_sub <- read.csv("sample_submission.csv")

# Pure CatBoost
sub_cb <- data.frame(transaction_id = sample_sub$transaction_id,
                     returned       = test_cb_avg)
write.csv(sub_cb, "submission_catboost_final.csv", row.names = FALSE)

# Best blend
blend_test_preds <- best_w * test_cb_avg + (1 - best_w) * test_xgb_avg
sub_blend <- data.frame(transaction_id = sample_sub$transaction_id,
                        returned       = blend_test_preds)
write.csv(sub_blend, "submission_blend_cb_xgb.csv", row.names = FALSE)

# OOF predictions (useful for further stacking or diagnostics)
oof_out <- data.frame(oof_cb = oof_cb, oof_xgb = oof_xgb, y = y)
write.csv(oof_out, "oof_predictions.csv", row.names = FALSE)

cat("\n=== FILES SAVED ===\n")
cat("submission_catboost_final.csv  — pure CatBoost\n")
cat("submission_blend_cb_xgb.csv   — optimal blend\n")
cat("oof_predictions.csv            — OOF preds for diagnostics\n")

cat("\n=== FINAL SUMMARY ===\n")
cat(sprintf("CatBoost OOF AUC:    %.4f\n", cb_oof_auc))
cat(sprintf("XGBoost  OOF AUC:    %.4f\n", xgb_oof_auc))
cat(sprintf("Best blend OOF AUC:  %.4f  (CB=%.2f, XGB=%.2f)\n",
            best_auc, best_w, 1 - best_w))
cat(sprintf("OOF correlation:     %.4f  (high = models similar, blend won't help)\n", corr))