# =============================================================================
# KAGGLE COMPETITION: Product Return Prediction
# Step 2b: XGBoost on Enhanced Features (v2)
# Requires: train_clean_v2.csv, test_clean_v2.csv, tuning_results.rds
# Produces:  submission_xgb_v2.csv
# =============================================================================

library(xgboost)
library(pROC)
library(caret)

set.seed(42)

# =============================================================================
# 1. LOAD
# =============================================================================
train   <- read.csv("train_clean_v2.csv", stringsAsFactors = FALSE)
test    <- read.csv("test_clean_v2.csv",  stringsAsFactors = FALSE)
tuning  <- readRDS("tuning_results.rds")

test_ids <- test$transaction_id
y_num    <- as.numeric(as.character(train$returned))
scale_pos <- sum(y_num == 0) / sum(y_num == 1)

drop_cols <- c("transaction_id", "returned")
X_train   <- as.matrix(train[, !names(train) %in% drop_cols])
X_test    <- as.matrix(test[,  !names(test)  %in% drop_cols])

# Align columns
for (col in setdiff(colnames(X_train), colnames(X_test)))
  X_test <- cbind(X_test, setNames(data.frame(0), col))
X_test <- X_test[, colnames(X_train)]

cat("Features:", ncol(X_train), "\n")

dtrain <- xgb.DMatrix(data = X_train, label = y_num)
dtest  <- xgb.DMatrix(data = X_test)

# =============================================================================
# 2. RE-ADD INTERACTION FEATURES FROM TUNING PIPELINE
# =============================================================================
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

dtrain_v2 <- xgb.DMatrix(data = X_train, label = y_num)
dtest_v2  <- xgb.DMatrix(data = X_test)

cat("Features after interactions:", ncol(X_train), "\n")

# =============================================================================
# 3. CV WITH BEST PARAMS FROM TUNING + v2 FEATURES
# =============================================================================
cat("\nRunning CV on v2 features...\n")

best_params <- tuning$best_params
best_params$scale_pos_weight <- scale_pos  # recalculate just in case

cv_v2 <- xgb.cv(
  params    = best_params,
  data      = dtrain_v2,
  nrounds   = 1000,
  nfold     = 5,
  early_stopping_rounds = 50,
  verbose   = 0,
  stratified = TRUE
)

best_r <- cv_v2$best_iteration
if (is.null(best_r) || length(best_r) == 0 || best_r == 0)
  best_r <- which.max(cv_v2$evaluation_log$test_auc_mean)

best_auc <- max(cv_v2$evaluation_log$test_auc_mean)
cat("v2 XGBoost CV AUC:", round(best_auc, 4), "| nrounds:", best_r, "\n")
cat("Previous best:      0.8215\n")
cat("Improvement:       ", round(best_auc - 0.8215, 4), "\n")

# =============================================================================
# 4. TRAIN FINAL MODEL & FEATURE IMPORTANCE
# =============================================================================
xgb_v2 <- xgb.train(
  params  = best_params,
  data    = dtrain_v2,
  nrounds = best_r,
  verbose = 0
)

cat("\nTop 20 features by Gain (v2 model):\n")
imp_v2 <- xgb.importance(model = xgb_v2)
print(head(imp_v2[, c("Feature", "Gain")], 20))

# =============================================================================
# 5. SUBMISSION
# =============================================================================
v2_probs <- predict(xgb_v2, dtest_v2)

write.csv(data.frame(transaction_id = test_ids, returned = v2_probs),
          "submission_xgb_v2.csv", row.names = FALSE)

cat("\n✓ Saved: submission_xgb_v2.csv\n")
cat("\n=== DECISION GUIDE ===\n")
cat("If v2 CV AUC > 0.8215: submit submission_xgb_v2.csv as primary\n")
cat("If v2 CV AUC <= 0.8215: stick with submission_xgb_tuned.csv\n")
cat("Always keep your best single model as one of your 2 private LB slots.\n")

train_v2 <- read.csv("train_clean_v2.csv", stringsAsFactors = FALSE)
train_raw <- read.csv("returns_train.csv",  stringsAsFactors = FALSE)

formal_rows <- which(train_raw$product_subcategory == "Formal")
ret1 <- mean(train_v2$te_subcategory[formal_rows[train_raw$returned[formal_rows] == 1]])
ret0 <- mean(train_v2$te_subcategory[formal_rows[train_raw$returned[formal_rows] == 0]])
cat("Gap:", abs(ret1 - ret0), "\n")

# Also check hour encoding - the most suspicious one given the sharp cliff
cor_hour <- cor(train_v2$te_hour, train_raw$returned)
cor_sub  <- cor(train_v2$te_subcategory, train_raw$returned)
cat("Correlation te_hour vs returned:", cor_hour, "\n")
cat("Correlation te_subcategory vs returned:", cor_sub, "\n")

# Sanity check: what are the actual unique values of te_hour?
cat("Unique te_hour values (should be ~24, not 35000):\n")
cat(length(unique(train_v2$te_hour)), "unique values\n")
cat("Unique te_subcategory values (should be ~22, not 35000):\n")
cat(length(unique(train_v2$te_subcategory)), "unique values\n")