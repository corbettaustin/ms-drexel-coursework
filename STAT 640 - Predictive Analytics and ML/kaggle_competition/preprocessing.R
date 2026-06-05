# =============================================================================
# KAGGLE COMPETITION: Product Return Prediction
# Step 1: Preprocessing Pipeline
# Produces: train_clean.csv and test_clean.csv
# =============================================================================

library(dplyr)
library(lubridate)
library(fastDummies)

set.seed(42)

# =============================================================================
# 1. LOAD DATA
# =============================================================================
train_raw <- read.csv("returns_train.csv", stringsAsFactors = FALSE)
test_raw  <- read.csv("returns_test.csv",  stringsAsFactors = FALSE)

# Store target and IDs before processing
train_ids     <- train_raw$transaction_id
test_ids      <- test_raw$transaction_id
train_target  <- train_raw$returned

# Tag train/test so we can split after joint processing
train_raw$is_train <- 1
test_raw$is_train  <- 0
test_raw$returned  <- NA

# Combine for consistent feature engineering
full <- bind_rows(train_raw, test_raw)

# =============================================================================
# 2. TIMESTAMP FEATURES
# Training data: Jan–Sep 2025 | Test data: Sep–Dec 2025
# =============================================================================
full$transaction_timestamp <- ymd_hms(full$transaction_timestamp)

full$hour_of_day    <- hour(full$transaction_timestamp)
full$day_of_week    <- wday(full$transaction_timestamp, label = FALSE)  # 1=Sun, 7=Sat
full$is_weekend     <- as.integer(full$day_of_week %in% c(1, 7))
full$month          <- month(full$transaction_timestamp)
full$day_of_month   <- mday(full$transaction_timestamp)

# Time-of-day buckets (captures impulse vs. deliberate shopping behavior)
full$time_bucket <- case_when(
  full$hour_of_day >= 0  & full$hour_of_day < 6  ~ "night",
  full$hour_of_day >= 6  & full$hour_of_day < 12 ~ "morning",
  full$hour_of_day >= 12 & full$hour_of_day < 18 ~ "afternoon",
  TRUE                                             ~ "evening"
)

# =============================================================================
# 3. PAYMENT METHOD — standardize dirty values
# 3 actual methods: Visa, MasterCard, PayPal (all had case/format variants)
# =============================================================================
full$payment_method_clean <- case_when(
  tolower(full$payment_method) %in% c("visa", "credit card - visa") ~ "Visa",
  tolower(full$payment_method) %in% c("mastercard")                 ~ "MasterCard",
  tolower(full$payment_method) %in% c("paypal")                     ~ "PayPal",
  TRUE ~ "Other"
)

# =============================================================================
# 4. PROMO CODES — parse multi-value field into binary flags
# applied_promo_codes: 40% missing = no promo applied
# =============================================================================
full$promo_none     <- as.integer(full$applied_promo_codes == "" | is.na(full$applied_promo_codes))
full$promo_freeship <- as.integer(grepl("FREESHIP", full$applied_promo_codes))
full$promo_winter50 <- as.integer(grepl("WINTER50", full$applied_promo_codes))
full$promo_save20   <- as.integer(grepl("SAVE20",   full$applied_promo_codes))
full$promo_newuser  <- as.integer(grepl("NEWUSER",  full$applied_promo_codes))
full$promo_count    <- full$promo_freeship + full$promo_winter50 +
  full$promo_save20   + full$promo_newuser

# =============================================================================
# 5. LOYALTY TIER — ordinal encoding + missing flag
# "None" in loyalty_tier co-occurs with missing customer_age and account_age_days
# This is a coherent missingness pattern (likely guest or new accounts)
# =============================================================================
full$loyalty_tier_missing <- as.integer(full$loyalty_tier == "None" | is.na(full$loyalty_tier))

full$loyalty_tier_ord <- case_when(
  full$loyalty_tier == "Bronze"   ~ 1,
  full$loyalty_tier == "Silver"   ~ 2,
  full$loyalty_tier == "Gold"     ~ 3,
  full$loyalty_tier == "Platinum" ~ 4,
  TRUE                             ~ 0   # None / missing → treated as lowest
)

# =============================================================================
# 6. CUSTOMER AGE & ACCOUNT AGE — impute + flag
# 30% missing; co-occurs with loyalty_tier = "None"
# Impute with median of observed values
# =============================================================================
median_age         <- median(full$customer_age[!is.na(full$customer_age)])
median_account_age <- median(full$account_age_days[!is.na(full$account_age_days)])

full$customer_age_missing    <- as.integer(is.na(full$customer_age))
full$account_age_days_missing <- as.integer(is.na(full$account_age_days))

full$customer_age     <- ifelse(is.na(full$customer_age),     median_age,         full$customer_age)
full$account_age_days <- ifelse(is.na(full$account_age_days), median_account_age, full$account_age_days)

# Fix negative ages (data entry errors; observed min = -23)
full$customer_age <- pmax(full$customer_age, 0)

# Age buckets (captures life-stage return behavior)
full$age_bucket <- case_when(
  full$customer_age < 25 ~ "under25",
  full$customer_age < 35 ~ "25to34",
  full$customer_age < 50 ~ "35to49",
  TRUE                    ~ "50plus"
)

# Account tenure buckets
full$account_tenure_bucket <- case_when(
  full$account_age_days < 30   ~ "new",
  full$account_age_days < 180  ~ "recent",
  full$account_age_days < 365  ~ "established",
  TRUE                          ~ "loyal"
)

# =============================================================================
# 7. PRICE & DISCOUNT — derived features
# =============================================================================
full$discount_amount    <- full$price * full$discount_pct
full$price_after_disc   <- full$price * (1 - full$discount_pct)
full$high_discount_flag <- as.integer(full$discount_pct > 0.3)  # aggressive promo threshold

# Price buckets (log scale handles skew from $8 to $1291)
full$log_price <- log1p(full$price)

full$price_bucket <- case_when(
  full$price < 25  ~ "low",
  full$price < 75  ~ "mid",
  full$price < 200 ~ "high",
  TRUE              ~ "premium"
)

# =============================================================================
# 8. GUEST CHECKOUT
# =============================================================================
full$guest_checkout <- as.integer(full$guest_checkout == "True")

# =============================================================================
# 9. DUMMY VARIABLES for nominal categoricals
# =============================================================================
full <- dummy_cols(full,
                   select_columns = c("marketing_channel", "product_category",
                                      "product_subcategory", "payment_method_clean",
                                      "time_bucket", "age_bucket",
                                      "account_tenure_bucket", "price_bucket"),
                   remove_first_dummy = TRUE,   # avoid multicollinearity
                   remove_selected_columns = TRUE
)

# =============================================================================
# 10. DROP COLUMNS NOT USED IN MODELING
# =============================================================================
drop_cols <- c(
  "transaction_id", "customer_id", "transaction_timestamp",
  "zip_code",           # too many levels; would need geo-aggregation
  "applied_promo_codes", # replaced by binary flags
  "payment_method",      # replaced by cleaned version dummies
  "loyalty_tier",        # replaced by ordinal + missing flag
  "is_train", "returned"
)
drop_cols <- intersect(drop_cols, names(full))

features <- full[, !names(full) %in% drop_cols]

# =============================================================================
# 11. SPLIT BACK INTO TRAIN / TEST
# =============================================================================
is_train_idx <- full$is_train == 1

X_train <- features[is_train_idx, ]
X_test  <- features[!is_train_idx, ]
y_train <- train_target

# =============================================================================
# 12. CLASS IMBALANCE CHECK & SUMMARY
# =============================================================================
cat("\n=== CLASS DISTRIBUTION (Train) ===\n")
cat("Not returned (0):", sum(y_train == 0), "\n")
cat("Returned    (1):", sum(y_train == 1), "\n")
cat("Return rate:", round(mean(y_train == 1) * 100, 1), "%\n")

cat("\n=== FEATURE MATRIX DIMENSIONS ===\n")
cat("X_train:", nrow(X_train), "rows x", ncol(X_train), "cols\n")
cat("X_test: ", nrow(X_test),  "rows x", ncol(X_test),  "cols\n")

cat("\n=== FEATURE NAMES ===\n")
print(sort(names(X_train)))

# =============================================================================
# 13. SAVE CLEAN DATA
# =============================================================================
train_clean        <- X_train
train_clean$returned <- y_train
train_clean$transaction_id <- train_ids

test_clean <- X_test
test_clean$transaction_id <- test_ids

write.csv(train_clean, "train_clean.csv", row.names = FALSE)
write.csv(test_clean,  "test_clean.csv",  row.names = FALSE)

cat("\n✓ Saved: train_clean.csv and test_clean.csv\n")

# =============================================================================
# 14. SAVE PREPROCESSING PARAMETERS (for documentation / reproducibility)
# =============================================================================
preprocessing_params <- list(
  median_customer_age     = median_age,
  median_account_age_days = median_account_age,
  high_discount_threshold = 0.30,
  price_buckets           = c(low = 25, mid = 75, high = 200),
  loyalty_ordinal_map     = c(None = 0, Bronze = 1, Silver = 2, Gold = 3, Platinum = 4),
  promo_codes_identified  = c("FREESHIP", "WINTER50", "SAVE20", "NEWUSER"),
  payment_method_mapping  = c("visa/Credit Card - Visa -> Visa",
                              "mastercard -> MasterCard",
                              "paypal -> PayPal")
)

saveRDS(preprocessing_params, "preprocessing_params.rds")
cat("✓ Saved: preprocessing_params.rds\n")