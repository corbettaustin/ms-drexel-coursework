library(ggplot2)

# Colors
blue <- "#07294D"
gold <- "#FFC600"

# Load and clean data
mpg_data <- read.csv("mpg_data.csv")
mpg_clean <- subset(mpg_data, !is.na(MPG) & !is.na(Type) & !is.na(Model) & MPG >= 5 & MPG <= 50)

# ===== QUESTION 1: Typical MPG per Model =====
# Calculate mean MPG for each model
model_summary <- aggregate(MPG ~ Model, data = mpg_clean, FUN = function(x) c(mean = mean(x), count = length(x)))
model_summary <- data.frame(Model = model_summary$Model, 
                            Mean_MPG = model_summary$MPG[,1], 
                            Count = model_summary$MPG[,2])
model_summary <- model_summary[order(-model_summary$Count), ][1:10, ]

print("=== TOP 10 MODELS ===")
print(model_summary)

# Plot
ggplot(model_summary, aes(x = reorder(Model, Mean_MPG), y = Mean_MPG)) +
  geom_col(fill = blue) +
  geom_text(aes(label = round(Mean_MPG, 1)), hjust = -0.2, size = 3) +
  coord_flip() +
  labs(title = "Typical MPG by Model (Top 10)", x = "", y = "Average MPG") +
  theme_minimal()

ggsave("/mnt/user-data/outputs/Q1_mpg_by_model.png", width = 8, height = 5)

# ===== QUESTION 2: Typical MPG per Vehicle Type =====
type_summary <- aggregate(MPG ~ Type, data = mpg_clean, FUN = function(x) c(mean = mean(x), count = length(x)))
type_summary <- data.frame(Type = type_summary$Type, 
                           Mean_MPG = type_summary$MPG[,1], 
                           Count = type_summary$MPG[,2])

print("\n=== MPG BY TYPE ===")
print(type_summary)

# Plot
ggplot(type_summary, aes(x = reorder(Type, -Mean_MPG), y = Mean_MPG)) +
  geom_col(fill = blue) +
  geom_text(aes(label = round(Mean_MPG, 1)), vjust = -0.5, size = 4) +
  labs(title = "Typical MPG by Vehicle Type", x = "", y = "Average MPG") +
  theme_minimal()

ggsave("/mnt/user-data/outputs/Q2_mpg_by_type.png", width = 7, height = 5)

# ===== QUESTION 3: Data Quality Issues =====
print("\n=== DATA QUALITY ISSUES ===")
print(paste("Total records:", nrow(mpg_data)))
print(paste("Missing values:", sum(is.na(mpg_data$MPG) | is.na(mpg_data$Type) | is.na(mpg_data$Model))))
print(paste("Extreme high (>100 MPG):", sum(mpg_data$MPG > 100, na.rm = TRUE)))
print(paste("Extreme low (<5 MPG):", sum(mpg_data$MPG < 5, na.rm = TRUE)))
print(paste("Clean records:", nrow(mpg_clean)))
print(paste("Retention rate:", round(100 * nrow(mpg_clean) / nrow(mpg_data), 1), "%"))

# Box plot showing clean data distribution
ggplot(mpg_clean, aes(x = Type, y = MPG)) +
  geom_boxplot(fill = blue, alpha = 0.7) +
  labs(title = "MPG Distribution by Type (Clean Data: 5-50 MPG)", 
       x = "", y = "MPG") +
  theme_minimal()

ggsave("/mnt/user-data/outputs/Q3_clean_data_boxplot.png", width = 7, height = 5)

# Histogram comparing raw vs clean data
par(mfrow = c(1, 2))
hist(mpg_data$MPG, breaks = 50, main = "Raw Data (with outliers)", 
     xlab = "MPG", col = "lightblue", xlim = c(0, max(mpg_data$MPG, na.rm = TRUE)))
abline(v = c(5, 50), col = "red", lwd = 2, lty = 2)

hist(mpg_clean$MPG, breaks = 30, main = "Clean Data (5-50 MPG)", 
     xlab = "MPG", col = blue)
dev.copy(png, "/mnt/user-data/outputs/Q3_data_quality_comparison.png", width = 800, height = 400)
dev.off()

print("\n=== ANALYSIS COMPLETE ===")
print("Files saved:")
print("  - Q1_mpg_by_model.png")
print("  - Q2_mpg_by_type.png")
print("  - Q3_clean_data_boxplot.png")
print("  - Q3_data_quality_comparison.png")