# Install tidyverse
install.packages("tidyverse")
install.packages("tsibble")
install.packages("feats")
install.packages("lubridate")

# Then load it
library(tidyverse)
library(tsibble)
library(feats)
library(lubridate)

# Load required libraries
require("tidyverse")
require("tsibble")
require("feasts")
require("lubridate")

# Load the data
avocado <- read.csv("avocado.csv")

# Data preprocessing
avocado <- avocado %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

# ====================================================================
# QUESTION 1: Analyze seasonality by Type for Philadelphia
# ====================================================================

# Filter for Philadelphia and create separate analyses by type
philly_conventional <- avocado %>%
  filter(region == "Philadelphia") %>%
  filter(type == "conventional") %>%
  filter(!is.na(Date)) %>%  
  filter(!is.na(AveragePrice)) %>% 
  arrange(Date)

philly_organic <- avocado %>%
  filter(region == "Philadelphia") %>%
  filter(type == "organic") %>%
  filter(!is.na(Date)) %>%  
  filter(!is.na(AveragePrice)) %>% 
  arrange(Date)

# Create tsibbles for each type
philly_conv_tsibble <- tsibble(
  date = yearweek(philly_conventional$Date),
  average_price = philly_conventional$AveragePrice,
  index = date
)

philly_org_tsibble <- tsibble(
  date = yearweek(philly_organic$Date),
  average_price = philly_organic$AveragePrice,
  index = date
)

# Aggregate to monthly for clearer seasonality patterns
philly_conv_monthly <- philly_conv_tsibble %>%
  index_by(month = ~ yearmonth(.)) %>%
  summarize(avg_price = mean(average_price, na.rm = TRUE))

philly_org_monthly <- philly_org_tsibble %>%
  index_by(month = ~ yearmonth(.)) %>%
  summarize(avg_price = mean(average_price, na.rm = TRUE))

# ====================================================================
# QUESTION 2: Top 3 regions by Total Bags (conventional) - 2017 prices
# ====================================================================

# Identify top 3 regions by Total Bags for conventional avocados
top_regions <- avocado %>%
  filter(type == "conventional") %>%
  group_by(region) %>%
  summarize(total_bags = sum(Total.Bags, na.rm = TRUE)) %>%
  arrange(desc(total_bags)) %>%
  slice_head(n = 3)

# Filter for 2017 data from top 3 regions
avocado_2017_top3 <- avocado %>%
  filter(year(Date) == 2017) %>%
  filter(type == "conventional") %>%
  filter(region %in% top_regions$region) %>%
  mutate(month = month(Date, label = TRUE)) %>%
  group_by(region, month) %>%
  summarize(avg_price = mean(AveragePrice, na.rm = TRUE), .groups = "drop")

# ====================================================================
# VISUALIZATIONS FOR QUESTION 1
# ====================================================================

# Plot 1: Philadelphia Conventional - Time Series
p1 <- philly_conv_monthly %>%
  mutate(month = as.Date(month)) %>%
  ggplot(aes(x = month, y = avg_price)) +
  geom_line(col = "#07294D", linewidth = 0.75) +
  ggtitle("Philadelphia Conventional Avocado Prices",
          subtitle = "Monthly Average 2015-2018") +
  xlab("") +
  ylab("Average Price ($)") +
  theme_minimal() +
  scale_x_date(date_labels = "%b %Y") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11)
  )

# Plot 2: Philadelphia Conventional - Subseries (Seasonality)
p2 <- philly_conv_monthly %>%
  gg_subseries(y = avg_price) +
  ggtitle("Philadelphia Conventional - Seasonal Pattern",
          subtitle = "Monthly subseries plot") +
  ylab("Average Price ($)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 10)
  )

# Plot 3: Philadelphia Organic - Time Series
p3 <- philly_org_monthly %>%
  mutate(month = as.Date(month)) %>%
  ggplot(aes(x = month, y = avg_price)) +
  geom_line(col = "#FFC600", linewidth = 0.75) +
  ggtitle("Philadelphia Organic Avocado Prices",
          subtitle = "Monthly Average 2015-2018") +
  xlab("") +
  ylab("Average Price ($)") +
  theme_minimal() +
  scale_x_date(date_labels = "%b %Y") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11)
  )

# Plot 4: Philadelphia Organic - Subseries (Seasonality)
p4 <- philly_org_monthly %>%
  gg_subseries(y = avg_price) +
  ggtitle("Philadelphia Organic - Seasonal Pattern",
          subtitle = "Monthly subseries plot") +
  ylab("Average Price ($)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 10)
  )

# Plot 5: Comparison of both types
philly_comparison <- bind_rows(
  philly_conv_monthly %>% mutate(type = "Conventional"),
  philly_org_monthly %>% mutate(type = "Organic")
)

p5 <- philly_comparison %>%
  mutate(month = as.Date(month)) %>%
  ggplot(aes(x = month, y = avg_price, col = type)) +
  geom_line(linewidth = 0.75) +
  scale_color_manual(values = c("Conventional" = "#07294D", "Organic" = "#FFC600")) +
  ggtitle("Philadelphia Avocado Prices by Type",
          subtitle = "Monthly Average 2015-2018") +
  xlab("") +
  ylab("Average Price ($)") +
  theme_minimal() +
  scale_x_date(date_labels = "%b %Y") +
  guides(color = guide_legend(position = "inside")) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11),
    legend.title = element_blank(),
    legend.position.inside = c(0.15, 0.85)
  )

# ====================================================================
# VISUALIZATIONS FOR QUESTION 2
# ====================================================================

# Plot 6: Top 3 Regions - 2017 Monthly Prices
p6 <- avocado_2017_top3 %>%
  ggplot(aes(x = month, y = avg_price, color = region, group = region)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("#07294D", "#FFC600", "#57068C")) +
  ggtitle("Top 3 Regions: Conventional Avocado Prices in 2017",
          subtitle = "Ranked by Total Volume Sold") +
  xlab("") +
  ylab("Average Price ($)") +
  theme_minimal() +
  guides(color = guide_legend(position = "inside")) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_blank(),
    legend.position.inside = c(0.15, 0.85),
    legend.text = element_text(size = 11)
  )

# Save all plots
ggsave("philly_conventional_timeseries.png", p1, width = 10, height = 6)
ggsave("philly_conventional_subseries.png", p2, width = 10, height = 6)
ggsave("philly_organic_timeseries.png", p3, width = 10, height = 6)
ggsave("philly_organic_subseries.png", p4, width = 10, height = 6)
ggsave("philly_comparison.png", p5, width = 10, height = 6)
ggsave("top3_regions_2017.png", p6, width = 10, height = 6)