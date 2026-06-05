# BSAN 615 – Week 8 Assignment

library(ggplot2)
library(dplyr)
library(scales)

# brand pallate
drexel_blue  <- "#07294D"
drexel_gold  <- "#FFC600"
accent_red   <- "#C8102E"

# load data
assets      <- read.csv("assets.csv",      stringsAsFactors = FALSE)
fuel        <- read.csv("fuel.csv",        stringsAsFactors = FALSE)
maintenance <- read.csv("maintenance.csv", stringsAsFactors = FALSE)

assets$IN_SERVICE_DATE <- as.Date(assets$IN_SERVICE_DATE)
fuel$TRANS_DATE        <- as.Date(fuel$TRANS_DATE)

# Q1 – Top 10 Makes / Models by Fleet Count

q1_data <- assets %>%
  mutate(MAKE_MODEL = paste(MAKE, MODEL)) %>%
  count(MAKE_MODEL, name = "count") %>%
  arrange(desc(count)) %>%
  slice_head(n = 10) %>%
  mutate(MAKE_MODEL = factor(MAKE_MODEL, levels = rev(MAKE_MODEL)))

q1 <- ggplot(q1_data, aes(x = count, y = MAKE_MODEL)) +
  geom_col(fill = drexel_blue, width = 0.7) +
  geom_text(aes(label = comma(count)), hjust = -0.15,
            size = 3.5, color = drexel_blue, fontface = "bold") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.13))) +
  labs(
    title    = "Ford F-150 & Chevrolet Silverado Dominate the Fleet",
    subtitle = "Top 10 makes/models by number of vehicles in service",
    x        = NULL,
    y        = NULL,
    caption  = "Source: Fleet Assets Dataset (2018)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", color = drexel_blue, size = 14),
    plot.subtitle      = element_text(color = "gray40", size = 10),
    axis.text.y        = element_text(color = drexel_blue, face = "bold"),
    axis.text.x        = element_blank(),
    axis.ticks.x       = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.caption       = element_text(color = "gray60", size = 8)
  )

ggsave("q1_top10_makes_models.png", q1, width = 9, height = 6, dpi = 200)
cat("Q1 saved.\n")

# Q2 – Assets by Age (months in service) & Odometer

reference_date <- as.Date("2018-11-01")

q2_data <- assets %>%
  filter(!is.na(IN_SERVICE_DATE), !is.na(CURRENT_ODOM)) %>%
  mutate(
    months_in_service = as.numeric(difftime(reference_date, IN_SERVICE_DATE,
                                            units = "days")) / 30.44,
    odom_bin = floor(CURRENT_ODOM / 5000) * 5000
  ) %>%
  filter(months_in_service >= 0, CURRENT_ODOM >= 0, CURRENT_ODOM < 300000)

# Panel A – Fleet age histogram
q2a <- ggplot(q2_data, aes(x = months_in_service)) +
  geom_histogram(binwidth = 6, fill = drexel_blue, color = "white", alpha = 0.9) +
  scale_x_continuous(breaks = seq(0, 84, 12)) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Fleet Age Shows Two Distinct Acquisition Cohorts",
    subtitle = "Months in service as of Nov 2018 — primary peak at ~30 months, secondary cluster at 60–72 months",
    x        = "Months in Service",
    y        = "Number of Vehicles",
    caption  = "Source: Fleet Assets Dataset (2018)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", color = drexel_blue, size = 14),
    plot.subtitle    = element_text(color = "gray40", size = 10),
    panel.grid.minor = element_blank(),
    plot.caption     = element_text(color = "gray60", size = 8)
  )

ggsave("q2a_fleet_age.png", q2a, width = 9, height = 5, dpi = 200)

# Panel B – Odometer distribution in 5,000-mile bins
q2b_data <- q2_data %>%
  count(odom_bin) %>%
  filter(odom_bin <= 230000)

q2b <- ggplot(q2b_data, aes(x = odom_bin, y = n)) +
  geom_col(fill = drexel_gold, color = "white") +
  scale_x_continuous(
    labels = function(x) paste0(x / 1000, "k"),
    breaks = seq(0, 230000, 25000),
    limits = c(-2500, 235000),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Odometer Readings Confirm a Right-Skewed, Low-Mileage Fleet",
    subtitle = "Vehicle count by odometer reading (5,000-mile bins); vehicles under 230k miles shown",
    x        = "Odometer (miles)",
    y        = "Number of Vehicles",
    caption  = "Source: Fleet Assets Dataset (2018)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", color = drexel_blue, size = 14),
    plot.subtitle    = element_text(color = "gray40", size = 10),
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 9),
    plot.caption     = element_text(color = "gray60", size = 8)
  )

ggsave("q2b_odometer_bins.png", q2b, width = 10, height = 5, dpi = 200)
cat("Q2 saved.\n")

# Q3 – Typical Annual Fuel Spend

q3_monthly <- fuel %>%
  mutate(month = format(TRANS_DATE, "%Y-%m")) %>%
  group_by(month) %>%
  summarise(monthly_spend = sum(AMOUNT, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    month_label = factor(
      format(as.Date(paste0(month, "-01")), "%b %Y"),
      levels = format(as.Date(paste0(sort(unique(month)), "-01")), "%b %Y")
    )
  )

avg_monthly <- mean(q3_monthly$monthly_spend)
annualised  <- avg_monthly * 12

q3 <- ggplot(q3_monthly, aes(x = month_label, y = monthly_spend, group = 1)) +
  geom_col(fill = drexel_blue, width = 0.55, alpha = 0.9) +
  geom_hline(yintercept = avg_monthly, linetype = "dashed",
             color = accent_red, linewidth = 0.9) +
  annotate("text", x = 3.45, y = avg_monthly + 42000,
           label = paste0("Monthly avg: $", format(round(avg_monthly), big.mark = ",")),
           color = accent_red, fontface = "bold", size = 3.8, hjust = 1) +
  annotate("text", x = 3.45, y = avg_monthly - 42000,
           label = paste0("Annualised est.: ~$", format(round(annualised / 1e6, 1), nsmall = 1), "M"),
           color = drexel_blue, fontface = "bold", size = 4.2, hjust = 1) +
  scale_y_continuous(
    labels = dollar_format(scale = 1e-6, suffix = "M"),
    limits = c(2.1e6, 2.7e6),
    oob    = scales::squish,
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title    = "Fleet Fuel Spend Is Consistent at ~$2.5M Per Month",
    subtitle = "Monthly fuel expenditure Jul–Oct 2018; annualised estimate based on 4-month average",
    x        = NULL,
    y        = NULL,
    caption  = "Source: Fleet Fuel Dataset (2018)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", color = drexel_blue, size = 14),
    plot.subtitle      = element_text(color = "gray40", size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.caption       = element_text(color = "gray60", size = 8)
  )

ggsave("q3_annual_fuel_spend.png", q3, width = 9, height = 5.5, dpi = 200)
cat("Q3 saved.\n")

# Q4 – Top 10 Makes/Models by Highest CPG (Cost Per Gallon)

fuel_clean <- fuel %>%
  filter(UNITS > 0, !is.na(UNITS), !is.na(AMOUNT)) %>%
  mutate(cpg = AMOUNT / UNITS) %>%
  filter(cpg > 1.5, cpg < 10)

q4_data <- fuel_clean %>%
  inner_join(assets %>% select(VEHICLE_ID, MAKE, MODEL), by = "VEHICLE_ID") %>%
  mutate(MAKE_MODEL = paste(MAKE, MODEL)) %>%
  group_by(MAKE_MODEL) %>%
  summarise(median_cpg     = median(cpg, na.rm = TRUE),
            n_transactions = n(), .groups = "drop") %>%
  filter(n_transactions >= 20) %>%
  arrange(desc(median_cpg)) %>%
  slice_head(n = 10) %>%
  mutate(MAKE_MODEL = factor(MAKE_MODEL, levels = rev(MAKE_MODEL)))

q4 <- ggplot(q4_data, aes(x = median_cpg, y = MAKE_MODEL)) +
  geom_col(fill = drexel_blue, width = 0.7, alpha = 0.9) +
  geom_text(aes(label = paste0("$", sprintf("%.2f", median_cpg))),
            hjust = -0.15, size = 3.5, color = drexel_blue, fontface = "bold") +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.14)),
    limits = c(0, 3.9),
    labels = dollar_format(accuracy = 0.01)
  ) +
  labs(
    title    = "Heavy-Duty Trucks Pay the Most Per Gallon",
    subtitle = "Top 10 makes/models by median CPG — $1.50–$10.00/gal range only; minimum 20 transactions",
    x        = "Median Cost Per Gallon (USD)",
    y        = NULL,
    caption  = "Source: Fleet Fuel & Assets Datasets (2018)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", color = drexel_blue, size = 14),
    plot.subtitle      = element_text(color = "gray40", size = 9),
    axis.text.y        = element_text(color = drexel_blue, face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.caption       = element_text(color = "gray60", size = 8)
  )

ggsave("q4_top10_cpg.png", q4, width = 9, height = 6, dpi = 200)
cat("Q4 saved.\n")







