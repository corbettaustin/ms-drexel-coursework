library(ggplot2)
library(dplyr)
library(scales)
library(maps)

# Drexel color palette
drexel_blue  <- "#07294D"
drexel_gold  <- "#FFC600"
drexel_light <- "#6CACE4"
drexel_gray  <- "#A7A9AC"
drexel_white <- "#FFFFFF"

df <- read.csv("investments_VC.csv",
               fileEncoding = "latin1",
               stringsAsFactors = FALSE,
               strip.white = TRUE)

# Trim column names
names(df) <- trimws(names(df))

# Parse funding total — remove commas, quotes, spaces
df$funding_clean <- suppressWarnings(
  as.numeric(gsub('[", ]', '', df$funding_total_usd))
)

# Parse dates using base R (no lubridate)
df$first_funding_date <- as.Date(df$first_funding_at, format = "%Y-%m-%d")
df$founded_date       <- as.Date(df$founded_at,       format = "%Y-%m-%d")
df$founded_year       <- suppressWarnings(as.numeric(df$founded_year))

# Days from founding to first funding
df$days_to_funding <- as.numeric(df$first_funding_date - df$founded_date)

cat("Rows loaded:", nrow(df), "\n")
cat("Funding values parsed:", sum(!is.na(df$funding_clean)), "\n\n")

#Bar chart
funnel_data <- data.frame(
  Stage = factor(
    c("Seed","Series A","Series B","Series C","Series D","Series E","Series F"),
    levels = c("Series F","Series E","Series D","Series C","Series B","Series A","Seed")
  ),
  Count = c(
    sum(df$seed    > 0, na.rm = TRUE),
    sum(df$round_A > 0, na.rm = TRUE),
    sum(df$round_B > 0, na.rm = TRUE),
    sum(df$round_C > 0, na.rm = TRUE),
    sum(df$round_D > 0, na.rm = TRUE),
    sum(df$round_E > 0, na.rm = TRUE),
    sum(df$round_F > 0, na.rm = TRUE)
  )
)
p1 <- ggplot(funnel_data, aes(x = Stage, y = Count)) +
  geom_col(fill = drexel_blue, width = 0.65) +
  geom_text(aes(label = scales::comma(Count)),
            hjust = -0.15, size = 3.8, color = drexel_blue, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma,
                     expand = expansion(mult = c(0, 0.2))) +
  labs(
    title    = "The VC Funding Funnel: Capital Concentrates at Early Stages",
    subtitle = "Number of companies reaching each funding round across 54,294 startups",
    x = NULL, y = "Number of Companies",
    caption  = "Source: Crunchbase VC Investment Dataset"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", color = drexel_blue, size = 14),
    plot.subtitle      = element_text(color = drexel_gray, size = 10),
    plot.caption       = element_text(color = drexel_gray, size = 8),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text          = element_text(color = drexel_blue),
    axis.title.x       = element_text(color = drexel_blue),
    plot.background    = element_rect(fill = drexel_white, color = NA)
  )

ggsave("chart1_bar_funnel.png", p1, width = 8, height = 5, dpi = 150)
cat("Chart 1 saved.\n")


# ============================================================
# CHART 2 — LINE CHART: Time to First Funding
# ============================================================

time_data <- df %>%
  filter(
    !is.na(days_to_funding),
    days_to_funding >= 0, days_to_funding <= 3650,
    !is.na(founded_year),
    founded_year >= 2005, founded_year <= 2013
  ) %>%
  group_by(founded_year) %>%
  summarise(
    median_months = median(days_to_funding, na.rm = TRUE) / 30.4,
    n = n(), .groups = "drop"
  )

p2 <- ggplot(time_data, aes(x = founded_year, y = median_months)) +
  geom_line(color = drexel_blue, linewidth = 1.4) +
  geom_point(shape = 21, size = 4,
             fill = drexel_gold, color = drexel_blue, stroke = 1.2) +
  geom_text(aes(label = paste0(round(median_months, 0), " mo")),
            vjust = -1.3, size = 3.5, color = drexel_blue, fontface = "bold") +
  scale_x_continuous(breaks = 2005:2013) +
  scale_y_continuous(limits = c(0, 52),
                     labels = function(x) paste0(x, " mo")) +
  labs(
    title    = "Startups Are Getting Funded Faster Every Year",
    subtitle = "Median months from founding to first funding round (2005–2013)",
    x = "Year Founded", y = "Median Months to First Funding",
    caption  = "Source: Crunchbase VC Investment Dataset"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", color = drexel_blue, size = 14),
    plot.subtitle    = element_text(color = drexel_gray, size = 10),
    plot.caption     = element_text(color = drexel_gray, size = 8),
    panel.grid.minor = element_blank(),
    axis.text        = element_text(color = drexel_blue),
    axis.title       = element_text(color = drexel_blue),
    plot.background  = element_rect(fill = drexel_white, color = NA)
  )

ggsave("chart2_line_time_to_funding.png", p2, width = 8, height = 5, dpi = 150)
cat("Chart 2 saved.\n")


# ============================================================
# CHART 3 — HISTOGRAM: Funding Distribution (Log Scale)
# ============================================================

hist_data <- df %>% filter(!is.na(funding_clean), funding_clean > 0)
med_val   <- median(hist_data$funding_clean, na.rm = TRUE)

p3 <- ggplot(hist_data, aes(x = funding_clean)) +
  geom_histogram(fill = drexel_blue, color = drexel_white, bins = 60, alpha = 0.9) +
  geom_vline(xintercept = med_val,
             color = drexel_gold, linewidth = 1.3, linetype = "dashed") +
  annotate("text",
           x = med_val * 2, y = Inf, vjust = 1.8, hjust = 0,
           size = 3.5, color = drexel_gold, fontface = "bold",
           label = paste0("Median: $", round(med_val / 1e6, 1), "M")) +
  scale_x_log10(
    labels = scales::dollar_format(scale = 1e-6, suffix = "M", accuracy = 0.1),
    breaks = c(1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10)
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "VC Funding is Heavily Right-Skewed: Most Companies Raise Under $10M",
    subtitle = paste0("Distribution of total funding — ",
                      scales::comma(nrow(hist_data)),
                      " companies with disclosed funding (log scale)"),
    x = "Total Funding Raised (log scale)", y = "Number of Companies",
    caption  = "Source: Crunchbase VC Investment Dataset"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", color = drexel_blue, size = 13),
    plot.subtitle    = element_text(color = drexel_gray, size = 10),
    plot.caption     = element_text(color = drexel_gray, size = 8),
    panel.grid.minor = element_blank(),
    axis.text        = element_text(color = drexel_blue),
    axis.title       = element_text(color = drexel_blue),
    plot.background  = element_rect(fill = drexel_white, color = NA)
  )

ggsave("chart3_histogram_funding_dist.png", p3, width = 9, height = 5, dpi = 150)
cat("Chart 3 saved.\n")


# ============================================================
# CHART 4 — BOX & WHISKER: Funding by Sector
# ============================================================

top_markets <- c("Software","Biotechnology","Mobile","E-Commerce",
                 "Health Care","Clean Technology","Finance","Education")

box_data <- df %>%
  filter(
    market %in% top_markets,
    !is.na(funding_clean),
    funding_clean > 0,
    funding_clean < 5e8
  )

median_order <- box_data %>%
  group_by(market) %>%
  summarise(med = median(funding_clean, na.rm = TRUE), .groups = "drop") %>%
  arrange(med) %>%
  pull(market)

box_data$market <- factor(box_data$market, levels = median_order)

p4 <- ggplot(box_data, aes(x = market, y = funding_clean)) +
  geom_boxplot(
    fill = drexel_blue, color = drexel_blue, alpha = 0.4,
    outlier.alpha = 0.08, outlier.color = drexel_gray,
    outlier.size = 0.7, width = 0.6
  ) +
  stat_summary(fun = median, geom = "point",
               shape = 21, size = 3.5,
               fill = drexel_gold, color = drexel_blue) +
  coord_flip() +
  scale_y_log10(
    labels = scales::dollar_format(scale = 1e-6, suffix = "M", accuracy = 0.1),
    breaks = c(1e5, 1e6, 1e7, 1e8)
  ) +
  labs(
    title    = "Biotech and Clean Tech Require More Capital; Software Scales on Less",
    subtitle = "Total funding by sector — gold dot = median; outliers > $500M excluded",
    x = NULL, y = "Total Funding (log scale)",
    caption  = "Source: Crunchbase VC Investment Dataset"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", color = drexel_blue, size = 12),
    plot.subtitle      = element_text(color = drexel_gray, size = 9),
    plot.caption       = element_text(color = drexel_gray, size = 8),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text          = element_text(color = drexel_blue),
    axis.title.x       = element_text(color = drexel_blue),
    plot.background    = element_rect(fill = drexel_white, color = NA)
  )

ggsave("chart4_boxplot_funding_by_sector.png", p4, width = 9, height = 5.5, dpi = 150)
cat("Chart 4 saved.\n")


# ============================================================
# CHART 5 — MAP: U.S. Startups by State
# ============================================================

us_counts <- df %>%
  filter(country_code == "USA", nchar(trimws(state_code)) == 2) %>%
  count(state_code, name = "startup_count")

state_lookup <- data.frame(
  state_code = state.abb,
  region     = tolower(state.name),
  stringsAsFactors = FALSE
)

us_counts  <- left_join(us_counts, state_lookup, by = "state_code") %>%
  filter(!is.na(region))
state_map  <- map_data("state")
map_joined <- left_join(state_map, us_counts, by = "region")

p5 <- ggplot(map_joined, aes(x = long, y = lat, group = group,
                             fill = startup_count)) +
  geom_polygon(color = drexel_white, linewidth = 0.3) +
  coord_map("albers", lat0 = 30, lat1 = 45) +
  scale_fill_gradientn(
    colors   = c("#D5E8F0", drexel_light, drexel_blue),
    na.value = drexel_gray,
    labels   = scales::comma,
    name     = "Startups"
  ) +
  labs(
    title    = "California Dominates U.S. VC Activity, But the Ecosystem Is Nationwide",
    subtitle = "Total VC-backed startups by state",
    caption  = "Source: Crunchbase VC Investment Dataset"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", color = drexel_blue,
                                   size = 13, hjust = 0.5),
    plot.subtitle   = element_text(color = drexel_gray, size = 10, hjust = 0.5),
    plot.caption    = element_text(color = drexel_gray, size = 8,  hjust = 0.5),
    legend.position = "bottom",
    legend.title    = element_text(color = drexel_blue, face = "bold"),
    legend.text     = element_text(color = drexel_blue),
    plot.background = element_rect(fill = drexel_white, color = NA)
  )

ggsave("chart5_map_us_states.png", p5, width = 9, height = 6, dpi = 150)
cat("Chart 5 saved.\n")


# ============================================================
# CHART 6 — BONUS LINE CHART: Sector Trends Over Time
# ============================================================

sector_time <- df %>%
  filter(
    !is.na(founded_year),
    founded_year >= 2007, founded_year <= 2013,
    market %in% c("Software","Mobile","E-Commerce",
                  "Finance","Education","Clean Technology")
  ) %>%
  count(founded_year, market, name = "count")

p6 <- ggplot(sector_time, aes(x = founded_year, y = count,
                              color = market, group = market)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c(
    "Software"         = drexel_blue,
    "Mobile"           = drexel_gold,
    "E-Commerce"       = drexel_light,
    "Finance"          = "#2EAE9B",
    "Education"        = "#E8711A",
    "Clean Technology" = drexel_gray
  )) +
  scale_x_continuous(breaks = 2007:2013) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "Software and E-Commerce Surge While Clean Tech Fades",
    subtitle = "New VC-backed companies founded per year by sector (2007–2013)",
    x = "Year Founded", y = "Companies Founded",
    color = "Sector",
    caption  = "Source: Crunchbase VC Investment Dataset"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", color = drexel_blue, size = 13),
    plot.subtitle    = element_text(color = drexel_gray, size = 10),
    plot.caption     = element_text(color = drexel_gray, size = 8),
    panel.grid.minor = element_blank(),
    legend.position  = "right",
    legend.title     = element_text(face = "bold", color = drexel_blue),
    legend.text      = element_text(color = drexel_blue),
    axis.text        = element_text(color = drexel_blue),
    axis.title       = element_text(color = drexel_blue),
    plot.background  = element_rect(fill = drexel_white, color = NA)
  )

ggsave("chart6_line_sector_trends.png", p6, width = 9, height = 5.5, dpi = 150)
cat("Chart 6 saved.\n")

cat("\n=== All charts complete ===\n")
