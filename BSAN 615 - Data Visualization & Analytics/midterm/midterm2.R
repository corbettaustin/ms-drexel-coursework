# --- SETUP ---
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(lubridate)
library(maps)

# Drexel color palette
drexel_blue   <- "#07294D"
drexel_gold   <- "#FFC600"
drexel_light  <- "#6CACE4"
drexel_gray   <- "#A7A9AC"
drexel_white  <- "#FFFFFF"

df <- read.csv("investments_VC.csv",
               fileEncoding = "latin1",
               stringsAsFactors = FALSE,
               strip.white = TRUE)

# Clean column names
names(df) <- trimws(names(df))

# Parse funding total (remove commas, quotes, spaces)
df$funding_clean <- as.numeric(
  gsub('[", ]', '', df$funding_total_usd)
)

# Parse dates
df$first_funding_date <- as.Date(df$first_funding_at, "%Y-%m-%d")
df$founded_date       <- as.Date(df$founded_at, "%Y-%m-%d")
df$founded_year       <- as.numeric(df$founded_year)

# Days from founding to first funding
df$days_to_funding <- as.numeric(df$first_funding_date - df$founded_date)

cat("Data loaded:", nrow(df), "rows\n")
cat("Funding values parsed:", sum(!is.na(df$funding_clean)), "\n\n")


# CHART 1 — BAR CHART

funnel_data <- data.frame(
  Stage   = c("Seed", "Series A", "Series B", "Series C", "Series D", "Series E", "Series F"),
  Count   = c(
    sum(df$seed    > 0, na.rm = TRUE),
    sum(df$round_A > 0, na.rm = TRUE),
    sum(df$round_B > 0, na.rm = TRUE),
    sum(df$round_C > 0, na.rm = TRUE),
    sum(df$round_D > 0, na.rm = TRUE),
    sum(df$round_E > 0, na.rm = TRUE),
    sum(df$round_F > 0, na.rm = TRUE)
  )
)

funnel_data$Stage <- factor(funnel_data$Stage, levels = rev(funnel_data$Stage))

p1 <- ggplot(funnel_data, aes(x = Stage, y = Count)) +
  geom_col(fill = drexel_blue, width = 0.65) +
  geom_text(aes(label = scales::comma(Count)),
            hjust = -0.15, size = 3.8, color = drexel_blue, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma,
                     expand = expansion(mult = c(0, 0.18))) +
  labs(
    title    = "The VC Funding Funnel: Capital Concentrates at Early Stages",
    subtitle = "Number of companies reaching each funding round (54,294 total startups)",
    x        = NULL,
    y        = "Number of Companies",
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

# CHART 2 — LINE CHART

time_data <- df %>%
  filter(
    !is.na(days_to_funding),
    days_to_funding >= 0,
    days_to_funding <= 3650,
    founded_year >= 2005,
    founded_year <= 2013
  ) %>%
  group_by(founded_year) %>%
  summarise(
    median_days   = median(days_to_funding, na.rm = TRUE),
    median_months = median_days / 30.4,
    n             = n(),
    .groups = "drop"
  )

p2 <- ggplot(time_data, aes(x = founded_year, y = median_months)) +
  geom_line(color = drexel_blue, linewidth = 1.4) +
  geom_point(color = drexel_gold, size = 4, stroke = 1.5, shape = 21,
             fill = drexel_gold) +
  geom_text(aes(label = paste0(round(median_months, 0), " mo")),
            vjust = -1.2, size = 3.5, color = drexel_blue, fontface = "bold") +
  scale_x_continuous(breaks = 2005:2013) +
  scale_y_continuous(limits = c(0, 52), labels = function(x) paste0(x, " mo")) +
  labs(
    title    = "Startups Are Getting Funded Faster Every Year",
    subtitle = "Median months from company founding to first funding round (2005-2013)",
    x        = "Year Founded",
    y        = "Median Months to First Funding",
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


# CHART 3 — HISTOGRAM

hist_data <- df %>%
  filter(!is.na(funding_clean), funding_clean > 0)

p3 <- ggplot(hist_data, aes(x = funding_clean)) +
  geom_histogram(fill = drexel_blue, color = drexel_white,
                 bins = 60, alpha = 0.9) +
  geom_vline(xintercept = median(hist_data$funding_clean, na.rm = TRUE),
             color = drexel_gold, linewidth = 1.2, linetype = "dashed") +
  annotate("text",
           x = median(hist_data$funding_clean, na.rm = TRUE) * 1.6,
           y = Inf, vjust = 1.5, size = 3.5, color = drexel_gold, fontface = "bold",
           label = paste0("Median: $",
                          scales::comma(round(median(hist_data$funding_clean, na.rm = TRUE) / 1e6, 1)),
                          "M")) +
  scale_x_log10(
    labels = scales::dollar_format(scale = 1e-6, suffix = "M", accuracy = 0.1),
    breaks = c(1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10)
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "VC Funding is Heavily Right-Skewed: Most Companies Raise Under $10M",
    subtitle = "Distribution of total funding per company (log scale) — 40,907 companies with disclosed funding",
    x        = "Total Funding Raised (log scale)",
    y        = "Number of Companies",
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

# CHART 4 — BOX & WHISKER

top_markets <- c("Software", "Biotechnology", "Mobile", "E-Commerce",
                 "Health Care", "Clean Technology", "Finance", "Education")

box_data <- df %>%
  filter(
    market %in% top_markets,
    !is.na(funding_clean),
    funding_clean > 0,
    funding_clean < 5e8
  ) %>%
  mutate(market = factor(market,
                         levels = top_markets[order(
                           sapply(top_markets, function(m)
                             median(funding_clean[market == m], na.rm = TRUE)),
                           decreasing = FALSE
                         )]))

p4 <- ggplot(box_data, aes(x = market, y = funding_clean)) +
  geom_boxplot(
    fill = drexel_blue, color = drexel_blue,
    alpha = 0.4, outlier.alpha = 0.1,
    outlier.color = drexel_gray, outlier.size = 0.8,
    width = 0.6
  ) +
  stat_summary(fun = median, geom = "point",
               shape = 21, size = 3, fill = drexel_gold, color = drexel_blue) +
  coord_flip() +
  scale_y_log10(
    labels = scales::dollar_format(scale = 1e-6, suffix = "M", accuracy = 0.1),
    breaks = c(1e5, 1e6, 1e7, 1e8)
  ) +
  labs(
    title    = "Biotech and Clean Tech Command Higher Funding; Software and Mobile Scale on Less",
    subtitle = "Median funding by sector (log scale) — gold dot = median; outliers > $500M excluded for clarity",
    x        = NULL,
    y        = "Total Funding (log scale)",
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

# CHART 5 — MAP 

us_data <- df %>%
  filter(country_code == "USA", state_code != "", !is.na(state_code)) %>%
  count(state_code, name = "startup_count")

state_map <- map_data("state")

state_lookup <- data.frame(
  state_code = state.abb,
  region     = tolower(state.name),
  stringsAsFactors = FALSE
)

us_data <- us_data %>%
  left_join(state_lookup, by = "state_code") %>%
  filter(!is.na(region))

map_joined <- state_map %>%
  left_join(us_data, by = "region")

p5 <- ggplot(map_joined, aes(x = long, y = lat, group = group, fill = startup_count)) +
  geom_polygon(color = drexel_white, linewidth = 0.3) +
  coord_map("albers", lat0 = 30, lat1 = 45) +
  scale_fill_gradientn(
    colors  = c("#D5E8F0", drexel_light, drexel_blue),
    na.value = drexel_gray,
    labels  = scales::comma,
    name    = "Startups"
  ) +
  labs(
    title   = "California Dominates U.S. VC Activity, But Ecosystem is Nationwide",
    subtitle = "Total number of VC-backed startups by state",
    caption  = "Source: Crunchbase VC Investment Dataset"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", color = drexel_blue, size = 13, hjust = 0.5),
    plot.subtitle   = element_text(color = drexel_gray, size = 10, hjust = 0.5),
    plot.caption    = element_text(color = drexel_gray, size = 8, hjust = 0.5),
    legend.position = "bottom",
    legend.title    = element_text(color = drexel_blue, face = "bold"),
    legend.text     = element_text(color = drexel_blue),
    plot.background = element_rect(fill = drexel_white, color = NA)
  )

ggsave("chart5_map_us_states.png", p5, width = 9, height = 6, dpi = 150)
cat("Chart 5 saved.\n")

