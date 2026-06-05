# ============================================================
# BSAN 615 — Final Presentation: All Charts
# Corbett | Drexel LeBow
# ============================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

NAVY  <- "#07294D"
GOLD  <- "#FFC600"
GRAY  <- "#888888"
WHITE <- "#FFFFFF"
GOLD_DARK <- "#9a6e00"

# ── Read pre-processed CSVs ───────────────────────────────────
c1 <- read.csv("c1_formations.csv")
c2 <- read.csv("c2_hist.csv")
c3 <- read.csv("c3_heatmap.csv")
c4 <- read.csv("c4_funnel.csv")
c5 <- read.csv("c5_slope.csv")
c6 <- read.csv("c6_box.csv")


# ============================================================
# CHART 1 — Line: Company formations 2005–2013
# (Exploratory: the anomaly that opens the story)
# ============================================================

p1 <- ggplot(c1, aes(x = founded_year, y = companies)) +
  geom_line(color = NAVY, linewidth = 1.2) +
  geom_point(color = NAVY, size = 3.5) +
  annotate("text", x = 2011.2, y = 5150,
           label = "4,905 in 2011", hjust = 0,
           size = 3.3, color = NAVY) +
  annotate("segment", x = 2011, xend = 2011,
           y = 5030, yend = 4905, color = GRAY, linewidth = 0.4) +
  scale_x_continuous(breaks = 2005:2013) +
  scale_y_continuous(labels = comma, limits = c(0, 5700)) +
  labs(
    title    = "VC-Backed Company Formation Peaked in 2011–2012",
    subtitle = "New companies founded per year, Crunchbase dataset (2005–2013)",
    x = NULL, y = "Companies Founded",
    caption = "Source: Crunchbase via Kaggle"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold", color = NAVY, size = 14),
    plot.subtitle      = element_text(color = GRAY, size = 10),
    plot.caption       = element_text(color = GRAY, size = 8, hjust = 1),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text          = element_text(color = NAVY)
  )

ggsave("chart1_formations.png", p1, width = 8, height = 5, dpi = 200)
message("Chart 1 saved.")


# ============================================================
# CHART 2 — Histogram: Funding distribution (log scale)
# (Exploratory: power-law structure of VC capital)
# ============================================================

med_val  <- median(c2$funding_usd)
mean_val <- mean(c2$funding_usd)

p2 <- ggplot(c2, aes(x = funding_usd)) +
  geom_histogram(bins = 40, fill = NAVY, color = WHITE, linewidth = 0.25) +
  geom_vline(xintercept = med_val,  color = GOLD,    linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = mean_val, color = "tomato", linetype = "dashed", linewidth = 1) +
  annotate("text", x = med_val * 1.8,  y = 2600,
           label = paste0("Median: $", round(med_val/1e6, 1), "M"),
           hjust = 0, size = 3.2, color = "#9a6e00") +
  annotate("text", x = mean_val * 1.5, y = 2350,
           label = paste0("Mean: $", round(mean_val/1e6, 1), "M"),
           hjust = 0, size = 3.2, color = "tomato4") +
  scale_x_log10(
    labels = dollar_format(scale = 1e-6, suffix = "M", accuracy = 0.1),
    breaks = c(1e4, 1e5, 1e6, 1e7, 1e8, 1e9)
  ) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Most VC-Backed Companies Raise Under $5M — A Power-Law Market",
    subtitle = "Total funding per company, log scale | Mean is 8× the median",
    x = "Total Funding Raised (log scale)", y = "Number of Companies",
    caption = "Source: Crunchbase via Kaggle"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold", color = NAVY, size = 14),
    plot.subtitle      = element_text(color = GRAY, size = 10),
    plot.caption       = element_text(color = GRAY, size = 8, hjust = 1),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text          = element_text(color = NAVY)
  )

ggsave("chart2_histogram.png", p2, width = 8, height = 5, dpi = 200)
message("Chart 2 saved.")


# ============================================================
# CHART 3 — Heatmap: Sector × Year indexed formations
# (Signal 1: sector rotation away from science toward screens)
# ============================================================

sector_order <- c("Mobile","E-Commerce","Social Media","Finance",
                  "Education","Software","Games",
                  "Health Care","Clean Technology","Biotechnology")

hm <- c3 %>%
  group_by(market) %>%
  mutate(
    base  = n[founded_year == 2005],
    index = ifelse(base > 0, round(n / base * 100), NA_real_)
  ) %>%
  ungroup() %>%
  mutate(
    market   = factor(market, levels = rev(sector_order)),
    lbl      = ifelse(!is.na(index), as.character(round(index)), ""),
    txt_dark = !is.na(index) & index <= 220
  )

p3 <- ggplot(hm, aes(x = founded_year, y = market, fill = index)) +
  geom_tile(color = WHITE, linewidth = 0.5) +
  geom_text(data = filter(hm,  txt_dark), aes(label = lbl),
            size = 2.9, color = NAVY) +
  geom_text(data = filter(hm, !txt_dark), aes(label = lbl),
            size = 2.9, color = WHITE) +
  scale_fill_gradient2(
    low = "#daeef7", mid = "#5b9ec9", high = NAVY,
    midpoint = 175, na.value = "#eeeeee",
    name = "Index\n(2005=100)"
  ) +
  scale_x_continuous(breaks = 2005:2013) +
  labs(
    title    = "Mobile Tripled While Clean Technology Faded — Capital Voted With Checkbooks",
    subtitle = "New VC-backed companies by sector, indexed to 2005 baseline (2005 = 100)",
    x = NULL, y = NULL,
    caption = "Source: Crunchbase via Kaggle"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", color = NAVY, size = 13),
    plot.subtitle = element_text(color = GRAY, size = 9.5),
    plot.caption  = element_text(color = GRAY, size = 8, hjust = 1),
    panel.grid    = element_blank(),
    axis.text     = element_text(color = NAVY),
    legend.title  = element_text(size = 9, color = NAVY),
    legend.text   = element_text(size = 8)
  )

ggsave("chart3_heatmap.png", p3, width = 9, height = 5.5, dpi = 200)
message("Chart 3 saved.")


# ============================================================
# CHART 4 — Grouped bar + line: Seed vs Series A funnel
# (Signal 2: Series A crunch)
# ============================================================

c4 <- c4 %>%
  mutate(conversion = ifelse(seed_cos > 0, roundA_cos / seed_cos * 100, NA))

c4_long <- c4 %>%
  select(founded_year, seed_cos, roundA_cos) %>%
  pivot_longer(c(seed_cos, roundA_cos), names_to = "stage", values_to = "count") %>%
  mutate(stage = recode(stage,
                        "seed_cos"   = "Seed Recipients",
                        "roundA_cos" = "Series A Recipients"))

sf <- 8  # scale factor for dual axis

p4 <- ggplot() +
  geom_col(data = c4_long,
           aes(x = founded_year, y = count, fill = stage),
           position = "dodge", width = 0.7) +
  geom_line(data = c4,
            aes(x = founded_year, y = conversion * sf, group = 1),
            color = GOLD, linewidth = 1.3) +
  geom_point(data = c4,
             aes(x = founded_year, y = conversion * sf),
             color = GOLD, size = 3) +
  geom_text(data = c4,
            aes(x = founded_year,
                y = conversion * sf + 95,
                label = paste0(round(conversion, 0), "%")),
            size = 2.9, color = GOLD_DARK) +
  scale_fill_manual(values = c("Seed Recipients"     = "#5b9ec9",
                               "Series A Recipients" = NAVY)) +
  scale_y_continuous(
    name     = "Companies",
    labels   = comma,
    sec.axis = sec_axis(~ . / sf,
                        name   = "Seed → Series A Conversion (%)",
                        labels = function(x) paste0(x, "%"))
  ) +
  scale_x_continuous(breaks = 2005:2013) +
  labs(
    title    = "Seed Flooded In — Series A Didn't Follow",
    subtitle = "Companies receiving seed vs. Series A funding by founding year | Gold line = conversion rate",
    x = NULL, fill = NULL,
    caption = "Source: Crunchbase via Kaggle"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", color = NAVY, size = 13),
    plot.subtitle      = element_text(color = GRAY, size = 9.5),
    plot.caption       = element_text(color = GRAY, size = 8, hjust = 1),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position    = "top",
    legend.text        = element_text(color = NAVY, size = 10),
    axis.text          = element_text(color = NAVY),
    axis.title.y.right = element_text(color = GOLD_DARK, size = 10)
  )

ggsave("chart4_funnel.png", p4, width = 9, height = 5.5, dpi = 200)
message("Chart 4 saved.")


# ============================================================
# CHART 5 — Slope chart: Median deal size 2005 vs 2013
# (Signal 3: the deal size collapse)
# ============================================================

slope_sectors <- c("Software","Biotechnology","Mobile","E-Commerce",
                   "Clean Technology","Health Care","Finance","Social Media")

s5 <- c5 %>%
  filter(market %in% slope_sectors) %>%
  mutate(is_bio   = market == "Biotechnology",
         yr_label = as.character(founded_year))

lft <- s5 %>% filter(founded_year == 2005)
rgt <- s5 %>% filter(founded_year == 2013)

# Format right-side labels: show K for < 1M
fmt_m <- function(x) {
  ifelse(x >= 1, paste0("$", round(x, 1), "M"),
         paste0("$", round(x * 1000, 0), "K"))
}

p5 <- ggplot(s5, aes(x = yr_label, y = median_m, group = market)) +
  geom_line(data = filter(s5, !is_bio),
            color = "#bbbbbb", linewidth = 0.85) +
  geom_line(data = filter(s5,  is_bio),
            color = GOLD, linewidth = 2) +
  geom_point(data = filter(s5, !is_bio),
             color = GRAY, size = 2.5) +
  geom_point(data = filter(s5,  is_bio),
             color = GOLD, size = 4.5) +
  geom_text(data = lft,
            aes(label = paste0(market, "  $", round(median_m, 1), "M")),
            hjust = 1.06, size = 3,
            color = ifelse(lft$is_bio, GOLD_DARK, "#777777")) +
  geom_text(data = rgt,
            aes(label = fmt_m(median_m)),
            hjust = -0.1, size = 3,
            color = ifelse(rgt$is_bio, GOLD_DARK, "#777777")) +
  scale_y_continuous(labels = dollar_format(suffix = "M"),
                     limits = c(-1, 33)) +
  scale_x_discrete(expand = expansion(mult = c(0.40, 0.22))) +
  labs(
    title    = "Deal Sizes Collapsed Across Every Sector — Except Biotechnology",
    subtitle = "Median total funding per company: 2005 vs. 2013 | Gold = Biotechnology",
    x = NULL, y = "Median Funding (USD millions)",
    caption = "Source: Crunchbase via Kaggle"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", color = NAVY, size = 13),
    plot.subtitle      = element_text(color = GRAY, size = 9.5),
    plot.caption       = element_text(color = GRAY, size = 8, hjust = 1),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text          = element_text(color = NAVY, size = 12)
  )

ggsave("chart5_slope.png", p5, width = 8.5, height = 6, dpi = 200)
message("Chart 5 saved.")


# ============================================================
# CHART 6 — Box & whisker: Funding spread by sector
# (Signal 3: Biotechnology's distribution stays wide)
# ============================================================

b6 <- c6 %>%
  filter(market %in% slope_sectors) %>%
  mutate(
    is_bio = market == "Biotechnology",
    market = reorder(market, funding_m, FUN = median)
  )

p6 <- ggplot(b6, aes(x = market, y = funding_m,
                     fill = is_bio, color = is_bio)) +
  geom_boxplot(outlier.size = 1, outlier.alpha = 0.3,
               width = 0.6, linewidth = 0.5) +
  scale_fill_manual(values  = c("FALSE" = "#c9dff0", "TRUE" = GOLD),
                    guide = "none") +
  scale_color_manual(values = c("FALSE" = NAVY, "TRUE" = GOLD_DARK),
                     guide = "none") +
  scale_y_continuous(labels = dollar_format(suffix = "M"),
                     limits = c(0, 100)) +
  coord_flip() +
  annotate("text", x = "Biotechnology", y = 68,
           label = "Higher floor —\nbiology isn't negotiable",
           hjust = 0.5, size = 2.9, color = GOLD_DARK, lineheight = 0.95) +
  labs(
    title    = "Biotechnology Holds a Higher Capital Floor Across the Full Distribution",
    subtitle = "Total funding per company by sector, 2005–2013 | Capped at $500M for readability",
    x = NULL, y = "Total Funding (USD millions)",
    caption = "Source: Crunchbase via Kaggle"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", color = NAVY, size = 13),
    plot.subtitle      = element_text(color = GRAY, size = 9.5),
    plot.caption       = element_text(color = GRAY, size = 8, hjust = 1),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text          = element_text(color = NAVY)
  )

ggsave("chart6_boxplot.png", p6, width = 9, height = 5.5, dpi = 200)
message("Chart 6 saved.")

message("\nAll 6 charts complete.")
