# ============================================================
# BSAN 615 — FINAL PRESENTATION
# "The Playbook Shift: How 2008–2013 Rewrote Venture Capital"
#
# Column name note: read.csv() with default check.names=TRUE
#   strips spaces from padded headers but keeps underscores:
#   " market "            -> market
#   " funding_total_usd " -> funding_total_usd
#   All other columns    -> unchanged (underscores preserved)
# ============================================================

library(ggplot2)
library(dplyr)
library(scales)

# ── Palette ─────────────────────────────────────────────────
drexel_blue  <- "#07294D"
drexel_gold  <- "#FFC600"
drexel_gray  <- "#A7A9AC"
drexel_light <- "#D9E8F5"
drexel_white <- "#FFFFFF"
col_red      <- "#C0392B"
col_teal     <- "#5B8DB8"
col_green    <- "#2ECC71"
col_purple   <- "#9B59B6"
col_sky      <- "#4A90D9"

# ── Load & clean ─────────────────────────────────────────────
df_raw <- read.csv("investments_VC.csv",
                   stringsAsFactors = FALSE,
                   fileEncoding     = "latin1")

# Strip whitespace from market values
df_raw$market <- trimws(df_raw$market)

# Clean funding column (remove embedded quotes/commas/spaces)
df_raw$funding_clean <- suppressWarnings(
  as.numeric(gsub('[",[:space:]]', '', df_raw$funding_total_usd))
)

# Parse dates
df_raw$founded_date <- as.Date(df_raw$founded_at,       format = "%Y-%m-%d")
df_raw$funded_date  <- as.Date(df_raw$first_funding_at, format = "%Y-%m-%d")
df_raw$year_founded <- as.integer(format(df_raw$founded_date, "%Y"))

# ── Shared theme ─────────────────────────────────────────────
theme_final <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title         = element_text(face = "bold", color = drexel_blue, size = 13),
      plot.subtitle      = element_text(color = drexel_gray, size = 9),
      plot.caption       = element_text(color = drexel_gray, size = 8),
      axis.text          = element_text(color = drexel_blue),
      axis.title         = element_text(color = drexel_blue),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_line(color = "#EEEEEE"),
      panel.grid.major.x = element_blank(),
      plot.background    = element_rect(fill = drexel_white, color = NA),
      legend.text        = element_text(color = drexel_blue, size = 9),
      legend.title       = element_text(color = drexel_blue, size = 9, face = "bold")
    )
}


# ============================================================
# CHART A — LINE: The Great Sector Rotation (indexed 2005–2013)
# ============================================================
sectors_a <- c("Mobile", "Software", "Clean Technology",
               "Biotechnology", "E-Commerce")

cohort_a <- df_raw[
  df_raw$market %in% sectors_a &
    !is.na(df_raw$year_founded) &
    df_raw$year_founded >= 2005 &
    df_raw$year_founded <= 2013, ]

ct_a <- as.data.frame(
  table(cohort_a$year_founded, cohort_a$market),
  stringsAsFactors = FALSE)
colnames(ct_a) <- c("year_founded", "market", "count")
ct_a$year_founded <- as.integer(as.character(ct_a$year_founded))

base_counts <- ct_a[ct_a$year_founded == 2005, c("market", "count")]
colnames(base_counts)[2] <- "base"
ct_a <- merge(ct_a, base_counts, by = "market")
ct_a$index <- ct_a$count / ct_a$base * 100

sector_colors <- c(
  "Mobile"           = drexel_gold,
  "Software"         = drexel_blue,
  "Clean Technology" = col_red,
  "Biotechnology"    = drexel_gray,
  "E-Commerce"       = col_teal
)

pA <- ggplot(ct_a, aes(x = year_founded, y = index,
                       color = market, linewidth = market)) +
  geom_hline(yintercept = 100, linetype = "dashed",
             color = "#CCCCCC", linewidth = 0.6) +
  geom_vline(xintercept = 2008, linetype = "dotted",
             color = "#DDDDDD", linewidth = 0.9) +
  annotate("text", x = 2008.15, y = 530,
           label = "Financial\nCrisis",
           hjust = 0, size = 3, color = drexel_gray) +
  geom_line(aes(group = market)) +
  geom_point(size = 2.5) +
  annotate("text", x = 2011.5, y = 545,
           label = "Mobile: +488%",
           color = drexel_gold, fontface = "bold", size = 3.5) +
  annotate("text", x = 2011.3, y = 62,
           label = "Clean Tech: -42%",
           color = col_red, fontface = "bold", size = 3.5) +
  scale_color_manual(values = sector_colors, name = "Sector") +
  scale_linewidth_manual(
    values = c("Mobile" = 1.5, "Software" = 1.0,
               "Clean Technology" = 1.5, "Biotechnology" = 0.7,
               "E-Commerce" = 0.7),
    guide = "none") +
  scale_x_continuous(breaks = 2005:2013) +
  labs(
    title    = "The Great Sector Rotation: Capital Fled Science, Chased Screens",
    subtitle = "Number of new VC-backed companies founded per sector, indexed to 2005 = 100",
    x        = "Year Founded",
    y        = "Index (2005 = 100)",
    caption  = "Source: Crunchbase VC Investment Dataset"
  ) +
  theme_final()

ggsave("chartA_sector_rotation.png", pA, width = 9, height = 5, dpi = 200)
cat("Chart A saved.\n")


# ============================================================
# CHART B — GROUPED BAR + LINE: The Series A Crunch
# ============================================================
years_b <- 2005:2012

seed_n <- sapply(years_b, function(yr) {
  sub <- df_raw[!is.na(df_raw$year_founded) & df_raw$year_founded == yr, ]
  sum(suppressWarnings(as.numeric(sub$seed)) > 0, na.rm = TRUE)
})

a_n <- sapply(years_b, function(yr) {
  sub <- df_raw[!is.na(df_raw$year_founded) & df_raw$year_founded == yr, ]
  sum(suppressWarnings(as.numeric(sub$round_A)) > 0, na.rm = TRUE)
})

conversion <- round(100 * a_n / seed_n, 1)

bar_df <- data.frame(
  year  = rep(years_b, 2),
  type  = rep(c("Seed Recipients", "Series A Recipients"), each = length(years_b)),
  count = c(seed_n, a_n)
)
bar_df$type <- factor(bar_df$type,
                      levels = c("Seed Recipients", "Series A Recipients"))

line_df_b <- data.frame(year = years_b, conversion = conversion)

pB <- ggplot() +
  geom_col(data = bar_df,
           aes(x = factor(year), y = count, fill = type),
           position = position_dodge(width = 0.7), width = 0.65) +
  geom_line(data = line_df_b,
            aes(x = factor(year), y = conversion * 8, group = 1),
            color = drexel_gold, linewidth = 2.0) +
  geom_point(data = line_df_b,
             aes(x = factor(year), y = conversion * 8),
             color = drexel_gold, shape = 18, size = 4) +
  geom_text(data = line_df_b,
            aes(x = factor(year), y = conversion * 8 + 130,
                label = paste0(conversion, "%")),
            color = drexel_gold, fontface = "bold", size = 3) +
  scale_fill_manual(
    values = c("Seed Recipients"     = drexel_light,
               "Series A Recipients" = drexel_blue),
    name = NULL) +
  scale_y_continuous(
    name     = "Number of Companies",
    labels   = comma,
    sec.axis = sec_axis(~ . / 8,
                        name   = "Seed to Series A Conversion (%)",
                        labels = function(x) paste0(x, "%"))
  ) +
  labs(
    title    = "The Series A Crunch: Seed Flooded the Market, Series A Didn't Follow",
    subtitle = "Companies receiving seed vs. Series A funding by founding year; conversion rate in gold",
    x        = "Year Founded",
    caption  = "Source: Crunchbase VC Investment Dataset"
  ) +
  theme_final() +
  theme(
    axis.title.y.right = element_text(color = drexel_gold),
    axis.text.y.right  = element_text(color = drexel_gold),
    legend.position    = "top"
  )


ggsave("chartB_series_a_crunch.png", pB, width = 9, height = 5, dpi = 200)
cat("Chart B saved.\n")


# ============================================================
# CHART C — SLOPE: Deal Size Collapse (3 snapshots)
# ============================================================
top_s_c <- c("Software", "Biotechnology", "Mobile",
             "E-Commerce", "Health Care", "Clean Technology")
yrs_c   <- c(2005, 2009, 2013)

colors_c <- c(
  "Software"         = drexel_blue,
  "Biotechnology"    = col_sky,
  "Mobile"           = drexel_gold,
  "E-Commerce"       = col_green,
  "Health Care"      = col_purple,
  "Clean Technology" = col_red
)

med_list <- list()
for (s in top_s_c) {
  for (yr in yrs_c) {
    sub <- df_raw[
      !is.na(df_raw$year_founded) &
        df_raw$year_founded == yr &
        df_raw$market == s &
        !is.na(df_raw$funding_clean) &
        df_raw$funding_clean >= 1e4 &
        df_raw$funding_clean <= 5e8,
      "funding_clean"]
    if (length(sub) > 5) {
      med_list[[paste(s, yr)]] <- data.frame(
        sector   = s,
        year     = yr,
        median_m = median(sub, na.rm = TRUE) / 1e6
      )
    }
  }
}
slope_df <- do.call(rbind, med_list)

pC <- ggplot(slope_df, aes(x = year, y = median_m,
                           color = sector, group = sector)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 4) +
  annotate("text", x = 2013.3, y = 2.7,
           label = "Biotech held\nat $2.7M",
           hjust = 0, size = 3.2, color = col_sky, fontface = "bold") +
  scale_color_manual(values = colors_c, name = "Sector") +
  scale_x_continuous(
    breaks = yrs_c,
    labels = c("2005\n(Pre-mobile)", "2009\n(Post-crisis)", "2013\n(Peak seed)")
  ) +
  scale_y_continuous(labels = dollar_format(suffix = "M", accuracy = 0.1)) +
  coord_cartesian(xlim = c(2004, 2015)) +
  labs(
    title    = "Every Sector Got Cheaper to Enter - But Not Equally",
    subtitle = "Median total funding for companies founded in that year, by sector",
    x        = NULL,
    y        = "Median Total Funding (USD)",
    caption  = "Source: Crunchbase VC Investment Dataset"
  ) +
  theme_final() +
  theme(legend.position = "right")

ggsave("chartC_deal_size_collapse.png", pC, width = 9, height = 5, dpi = 200)
cat("Chart C saved.\n")


# ============================================================
# CHART D — STACKED BAR: Cohort Outcomes (2005–2010)
# ============================================================
years_d <- 2005:2010

outcome_list <- lapply(years_d, function(yr) {
  sub <- df_raw[!is.na(df_raw$year_founded) & df_raw$year_founded == yr, ]
  tot <- nrow(sub)
  data.frame(
    year   = yr,
    status = c("Acquired", "Closed", "Still Operating"),
    pct    = c(
      sum(sub$status == "acquired",  na.rm = TRUE) / tot * 100,
      sum(sub$status == "closed",    na.rm = TRUE) / tot * 100,
      sum(sub$status == "operating", na.rm = TRUE) / tot * 100
    )
  )
})
outcome_df <- do.call(rbind, outcome_list)
outcome_df$status <- factor(outcome_df$status,
                            levels = c("Still Operating", "Closed", "Acquired"))

label_df <- outcome_df[outcome_df$status %in% c("Acquired", "Closed"), ]

pD <- ggplot(outcome_df, aes(x = factor(year), y = pct, fill = status)) +
  geom_col(width = 0.65, position = "stack") +
  geom_text(
    data     = label_df,
    aes(label = paste0(round(pct, 1), "%")),
    position = position_stack(vjust = 0.5),
    size = 3.5, fontface = "bold", color = drexel_white
  ) +
  scale_fill_manual(
    values = c("Acquired"        = drexel_gold,
               "Closed"          = col_red,
               "Still Operating" = drexel_light),
    name = NULL
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, 105)) +
  labs(
    title    = "Earlier Cohorts Show Higher Exits - And More Failures",
    subtitle = "Outcome distribution for companies founded 2005-2010 (later cohorts excluded - too early to judge)",
    x        = "Founding Year",
    y        = "Share of Companies",
    caption  = "Source: Crunchbase VC Investment Dataset"
  ) +
  theme_final() +
  theme(
    panel.grid.major.x = element_blank(),
    legend.position    = "top"
  )

ggsave("chartD_cohort_outcomes.png", pD, width = 9, height = 5, dpi = 200)
cat("Chart D saved.\n")


# ============================================================
#CHART E — STACKED BAR: The Startup Explosion (Cliffhanger)
# ============================================================
sectors_e <- c("Software", "Mobile", "E-Commerce",
               "Biotechnology", "Clean Technology", "Health Care")

colors_e <- c(
  "Software"         = drexel_blue,
  "Mobile"           = drexel_gold,
  "E-Commerce"       = col_teal,
  "Biotechnology"    = drexel_light,
  "Clean Technology" = col_red,
  "Health Care"      = col_purple
)

cohort_e <- df_raw[
  df_raw$market %in% sectors_e &
    !is.na(df_raw$year_founded) &
    df_raw$year_founded >= 2004 &
    df_raw$year_founded <= 2013, ]

count_e <- as.data.frame(
  table(cohort_e$year_founded, cohort_e$market),
  stringsAsFactors = FALSE)
colnames(count_e) <- c("year_founded", "market", "count")
count_e$year_founded <- as.integer(as.character(count_e$year_founded))
count_e$market <- factor(count_e$market,
                         levels = c("Biotechnology", "Health Care",
                                    "Clean Technology", "E-Commerce",
                                    "Software", "Mobile"))

pE <- ggplot(count_e, aes(x = year_founded, y = count, fill = market)) +
  geom_col(position = "stack", width = 0.75,
           color = drexel_white, linewidth = 0.2) +
  geom_vline(xintercept = 2013.42, linetype = "dashed",
             color = drexel_gray, linewidth = 0.8) +
  annotate("label", x = 2013.42, y = 1220,
           label = "dataset ends",
           hjust = 0.5, size = 2.8, fontface = "italic",
           color = drexel_gray, fill = drexel_white,
           label.size = 0, label.padding = unit(0.15, "lines")) +
  
  scale_fill_manual(values = colors_e, name = "Sector") +
  scale_x_continuous(breaks = 2004:2013,
                     limits = c(2003.4, 2014.5)) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "The Startup Explosion Peaked in 2011-2012 - Then What?",
    subtitle = "New VC-backed companies founded per year across six major sectors",
    x        = "Year Founded",
    y        = "New Companies Founded",
    caption  = "Source: Crunchbase VC Investment Dataset"
  ) +
  theme_final() +
  theme(legend.position = "right")
ggsave("chartE_startup_explosion.png", pE, width = 10, height = 5, dpi = 200)
cat("Chart E saved.\n")


print(pE)
ggsave("chartE_startup_explosion.png", pE, width = 10, height = 5, dpi = 200)
cat("Chart E saved.\n")

cat("\n=== All 5 final charts complete ===\n")