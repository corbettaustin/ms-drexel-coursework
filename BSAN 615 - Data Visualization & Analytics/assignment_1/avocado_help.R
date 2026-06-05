require("tidyverse")
require("tsibble")

avocado <- read.csv("avocado.csv")

avocado <- avocado %>%
  filter(region == ?) %>%
  filter(type == ?) %>%
  mutate(Date = ?)

# Create a tsibble
avocado_tsibble = tsibble(
  date = yearweek(avocado$Date),
  average_price = avocado$AveragePrice
)

avocado_tsibble %>%
  ggplot(aes(x=date, y = average_price)) +
  geom_line()


avocado_tsibble %>%
  mutate(date = as.Date(date)) %>%
  ggplot(aes(x=date, y = average_price)) +
  geom_line(col = "darkblue", linewidth = 1) +
  theme_minimal() +
  xlab("") +
  ylab("Average Price") +
  scale_x_date(
    date_labels = "%Y" # Formats dates as "Jan 2023", "Feb 2023", etc.
  ) 

avocado_tsibble %>%
  mutate(date = as.Date(date)) %>%
  mutate(month = month(date)) %>% # Aggregate by week using yearweek
  mutate(year = year(date)) %>%
  group_by(year, month) %>%
  summarize(average_price = mean(average_price, na.rm = TRUE)) %>% # Calculate average price per week
  ggplot(aes(x = date, y = average_price)) +
  geom_line(col = "darkblue", linewidth = 1) +
  theme_minimal() +
  xlab("") +
  ylab("Average Price") +
  scale_x_date(
    date_labels = "%b %Y" # Formats dates as "Jan 2023", "Feb 2023", etc.
  )


require("feasts")
avocado_tsibble %>%
  gg_subseries()

avocado_monthly <- avocado_tsibble %>%
  index_by(month = ~ yearmonth(.)) %>%  # Convert to monthly granularity
  summarize(avg_price = mean(average_price, na.rm = TRUE)) # Aggregate

# Plot subseries at a monthly level
avocado_monthly %>%
  gg_subseries()
