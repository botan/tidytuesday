# Load packages
library(tidyverse)
library(lubridate)
library(ggtext)
library(showtext)
library(here)

# Load fonts
font_add_google("Merriweather")
font_add_google("Old Standard TT")
showtext_auto()

# Load data
tt <- tidytuesdayR::tt_load(2020, week = 52)
bigmac <- tt$`big-mac`

# Wrangle
bigmac_tr <- bigmac %>% 
  filter(name == "Turkey") %>% 
  mutate(
    local_price = if_else(year(date) < 2005, local_price / 1e6, local_price), 
    local_price_lag = lag(local_price),
    inflation_rate = (local_price - local_price_lag) / local_price_lag * 100,
    inflation = if_else(inflation_rate < 0, "negative", "positive"),
    valued = if_else(usd_adjusted < 0, "under", "over")
  ) %>% 
  select(date, local_price, inflation_rate, inflation, usd_adjusted, valued) %>% 
  drop_na() %>% 
  # Add zero value to in between dates manually
  add_row(date = as_date("2016-10-12"), usd_adjusted = 0.001, valued = "over", .after = 11) %>% 
  add_row(date = as_date("2016-10-12"), usd_adjusted = -0.001, valued = "under", .after = 12) %>% 
  # Specify min and max geom text labels
  mutate(label = case_when(usd_adjusted == max(usd_adjusted) ~ usd_adjusted,
                           usd_adjusted == min(usd_adjusted) ~ usd_adjusted,
                           TRUE ~ NA_real_)) 


# Plot
bigmac_tr %>% 
  ggplot(aes(date, usd_adjusted, fill = valued, label = scales::percent(label))) +
  geom_area() +
  geom_text(data = filter(bigmac_tr, label > 0),
            family = "Old Standard TT", fontface = "bold",
            position = position_nudge(y = -0.06)) +
  geom_text(data = filter(bigmac_tr, label < 0),
            family = "Old Standard TT", fontface = "bold",
            position = position_nudge(y = 0.06)) +
  geom_point(aes(y = label)) +
  scale_fill_manual(values = c("#de2a42", "#ffffff")) +
  labs(
    title = "**Dive Into The Turkish Lira (₺) with The <span style='color: #ffc72c'>Big Mac</span> Index <br>**",
    subtitle = "*“The big mac index was invented by The Economist in 1986 as a lighthearted guide to whether currencies<br>are at their “correct” level. It is based on the theory of purchasing-power parity (PPP), the notion that<br>in the long run exchange rates should move towards the rate that would equalise the prices of<br>an identical basket of goods and services (in this case, a burger) in any two countries” — The Economist*<br><br>This graph shows how much the Turkish Lira has been over/under valued against the US Dollar in the last decade.<br>",
    caption = "\n\n\nData: The Economist || Visualization: Botan Ağın"
  ) +
  theme_void() +
  theme(
    axis.text.x = element_text(family = "Old Standard TT", face = "bold",
                               margin = margin(t = -5.25, b = 5.25, unit = "cm")),
    legend.position = "none",
    plot.background = element_rect(fill = "gray60"),
    plot.title = element_markdown(family = "Merriweather", size = 20, 
                                  hjust = 0.5),
    plot.subtitle = element_markdown(family = "Merriweather", size = 12, 
                                     hjust = 0.5, lineheight = 1.2),
    plot.caption = element_text(family = "Merriweather", size = 10, 
                                hjust = 0.5),
    plot.margin = margin(t = 2, r = 2, b = 1, l = 2, unit = "cm")
  )
ggsave(here("plots", "2020-w52-bigmacindex.png"), width = 11, height = 9, dpi = 450)