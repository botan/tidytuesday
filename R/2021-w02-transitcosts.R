# Load packages
library(tidyverse)
library(patchwork)
library(showtext)
library(here)

# Load font
font_add_google("Raleway")
showtext_auto()

# Load data
tt <- tidytuesdayR::tt_load(2021, week = 2)
transit_cost <- tt$transit_cost

# Data wrangling
cities_exp <- transit_cost %>% 
  select(city, start_year, end_year, length, real_cost) %>% 
  drop_na() %>% 
  filter(end_year != "X") %>% 
  mutate(start_year = as.integer(start_year),
         end_year = as.integer(end_year),
         real_cost = as.numeric(real_cost),
         duration = end_year - start_year,
         city = str_to_upper(city)) %>% 
  group_by(city) %>% 
  summarise(exp_length = sum(length) / sum(duration),
            exp_cost = sum(real_cost) / sum(length)) %>% 
  ungroup()

# Plot

p1 <- cities_exp %>% 
  mutate(city = fct_reorder(city, exp_cost)) %>%
  ggplot() +
  geom_col(aes(exp_cost, city), fill = "white") + 
  scale_x_continuous(trans = "reverse", breaks = c(0, 1000, 2000), labels=scales::dollar_format()) +
  scale_y_discrete(position = "right") +
  labs(x = "Expected cost (million) per km") +
  theme_minimal() +
  theme(
    axis.text = element_text(family = "Raleway", face = "bold", color = "white"),
    axis.title.x = element_text(family = "Raleway", face = "bold", color = "white"),
    axis.title.y = element_blank(),
    panel.grid = element_line(color = "#85929E"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

p2 <- cities_exp %>% 
  mutate(city = fct_reorder(city, exp_length)) %>%
  ggplot() +
  geom_col(aes(exp_length, city), fill = "white") + 
  scale_x_continuous(breaks = c(0, 15, 30), labels = scales::unit_format(unit = "km")) +
  labs(x = "Expected progress per year") +
  theme_minimal() +
  theme(
    axis.text = element_text(family = "Raleway", face = "bold", color = "white"),
    axis.title.x = element_text(family = "Raleway", face = "bold", color = "white"),
    axis.title.y = element_blank(),
    panel.grid = element_line(color = "#85929E"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

(p1 + p2) +
  plot_annotation(
    title = "TRANSIT PROJECTS AROUND THE WORLD\nINFRASTRUCTURE COSTS AND CONSTRUCTION SPEEDS",
    caption = "\nData from Transit Cost Project || Visualization by Botan Ağın",
    theme = theme(
      plot.title = element_text(hjust = 0.5, family = "Raleway", face = "bold", size = 26, colour = "white"),
      plot.caption = element_text(hjust = 0.5, family = "Raleway", face = "bold", colour = "white"),
      plot.background = element_rect(fill = "#212F3C"),
      plot.margin = margin(3, 3, 1, 3, unit = "cm")
    )
  )
ggsave(here("plots", "2021-w02-transitcosts.png"), width = 12, height = 18, dpi = 600)

