# Load packages
library(tidyverse)
library(waffle)
library(hrbrthemes)
library(ggtext)
library(showtext)
library(here)

# Load fonts
font_add("Roboto Condensed",
         "RobotoCondensed-Regular.ttf", 
         bold = "RobotoCondensed-Bold.ttf",
         italic = "RobotoCondensed-BoldItalic.ttf")
font_add("Roboto Condensed Light", 
         "RobotoCondensed-Light.ttf")
font_add("FontAwesome5Free-Solid", 
         "fa-solid-900.ttf")
font_add("FontAwesome5Free-Regular", 
         "fa-regular-400.ttf")
font_add("FontAwesome5Brands-Regular",
         "fa-brands-400.ttf")
showtext_auto()

# Load data
tt <- tidytuesdayR::tt_load(2020, week = 46)
mobile <- tt$mobile

# Wrangle
tr_mobile <- mobile %>% 
  filter(entity == "Turkey") %>% 
  mutate(mobile_subs = round(mobile_subs, 0)) %>% 
  filter(year %in% c(1997, 2007, 2017)) %>% 
  select(year, mobile_subs) %>% 
  mutate(no_subs = 100 - mobile_subs) %>% 
  pivot_longer(!year, names_to = "subscription", values_to = "n")


# Plot
tr_mobile %>% 
  ggplot(aes(label = subscription, values = n)) +
  geom_pictogram(aes(colour = subscription), 
                 size = 5,
                 n_rows = 10,
                 make_proportional = TRUE) + 
  labs(
    title = "MOBILE PHONE USAGE IN <span style='color:#00CED1'>TURKEY</span>",
    subtitle = "It started as a <span style='color:#ffb3bf'>**luxury**</span> and became a <span style='color:gray30'>**necessity**</span> in two decades<br><br><br>",
    caption = "\n\n\nData: ourworldindata.org/technology-adoption\nSource: Hannah Ritchie (2017)\nVisualization: Botan Ağın"
  ) +
  scale_color_manual(
    guide = FALSE,
    values = c("black", "#b30000")
  ) +
  scale_label_pictogram(
    guide = FALSE,
    values = c("mobile-alt", "times")
  ) +
  coord_cartesian() +
  theme_ipsum_rc(grid = " ") +
  theme_enhance_waffle() +
  theme(
    strip.text = element_text(hjust = 0.5, face = "bold"),
    plot.title = element_markdown(hjust = 0.5, vjust = 0),
    plot.subtitle = element_markdown(hjust = 0.5, family = "Roboto Condensed"),
    plot.caption = element_text(hjust = 0.5),
    plot.background = element_rect(fill = "gray100"),
    plot.margin = margin(55, 20, 35, 20, unit = "pt")
  ) +
  facet_wrap(~ year, ncol = 3)
ggsave(here("plots", "2020-w46-phones.png"), width = 10, height = 6.5, dpi = 450)