# Load packages
library(tidyverse)
library(showtext)
library(here)

# Load font
font_add_google("Teko")
showtext_auto()

# Load data
freed_slaves <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/freed_slaves.csv")

# Wrangle
freed_slaves <- 
  freed_slaves %>% 
  janitor::clean_names() %>% 
  mutate(
    label_y = if_else(free == last(free), NA_real_, slave)
  ) %>% 
  fill(label_y)

# Plot
freed_slaves %>% 
  ggplot(aes(year, slave)) +
  geom_area(fill = "black") + 
  geom_text(aes(year, label_y + 2, label = paste0(free, "%")), size = 6, family = "Teko", fontface = "bold") +
  annotate("text", x = 1830, y = 95, label = "FREE \u2014 LIBRE", size = 8, colour = "black", family = "Teko", fontface = "bold") +
  annotate("text", x = 1830, y = 60, label = "SLAVES", size = 10, colour = "#e0d5c8", family = "Teko", fontface = "bold") +
  annotate("text", x = 1830, y = 55, label = "ESCLAVES", size = 10, colour = "#e0d5c8", family = "Teko", fontface = "bold") +
  scale_y_continuous(limits = c(0, 100), expand = c(0,0)) + 
  scale_x_continuous(limits = c(NA, NA), n.breaks = 9, 
                     expand = c(0, 0), position = "top") +
  coord_cartesian(clip = 'off') +
  labs(
    title = "PROPORTION OF FREEMEN AND SLAVES AMONG AMERICAN NEGROES.\n\nPROPORTION DES NÈGRES LIBRES ET DES ESCLAVES EN AMÈRIQUE.",
    subtitle = "\n\nDONE BY ATLANTA UNIVERSITY.\n\n\n\n\n",
    caption = "Dubois Visualization Challenge || Reproduced by Botan Ağın"
  ) + 
  theme_void(base_family = "Teko") + 
  theme(
    axis.text.x.top = element_text(vjust = 1, size = 18, face = "bold"),
    panel.grid.major.x = element_line(color = "#2b543a"),
    legend.position = "none",
    panel.background = element_rect(fill = "#3c7753"),
    plot.title = element_text(hjust = 0.4, size = 18, face = "bold"), 
    plot.subtitle = element_text(hjust = 0.5, size = 12, face = "bold"),
    plot.background = element_rect(fill = "#e0d5c8"),
    plot.margin = margin(0.5, 1, 0.25, 1, unit = "cm")
  )
ggsave(here("plots", "2021-w08-duboischallenge.png"), height = 10.6, width = 8.3, dpi = 320)

