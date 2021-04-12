# Load packages
library(tidyverse)
library(ggtext)
library(showtext)
library(here)

# Load font
font_add_google("Inria Serif")
showtext_auto()

# Load data
tt <- tidytuesdayR::tt_load(2021, week = 3)
artists <- tt$artists %>% janitor::clean_names()
artwork <- tt$artwork %>% janitor::clean_names()

# Data wrangling
artwork_sizes <- artwork %>% 
  drop_na(year, width, height) %>% 
  filter(is.na(depth)) %>% 
  mutate(ratio = width / height,
         area = width * height) %>% 
  group_by(year) %>% 
  summarise(avg_ratio = mean(ratio, trim = .1),
            avg_area = mean(area)) %>% 
  ungroup()

# Plot
artwork_sizes %>% 
  ggplot(aes(year, 0, size = avg_area, color = avg_ratio)) +
  geom_point(alpha = .8) +
  scale_y_continuous(limits = c(-100, NA)) +
  scale_x_continuous(limits = c(NA, 2100)) +
  scale_size(range = c(0, 25)) +
  scale_color_gradient(low = "white", high = "#990000") +
  coord_polar(start = 0) +
  labs(
    title = "A Timeline for 2D Artwork Sizes\nin The Tate Gallery Collection\n",
    subtitle = "The “coffee stain” plot shows the average areas (width x height) and aspect ratios<br>(width / height) by years of all 2D artworks created between **1545** and **2012** which<br>are exhibited by the Tate. Each point represents a year and their sizes scaled by<br>average area value. Their colours are also scaled by an average aspect ratio.<br>Wider aspects are more <span style='color: #e60000'>***red***</span>, longer aspects are more ***white*** for each year shown.",
    caption = "\n\nData from Tate\nVisualization by Botan Ağın"
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, color = "white", family = "Inria Serif", face = "bold", size = 20),
    plot.subtitle = element_markdown(hjust = 0.5, color = "gray90", family = "Inria Serif", size = 12, lineheight = 1.2),
    plot.caption = element_text(hjust = 0.5, color = "gray90", family = "Inria Serif", size = 8),
    plot.background = element_rect(fill = "black"),
    plot.margin = margin(1, 2, 1, 2, unit = "cm")
  )
ggsave(here("plots", "2021-w03-artcollections.png"), width = 7, height = 9, dpi = 500)
knitr::plot_crop(here("plots", "2021-w03-artcollections.png"))