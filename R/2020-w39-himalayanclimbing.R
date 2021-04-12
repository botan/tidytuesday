# Load packages
library(tidyverse)
library(ggtext)
library(showtext)
library(here)

# Load font
font_add_google("Fjalla One")
font_add_google("Raleway")
showtext_auto()

# Load data
tt <- tidytuesdayR::tt_load(2020, week = 39)
peaks <- tt$peaks
members <- tt$members

# Wrangle
peaks_tidy <- members %>% 
  group_by(peak_name) %>% 
  summarise(n = n(),
            success = sum(success),
            fail = n - success) %>% 
  ungroup() %>% 
  pivot_longer(success:fail, "success") %>% 
  mutate(peak_name = str_to_upper(peak_name) %>% fct_reorder(n)) %>% 
  arrange(desc(n)) %>% 
  slice_head(n = 40) 

# Plot
peaks_tidy %>% 
  ggplot(aes(value, peak_name, group = peak_name, color = success)) +
  geom_point(size = 4, stroke = 4, shape = 1) +
  geom_line(color = "gray20", size = 1, linetype = "longdash") +
  labs(
    x = "",
    y = "",
    title = "MOST POPULAR TWENTY <span style='color:lightsteelblue'>PEAKS</span>",
    subtitle = "How many mountain climbers have <span style='color:navy'>**succeeded**</span> or <span style='color:firebrick'>**failed**</span><br>with their attempted on the most popular twenty <span style='color:black'>Himalayan</span> peaks",
    caption = "Data: himalayandatabase.com<br>Visualization: Botan Ağın"
  ) +
  scale_x_log10() + 
  scale_color_manual(values = c("firebrick", "navy")) +
  theme_minimal(base_family = "Raleway") +
  theme(
    axis.text = element_text(size = 9),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_markdown(size = 20, face = "bold"),
    plot.subtitle = element_markdown(size = 14, color = "gray30"),
    plot.caption = element_markdown(size = 8, color = "gray30",
                                margin = unit(c(0.2, -2, 0, 2), units = "cm")),
    plot.background = element_rect(fill = "whitesmoke"),
    plot.margin = margin(1.5, 3, 0.5, 0.5, unit = "cm")
  )
ggsave(here("plots", "2020-w39-himalayanclimbing.png"), height = 10, width = 8, dpi = 320)