# Load packages
library(palmerpenguins)
library(tidyverse)
library(ggtext)
library(showtext)
library(here)

# Load font
font_add_google("Comfortaa")
showtext_auto()

# Generate circular distributed locations
set.seed(0)
dots <- tibble(
  x = c(runif(1e3), runif(1e3), -runif(1e3), -runif(1e3)),
  y = c(runif(1e3), -runif(1e3), -runif(1e3), runif(1e3)) 
) %>% 
  filter(sqrt(x ** 2 + y ** 2) <= 1)

# Drop NAs and unneeded columns, re-level for facets
penguins <- penguins %>% 
  select(island, species, body_mass_g) %>% 
  drop_na() %>% 
  mutate(island = fct_relevel(island, "Dream", "Torgersen", "Biscoe"))

# Join penguins with their dummy locations
penguin_dots <- bind_rows(
  filter(penguins, island == "Biscoe") %>% 
    bind_cols(slice_sample(dots, n = nrow(.))),
  filter(penguins, island == "Dream") %>% 
    bind_cols(slice_sample(dots, n = nrow(.))),
  filter(penguins, island == "Torgersen") %>% 
    bind_cols(slice_sample(dots, n = nrow(.))),
)

# Make plot
ggplot(penguin_dots, aes(x, y, size = body_mass_g, colour = species)) +
  geom_point(aes(0, 0), size = 160, colour = "snow", show.legend = FALSE) +
  geom_point(alpha = 0.6, show.legend = FALSE) +
  scale_color_manual(values = c("darkorange", "darkorchid", "darkcyan")) +
  labs(
    title = "Palmer Penguins<br>\u2014<br>Distrubutions of Species:<br><span style='color: darkorange'>Adelie</span>,<br><span style='color: darkorchid'>Chinstrap</span>,<br>and <span style='color: darkcyan'>Gentoo</span><br>by Islands:<br><span style='color: grey30'>Biscoe</span>,<br><span style='color: grey30'>Dream</span>,<br>and <span style='color: grey30'>Torgersen</span><br>\u2014",
    subtitle = "*Each point represents a penguin<br>and their sizes scaled by body mass.",
    caption = "Data: Gorman, Williams & Fraser (2014)\nVisualization: Botan Ağın"
  ) + 
  theme_void(base_family = "Comfortaa") + 
  theme(
    strip.text = element_text(size = 24, face = "bold", color = "grey30"),
    plot.title = element_markdown(hjust = 1, size = 28, face = "bold",
                                  margin = margin(t = 55, r = 50, b = -380)),
    plot.subtitle = element_markdown(hjust = 1, size = 20,
                                     margin = margin(t = 380, r = 50, b = -380)),
    plot.caption = element_text(hjust = 0.5, size = 12),
    plot.background = element_rect(fill = "#cce6ff"),
    plot.margin = margin(.5, .5, .5, .5, unit = "cm")
  ) +
  facet_wrap(~ island, ncol = 2, as.table = FALSE)
ggsave(here("plots", "2020-w31-palmerpenguins.png"), width = 12, height = 14, dpi = 320)