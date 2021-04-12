# Packages
library(tidyverse)
library(sf)
library(ggnewscale)
library(ggtext)
library(showtext)
library(here)

# Font
font_add_google("Playfair Display", "pd")
showtext_auto()

# Data wrangling
religions <- rKenyaCensus::V4_T2.30 %>% 
  janitor::clean_names() %>% 
  filter(county != "KENYA") %>% 
  pivot_longer(-c(county, total), names_to = "religion") %>% 
  mutate(prop = value / total) %>% 
  group_by(county) %>% 
  filter(prop == max(prop)) %>% 
  ungroup() %>% 
  right_join(rKenyaCensus::KenyaCounties_SHP %>% 
              st_as_sf(), #%>% 
              #st_simplify(dTolerance = 50),
            by = c("county" = "County")) %>% 
  st_as_sf()


# Plot
religions %>% 
  ggplot() +
  geom_sf(data = filter(religions, religion %in% c("islam", NA)), aes(fill = prop), color = "white", size = .2, show.legend = FALSE) +
  scale_fill_gradient(low = "#e6ffe6", high = "#009000", na.value = "black") +
  new_scale_fill() +
  geom_sf(data = filter(religions, religion != "islam"), aes(fill = prop), color = "white", size = .2, show.legend = FALSE) +
  scale_fill_gradient(low = "#fbd0d2", high = "#ee1f25") +
  labs(
    title = "Major Religions by County in Kenya",
    subtitle = "Kenya has different concentrations of religions across the country. This map shows<br>the dominant religion for each county, with red and green representing<br> <span style='color: #ee1f25'>__Christianity__</span> and <span style='color: #009000'>__Islam__</span> respectively. Darker tones represent<br>greater dominance. No data is available for **Nairobi City**,<br>therefore it is represented in black.",
    caption = "Data from {rKenyaCensus} || Visualization by Botan Ağın"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, family = "pd", face = "bold", size = 18),
    plot.subtitle = element_markdown(hjust = 0.5, family = "pd", size = 10, lineheight = 1.2),
    plot.caption = element_text(hjust = 0.5, family = "pd", size = 8),
    plot.background = element_rect(fill = "#f7f4ee"),
    plot.margin = margin(1, 2, .5, 2, unit = "cm")
  )
ggsave(here("plots", "2021-w04-kenyacensus.png"), width = 8, height = 8, dpi = 500)
knitr::plot_crop(here("plots", "2021-w04-kenyacensus.png"))