library(tidyverse)
library(scales)
library(ggtext)
library(showtext)
library(here)

font_add_google("Playfair Display")
showtext_auto()

vdem_tr <- vdemdata::vdem %>% 
  filter(country_name == "Turkey", year %in% c(2002, max(.$year))) %>% 
  select(year, v2xca_academ) %>% 
  mutate(pct = percent(v2xca_academ))

vdem_tr %>% 
  ggplot(aes(year, v2xca_academ, label = pct)) +
  geom_line(colour = "#e60000", size = 4) +
  geom_point(colour = "#e60000", size = 3) +
  geom_text(data = filter(vdem_tr, year == min(vdem_tr$year)), 
            nudge_y = 0.025, family = "pd", fontface = "bold", size = 8) +
  geom_text(data = filter(vdem_tr, year == max(vdem_tr$year)), 
            nudge_y = -0.025, family = "pd", fontface = "bold", size = 8) +
  labs(
    title = "ACADEMIC FREEDOM IN TURKEY",
    subtitle = "Turkey has been ruling by <span style='color:gray30'>**Justice and Development Party**</span> since 2002.<br>The slope shows the academic freedom between **2002** and **2019** according to V-Dem index.<br><br>",
    caption = "\n\nData by V-Dem\nChart by Botan Ağın"
  ) +
  theme_void(base_family = "Playfair Display") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 22, face = "bold"),
    plot.subtitle = element_markdown(hjust = 0.5, size = 14),
    plot.caption = element_text(hjust = 0.5, size = 10),
    plot.margin = margin(2, 2, 1, 2, "cm")
  )
ggsave(here("plots", "2021-w01-academicfreedom.png"), 
       width = 9, height = 9, dpi = 320)
  