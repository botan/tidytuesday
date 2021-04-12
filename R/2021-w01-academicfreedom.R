# Load packages
library(tidyverse)
library(scales)
library(ggtext)
library(showtext)
library(here)

# Load fonts
font_add_google("Catamaran")
font_add_google("Nunito")
showtext_auto()

# Prepare data
df <- vdemdata::vdem %>% 
  select(country_name, v2xca_academ, year) %>% 
  filter(year %in% c(2002, max(.$year))) %>% 
  as_tibble()

# Make plot
df %>% 
  ggplot(aes(year, v2xca_academ, group = country_name)) +
  geom_line(data = filter(df, country_name != "Turkey"), alpha = 0.2) +
  geom_line(data = filter(df, country_name == "Turkey"), colour = "#a91101", size = 2) +
  geom_point(data = filter(df, country_name == "Turkey"), colour = "#a91101", size = 1) +
  scale_x_continuous(breaks = c(2002, 2019), position = "top") +
  scale_y_continuous(breaks = c(0, 0.6, 1),
                     sec.axis = sec_axis(~ ., breaks = c(0, 0.1, 1))) +
  labs(
    x = NULL,
    y = NULL,
    title = "Academic Freedom in <span style='color:#a91101'>Turkey</span>", #flag = #e30a17
    subtitle = "Turkey has been ruling by <span style='color:grey40'>**Justice and Development Party**</span> since 2002. The red slope shows how the<br>academic freedom in Turkey changes 2002 to 2019 according to V-Dem academic freedom index. Grey<br>lines represents the other countries in the world.",
    caption = "Data by V-Dem<br>Viz by Botan Ağın"
  ) +
  theme_void(base_family = "Catamaran") + 
  theme(
    axis.ticks = element_line(colour = "grey10", size = 0.4),
    axis.ticks.length = unit(0.2, "cm"), 
    axis.text = element_text(size = 10, family = "Nunito"),
    axis.text.x.top = element_text(face = "bold", margin = unit(c(0, 0, 3, 0), "mm")),
    axis.text.y.right = element_text(margin = unit(c(0, 0, 0, 2), "mm")),
    axis.text.y.left = element_text(margin = unit(c(0, 2, 0, 0), "mm")),
    legend.key = element_rect(fill = "NA", size = 3, colour = "NA"),
    legend.position = "top",
    legend.box = "horizontal",
    legend.text = element_text(size = 10),
    plot.title = element_markdown(size = 20, face = "bold",
                                  margin = unit(c(0, 2.8, 0, -2.8), units = "cm")),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(size = 12, lineheight = 1.2, 
                                     margin = unit(c(0.2, 2.8, 0.7, -2.8), units = "cm")),
    plot.caption = element_markdown(margin = unit(c(0.7, -3.8, 0, 3.8), units = "cm")),
    plot.margin = margin(1, 4, 0.5, 4, unit = "cm")
  )
ggsave(here("plots", "2021-w01-academicfreedom.png"), width = 8, height = 6, dpi = 320)