library(tidyverse)
library(scales)

# ------------------------------------------------------------
# Data
# ------------------------------------------------------------

imo <- tribble(
  ~country,           ~medals, ~population_m,
  "Romania",              156,      19.068,
  "Hungary",              152,       9.690,
  "Ukraine",              152,      37.441,
  "Bulgaria",             146,       6.778,
  "Belarus",              144,       9.086,
  "Poland",               139,      38.712,
  "Slovakia",             116,       5.520,
  "Croatia",              107,       3.889,
  "Czech Republic",       104,      10.792,
  "Serbia",               102,       6.758,
  "Greece",                94,      10.137,
  "Moldova",               85,       3.053,
  "Bosnia",                77,       3.176,
  "Lithuania",             66,       2.867,
  "Estonia",               51,       1.367,
  "N. Macedonia",          50,       1.828,
  "Slovenia",              47,       2.119,
  "Latvia",                41,       1.880,
  "Cyprus",                26,       1.352,
  "Albania",               14,       2.832,
  "Montenegro",             5,       0.639
)

# medals per million inhabitants
imo <- imo %>%
  mutate(
    medals_per_million = medals / population_m
  ) %>%
  arrange(medals_per_million) %>%
  mutate(country = factor(country, levels = country))

# ------------------------------------------------------------
# Plot
# ------------------------------------------------------------

ggplot(imo, aes(x = medals_per_million, y = country)) +
  
  geom_col(
    fill = "#00B35A",
    width = 0.70
  ) +
  
  geom_text(
    aes(
      label = sprintf("%.1f", medals_per_million),
      x = medals_per_million + 0.6
    ),
    hjust = 0,
    family = "serif",
    size = 4.2
  ) +
  
  labs(
    title = "International Math Olympiad medals\nCEE countries, 2000–2026",
    subtitle = "IMO medals per million inhabitants",
    x = NULL,
    y = NULL,
    caption = paste0(
      "Source: International Mathematical Olympiad official results | ",
      "Includes gold, silver and bronze medals\n",
      "Population denominator: 2024 population"
    )
  ) +
  
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.08))
  ) +
  
  theme_minimal(base_family = "serif") +
  
  theme(
    plot.background = element_rect(fill = "#EFFBFC", colour = NA),
    panel.background = element_rect(fill = "#EFFBFC", colour = NA),
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 13, colour = "black", margin = margin(r = 8)),
    axis.text.x = element_text(size = 10, colour = "black"),
    plot.title = element_text(size = 24, face = "bold", colour = "#002B80",
                              lineheight = 0.95, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 15, face = "bold", colour = "#002B80",
                                 margin = margin(b = 20)),
    plot.caption = element_text(size = 9, colour = "#002B80", hjust = 0,
                                margin = margin(t = 15)),
    axis.ticks = element_blank(),
    plot.margin = margin(t = 20, r = 35, b = 20, l = 20)
  )

ggsave(
  "imo_cee_per_capita.png",
  width = 10,
  height = 12,
  units = "in",
  dpi = 300
)