library(tidyverse)
library(scales)

# ------------------------------------------------------------
# Data
# ------------------------------------------------------------

ipho <- tribble(
  ~country,           ~medals, ~population_m,
  "Romania",              120,      19.068,
  "Ukraine",              119,      37.441,
  "Hungary",              118,       9.690,
  "Belarus",              103,       9.086,
  "Bulgaria",             102,       6.778,
  "Czechia",                92,      10.792,
  "Poland",                 88,      38.712,
  "Slovakia",               83,       5.520,
  "Serbia",                 77,       6.758,
  "Estonia",                66,       1.367,
  "Slovenia",               62,       2.119,
  "Lithuania",              60,       2.867,
  "Moldova",                54,       3.053,
  "Croatia",                53,       3.889,
  "Latvia",                 43,       1.880,
  "Bosnia & Herz.",         23,       3.176,
  "N. Macedonia",           12,       1.828,
  "Greece",                  8,      10.137,
  "Cyprus",                  4,       1.352,
  "Montenegro",              3,       0.639
)

# medals per million inhabitants
ipho <- ipho %>%
  mutate(
    medals_per_million = medals / population_m
  ) %>%
  arrange(medals_per_million) %>%
  mutate(country = factor(country, levels = country))

# ------------------------------------------------------------
# Plot
# ------------------------------------------------------------

ggplot(ipho, aes(x = medals_per_million, y = country)) +
  
  geom_col(
    fill = "#00B35A",
    width = 0.78
  ) +
  
  geom_text(
    aes(
      label = sprintf("%.1f", medals_per_million),
      x = medals_per_million + 0.7
    ),
    hjust = 0,
    family = "serif",
    size = 5
  ) +
  
  labs(
    title = "International Physics Olympiad medals\nCEE countries, 2000–2026",
    subtitle = "IPho medals per million inhabitants",
    x = NULL,
    y = NULL,
    caption = paste0(
      "Source: IPho historical results database | ",
      "Includes gold, silver and bronze medals\n",
      "Population denominator: 2024 population (millions)"
    )
  ) +
  
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.08)),
    labels = comma
  ) +
  
  theme_minimal(base_family = "serif") +
  
  theme(
    plot.background = element_rect(
      fill = "#EFFBFC",
      colour = NA
    ),
    panel.background = element_rect(
      fill = "#EFFBFC",
      colour = NA
    ),
    
    panel.grid = element_blank(),
    
    axis.text.y = element_text(
      size = 15,
      colour = "black"
    ),
    
    axis.text.x = element_text(
      size = 11,
      colour = "black"
    ),
    
    plot.title = element_text(
      size = 27,
      face = "bold",
      colour = "#002B80",
      lineheight = 1.0
    ),
    
    plot.subtitle = element_text(
      size = 17,
      face = "bold",
      colour = "#002B80",
      margin = margin(b = 25)
    ),
    
    plot.caption = element_text(
      size = 10,
      face = "bold",
      colour = "#002B80",
      hjust = 0,
      margin = margin(t = 20)
    ),
    
    axis.ticks = element_blank(),
    
    plot.margin = margin(
      t = 20, r = 30, b = 20, l = 20
    )
  )