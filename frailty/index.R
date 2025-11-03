library(dplyr)
library(ggplot2)
library(scales)
library(ineq)  # For Gini calc

# Stats with top2_contrib
team_value_stats <- df %>%
  group_by(Current.Club) %>%
  summarise(
    n_players = n(),
    mean_value = mean(MarketValue_num, na.rm = TRUE),
    median_value = median(MarketValue_num, na.rm = TRUE),
    max_value = max(MarketValue_num, na.rm = TRUE),
    total_value = sum(MarketValue_num, na.rm = TRUE),
    gini_value = ifelse(sum(!is.na(MarketValue_num)) > 1, ineq(MarketValue_num, type = "Gini"), NA),
    var_value = var(MarketValue_num, na.rm = TRUE),
    superstar_ratio = max_value / median_value,
    top2_value = sum(sort(MarketValue_num, decreasing = TRUE)[1:2], na.rm = TRUE),  # Top 2 sum
    top2_contrib = top2_value / total_value  # % from top 2
  ) %>%
  arrange(desc(top2_contrib))  # Order by duo dependence

# Horizontal bar plot with gradient fill
ggplot(team_value_stats, aes(x = reorder(Current.Club, top2_contrib), y = top2_contrib)) +
  geom_col(aes(fill = top2_contrib),  # Continuous fill by dependency
           width = 0.7, alpha = 0.8) +
  geom_text(aes(label = scales::percent(top2_contrib, accuracy = 1)), 
            hjust = -0.1, size = 4, fontface = "bold") +
  scale_y_continuous(labels = percent_format(), limits = c(0, max(team_value_stats$top2_contrib) * 1.1)) +
  scale_fill_gradient(low = "#3498DB", high = "#E74C3C",  # Blue to red gradient
                      name = "% from Top 2\nPlayers") +
  labs(
    title = "How Reliant Are Teams on Their Top 2 Superstars?",
    subtitle = "Share of total squad market value from the #1 & #2 players. Gradient shows risk: blue (low) to red (high).",
    x = "Team",
    y = "% of Total Market Value from Top 2 Players",
    caption = "Source: Transfermarkt data | Analysis highlights inequality risks"
  ) +
  coord_flip() +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 10)
  )

# Export
ggsave("top2_superstar_dependence.png", width = 10, height = 6, dpi = 300)