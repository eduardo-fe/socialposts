# ============================================================
# MINIMAL CODE: Labour loss vs Reform gain
# Tertile deprivation only, no population size
# ============================================================

library(tidyverse)
library(ggplot2)
library(ggrepel)


df <- read_csv("manchester_election_socioeconomic.csv")

# Derived columns
df <- df %>%
  mutate(
    labour_loss      = -labour_shift,          # positive = Labour lost votes
    reform_gain      = reform_uk_shift,         # positive = Reform gained
    con_marginal     = con_share_2024 < 15,     # wards where Con was not a factor
    transfer_proxy   = labour_loss + reform_uk_2026,  # crude combined signal
    deprivation_cat  = case_when(
      imd_mean_decile <= 2 ~ "Most deprived (decile 1–2)",
      imd_mean_decile <= 4 ~ "Deprived (decile 3–4)",
      imd_mean_decile <= 6 ~ "Moderate (decile 5–6)",
      TRUE                 ~ "Least deprived (decile 7+)"
    ),
    deprivation_cat = factor(deprivation_cat, levels = c(
      "Most deprived (decile 1–2)", "Deprived (decile 3–4)",
      "Moderate (decile 5–6)", "Least deprived (decile 7+)"))
  )

# Create tertiles
manchester_tertiles <- df %>%
  pull(imd_mean_decile) %>%
  quantile(c(1/3, 2/3), na.rm = TRUE)

df_plot <- df %>%
  mutate(
    labour_loss = -labour_shift,
    reform_gain = reform_uk_shift,
    deprivation_tertile = case_when(
      imd_mean_decile <= manchester_tertiles[1] ~ "Most deprived\n(bottom third)",
      imd_mean_decile >= manchester_tertiles[2] ~ "Least deprived\n(top third)",
      TRUE ~ "Middle third"
    ),
    deprivation_tertile = factor(deprivation_tertile,
                                 levels = c("Most deprived\n(bottom third)",
                                            "Middle third",
                                            "Least deprived\n(top third)"))
  )

# Plot
p_reform <- df_plot %>%
  mutate(
    point_alpha = if_else(deprivation_tertile == "Middle third", 0.7, 0.85)
  ) %>%
  ggplot(aes(x = labour_loss, y = reform_gain, 
             colour = deprivation_tertile, label = ward, alpha = point_alpha)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
           fill = "#e8f4f8", alpha = 0.3) +
  geom_point(show.legend = FALSE) +
  geom_smooth(data = . %>% filter(deprivation_tertile != "Middle third"), 
              method = "lm", se = FALSE,
              colour = "#333333", fill = "#999999", alpha = 0.15, linewidth = 1) +
  geom_text_repel(size = 3, max.overlaps = 32, segment.colour = NA) +
  scale_colour_manual(
    values = c("Most deprived\n(bottom third)" = "red",
               "Middle third" = "#4a9fd8",
               "Least deprived\n(top third)" = "#003d7a"),
    name = "Deprivation\nTertile"
  ) +
  scale_alpha_identity() +
  labs(
    title = "Labour vote loss vs Reform vote gain by ward (2024→2026)",
    subtitle = "Manchester: Dark red (most deprived third) vs Dark blue (least deprived third)\nLight blue zone: middle deprivation tertile | Grey line: regression on extreme wards",
    x = "Labour vote share loss (pp)", 
    y = "Reform vote share gain (pp)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 11, color = "gray40")
  )

ggsave("plot_labour_reform_tertiles.png", p_reform, width = 12, height = 7, dpi = 150)

cat("✓ Saved: plot_labour_reform_tertiles.png\n")

# Optional: Quick regression stats for the post
cat("\n=== Reform gain by deprivation tertile ===\n\n")

for (tertile in levels(df_plot$deprivation_tertile)) {
  subset_data <- df_plot %>% filter(deprivation_tertile == tertile)
  m <- lm(reform_gain ~ labour_loss, data = subset_data)
  coef_val <- coef(m)["labour_loss"]
  cat(sprintf("%s (n=%d): %.3f\n", tertile, nrow(subset_data), coef_val))
}
