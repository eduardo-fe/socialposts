
library(dplyr)
library(broom)
library(ggplot2)
library(stringr)
library(scales)



# Clean / set up
df_reg <- df %>%
  mutate(
    Preferred.Foot = stringr::str_to_title(stringr::str_trim(Preferred.Foot)),
    Preferred.Foot = factor(Preferred.Foot, levels = c("Right", "Left", "Both")), # set Right as base
    PositionGroup = case_when(
      grepl("Goalkeeper", Position, ignore.case = TRUE) ~ "Goalkeeper",
      grepl("Back", Position, ignore.case = TRUE) ~ "Defender",
      grepl("Midfield", Position, ignore.case = TRUE) ~ "Midfielder",
      grepl("Wing|Forward|Striker", Position, ignore.case = TRUE) ~ "Forward",
      TRUE ~ "Other"
    )
  ) %>%
  filter(!is.na(MarketValue_num), !is.na(Preferred.Foot), Preferred.Foot != "") %>%
  filter(
    !is.na(MarketValue_num), 
    !is.na(Preferred.Foot), 
    Preferred.Foot != "",
    Full.Name != "Mohamed Salah"      # <-- exclude Salah
  )

# Function to safely run regression and extract the Preferred.FootLeft row
extract_left_coef <- function(data_subset, group_keys) {
  # require at least some variation in Preferred.Foot and some observations
  tryCatch({
    # only run if there are at least 2 levels of Preferred.Foot and > 10 obs (adjust threshold as desired)
    if(nrow(data_subset) < 6 || nlevels(droplevels(data_subset$Preferred.Foot)) < 2) {
      return(tibble(
        PositionGroup = group_keys$PositionGroup,
        estimate = NA_real_,
        std.error = NA_real_,
        p.value = NA_real_,
        n = nrow(data_subset)
      ))
    }
    fit <- lm(MarketValue_num ~ Preferred.Foot + Age + Height_num, data = data_subset)
    
    td <- tidy(fit)
    row <- td %>% filter(term == "Preferred.FootLeft")
    if(nrow(row) == 0) {
      # no Left term (maybe factor coding different), return NA
      return(tibble(
        PositionGroup = group_keys$PositionGroup,
        estimate = NA_real_,
        std.error = NA_real_,
        p.value = NA_real_,
        n = nrow(data_subset)
      ))
    }
    tibble(
      PositionGroup = group_keys$PositionGroup,
      estimate = row$estimate,
      std.error = row$std.error,
      p.value = row$p.value,
      n = nrow(data_subset)
    )
  }, error = function(e) {
    tibble(
      PositionGroup = group_keys$PositionGroup,
      estimate = NA_real_,
      std.error = NA_real_,
      p.value = NA_real_,
      n = nrow(data_subset)
    )
  })
}


df_ref<-df_reg 
# Apply by group and bind results
foot_premiums <- df_reg %>%
  group_by(PositionGroup) %>%
  group_map(~ extract_left_coef(.x, .y)) %>%
  bind_rows() %>%
  arrange(desc(estimate))

foot_premiums


# Remove groups with NA estimates (insufficient data)
plot_data <- foot_premiums %>% filter(!is.na(estimate))

ggplot(plot_data, aes(x = reorder(PositionGroup, estimate), y = estimate)) +
  geom_col(fill = "steelblue", alpha = 0.9) +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error),
                width = 0.25, color = "black") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Premier League Estimated Left-Foot Premium",
    subtitle = "Effect of being left-footed on Market Value (€), controlling for Age and Height",
    x = "Position Group",
    y = "Estimated Premium (€)"
  ) +
  theme_minimal(base_size = 13)




# Remove groups with NA estimates
plot_data <- foot_premiums %>% filter(!is.na(estimate))

# Define a color palette, highlight forwards
plot_data <- plot_data %>%
  mutate(color_fill = ifelse(PositionGroup == "Forward", "highlight", "normal"))

ggplot(plot_data, aes(x = reorder(PositionGroup, estimate), y = estimate, fill = color_fill)) +
  geom_col(width = 0.7, color = "black", alpha = 0.9) +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error),
                width = 0.25, color = "black") +
  geom_text(aes(label = scales::comma(round(estimate/1e6,1))), 
            hjust = -0.1, size = 4.5, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.2))) +
  scale_fill_manual(values = c("highlight" = "#E63946", "normal" = "#457B9D")) +
  labs(
    title = "Premier League Left-Foot Premium by Position",
    subtitle = "Effect of being left-footed on Market Value (€), controlling for Age and Height",
    x = "Position Group",
    y = "Estimated Premium (€)",
    fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 13, color = "grey30"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold", color = "grey20")
  )

