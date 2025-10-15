library(tidyverse)
library(scales)

# --- Load Data ---
df <- read_csv("interest.csv")

# --- Reshape to long format ---
df_long <- df %>%
  pivot_longer(
    cols = -Month,
    names_to = "term",
    values_to = "interest"
  ) %>%
  mutate(Month = as.Date(paste0(Month, "-01")))

# --- 1. Standardize each series (z-score) ---
df_std <- df_long %>%
  group_by(term) %>%
  mutate(std_interest = scale(interest)[, 1]) %>%
  ungroup()

# --- Define event dates ---
chatgpt_date <- as.Date("2022-11-30")
isl_date <- as.Date("2013-01-01")

# --- Common annotation layer ---
add_event_lines <- list(
  geom_vline(xintercept = isl_date, linetype = "dashed", color = "blue", linewidth = 0.8),
  geom_vline(xintercept = chatgpt_date, linetype = "dashed", color = "black", linewidth = 0.8),
  annotate("text", x = isl_date, y = Inf, label = "ISL published (2013)", 
           vjust = 2, hjust = -0.05, angle = 90, size = 4, color = "blue"),
  annotate("text", x = chatgpt_date, y = Inf, label = "ChatGPT released (2022)", 
           vjust = 2, hjust = -0.05, angle = 90, size = 4, color = "black")
)

# --- Plot 1: Standardized time series ---
p1 <- ggplot(df_std, aes(x = Month, y = std_interest, color = term)) +
  geom_line(size = 1) +
  add_event_lines +
  labs(
    title = "Standardized Google Search Interest Over Time",
    subtitle = "Causal inference vs Machine Learning vs AI\n(Standardized to mean=0, sd=1)",
    x = NULL, y = "Standardized Interest (z-score)",
    color = "Search Term"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")

# --- 2. Percentage change since the first month ---
df_pct <- df_long %>%
  group_by(term) %>%
  mutate(pct_change = (interest / first(interest) - 1) * 100) %>%
  ungroup()

# --- Plot 2: Percentage change over time ---
p2 <- ggplot(df_pct, aes(x = Month, y = pct_change, color = term)) +
  geom_line(size = 1) +
  add_event_lines +
  labs(
    title = "% Change in Google Search Interest",
    subtitle = "Relative growth since start (January 2010 = 0%)",
    x = NULL, y = "Percentage Change (%)",
    color = "Search Term"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top") +
  scale_y_continuous(labels = percent_format(scale = 1))

# --- Display plots ---
p1
p2
