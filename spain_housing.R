library(ggplot2)
library(dplyr)

df <- data.frame(
  year = 2000:2025,
  
  dwellings_thousands = c(
    594.8, 561.2, 575.5, 690.2, 739.7, 786.3, 911.6,
    688.9, 299.7, 146.6, 127.5, 109.9, 69.7, 58.7,
    58.8, 76.5, 92.1, 109.0, 128.8, 137.4,
    113.8, 138.8, 135.8, 134.4, 154.4, 166.4
  ),
  
  population_millions = c(
    40.6, 41.8, 42.7, 43.5, 43.8, 44.7, 45.9,
    46.0, 45.9, 46.1, 46.6, 46.7, 46.5, 46.4,
    46.4, 46.4, 46.4, 46.6, 46.8, 47.1,
    47.4, 47.4, 47.8, 48.35, 48.85, 49.1
  )
)

# --- ADD HOUSE PRICES (stylised Spain index, 2000=100) ---
df$house_prices <- c(
  100, 110, 123, 140, 160, 185, 210,
  220, 200, 175, 160, 150, 140, 135,
  133, 135, 138, 142, 148, 155,
  165, 180, 195, 210, 230, 250
)

# --- Scaling setup ---
pop_min <- 35
pop_max <- 50

scale_factor <- max(df$dwellings_thousands) / (pop_max - pop_min)

# Population scaled
df$pop_scaled <- (df$population_millions - pop_min) * scale_factor

# House prices scaled (rebased visually to same axis range)
price_scale <- max(df$dwellings_thousands) / max(df$house_prices)
df$price_scaled <- df$house_prices * price_scale

# --- PLOT ---
ggplot(df, aes(x = year)) +
  
  # Housing construction
  geom_line(aes(y = dwellings_thousands),
            color = "#D32F2F", linewidth = 1.2) +
  
  # Population
  geom_line(aes(y = pop_scaled),
            color = "#2F2F2F", linewidth = 1) +
  
  # House prices
  geom_line(aes(y = price_scaled),
            color = "#1F77B4", linewidth = 1.1) +
  
  scale_y_continuous(
    name = "New dwellings (thousands)",
    
    sec.axis = sec_axis(
      ~ . / scale_factor + pop_min,
      name = "Population (millions)",
      breaks = seq(35, 50, 2.5)
    )
  ) +
  
  labs(
    title = "Spain’s housing market: supply, population and prices diverge",
    subtitle = "Post-2008 supply collapse coincides with renewed demographic pressure and rising prices",
    x = NULL,
    caption = "Source: INE; Ministry of Transport; Banco de España (stylised price index)"
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    plot.background = element_rect(fill = "#E8EEF4", color = NA),
    panel.background = element_rect(fill = "#E8EEF4", color = NA),
    
    panel.grid.major = element_line(color = "white", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    
    axis.text = element_text(color = "#2F2F2F"),
    axis.title = element_text(color = "#2F2F2F"),
    
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    
    legend.position = "none"
  ). 


# --- Base years ---
price_2008 <- df$house_prices[df$year == 2008]
price_2016 <- df$house_prices[df$year == 2016]

# --- Create indexed series ---
df <- df %>%
  mutate(
    price_index_2008 = house_prices / price_2008 * 100,
    price_index_2016 = house_prices / price_2016 * 100
  )

# Latest year
latest_year <- max(df$year)

# Extract values
price_2025 <- df$house_prices[df$year == latest_year]

increase_since_2008 <- (price_2025 / price_2008 - 1) * 100
increase_since_2016 <- (price_2025 / price_2016 - 1) * 100

increase_since_2008
increase_since_2016
