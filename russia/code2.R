library(WDI)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# --------------------------------------------------
# 1. DATA (unchanged logic, just streamlined)
# --------------------------------------------------

former_soviet <- c(
  "ARM","AZE","BLR","EST","GEO","KAZ","KGZ","LVA","LTU","MDA",
  "RUS","TJK","TKM","UKR","UZB"
)

indicator <- "NY.GDP.PCAP.KD"

gdp_clean <- WDI(
  country = former_soviet,
  indicator = indicator,
  start = 2000,
  end = as.integer(format(Sys.Date(), "%Y"))
) %>%
  rename(gdp_per_cap = NY.GDP.PCAP.KD) %>%
  arrange(country, year)

# --------------------------------------------------
# 2. DEFINE GEOPOLITICAL GROUPS
# --------------------------------------------------

gdp_clean <- gdp_clean %>%
  mutate(group = case_when(
    country == "Russian Federation" ~ "Russia",
    country %in% c("Estonia","Latvia","Lithuania") ~ "Baltics",
    country %in% c("Armenia","Azerbaijan","Georgia") ~ "Caucasus",
    country %in% c("Kazakhstan","Uzbekistan","Turkmenistan") ~ "Western Stans",
    country %in% c("Kyrgyz Republic","Tajikistan") ~ "Eastern Stans",
    TRUE ~ "Other"
  ))

# --------------------------------------------------
# 3. COUNTRY STYLES (AS YOU DEFINED)
# --------------------------------------------------

country_colors <- c(
  "Russian Federation" = "black",
  "Estonia"="red","Latvia"="red","Lithuania"="red",
  "Armenia"="blue","Azerbaijan"="blue","Georgia"="blue",
  "Kazakhstan"="green","Uzbekistan"="green","Turkmenistan"="green",
  "Kyrgyz Republic"="pink","Tajikistan"="pink",
  "Belarus"="orange","Moldova"="orange","Ukraine"="orange"
)

country_linetypes <- c(
  "Russian Federation"="solid",
  "Estonia"="solid","Latvia"="dashed","Lithuania"="dotted",
  "Armenia"="solid","Azerbaijan"="dashed","Georgia"="dotted",
  "Kazakhstan"="solid","Uzbekistan"="dashed","Turkmenistan"="dotted",
  "Kyrgyz Republic"="solid","Tajikistan"="dashed",
  "Belarus"="solid","Moldova"="dashed","Ukraine"="dotted"
)

# --------------------------------------------------
# 4. ONE FUNCTION TO REBASE + PLOT
# --------------------------------------------------

library(RColorBrewer)

plot_gdp_index <- function(data,
                           ref_year,
                           start_year = NULL,
                           end_year = NULL,
                           level = c("country","area"),
                           title_suffix = "") {
  
  level <- match.arg(level)
  
  df <- data
  
  if (!is.null(start_year)) df <- df %>% filter(year >= start_year)
  if (!is.null(end_year))   df <- df %>% filter(year <= end_year)
  
  # Aggregate if area-level
  if (level == "area") {
    df <- df %>%
      group_by(group, year) %>%
      summarise(gdp = mean(gdp_per_cap, na.rm = TRUE), .groups = "drop")
    id_var <- "group"
  } else {
    df <- df %>% rename(gdp = gdp_per_cap)
    id_var <- "country"
  }
  
  # Rebase to reference year
  df <- df %>%
    group_by(.data[[id_var]]) %>%
    mutate(base = gdp[year == ref_year][1],
           index = gdp / base * 100) %>%
    ungroup()
  
  # Define color and linetype scales
  if (level == "country") {
    col_scale <- scale_color_manual(values = country_colors)
    lty_scale <- scale_linetype_manual(values = country_linetypes)
  } else {
    n_areas <- length(unique(df[[id_var]]))
    col_scale <- scale_color_manual(values = RColorBrewer::brewer.pal(n = max(3,n_areas), name = "Set1"))
    lty_scale <- scale_linetype_manual(values = rep(c("solid","dashed","dotted","dotdash","longdash","twodash"), length.out = n_areas))
  }
  
  ggplot(df, aes(x = year, y = index,
                 color = .data[[id_var]],
                 linetype = .data[[id_var]])) +
    geom_hline(yintercept = 100, linetype = "dashed", color = "grey60") +
    geom_line(linewidth = ifelse(level == "area", 1.2, 1)) +
    col_scale + lty_scale +
    labs(
      title = paste0("GDP per Capita Index (", ref_year, " = 100)"),
      subtitle = title_suffix,
      x = "Year",
      y = "Index"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.background = element_rect(fill = "#EAF2F8", color = NA),
      plot.background  = element_rect(fill = "#EAF2F8", color = NA),
      panel.grid.major = element_line(color = "white", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      legend.position = "right"
    )
}


# --------------------------------------------------
# 5. REPRODUCE ALL FIGURES (CLEANLY)
# --------------------------------------------------

# Countries – full period (2000 = 100)
plot_gdp_index(gdp_clean, ref_year = 2000,
               level = "country",
               title_suffix = "Former Soviet countries")

# Countries – 2000–2013
plot_gdp_index(gdp_clean, ref_year = 2000,
               start_year = 2000, end_year = 2013,
               level = "country",
               title_suffix = "Former Soviet countries, 2000–2013")

# Countries – 2014–2024 (rebased to 2014)
plot_gdp_index(gdp_clean, ref_year = 2014,
               start_year = 2014, end_year = 2024,
               level = "country",
               title_suffix = "Former Soviet countries, 2014–2024")

# Areas – 2000–2013
plot_gdp_index(gdp_clean, ref_year = 2000,
               start_year = 2000, end_year = 2013,
               level = "area",
               title_suffix = "By area, 2000–2013")

# Areas – 2014–2024
plot_gdp_index(gdp_clean, ref_year = 2014,
               start_year = 2014, end_year = 2024,
               level = "area",
               title_suffix = "By area, 2014–2024")






#Counterfactual
library(dplyr)
library(ggplot2)

# --------------------------------------------------
# 1. Prepare annual growth rates
# --------------------------------------------------
years <- sort(unique(gdp_clean$year))
n_years <- max(years) - min(years)

gdp_growth <- gdp_clean %>%
  group_by(country) %>%
  summarise(
    start_gdp = gdp_per_cap[year == min(years)],
    end_gdp   = gdp_per_cap[year == max(years)],
    .groups = "drop"
  ) %>%
  mutate(
    annual_growth = (end_gdp / start_gdp)^(1/n_years) - 1
  )

# --------------------------------------------------
# 2. Average growth excluding Caucasus
# --------------------------------------------------
avg_growth_excl_caucasus <- gdp_growth %>%
  filter(!country %in% c("Armenia","Azerbaijan","Georgia")) %>%
  summarise(avg_growth = mean(annual_growth)) %>%
  pull(avg_growth)

# --------------------------------------------------
# 3. Best 5 countries (excluding Russia) and worst 3
# --------------------------------------------------
best5 <- gdp_growth %>%
  filter(country != "Russian Federation") %>%
  arrange(desc(annual_growth)) %>%
  slice(1:3)

worst3 <- gdp_growth %>%
  filter(country != "Russian Federation") %>%
  arrange(annual_growth) %>%
  slice(1:3)
# --------------------------------------------------
# 4. Construct counterfactual index series WITH CONES
# --------------------------------------------------

# Russia actual
russia_index <- gdp_clean %>%
  filter(country == "Russian Federation") %>%
  arrange(year) %>%
  mutate(index = gdp_per_cap / gdp_per_cap[year == min(years)] * 100,
         series = "Russia")

# Create cone boundaries using best and worst growth rates
best_growth <- max(best5$annual_growth)
worst_growth <- min(worst3$annual_growth)

cone_df <- data.frame(year = years) %>%
  mutate(
    upper = 100 * ((1 + best_growth) ^ (year - min(years))),
    lower = 100 * ((1 + worst_growth) ^ (year - min(years))),
    avg   = 100 * ((1 + avg_growth_excl_caucasus) ^ (year - min(years)))
  )

# --------------------------------------------------
# 5. Plot with cone (ribbon)
# --------------------------------------------------
ggplot() +
  geom_ribbon(data = cone_df, aes(x = year, ymin = lower, ymax = upper),
              fill = "grey80", alpha = 0.5) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "grey60") +
  geom_line(data = cone_df, aes(x = year, y = avg, color = "Average (excl. Caucasus)"),
            linewidth = 1.2, linetype = "dashed") +
  geom_line(data = russia_index, aes(x = year, y = index, color = "Russia"),
            linewidth = 1.2) +
  labs(
    title = "Counterfactual GDP per Capita Index (2000 = 100)",
    subtitle = "Russia vs former USSR peers (shaded cone = best to worst performers)",
    x = "Year",
    y = "Index",
    color = "Series"
  ) +
  scale_color_manual(values = c("Russia" = "black", "Average (excl. Caucasus)" = "purple")) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "#EAF2F8", color = NA),
    plot.background  = element_rect(fill = "#EAF2F8", color = NA),
    panel.grid.major = element_line(color = "white", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

# --------------------------------------------------
# 6. Compute counterfactual GDP for 2024 and % variation vs actual
# --------------------------------------------------

# Russia's actual GDP in 2024
russia_2024_actual <- gdp_clean %>%
  filter(country == "Russian Federation", year == 2024) %>%
  pull(gdp_per_cap)

# Add actual GDP and % difference to counterfactuals
gdp_2024_comparison <- gdp_2024_counterfactual %>%
  mutate(
    actual_gdp = russia_2024_actual,
    pct_diff = (gdp_2024 - actual_gdp) / actual_gdp * 100
  )

# Show results
gdp_2024_comparison






 



#Counterfactual
library(dplyr)
library(ggplot2)

# --------------------------------------------------
# 1. Prepare annual growth rates
# --------------------------------------------------
years <- 2000:2024
n_years <- max(years) - min(years)

gdp_growth <- gdp_clean %>%
  filter(year %in% years) %>%
  group_by(country) %>%
  summarise(
    start_gdp = gdp_per_cap[year == min(years)],
    end_gdp   = gdp_per_cap[year == max(years)],
    .groups = "drop"
  ) %>%
  mutate(
    annual_growth = (end_gdp / start_gdp)^(1/n_years) - 1
  )

# --------------------------------------------------
# 2. Average growth excluding Caucasus
# --------------------------------------------------
avg_growth_excl_caucasus <- gdp_growth %>%
  filter(!country %in% c("Armenia","Azerbaijan","Georgia")) %>%
  summarise(avg_growth = mean(annual_growth, na.rm = TRUE)) %>%
  pull(avg_growth)

# --------------------------------------------------
# 3. Best 5 countries (excluding Russia) and worst 3
# --------------------------------------------------
best5 <- gdp_growth %>%
  filter(country != "Russian Federation") %>%
  arrange(desc(annual_growth)) %>%
  slice(1:5)  # Fixed: now actually getting 5

worst3 <- gdp_growth %>%
  filter(country != "Russian Federation") %>%
  arrange(annual_growth) %>%
  slice(1:3)

# Print to verify
print("Best 5 performers:")
print(best5)
print("Worst 3 performers:")
print(worst3)

# --------------------------------------------------
# 4. Construct cone boundaries
# --------------------------------------------------

# Russia actual index
russia_index <- gdp_clean %>%
  filter(country == "Russian Federation", year %in% years) %>%
  arrange(year) %>%
  mutate(index = gdp_per_cap / gdp_per_cap[year == 2000] * 100)

# Use max of best and min of worst for cone boundaries
best_growth <- max(best5$annual_growth)
worst_growth <- min(worst3$annual_growth)

# Create cone dataframe
cone_df <- data.frame(year = years) %>%
  mutate(
    upper = 100 * ((1 + best_growth) ^ (year - 2000)),
    lower = 100 * ((1 + worst_growth) ^ (year - 2000)),
    avg   = 100 * ((1 + avg_growth_excl_caucasus) ^ (year - 2000))
  )

# --------------------------------------------------
# 5. Plot with cone
# --------------------------------------------------
ggplot() +
  # Shaded cone
  geom_ribbon(data = cone_df, aes(x = year, ymin = lower, ymax = upper),
              fill = "grey70", alpha = 0.4) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "grey60") +
  # Best case line
  geom_line(data = cone_df, aes(x = year, y = upper, color = "Best case"),
            linewidth = 1, linetype = "dashed") +
  # Worst case line
  geom_line(data = cone_df, aes(x = year, y = lower, color = "Worst case"),
            linewidth = 1, linetype = "dotted") +
  # Average line
  geom_line(data = cone_df, aes(x = year, y = avg, color = "Average (excl. Caucasus)"),
            linewidth = 1.2, linetype = "longdash") +
  # Russia actual
  geom_line(data = russia_index, aes(x = year, y = index, color = "Russia"),
            linewidth = 1.4) +
  labs(
    title = "Counterfactual GDP per Capita Index (2000 = 100)",
    subtitle = "Russia vs former USSR peers (shaded cone = worst to best performers)",
    x = "Year",
    y = "Index",
    color = "Series"
  ) +
  scale_color_manual(values = c(
    "Russia" = "black",
    "Average (excl. Caucasus)" = "purple",
    "Best case" = "darkgreen",
    "Worst case" = "firebrick"
  )) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "#EAF2F8", color = NA),
    plot.background  = element_rect(fill = "#EAF2F8", color = NA),
    panel.grid.major = element_line(color = "white", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )
