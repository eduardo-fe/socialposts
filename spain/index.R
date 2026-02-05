library(WDI)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# --------------------------------------------------
# 1. DATA - OECD Countries
# --------------------------------------------------

# OECD member countries (38 members as of 2024)
oecd_countries <- c(
  "AUS", "AUT", "BEL", "CAN", "CHL", "COL", "CRC", "CZE", "DNK", "EST",
  "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "IRL", "ISR", "ITA", "JPN",
  "KOR", "LVA", "LTU", "LUX", "MEX", "NLD", "NZL", "NOR", "POL", "PRT",
  "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "GBR", "USA"
)

indicator <- "NY.GDP.PCAP.KD"

gdp_clean <- WDI(
  country = oecd_countries,
  indicator = indicator,
  start = 2000,
  end = as.integer(format(Sys.Date(), "%Y"))
) %>%
  rename(gdp_per_cap = NY.GDP.PCAP.KD) %>%
  arrange(country, year)

# --------------------------------------------------
# 2. DEFINE GEOPOLITICAL/ECONOMIC GROUPS
# --------------------------------------------------

gdp_clean <- gdp_clean %>%
  mutate(group = case_when(
    country == "Spain" ~ "Spain",
    country %in% c("France", "Germany", "Italy", "Netherlands", "Belgium", "Austria") ~ "Core EU",
    country %in% c("Portugal", "Greece", "Ireland") ~ "EU Periphery",
    country %in% c("Poland", "Czech Republic", "Hungary", "Slovak Republic", "Slovenia", "Estonia", "Latvia", "Lithuania") ~ "Central/Eastern Europe",
    country %in% c("United States", "Canada") ~ "North America",
    country %in% c("United Kingdom", "Denmark", "Sweden", "Finland", "Norway", "Iceland") ~ "Northern Europe",
    country %in% c("Australia", "New Zealand", "Japan", "Korea, Rep.") ~ "Asia-Pacific",
    country %in% c("Chile", "Colombia", "Costa Rica", "Mexico") ~ "Latin America",
    country %in% c("Switzerland", "Luxembourg") ~ "High Income",
    TRUE ~ "Other"
  ))

# --------------------------------------------------
# 3. COUNTRY STYLES (FOCUSING ON SPAIN AND KEY COMPARISONS)
# --------------------------------------------------

# Create color scheme with Spain highlighted
country_colors <- setNames(
  c("red", rep("#1f77b4", 6), rep("#ff7f0e", 3), rep("#2ca02c", 8), 
    rep("#d62728", 2), rep("#9467bd", 6), rep("#8c564b", 4), 
    rep("#e377c2", 4), rep("#7f7f7f", 2), rep("#bcbd22", 2)),
  c("Spain", 
    c("France", "Germany", "Italy", "Netherlands", "Belgium", "Austria"),
    c("Portugal", "Greece", "Ireland"),
    c("Poland", "Czech Republic", "Hungary", "Slovak Republic", "Slovenia", "Estonia", "Latvia", "Lithuania"),
    c("United States", "Canada"),
    c("United Kingdom", "Denmark", "Sweden", "Finland", "Norway", "Iceland"),
    c("Australia", "New Zealand", "Japan", "Korea, Rep."),
    c("Chile", "Colombia", "Costa Rica", "Mexico"),
    c("Switzerland", "Luxembourg"),
    c("Turkey", "Israel"))
)

# Create line types with Spain as solid, others varied
country_linetypes <- setNames(
  c("solid", rep(c("solid", "dashed", "dotted"), length.out = length(oecd_countries) - 1)),
  c("Spain", setdiff(sort(unique(gdp_clean$country)), "Spain"))
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
    # Highlight Spain in red, others in muted colors
    colors_to_use <- ifelse(unique(df[[id_var]]) == "Spain", "red", "grey60")
    names(colors_to_use) <- unique(df[[id_var]])
    
    linetypes_to_use <- ifelse(unique(df[[id_var]]) == "Spain", "solid", "solid")
    names(linetypes_to_use) <- unique(df[[id_var]])
    
    col_scale <- scale_color_manual(values = colors_to_use)
    lty_scale <- scale_linetype_manual(values = linetypes_to_use)
  } else {
    n_areas <- length(unique(df[[id_var]]))
    colors_to_use <- RColorBrewer::brewer.pal(n = max(3, min(n_areas, 11)), name = "Spectral")
    # Make Spain red if it exists
    if("Spain" %in% unique(df[[id_var]])) {
      colors_to_use[which(unique(df[[id_var]]) == "Spain")] <- "red"
    }
    names(colors_to_use) <- unique(df[[id_var]])
    
    col_scale <- scale_color_manual(values = colors_to_use)
    lty_scale <- scale_linetype_manual(values = rep(c("solid","dashed","dotted","dotdash","longdash","twodash"), length.out = n_areas))
  }
  
  ggplot(df, aes(x = year, y = index,
                 color = .data[[id_var]],
                 linetype = .data[[id_var]])) +
    geom_hline(yintercept = 100, linetype = "dashed", color = "grey60") +
    geom_line(linewidth = ifelse(level == "area", 1.2, 
                                 ifelse(df[[id_var]] == "Spain", 2, 0.8))) +
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
# 5. REPRODUCE ALL FIGURES (ADAPTED FOR SPAIN/OECD)
# --------------------------------------------------

# Countries – full period (2000 = 100)
plot_gdp_index(gdp_clean, ref_year = 2000,
               level = "country",
               title_suffix = "OECD countries (Spain highlighted)")

# Countries – 2000–2008 (pre-crisis)
plot_gdp_index(gdp_clean, ref_year = 2000,
               start_year = 2000, end_year = 2008,
               level = "country",
               title_suffix = "OECD countries, 2000–2008 (pre-crisis)")

# Countries – 2008–2024 (post-crisis, rebased to 2008)
plot_gdp_index(gdp_clean, ref_year = 2008,
               start_year = 2008, end_year = 2024,
               level = "country",
               title_suffix = "OECD countries, 2008–2024 (post-crisis)")

# Areas/Groups – 2000–2008
plot_gdp_index(gdp_clean, ref_year = 2000,
               start_year = 2000, end_year = 2008,
               level = "area",
               title_suffix = "By economic region, 2000–2008")

# Areas/Groups – 2008–2024
plot_gdp_index(gdp_clean, ref_year = 2008,
               start_year = 2008, end_year = 2024,
               level = "area",
               title_suffix = "By economic region, 2008–2024")

# --------------------------------------------------
# COUNTERFACTUAL ANALYSIS: SPAIN vs OECD PERFORMANCE
# --------------------------------------------------

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
# 2. Average growth excluding certain volatile countries
# --------------------------------------------------
# Exclude countries that might skew averages (very small or resource-dependent economies)
avg_growth_excl_outliers <- gdp_growth %>%
  filter(!country %in% c("Luxembourg", "Iceland", "Chile", "Turkey", "Colombia", "Costa Rica")) %>%
  filter(country != "Spain") %>%  # Exclude Spain from comparison group
  summarise(avg_growth = mean(annual_growth, na.rm = TRUE)) %>%
  pull(avg_growth)

# --------------------------------------------------
# 3. Best 5 countries (excluding Spain) and worst 5
# --------------------------------------------------
best5 <- gdp_growth %>%
  filter(country != "Spain") %>%
  arrange(desc(annual_growth)) %>%
  slice(1:5)

worst5 <- gdp_growth %>%
  filter(country != "Spain") %>%
  arrange(annual_growth) %>%
  slice(1:5)

# --------------------------------------------------
# 4. Construct counterfactual index series
# --------------------------------------------------
create_index <- function(base = 100, growth_rate, years = years) {
  idx <- base * ((1 + growth_rate) ^ (years - min(years)))
  data.frame(year = years, index = idx)
}

# Spain actual
spain_index <- gdp_clean %>%
  filter(country == "Spain") %>%
  arrange(year) %>%
  mutate(index = gdp_per_cap / gdp_per_cap[year == min(years)] * 100,
         series = "Spain")

# Average growth excluding outliers
avg_index <- create_index(base = 100, growth_rate = avg_growth_excl_outliers, years = years) %>%
  mutate(series = "OECD Average (excl. outliers)")

# Best 5 average
best5_index <- create_index(base = 100, growth_rate = mean(best5$annual_growth), years = years) %>%
  mutate(series = "Best 5 OECD (excl. Spain)")

# Worst 5 average
worst5_index <- create_index(base = 100, growth_rate = mean(worst5$annual_growth), years = years) %>%
  mutate(series = "Worst 5 OECD (excl. Spain)")

# Combine for plotting
counterfactual_df <- bind_rows(
  spain_index %>% dplyr::select(year, index, series),
  avg_index,
  best5_index,
  worst5_index
)

# --------------------------------------------------
# 5. Plot counterfactual analysis
# --------------------------------------------------
ggplot(counterfactual_df, aes(x = year, y = index, color = series, linetype = series)) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "grey60") +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Counterfactual GDP per Capita Index (2000 = 100)",
    subtitle = "Spain vs OECD average, best/worst performing countries",
    x = "Year",
    y = "Index",
    color = "Series",
    linetype = "Series"
  ) +
  scale_color_manual(values = c(
    "Spain" = "red",
    "OECD Average (excl. outliers)" = "blue",
    "Best 5 OECD (excl. Spain)" = "green",
    "Worst 5 OECD (excl. Spain)" = "orange"
  )) +
  scale_linetype_manual(values = c(
    "Spain" = "solid",
    "OECD Average (excl. outliers)" = "solid",
    "Best 5 OECD (excl. Spain)" = "dashed",
    "Worst 5 OECD (excl. Spain)" = "dotted"
  )) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "#EAF2F8", color = NA),
    plot.background  = element_rect(fill = "#EAF2F8", color = NA),
    panel.grid.major = element_line(color = "white", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

# --------------------------------------------------
# 6. Print best and worst performing countries for reference
# --------------------------------------------------
cat("Best 5 OECD performers (excluding Spain):\n")
print(best5 %>% dplyr::select(country, annual_growth))

cat("\nWorst 5 OECD performers (excluding Spain):\n")
print(worst5 %>% dplyr::select(country, annual_growth))

# --------------------------------------------------
# 7. Compute counterfactual GDP for 2024 and % variation vs actual
# --------------------------------------------------

# Spain's actual GDP per capita in 2000
spain_2000 <- gdp_clean %>%
  filter(country == "Spain", year == 2000) %>%
  pull(gdp_per_cap)

# Compute counterfactual GDP per capita in 2024
years_to_2024 <- 2024 - 2000

gdp_2024_counterfactual <- tibble(
  scenario = c("OECD Average (excl. outliers)", "Best 5 OECD (excl. Spain)", "Worst 5 OECD (excl. Spain)"),
  annual_growth = c(avg_growth_excl_outliers, mean(best5$annual_growth), mean(worst5$annual_growth))
) %>%
  mutate(
    gdp_2024 = spain_2000 * (1 + annual_growth)^years_to_2024
  )

print("Counterfactual GDP per capita for Spain in 2024:")
print(gdp_2024_counterfactual)

# Spain's actual GDP in 2024
spain_2024_actual <- gdp_clean %>%
  filter(country == "Spain", year == 2024) %>%
  pull(gdp_per_cap)

# Add actual GDP and % difference to counterfactuals
gdp_2024_comparison <- gdp_2024_counterfactual %>%
  mutate(
    actual_gdp = spain_2024_actual,
    pct_diff = (gdp_2024 - actual_gdp) / actual_gdp * 100
  )

cat("\nSpain's performance vs counterfactual scenarios:\n")
print(gdp_2024_comparison)

# --------------------------------------------------
# 8. ADDITIONAL ANALYSIS: Focus on 2008 crisis impact
# --------------------------------------------------

# Calculate growth rates for pre-crisis (2000-2008) and post-crisis (2008-2024)
crisis_analysis <- gdp_clean %>%
  group_by(country) %>%
  summarise(
    gdp_2000 = gdp_per_cap[year == 2000],
    gdp_2008 = gdp_per_cap[year == 2008],
    gdp_2024 = gdp_per_cap[year == 2024],
    .groups = "drop"
  ) %>%
  mutate(
    growth_2000_2008 = (gdp_2008 / gdp_2000)^(1/8) - 1,
    growth_2008_2024 = (gdp_2024 / gdp_2008)^(1/16) - 1,
    total_growth = (gdp_2024 / gdp_2000)^(1/24) - 1
  )

# Spain's performance in different periods
spain_crisis_performance <- crisis_analysis %>%
  filter(country == "Spain")

cat("\nSpain's performance by period:\n")
cat("Pre-crisis (2000-2008):", round(spain_crisis_performance$growth_2000_2008 * 100, 2), "%\n")
cat("Post-crisis (2008-2024):", round(spain_crisis_performance$growth_2008_2024 * 100, 2), "%\n")
cat("Overall (2000-2024):", round(spain_crisis_performance$total_growth * 100, 2), "%\n")

# Compare with OECD averages
oecd_crisis_averages <- crisis_analysis %>%
  filter(country != "Spain") %>%
  summarise(
    avg_growth_2000_2008 = mean(growth_2000_2008, na.rm = TRUE),
    avg_growth_2008_2024 = mean(growth_2008_2024, na.rm = TRUE),
    avg_total_growth = mean(total_growth, na.rm = TRUE)
  )

cat("\nOECD averages (excluding Spain):\n")
cat("Pre-crisis (2000-2008):", round(oecd_crisis_averages$avg_growth_2000_2008 * 100, 2), "%\n")
cat("Post-crisis (2008-2024):", round(oecd_crisis_averages$avg_growth_2008_2024 * 100, 2), "%\n")
cat("Overall (2000-2024):", round(oecd_crisis_averages$avg_total_growth * 100, 2), "%\n")