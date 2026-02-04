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
# 4. Construct counterfactual index series
# --------------------------------------------------
create_index <- function(base = 100, growth_rate, years = years) {
  idx <- base * ((1 + growth_rate) ^ (years - min(years)))
  data.frame(year = years, index = idx)
}

# Russia actual
russia_index <- gdp_clean %>%
  filter(country == "Russian Federation") %>%
  arrange(year) %>%
  mutate(index = gdp_per_cap / gdp_per_cap[year == min(years)] * 100,
         series = "Russia")

# Average growth excluding Caucasus
avg_index <- create_index(base = 100, growth_rate = avg_growth_excl_caucasus, years = years) %>%
  mutate(series = "Average (excl. Caucasus)")

# Best 5 average
best5_index <- create_index(base = 100, growth_rate = mean(best5$annual_growth), years = years) %>%
  mutate(series = "Best 5 (excl. Russia)")

# Worst 3 average
worst3_index <- create_index(base = 100, growth_rate = mean(worst3$annual_growth), years = years) %>%
  mutate(series = "Worst 3 (excl. Russia)")

# Combine for plotting
counterfactual_df <- bind_rows(
  russia_index %>% dplyr::select(year, index, series),
  avg_index,
  best5_index,
  worst3_index
)


# --------------------------------------------------
# 5. Plot
# --------------------------------------------------
ggplot(counterfactual_df, aes(x = year, y = index, color = series, linetype = series)) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "grey60") +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Counterfactual GDP per Capita Index (2000 = 100)",
    subtitle = "Russia vs USSR average, best/worst performing countries",
    x = "Year",
    y = "Index",
    color = "Series",
    linetype = "Series"
  ) +
  scale_color_manual(values = c(
    "Russia" = "black",
    "Average (excl. Caucasus)" = "purple",
    "Best 5 (excl. Russia)" = "green",
    "Worst 3 (excl. Russia)" = "red"
  )) +
  scale_linetype_manual(values = c(
    "Russia" = "solid",
    "Average (excl. Caucasus)" = "solid",
    "Best 5 (excl. Russia)" = "dashed",
    "Worst 3 (excl. Russia)" = "dotted"
  )) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "#EAF2F8", color = NA),
    plot.background  = element_rect(fill = "#EAF2F8", color = NA),
    panel.grid.major = element_line(color = "white", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )



# Russia's actual GDP per capita in 2000
russia_2000 <- gdp_clean %>%
  filter(country == "Russian Federation", year == 2000) %>%
  pull(gdp_per_cap)

# Compute counterfactual GDP per capita in 2024
years_to_2024 <- 2024 - 2000

gdp_2024_counterfactual <- tibble(
  scenario = c("Average (excl. Caucasus)", "Best 5 (excl. Russia)", "Worst 3 (excl. Russia)"),
  annual_growth = c(avg_growth_excl_caucasus, mean(best5$annual_growth), mean(worst3$annual_growth))
) %>%
  mutate(
    gdp_2024 = russia_2000 * (1 + annual_growth)^years_to_2024
  )

gdp_2024_counterfactual

