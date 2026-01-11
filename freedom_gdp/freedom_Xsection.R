# =========================================================
# Required packages
# =========================================================
library(tidyverse)
library(countrycode)
library(ggrepel)
library(dplyr)
library(plotly)
# library(haven)   # uncomment if loading V-Dem from .dta

# =========================================================
# Step 1: Load Heritage data
# =========================================================
heritage <- read.csv("~/Desktop/tweets/economicFreedom/heritage-index-of-economic-freedom-20260105203507.csv")

heritage_data <- heritage %>%
  distinct(Country, .keep_all = TRUE) %>%
  select(Country, Overall = Overall.Score) %>%
  mutate(Overall = as.numeric(Overall)) %>%
  filter(!is.na(Overall) & Overall > 0)

# =========================================================
# Step 2: Download World Bank GDP per capita PPP
# =========================================================
url_gdp <- "https://api.worldbank.org/v2/en/indicator/NY.GDP.PCAP.PP.CD?downloadformat=csv"
temp <- tempfile()
download.file(url_gdp, temp)
unzip(temp, exdir = tempdir())

gdp_file <- list.files(tempdir(), pattern = "^API_NY.GDP.PCAP.PP.CD", full.names = TRUE)
wb_raw <- read_csv(gdp_file, skip = 4)

latest_year <- tail(sort(names(wb_raw)[grepl("^\\d{4}$", names(wb_raw))]), 1)

gdp_data <- wb_raw %>%
  select(`Country Name`, all_of(latest_year)) %>%
  rename(
    GDP_per_capita_PPP = all_of(latest_year),
    Country = `Country Name`
  ) %>%
  filter(!is.na(GDP_per_capita_PPP)) %>%
  mutate(iso3 = countrycode(Country, "country.name", "iso3c"))

unlink(temp)

# =========================================================
# Step 3: Country name harmonisation (Heritage)
# =========================================================
custom_match <- c(
  "Congo, Democratic Rep." = "Congo (Kinshasa)",
  "Congo, Rep." = "Congo (Brazzaville)",
  "Korea, South" = "Korea, Rep.",
  "Korea, North" = "Korea, Dem. People’s Rep.",
  "Hong Kong" = "Hong Kong SAR, China",
  "Macau" = "Macao SAR, China",
  "Macao" = "Macao SAR, China",
  "Cape Verde" = "Cabo Verde",
  "Kyrgyz Republic" = "Kyrgyzstan",
  "Slovak Republic" = "Slovakia",
  "Czech Republic" = "Czechia",
  "Bahamas, The" = "Bahamas",
  "Gambia, The" = "Gambia",
  "Micronesia" = "Micronesia, Fed. Sts."
)

heritage_data <- heritage_data %>%
  mutate(
    Country_std = recode(Country, !!!custom_match),
    Country_std = if_else(is.na(Country_std), Country, Country_std),
    iso3 = countrycode(Country_std, "country.name", "iso3c"),
    iso3 = case_when(
      str_detect(Country, regex("Kosovo", ignore_case = TRUE)) ~ "XK",
      str_detect(Country_std, regex("Micronesia", ignore_case = TRUE)) ~ "FSM",
      TRUE ~ iso3
    )
  )

# =========================================================
# Step 4: Merge Heritage + GDP (keep iso3!)
# =========================================================
merged_data <- heritage_data %>%
  filter(!is.na(iso3)) %>%
  inner_join(gdp_data, by = "iso3") %>%
  select(
    iso3,
    Country = Country.x,
    Overall,
    GDP_per_capita_PPP
  )

# =========================================================
# Step 5: V-Dem (2023) — THREE regime groups
# =========================================================
# vdem <- read_dta("V-Dem-CY-Core-v15.dta")

vdem_2023 <- vdem %>%
  select(v2x_libdem, country_name, year) %>%
  filter(year == 2023) %>%
  mutate(
    iso3 = countrycode(country_name, "country.name", "iso3c"),
    iso3 = case_when(
      country_name == "Kosovo" ~ "XK",
      country_name == "Myanmar" ~ "MMR",
      country_name == "Congo, Democratic Republic of the" ~ "COD",
      country_name == "Congo, Republic of the" ~ "COG",
      TRUE ~ iso3
    ),
    regime_group = case_when(
      v2x_libdem >= 0.7 ~ "High democracy",
      v2x_libdem < 0.4 ~ "Low democracy",
      TRUE ~ "Middle"
    )
  )

# =========================================================
# Step 6: Merge V-Dem onto main data
# =========================================================
merged_data <- merged_data %>%
  left_join(
    vdem_2023 %>% select(iso3, v2x_libdem, regime_group),
    by = "iso3"
  )

# =========================================================
# Step 7: Static plot
# =========================================================
cor_value <- cor(merged_data$Overall, merged_data$GDP_per_capita_PPP, use = "complete.obs")

ggplot(
  merged_data,
  aes(
    x = Overall,
    y = GDP_per_capita_PPP,
    color = regime_group
  )
) +
  geom_point(size = 2.7, alpha = 0.75) +
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed", color = "grey40") +
  scale_y_continuous(
    trans = "log10",
    labels = scales::comma,
    name = "GDP per capita (PPP, current intl. $)"
  ) +
  scale_x_continuous(name = "Economic Freedom Overall Score (2025)") +
  scale_color_manual(
    values = c(
      "High democracy" = "#0072B2",
      "Middle" = "grey60",
      "Low democracy" = "#D55E00"
    ),
    name = "V-Dem regime (2023)"
  ) +
  labs(
    title = "Economic Freedom vs. GDP per Capita",
    subtitle = paste(
      "Heritage 2025 · World Bank PPP · V-Dem 2023 | Correlation:",
      round(cor_value, 3)
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )

# =========================================================
# Step 8: Interactive HTML plot
# =========================================================
plotly_data <- merged_data %>%
  mutate(
    hover_text = paste0(
      "<b>", Country, "</b><br>",
      "Economic Freedom: ", round(Overall, 1), "<br>",
      "GDP per capita (PPP): $", scales::comma(round(GDP_per_capita_PPP)), "<br>",
      "V-Dem Liberal Democracy: ", round(v2x_libdem, 2), "<br>",
      "Regime group: ", regime_group
    )
  )

p_html <- plot_ly(
  data = plotly_data,
  x = ~Overall,
  y = ~GDP_per_capita_PPP,
  type = "scatter",
  mode = "markers",
  text = ~hover_text,
  hoverinfo = "text",
  color = ~regime_group,
  colors = c(
    "#0072B2",  # High democracy
    "grey60",   # Middle
    "#D55E00"   # Low democracy
  ),
  marker = list(size = 9, opacity = 0.75)
) %>%
  layout(
    title = list(
      text = paste0(
        "<b>Economic Freedom vs GDP per Capita</b><br>",
        "<span style='font-size:12px;'>Heritage 2025 · World Bank PPP · V-Dem 2023</span>"
      )
    ),
    xaxis = list(title = "Economic Freedom Overall Score (2025)"),
    yaxis = list(
      title = "GDP per capita (PPP, current intl. $)",
      type = "log"
    ),
    legend = list(title = list(text = "V-Dem regime (2023)"))
  )

p_html
