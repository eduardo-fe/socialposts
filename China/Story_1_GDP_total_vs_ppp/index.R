# ── China's changing position in the world economy ───────────────────────────
# Source: World Bank, World Development Indicators
# Eduardo Fé | University of Manchester
#
# Visual: bump/slope chart — China (red), Singapore (gold), all others grey
# Data:   World Bank API (with hardcoded fallback if API unreachable)

library(ggplot2)
library(dplyr)
library(tidyr)
library(jsonlite)

# ── Colour palette ───────────────────────────────────────────────────────────
col_china <- "#D62728"
col_sgp   <- "#DAA520"
col_grey  <- "#D0D0D0"
col_bg    <- "#FAFAFA"
col_grid  <- "#E0E0E0"
col_text  <- "#2B2B2B"
col_usa   <- "#1F77B4"
col_eth   <- "#8C564B"
col_lux   <- "#9467BD"

# =============================================================================
# 1. DOWNLOAD WORLD BANK DATA (or fall back to hardcoded)
# =============================================================================

get_wb_data <- function(indicator) {
  url <- paste0(
    "https://api.worldbank.org/v2/country/all/indicator/",
    indicator,
    "?format=json&per_page=20000"
  )
  raw <- fromJSON(url)
  dat <- raw[[2]]
  dat %>%
    transmute(
      iso3    = countryiso3code,
      country = country$value,
      year    = as.integer(date),
      value   = value
    )
}

# Try live API; fall back to hardcoded if blocked
api_ok <- tryCatch({
  cat("Trying World Bank API...\n")
  
  indicators <- c(
    total_gdp  = "NY.GDP.MKTP.CD",
    ppp_pc     = "NY.GDP.PCAP.PP.CD",
    nominal_pc = "NY.GDP.PCAP.CD"
  )
  
  wb_list <- purrr::map(indicators, get_wb_data)
  
  # Rename value columns
  wb_long <- purrr::imap_dfr(
    wb_list,
    ~ .x %>% rename(!!paste0(.y) := value)
  )
  
  # Combine by country-year
  wb_wide <- wb_long %>%
    group_by(iso3, country, year) %>%
    summarise(
      total_gdp  = first(total_gdp[!is.na(total_gdp)],  default = NA_real_),
      ppp_pc     = first(ppp_pc[!is.na(ppp_pc)],         default = NA_real_),
      nominal_pc = first(nominal_pc[!is.na(nominal_pc)], default = NA_real_),
      .groups = "drop"
    )
  
  # Remove WB aggregates using metadata
  countries_raw <- fromJSON(
    "https://api.worldbank.org/v2/country?format=json&per_page=400"
  )
  real_countries <- countries_raw[[2]] %>%
    filter(region$id != "NA") %>%
    transmute(iso3 = id, country_name = name)
  
  wb_wide <- wb_wide %>%
    inner_join(real_countries, by = "iso3") %>%
    select(iso3, country = country_name, year, total_gdp, ppp_pc, nominal_pc)
  
  # Find latest year with >= 150 countries having all three
  coverage <- wb_wide %>%
    filter(!is.na(total_gdp), !is.na(ppp_pc), !is.na(nominal_pc)) %>%
    count(year, name = "n_countries") %>%
    arrange(desc(year))
  
  latest_year <- coverage %>%
    filter(n_countries >= 150) %>%
    slice(1) %>%
    pull(year)
  
  cat("Using World Bank data for:", latest_year, "\n")
  
  # Rank all countries
  df_rank <<- wb_wide %>%
    filter(year == latest_year) %>%
    filter(!is.na(total_gdp), !is.na(ppp_pc), !is.na(nominal_pc)) %>%
    mutate(
      rank_total = min_rank(desc(total_gdp)),
      rank_ppp   = min_rank(desc(ppp_pc)),
      rank_nom   = min_rank(desc(nominal_pc))
    )
  
  df_rank <- df_rank %>%
    filter(rank_total <= 75)
  
  
  latest_year_global <<- latest_year
  TRUE
}, error = function(e) {
  cat("API unavailable:", conditionMessage(e), "\n")
  FALSE
})

 

# =============================================================================
# 2. HIGHLIGHT GROUPS
# =============================================================================



df_rank$highlight <- case_when(
  df_rank$iso3 == "CHN" ~ "China",
  df_rank$iso3 == "SGP" ~ "Singapore",
  df_rank$iso3 == "USA" ~ "USA",
  df_rank$iso3 == "ETH" ~ "Ethiopia",
  df_rank$iso3 == "LUX" ~ "Luxembourg",
  TRUE ~ "Other"
)


# Print key ranks
cat("\n=== KEY RANKS ===\n")
for (h in c("CHN", "SGP")) {
  r <- df_rank[df_rank$iso3 == h, ]
  cat(sprintf("%-12s  total=#%-3d  PPP=#%-3d  nominal=#%-3d\n",
              r$country, r$rank_total, r$rank_ppp, r$rank_nom))
}

# =============================================================================
# 3. RESHAPE FOR BUMP CHART
# =============================================================================

df_long <- df_rank %>%
  select(iso3, country, highlight, rank_total, rank_ppp, rank_nom) %>%
  pivot_longer(
    cols      = c(rank_total, rank_ppp, rank_nom),
    names_to  = "metric",
    values_to = "rank"
  ) %>%
  mutate(
    metric = factor(
      metric,
      levels = c("rank_total", "rank_ppp", "rank_nom"),
      labels = c("GDP\n(total)", "GDP per capita\n(PPP)", "GDP per capita\n(nominal)")
    )
  )


grey_data  <- df_long %>% filter(highlight == "Other")
china_data <- df_long %>% filter(highlight == "China")
sgp_data   <- df_long %>% filter(highlight == "Singapore")
usa_data   <- df_long %>% filter(highlight == "USA")
eth_data   <- df_long %>% filter(highlight == "Ethiopia")
lux_data   <- df_long %>% filter(highlight == "Luxembourg")

# =============================================================================
# 4. BUMP CHART
# =============================================================================

n_countries <- nrow(df_rank)

max_rank <- max(df_rank$rank_ppp, df_rank$rank_nom, df_rank$rank_total)

p <- ggplot() +
  
  # ── Grey background lines ──────────────────────────────────────────────────
  geom_line(data = grey_data,
            aes(x = metric, y = rank, group = country),
            colour = col_grey, linewidth = 0.4, alpha = 0.6) +
  geom_point(data = grey_data,
             aes(x = metric, y = rank),
             colour = col_grey, size = 1.5, alpha = 0.4) +
  
  # ── China ──────────────────────────────────────────────────────────────────
  geom_line(data = china_data,
            aes(x = metric, y = rank, group = country),
            colour = col_china, linewidth = 2.2) +
  geom_point(data = china_data,
             aes(x = metric, y = rank),
             colour = col_china, size = 7) +
  geom_text(data = china_data,
            aes(x = metric, y = rank, label = paste0("#", rank)),
            colour = "white", size = 2.5, fontface = "bold") +
  
  # ── Singapore ──────────────────────────────────────────────────────────────
  geom_line(data = sgp_data,
            aes(x = metric, y = rank, group = country),
            colour = col_sgp, linewidth = 2.2) +
  geom_point(data = sgp_data,
             aes(x = metric, y = rank),
             colour = col_sgp, size = 7) +
  geom_text(data = sgp_data,
            aes(x = metric, y = rank, label = paste0("#", rank)),
            colour = "white", size = 2.5, fontface = "bold") +
  
  # ── USA ────────────────────────────────────────────────────────────────────
  geom_line(data = usa_data,
            aes(x = metric, y = rank, group = country),
            colour = col_usa, linewidth = 2.2) +
  geom_point(data = usa_data,
             aes(x = metric, y = rank),
             colour = col_usa, size = 7) +
  geom_text(data = usa_data,
            aes(x = metric, y = rank, label = paste0("#", rank)),
            colour = "white", size = 2.5, fontface = "bold") +
  
  # ── Ethiopia ───────────────────────────────────────────────────────────────
  geom_line(data = eth_data,
            aes(x = metric, y = rank, group = country),
            colour = col_eth, linewidth = 2.2) +
  geom_point(data = eth_data,
             aes(x = metric, y = rank),
             colour = col_eth, size = 7) +
  geom_text(data = eth_data,
            aes(x = metric, y = rank, label = paste0("#", rank)),
            colour = "white", size = 2.5, fontface = "bold") +
  # ── Luxembourg ─────────────────────────────────────────────────────────────
  geom_line(data = lux_data,
            aes(x = metric, y = rank, group = country),
            colour = col_lux, linewidth = 2.2) +
  geom_point(data = lux_data,
             aes(x = metric, y = rank),
             colour = col_lux, size = 7) +
  geom_text(data = lux_data,
            aes(x = metric, y = rank, label = paste0("#", rank)),
            colour = "white", size = 2.5, fontface = "bold") +
  
  # ── Country name labels ────────────────────────────────────────────────────
  geom_text(data = china_data %>% filter(metric == "GDP\n(total)"),
            aes(x = metric, y = rank, label = "China"),
            hjust = 1.2, vjust =1.4, colour = col_china, size = 5, fontface = "bold") +
  geom_text(data = china_data %>% filter(metric == "GDP per capita\n(nominal)"),
            aes(x = metric, y = rank, label = "China"),
            hjust = -0.5, colour = col_china, size = 5, fontface = "bold") +
  
  geom_text(data = sgp_data %>% filter(metric == "GDP\n(total)"),
            aes(x = metric, y = rank, label = "Singapore"),
            hjust = 1.3, colour = col_sgp, size = 4.2, fontface = "bold") +
  geom_text(data = sgp_data %>% filter(metric == "GDP per capita\n(nominal)"),
            aes(x = metric, y = rank, label = "Singapore"),
            hjust = -0.3, vjust=1.2, colour = col_sgp, size = 4.2, fontface = "bold") +
  
  # USA labels
  geom_text(data = usa_data %>% filter(metric == "GDP\n(total)"),
            aes(x = metric, y = rank, label = "USA"),
            hjust = 1.4, colour = col_usa, size = 5, fontface = "bold") +
  geom_text(data = usa_data %>% filter(metric == "GDP per capita\n(nominal)"),
            aes(x = metric, y = rank, label = "USA"),
            hjust = -0.5, vjust=1.8, colour = col_usa, size = 5, fontface = "bold") +
  
  # Ethiopia labels
  geom_text(data = eth_data %>% filter(metric == "GDP\n(total)"),
            aes(x = metric, y = rank, label = "Ethiopia"),
            hjust = 1.3, colour = col_eth, size = 4.2, fontface = "bold") +
  geom_text(data = eth_data %>% filter(metric == "GDP per capita\n(nominal)"),
            aes(x = metric, y = rank, label = "Ethiopia"),
            hjust = -0.3, colour = col_eth, size = 4.2, fontface = "bold") +
  # Luxembourg labels
  geom_text(data = lux_data %>% filter(metric == "GDP\n(total)"),
            aes(x = metric, y = rank, label = "Luxembourg"),
            hjust = 1.3, colour = col_lux, size = 4.2, fontface = "bold") +
  geom_text(data = lux_data %>% filter(metric == "GDP per capita\n(nominal)"),
            aes(x = metric, y = rank, label = "Luxembourg"),
            hjust = -0.3, colour = col_lux, size = 4.2, fontface = "bold") +
  
  # ── Axes ───────────────────────────────────────────────────────────────────
  scale_y_reverse(
    breaks = c(1, seq(10, max_rank, 10)),
    limits = c(max_rank + 2, 1),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  
  # ── Labels ─────────────────────────────────────────────────────────────────
  labs(
    title = "Everything is relative: Total vs Per capita GDP",
    
    subtitle = paste0(
      "Absolute vs Relative Wealth of the (n=", n_countries, ") largest (by GDP) countries. Rank 1 = largest (Left) Wealthiest (Right).\n",
      "USA (blue), China (red), Singapore (gold),  Luxembourg (purple), Ethiopia (brown)."
    ),
    
    x = NULL,
    y = "Rank (1 = Largest (left) Wealthiest (right))",
    
    caption = paste0(
      "Source: World Bank, World Development Indicators (",
      latest_year_global, ").\n"
    )
  ) +
  
  theme_minimal(base_size = 14, base_family = "sans") +
  theme(
    plot.background    = element_rect(fill = col_bg, colour = NA),
    panel.background   = element_rect(fill = col_bg, colour = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(colour = "#E8E8E8", linewidth = 0.2),
    axis.text.x  = element_text(colour = col_text, size = 13, face = "bold",
                                lineheight = 0.9),
    axis.text.y  = element_text(colour = col_text, size = 10),
    axis.title.y = element_text(colour = col_text, size = 11),
    plot.title    = element_text(colour = col_text, size = 17, face = "bold",
                                 margin = margin(b = 4)),
    plot.subtitle = element_text(colour = "#555555", size = 11,
                                 margin = margin(b = 15), lineheight = 1.1),
    plot.caption  = element_text(colour = "#888888", size = 8, hjust = 0,
                                 margin = margin(t = 12)),
    plot.margin   = margin(20, 50, 15, 50)
  )

# =============================================================================
# 5. SAVE
# =============================================================================
p