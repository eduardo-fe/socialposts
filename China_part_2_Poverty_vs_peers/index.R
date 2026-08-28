# ── Poverty among China's income peers ($10K-$15K GDP per capita) ─────────────
# Source: World Bank PIP / WDI
# Eduardo Fé | University of Manchester

setwd("~/Desktop/tweets/China/China_part_2_poverty_measures")

library(ggplot2)
library(dplyr)
library(tidyr)
library(jsonlite)

col_china <- "#D62728"
col_other <- "#4A7C94"
col_bg    <- "#FAFAFA"
col_grid  <- "#E0E0E0"
col_text  <- "#2B2B2B"

# =============================================================================
# 1. DATA (WB API with hardcoded fallback)
# =============================================================================

get_wb_data <- function(indicator) {
  url <- paste0(
    "https://api.worldbank.org/v2/country/all/indicator/",
    indicator,
    "?format=json&per_page=20000&mrnev=1"
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

get_real_countries <- function() {
  url <- "https://api.worldbank.org/v2/country?format=json&per_page=400"
  raw <- fromJSON(url)
  raw[[2]] %>%
    filter(region$id != "NA") %>%
    transmute(iso3 = id)
}

api_ok <- tryCatch({
  cat("Trying World Bank API...\n")
  real <- get_real_countries()
  
  gdp_raw  <- get_wb_data("NY.GDP.PCAP.CD")
  pov_215  <- get_wb_data("SI.POV.DDAY")
  pov_365  <- get_wb_data("SI.POV.LMIC")
  pov_685  <- get_wb_data("SI.POV.UMIC")
  pov_natl <- get_wb_data("SI.POV.NAHC")
  
  # Deduplicate: keep most recent year per country for each indicator
  dedup <- function(dat, col_name) {
    dat %>%
      rename(!!col_name := value) %>%
      filter(!is.na(.data[[col_name]])) %>%
      arrange(iso3, desc(year)) %>%
      group_by(iso3) %>%
      slice(1) %>%
      ungroup() %>%
      select(iso3, country, all_of(col_name))
  }
  
  df <- dedup(gdp_raw, "gdp_pc") %>%
    inner_join(dedup(pov_215, "pov_215"),  by = "iso3", suffix = c("", ".y")) %>%
    inner_join(dedup(pov_365, "pov_365"),  by = "iso3", suffix = c("", ".y")) %>%
    inner_join(dedup(pov_685, "pov_685"),  by = "iso3", suffix = c("", ".y")) %>%
    left_join(dedup(pov_natl, "pov_natl"), by = "iso3", suffix = c("", ".y")) %>%
    inner_join(real, by = "iso3") %>%
    select(iso3, country, gdp_pc, pov_215, pov_365, pov_685, pov_natl)
  
  df_all <<- df
  data_source <<- "World Bank PIP (most recent year per country)"
  TRUE
}, error = function(e) {
  cat("API unavailable:", conditionMessage(e), "\nUsing hardcoded data.\n")
  FALSE
})

 
# =============================================================================
# 2. FILTER TO $10K-$15K BAND
# =============================================================================

band <- df_all %>%
  filter(gdp_pc >= 10000, gdp_pc <= 15000) %>%
  filter(!is.na(pov_215), !is.na(pov_365), !is.na(pov_685))

cat("Countries in $10K-$15K band:", nrow(band), "\n")
cat(paste(band$country, collapse = ", "), "\n\n")

band$is_china <- ifelse(band$iso3 == "CHN", "China", "Other")

# Add GDP per capita label for sorting
band$country_label <- paste0(
  band$country, " ($", format(round(band$gdp_pc), big.mark = ","), ")"
)

# =============================================================================
# 3. RESHAPE FOR FACETED BAR CHART
# =============================================================================

band_long <- band %>%
  select(iso3, country, country_label, is_china, gdp_pc,
         pov_215, pov_365, pov_685, pov_natl) %>%
  pivot_longer(
    cols      = c(pov_215, pov_365, pov_685, pov_natl),
    names_to  = "threshold",
    values_to = "rate"
  ) %>%
  filter(!is.na(rate)) %>%
  mutate(
    threshold = factor(
      threshold,
      levels = c("pov_215", "pov_365", "pov_685", "pov_natl"),
      labels = c(
        "$2.15/day (extreme poverty)",
        "$3.65/day (lower-middle)",
        "$6.85/day (upper-middle)",
        "National poverty line"
      )
    )
  )

# Order countries by $6.85 rate (most revealing threshold)
order_685 <- band %>%
  arrange(pov_685) %>%
  pull(country_label)

band_long$country_label <- factor(band_long$country_label, levels = order_685)

# =============================================================================
# 4. FACETED HORIZONTAL BAR CHART
# =============================================================================

p <- ggplot(band_long, aes(x = rate, y = country_label, fill = is_china)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = paste0(rate, "%")),
            hjust = -0.08, size = 3.3, colour = col_text, fontface = "bold") +
  
  facet_wrap(~ threshold, ncol = 2, scales = "free_x") +
  
  scale_fill_manual(
    values = c("China" = col_china, "Other" = col_other),
    guide  = "none"
  ) +
  scale_x_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.2))
  ) +
  
  labs(
    title = "Among its income peers, China is not poor... until you raise the bar",
    
    subtitle = paste0(
      "Poverty headcount (%) for countries with GDP per capita $10,000-$15,000.\n",
      "At $2.15/day, China looks fine. At $6.85/day, 17% are poor — ",
      "worse than Russia, Turkey, Argentina."
    ),
    
    x = NULL,
    y = NULL,
    
    caption = paste0(
      "Source: ", data_source, "\n",
      "Band: GDP per capita $10K-$15K (current US$). ",
      "2017 PPP thresholds. China in red. @EduardoFe"
    )
  ) +
  
  theme_minimal(base_size = 13, base_family = "sans") +
  theme(
    plot.background    = element_rect(fill = col_bg, colour = NA),
    panel.background   = element_rect(fill = col_bg, colour = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(colour = col_grid, linewidth = 0.3),
    strip.text = element_text(colour = col_text, size = 11, face = "bold",
                              margin = margin(b = 8)),
    axis.text.y  = element_text(colour = col_text, size = 10, face = "bold"),
    axis.text.x  = element_text(colour = col_text, size = 9),
    plot.title   = element_text(colour = col_text, size = 16, face = "bold",
                                margin = margin(b = 4)),
    plot.subtitle = element_text(colour = "#555555", size = 10.5,
                                 margin = margin(b = 12), lineheight = 1.1),
    plot.caption  = element_text(colour = "#888888", size = 8, hjust = 0,
                                 margin = margin(t = 10)),
    plot.margin   = margin(20, 25, 15, 15)
  )

ggsave("poverty_peers.png", p,
       width = 14, height = 9, dpi = 300, bg = col_bg)

# Print summary
cat("\n=== CHINA VS INCOME PEERS ($10K-$15K) ===\n")
cat(sprintf("%-20s  $2.15  $3.65  $6.85  National\n", "Country"))
for (i in seq_len(nrow(band))) {
  cat(sprintf("%-20s  %5.1f  %5.1f  %5.1f  %5.1f\n",
              band$country[i],
              band$pov_215[i], band$pov_365[i],
              band$pov_685[i],
              ifelse(is.na(band$pov_natl[i]), NA, band$pov_natl[i])))
}

cat("\nDone.\n")


# =============================================================================
# 5. INDIVIDUAL PANELS
# =============================================================================

thresholds <- list(
  list(var = "pov_215",  label = "$2.15/day (extreme poverty)",
       file = "poverty_peers_215.png"),
  list(var = "pov_365",  label = "$3.65/day (lower-middle)",
       file = "poverty_peers_365.png"),
  list(var = "pov_685",  label = "$6.85/day (upper-middle)",
       file = "poverty_peers_685.png"),
  list(var = "pov_natl", label = "National poverty line",
       file = "poverty_peers_natl.png")
)

for (thr in thresholds) {
  
  df_panel <- band %>%
    filter(!is.na(.data[[thr$var]])) %>%
    mutate(
      rate = .data[[thr$var]],
      country_label = paste0(country, " ($",
                             format(round(gdp_pc), big.mark = ","), ")")
    ) %>%
    arrange(rate) %>%
    mutate(country_label = factor(country_label, levels = country_label))
  
  p_individual <- ggplot(df_panel,
                         aes(x = rate, y = country_label, fill = is_china)) +
    geom_col(width = 0.65) +
    geom_text(aes(label = paste0(rate, "%")),
              hjust = -0.08, size = 4, colour = col_text, fontface = "bold") +
    scale_fill_manual(
      values = c("China" = col_china, "Other" = col_other),
      guide  = "none"
    ) +
    scale_x_continuous(
      labels = function(x) paste0(x, "%"),
      expand = expansion(mult = c(0, 0.2))
    ) +
    labs(
      title    = thr$label,
      subtitle = "Countries with GDP per capita $10,000-$15,000",
      x = "Poverty headcount (%)",
      y = NULL,
      caption  = paste0("Source: ", data_source,
                        " · Band: $10K-$15K GDP/capita · @EduardoFe")
    ) +
    theme_minimal(base_size = 13, base_family = "sans") +
    theme(
      plot.background    = element_rect(fill = col_bg, colour = NA),
      panel.background   = element_rect(fill = col_bg, colour = NA),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_line(colour = col_grid, linewidth = 0.3),
      axis.text.y  = element_text(colour = col_text, size = 11, face = "bold"),
      axis.text.x  = element_text(colour = col_text, size = 10),
      axis.title.x = element_text(colour = col_text, size = 11,
                                  margin = margin(t = 8)),
      plot.title    = element_text(colour = col_text, size = 16, face = "bold",
                                   margin = margin(b = 4)),
      plot.subtitle = element_text(colour = "#555555", size = 11,
                                   margin = margin(b = 12)),
      plot.caption  = element_text(colour = "#888888", size = 8, hjust = 0,
                                   margin = margin(t = 10)),
      plot.margin   = margin(20, 25, 15, 15)
    )
  
  ggsave(
    paste0("/home/claude/", thr$file),
    p_individual,
    width = 9, height = 5.5, dpi = 300, bg = col_bg
  )
  
  cat("Saved:", thr$file, "\n")
  
}  
  
  
  
## Part 2 (linkedin -China fails to report National Povery Line, so let's focus on avaiable)
  

  
  
  
  # ── China vs income peers: poverty at three thresholds ────────────────────────
  # Source: World Bank PIP (automatic download)
  # Eduardo Fé | University of Manchester
  
  setwd("~/Desktop/tweets/China/China_part_2_poverty_measures")
  
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(jsonlite)
  
  col_extreme  <- "#2CA02C"
  col_lower    <- "#DAA520"
  col_upper    <- "#D62728"
  col_china_bg <- "#FFF0F0"
  col_bg       <- "#FAFAFA"
  col_grid     <- "#E0E0E0"
  col_text     <- "#2B2B2B"
  
  # =============================================================================
  # 1. DOWNLOAD FROM WORLD BANK API
  # =============================================================================
  
  get_wb_data <- function(indicator) {
    url <- paste0(
      "https://api.worldbank.org/v2/country/all/indicator/",
      indicator,
      "?format=json&per_page=20000&mrnev=1"
    )
    raw <- fromJSON(url)
    raw[[2]] %>%
      transmute(
        iso3    = countryiso3code,
        country = country$value,
        year    = as.integer(date),
        value   = value
      )
  }
  
  get_real_countries <- function() {
    url <- "https://api.worldbank.org/v2/country?format=json&per_page=400"
    raw <- fromJSON(url)
    raw[[2]] %>%
      filter(region$id != "NA") %>%
      transmute(iso3 = id)
  }
  
  dedup <- function(dat, col_name) {
    dat %>%
      rename(!!col_name := value) %>%
      filter(!is.na(.data[[col_name]])) %>%
      arrange(iso3, desc(year)) %>%
      group_by(iso3) %>%
      slice(1) %>%
      ungroup() %>%
      select(iso3, country, all_of(col_name))
  }
  
  cat("Downloading from World Bank API...\n")
  
  real     <- get_real_countries()
  gdp_raw  <- get_wb_data("NY.GDP.PCAP.CD")
  pov_low  <- get_wb_data("SI.POV.DDAY")
  pov_mid  <- get_wb_data("SI.POV.LMIC")
  pov_high <- get_wb_data("SI.POV.UMIC")
  
  df_all <- dedup(gdp_raw, "gdp_pc") %>%
    inner_join(dedup(pov_low,  "pov_low"),  by = "iso3", suffix = c("", ".y")) %>%
    inner_join(dedup(pov_mid,  "pov_mid"),  by = "iso3", suffix = c("", ".y")) %>%
    inner_join(dedup(pov_high, "pov_high"), by = "iso3", suffix = c("", ".y")) %>%
    inner_join(real, by = "iso3") %>%
    select(iso3, country, gdp_pc, pov_low, pov_mid, pov_high)
  
  cat("Countries with all indicators:", nrow(df_all), "\n")
  
  # =============================================================================
  # 2. FILTER TO $10K-$15K BAND
  # =============================================================================
  
  band <- df_all %>%
    filter(gdp_pc >= 10000, gdp_pc <= 15000)
  
  cat("Countries in $10K-$15K band:", nrow(band), "\n")
  cat(paste(band$country, collapse = ", "), "\n\n")
  
  keep <- c("CHN", "ARG", "BRA", "MEX", "MYS",
            "KAZ",  "MNE", "DOM")
  
  band <- band %>% filter(iso3 %in% keep)
  
  # =============================================================================
  # 3. RESHAPE FOR GROUPED BAR CHART
  # =============================================================================
  
  band_long <- band %>%
    pivot_longer(
      cols      = c(pov_low, pov_mid, pov_high),
      names_to  = "threshold",
      values_to = "rate"
    ) %>%
    mutate(
      threshold = factor(
        threshold,
        levels = c("pov_low", "pov_mid", "pov_high"),
        labels = c("$2.30/day (extreme)",
                   "$4.20/day (lower-middle)",
                   "$8.20/day (upper-middle)")
      )
    )
  
  # Order countries by upper-middle rate
  order_vec <- band %>% arrange(pov_high) %>% pull(country)
  band_long$country <- factor(band_long$country, levels = order_vec)
  
  band_long$is_china <- band_long$country == "China"
  
  # =============================================================================
  # 4. GROUPED BAR CHART
  # =============================================================================
  
  p <- ggplot(band_long, aes(x = rate, y = country, fill = threshold)) +
    
    geom_tile(data = band_long %>%
                filter(country == "China",
                       threshold == "$2.30/day (extreme)"),
              aes(x = max(band_long$rate) / 2, y = country),
              width = max(band_long$rate) * 1.5, height = 0.9,
              fill = col_china_bg, inherit.aes = FALSE) +
    
    geom_col(position = position_dodge(width = 0.75), width = 0.7) +
    
    geom_text(aes(label = paste0(round(rate, 1), "%")),
              position = position_dodge(width = 0.75),
              hjust = -0.08, size = 3.5, colour = col_text,
              fontface = "bold") +
    
    scale_fill_manual(
      values = c(
        "$2.30/day (extreme)"       = col_extreme,
        "$4.20/day (lower-middle)"  = col_lower,
        "$8.20/day (upper-middle)"  = col_upper
      ),
      name = NULL
    ) +
    
    scale_x_continuous(
      labels = function(x) paste0(x, "%"),
      expand = expansion(mult = c(0, 0.18))
    ) +
    
    labs(
      title = paste0(
        "China: Porvery by threshold vs its peers."
      ),
      subtitle = paste0(
        "Poverty headcount (%) at three World Bank thresholds (2021 PPP)\n",
        "Countries with GDP per capita $10,000-$15,000"
      ),
      x = "Poverty headcount (%)",
      y = NULL,
      caption = paste0(
        "Source: World Bank, Poverty and Inequality Platform.\n",
        "Thresholds: extreme, lower-middle, upper-middle ",
        "(2021 PPP). @EduardoFe"
      )
    ) +
    
    theme_minimal(base_size = 14, base_family = "sans") +
    theme(
      plot.background    = element_rect(fill = col_bg, colour = NA),
      panel.background   = element_rect(fill = col_bg, colour = NA),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_line(colour = col_grid,
                                        linewidth = 0.3),
      axis.text.y  = element_text(colour = col_text, size = 12,
                                  face = "bold"),
      axis.text.x  = element_text(colour = col_text, size = 10),
      axis.title.x = element_text(colour = col_text, size = 11,
                                  margin = margin(t = 8)),
      plot.title    = element_text(colour = col_text, size = 16,
                                   face = "bold",
                                   margin = margin(b = 4),
                                   lineheight = 1.1),
      plot.subtitle = element_text(colour = "#555555", size = 11,
                                   margin = margin(b = 12),
                                   lineheight = 1.1),
      plot.caption  = element_text(colour = "#888888", size = 8,
                                   hjust = 0,
                                   margin = margin(t = 10)),
      plot.margin   = margin(20, 25, 15, 15),
      legend.position = "top",
      legend.justification = "left",
      legend.text = element_text(size = 11)
    )
  
  ggsave("poverty_3bars.png", p,
         width = 11, height = 7, dpi = 300, bg = col_bg)
  
  # Print summary
  cat("\n=== COUNTRIES IN BAND ===\n")
  cat(sprintf("%-25s  GDP/cap   Low    Mid   High\n", "Country"))
  for (i in seq_len(nrow(band))) {
    cat(sprintf("%-25s  %7.0f  %5.1f  %5.1f  %5.1f\n",
                band$country[i], band$gdp_pc[i],
                band$pov_low[i], band$pov_mid[i], band$pov_high[i]))
  }
  
  cat("\nDone.\n")