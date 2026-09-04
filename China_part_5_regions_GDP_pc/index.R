# ── Urban-rural divide: OECD regional GDP data ──────────────────────────────
# Source: OECD Regional Economy Database (DSD_REG_ECO@DF_ECO)
# Eduardo Fé | University of Manchester

setwd("~/Desktop/tweets/China/China_part_5_urban_rural")

library(ggplot2)
library(dplyr)
library(tidyr)
library(jsonlite)

col_china  <- "#D62728"
col_other  <- "#4A7C94"
col_bg     <- "#FAFAFA"
col_grid   <- "#E0E0E0"
col_text   <- "#2B2B2B"

# =============================================================================
# 1. DOWNLOAD OECD REGIONAL GDP DATA
# =============================================================================

# OECD SDMX REST API
# Dataflow: OECD.CFE.EDS,DSD_REG_ECO@DF_ECO
# Dimensions: Freq.REF_AREA.Region.Measure.Activity.Unit
# We want: Annual, China provinces, GDP, Total, USD PPP per person

oecd_url <- paste0(
  "https://sdmx.oecd.org/public/rest/data/",
  "OECD.CFE.EDS,DSD_REG_ECO@DF_ECO,/",
  "A..",
  "CHN+CN01+CN02+CN03+CN04+CN05+CN06+CN07+CN08+",
  "CN09+CN10+CN11+CN12+CN13+CN14+CN15+CN16+CN17+",
  "CN18+CN19+CN20+CN21+CN22+CN23+CN24+CN25+CN26+",
  "CN27+CN28+CN29+CN30+CN31",
  "..GDP...USD_PPP_PS+USD_PPP",
  "?dimensionAtObservation=AllDimensions",
  "&format=csvfilewithlabels"
)

cat("Downloading OECD regional data...\n")
oecd_raw <- read.csv(url(oecd_url), stringsAsFactors = FALSE)

cat("Rows:", nrow(oecd_raw), "\n")
cat("Columns:", paste(names(oecd_raw), collapse = ", "), "\n\n")

# Inspect what we got
cat("Regions:\n")
print(sort(unique(oecd_raw$Reference.area)))

# =============================================================================
# 2. CLEAN: keep most recent year, TL2 regions only
# =============================================================================

# TL2 regions have 4-character codes (e.g., CN11 = Beijing)
# National level is just "CHN"
df_oecd <- oecd_raw %>%
  transmute(
    region_code = REF_AREA,
    region_name = Reference.area,
    year        = as.integer(TIME_PERIOD),
    gdp_pc_ppp  = as.numeric(OBS_VALUE)
  ) %>%
  filter(!is.na(gdp_pc_ppp)) %>%
  # Keep TL2 regions (exclude national aggregate)
  filter(nchar(region_code) > 3) %>%
  # Most recent year per region
  arrange(region_code, desc(year)) %>%
  group_by(region_code) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(desc(gdp_pc_ppp))

cat("\nProvinces found:", nrow(df_oecd), "\n")
cat("Year(s):", paste(unique(df_oecd$year), collapse = ", "), "\n\n")

# =============================================================================
# 3. ADD INTERNATIONAL COMPARATORS
# =============================================================================

# Fetch country-level GDP/capita PPP for comparators from same OECD dataset
comparators <- c("USA", "DEU", "GBR", "ESP", "PRT",
                 "HUN", "MEX", "TUR", "POL", "CHL")

comp_url <- paste0(
  "https://sdmx.oecd.org/public/rest/data/",
  "OECD.CFE.EDS,DSD_REG_ECO@DF_ECO,/",
  "A.", paste(comparators, collapse = "+"),
  "..GDP...USD_PPP_PS",
  "?dimensionAtObservation=AllDimensions",
  "&format=csvfilewithlabels",
  "&lastNObservations=1"
)

comp_raw <- tryCatch({
  read.csv(url(comp_url), stringsAsFactors = FALSE)
}, error = function(e) NULL)

if (!is.null(comp_raw)) {
  df_comp <- comp_raw %>%
    filter(nchar(REF_AREA) == 3) %>%  # country-level only
    transmute(
      country = Reference.area,
      gdp_pc_ppp = as.numeric(OBS_VALUE)
    ) %>%
    filter(!is.na(gdp_pc_ppp)) %>%
    arrange(desc(gdp_pc_ppp)) %>%
    group_by(country) %>%
    slice(1) %>%
    ungroup()
  
  cat("Comparator countries:\n")
  print(df_comp)
}

# =============================================================================
# 4. CHART 1: Provincial GDP per capita with comparator lines
# =============================================================================

df_oecd <- df_oecd %>%
  mutate(region_name = factor(region_name, levels = region_name))

# Find closest international match for key provinces
p_provinces <- ggplot(df_oecd,
                      aes(x = gdp_pc_ppp, y = region_name)) +
  geom_col(fill = col_other, width = 0.6) +
  geom_text(aes(label = paste0("$", format(round(gdp_pc_ppp), big.mark = ","))),
            hjust = -0.05, size = 3.2, colour = col_text, fontface = "bold") +
  
  # Comparator lines (if we got them)
  {if (!is.null(comp_raw) && nrow(df_comp) > 0) {
    list(
      geom_vline(data = df_comp,
                 aes(xintercept = gdp_pc_ppp),
                 linetype = "dashed", colour = col_china,
                 linewidth = 0.3, alpha = 0.6),
      geom_text(data = df_comp,
                aes(x = gdp_pc_ppp, y = nrow(df_oecd) * 0.95,
                    label = country),
                angle = 90, hjust = 1, vjust = -0.3,
                size = 2.8, colour = col_china, fontface = "italic")
    )
  }} +
  
  scale_x_continuous(labels = scales::dollar_format(),
                     expand = expansion(mult = c(0, 0.18))) +
  labs(
    title    = "Portugal to Moldova in one country",
    subtitle = paste0(
      "GDP per capita PPP by Chinese province (OECD TL2 regions)\n",
      "Dashed lines = international comparators"
    ),
    x = "GDP per capita, PPP (current international $)",
    y = NULL,
    caption  = paste0(
      "Source: OECD Regional Economy Database (DSD_REG_ECO).\n",
      "TL2 regions, most recent year. @EduardoFe"
    )
  ) +
  theme_minimal(base_size = 13, base_family = "sans") +
  theme(
    plot.background    = element_rect(fill = col_bg, colour = NA),
    panel.background   = element_rect(fill = col_bg, colour = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(colour = col_grid, linewidth = 0.3),
    axis.text.y  = element_text(colour = col_text, size = 9, face = "bold"),
    axis.text.x  = element_text(colour = col_text, size = 10),
    axis.title.x = element_text(colour = col_text, size = 11,
                                margin = margin(t = 8)),
    plot.title    = element_text(colour = col_text, size = 16, face = "bold",
                                 margin = margin(b = 4)),
    plot.subtitle = element_text(colour = "#555555", size = 10.5,
                                 margin = margin(b = 12), lineheight = 1.1),
    plot.caption  = element_text(colour = "#888888", size = 8, hjust = 0,
                                 margin = margin(t = 10)),
    plot.margin   = margin(20, 25, 15, 15)
  )

ggsave("provincial_gdp_oecd.png", p_provinces,
       width = 11, height = 10, dpi = 300, bg = col_bg)

# =============================================================================
# 5. CHART 2: Ratio of richest to poorest province
# =============================================================================

richest <- df_oecd %>% slice(1)
poorest <- df_oecd %>% slice(n())
ratio   <- richest$gdp_pc_ppp / poorest$gdp_pc_ppp

cat(sprintf("\nRichest: %s ($%s)\n", richest$region_name,
            format(round(richest$gdp_pc_ppp), big.mark = ",")))
cat(sprintf("Poorest: %s ($%s)\n", poorest$region_name,
            format(round(poorest$gdp_pc_ppp), big.mark = ",")))
cat(sprintf("Ratio: %.1fx\n", ratio))

# =============================================================================
# 6. CHART 3: Top 5 vs bottom 5 provinces
# =============================================================================

top5    <- df_oecd %>% slice(1:5) %>% mutate(group = "Top 5")
bottom5 <- df_oecd %>% slice((n()-4):n()) %>% mutate(group = "Bottom 5")
extremes <- bind_rows(top5, bottom5) %>%
  mutate(region_name = factor(region_name, levels = rev(region_name)))

p_extremes <- ggplot(extremes,
                     aes(x = gdp_pc_ppp, y = region_name,
                         fill = group)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0("$", format(round(gdp_pc_ppp), big.mark = ","))),
            hjust = -0.05, size = 4, colour = col_text, fontface = "bold") +
  scale_fill_manual(values = c("Top 5" = col_other, "Bottom 5" = col_china),
                    name = NULL) +
  scale_x_continuous(labels = scales::dollar_format(),
                     expand = expansion(mult = c(0, 0.2))) +
  labs(
    title = paste0("The gap within China: ",
                   round(ratio, 1), "x from richest to poorest"),
    subtitle = paste0(
      "GDP per capita PPP: top 5 vs bottom 5 provinces\n",
      richest$region_name, " ($",
      format(round(richest$gdp_pc_ppp), big.mark = ","),
      ") vs ", poorest$region_name, " ($",
      format(round(poorest$gdp_pc_ppp), big.mark = ","), ")"
    ),
    x = "GDP per capita, PPP (current international $)",
    y = NULL,
    caption = paste0(
      "Source: OECD Regional Economy Database.\n",
      "For context: Germany/Spain gap = 1.4x. @EduardoFe"
    )
  ) +
  theme_minimal(base_size = 14, base_family = "sans") +
  theme(
    plot.background    = element_rect(fill = col_bg, colour = NA),
    panel.background   = element_rect(fill = col_bg, colour = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(colour = col_grid, linewidth = 0.3),
    axis.text.y  = element_text(colour = col_text, size = 12, face = "bold"),
    axis.text.x  = element_text(colour = col_text, size = 10),
    axis.title.x = element_text(colour = col_text, size = 11,
                                margin = margin(t = 8)),
    plot.title    = element_text(colour = col_text, size = 15, face = "bold",
                                 margin = margin(b = 4)),
    plot.subtitle = element_text(colour = "#555555", size = 10.5,
                                 margin = margin(b = 12), lineheight = 1.1),
    plot.caption  = element_text(colour = "#888888", size = 8, hjust = 0,
                                 margin = margin(t = 10)),
    plot.margin   = margin(20, 25, 15, 15),
    legend.position = "top",
    legend.justification = "left"
  )

ggsave("provincial_gap_oecd.png", p_extremes,
       width = 10, height = 6, dpi = 300, bg = col_bg)

# =============================================================================
# 7. SUMMARY
# =============================================================================

cat("\n=== CHINESE PROVINCES BY GDP/CAPITA PPP ===\n")
cat(sprintf("%-25s  GDP/cap PPP   Year\n", "Province"))
for (i in seq_len(nrow(df_oecd))) {
  cat(sprintf("%-25s  $%s   %d\n",
              df_oecd$region_name[i],
              format(round(df_oecd$gdp_pc_ppp[i]), big.mark = ","),
              df_oecd$year[i]))
}

cat("\nDone. Charts saved.\n")