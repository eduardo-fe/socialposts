## =========================================================================
## Radcliffe Meteorological Station (Oxford) — exploratory climate graphics
## Source: SOGE / Oxford Weather and Climate since 1767 (Burt & Burt, 2019)
## Daily series 1815 -> present (updated periodically)
## =========================================================================

library(tidyverse)   # dplyr, ggplot2, readr, etc.
library(lubridate)
library(zoo)          # rolling means
library(scales)       # nicer axis labels

# -------------------------------------------------------------------------
# 1. READ AND CLEAN
# -------------------------------------------------------------------------
# Point this at wherever you keep the full, up-to-date CSV. The column
# names below match the header in the file you shared (degree symbols and
# all) — adjust if the SOGE export changes its naming.

raw_path <- "RadcliffeMet_daily.csv"   # <- change to your local path

oxford_raw <- read_csv(raw_path, show_col_types = FALSE)

# Tidy names (works whether the source uses ° or the spelled-out 'deg C')
oxford <- oxford_raw %>%
  rename_with(~ .x %>%
                str_replace_all("°", "") %>%
                str_squish()) %>%
  rename(
    year       = YYYY,
    month      = MM,
    day        = DD,
    tmax       = `Tmax C`,
    tmin       = `Tmin C`,
    tmean      = `Daily Tmean C`,
    trange     = `Daily range degC`,
    grass_min  = `Grass min C`,
    rain_notr  = `Rainfall mm 1 dpl no traces`,
    sun_hrs    = `Sunshine duration h`
  ) %>%
  mutate(
    date  = make_date(year, month, day),
    tmax  = as.numeric(tmax),
    tmin  = as.numeric(tmin),
    tmean = as.numeric(tmean),
    rain_notr = as.numeric(rain_notr),
    sun_hrs   = as.numeric(sun_hrs)
  ) %>%
  filter(!is.na(date))

# -------------------------------------------------------------------------
# 2. ANNUAL AGGREGATES
# -------------------------------------------------------------------------

annual <- oxford %>%
  group_by(year) %>%
  summarise(
    mean_tmean   = mean(tmean, na.rm = TRUE),
    mean_tmax    = mean(tmax,  na.rm = TRUE),
    mean_tmin    = mean(tmin,  na.rm = TRUE),
    total_rain   = sum(rain_notr, na.rm = TRUE),
    total_sun    = sum(sun_hrs, na.rm = TRUE),
    n_days       = n(),
    .groups = "drop"
  ) %>%
  # Drop years with very incomplete records (esp. very early / current year)
  filter(n_days >= 350)

baseline <- annual %>%
  filter(year >= 1991, year <= 2020) %>%
  summarise(base_tmean = mean(mean_tmean, na.rm = TRUE)) %>%
  pull(base_tmean)

annual <- annual %>%
  mutate(anomaly = mean_tmean - baseline)

# A theme to reuse across plots for a consistent, publication-ready look
theme_oxford <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title    = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(color = "grey40", size = 11),
      plot.caption  = element_text(color = "grey50", size = 8),
      panel.grid.minor = element_blank(),
      axis.title = element_text(color = "grey30")
    )
}

# -------------------------------------------------------------------------
# 3. PLOT — Annual mean temperature, 1815–present, with 10y rolling mean
# -------------------------------------------------------------------------

p_annual_temp <- annual %>%
  mutate(roll10 = rollmean(mean_tmean, 10, fill = NA, align = "right")) %>%
  ggplot(aes(year, mean_tmean)) +
  geom_line(color = "grey70", linewidth = 0.4) +
  geom_line(aes(y = roll10), color = "#B5311A", linewidth = 1.1) +
  labs(
    title = "Oxford (Radcliffe Observatory): Annual Mean Temperature",
    subtitle = "Daily series since 1815 | red line = 10-year rolling mean",
    x = NULL, y = "°C",
    caption = "Source: Radcliffe Meteorological Station / Burt & Burt (2019), updated"
  ) +
  theme_oxford()

# -------------------------------------------------------------------------
# 4. PLOT — "Warming stripes" style anomaly bars vs 1991-2020 baseline
# -------------------------------------------------------------------------

p_stripes <- annual %>%
  ggplot(aes(year, anomaly, fill = anomaly)) +
  geom_col(width = 1) +
  scale_fill_gradient2(
    low = "#08519c", mid = "white", high = "#a50f15",
    midpoint = 0, name = "Anomaly (°C)"
  ) +
  labs(
    title = "Oxford Temperature Anomaly, 1815–present",
    subtitle = "Relative to 1991–2020 average annual mean temperature",
    x = NULL, y = "Anomaly (°C)",
    caption = "Source: Radcliffe Meteorological Station, Oxford"
  ) +
  theme_oxford() +
  theme(legend.position = "bottom")

# -------------------------------------------------------------------------
# 5. PLOT — Monthly climatology heatmap (mean Tmean by month x decade)
# -------------------------------------------------------------------------

monthly_decade <- oxford %>%
  mutate(decade = (year %/% 10) * 10) %>%
  group_by(decade, month) %>%
  summarise(mean_tmean = mean(tmean, na.rm = TRUE), .groups = "drop") %>%
  filter(decade >= 1820)   # drop the partial first decade

p_heatmap <- monthly_decade %>%
  ggplot(aes(x = factor(month, labels = month.abb), y = factor(decade), fill = mean_tmean)) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_fill_viridis_c(option = "inferno", name = "°C") +
  labs(
    title = "Monthly Mean Temperature by Decade — Oxford",
    subtitle = "Each cell = average daily mean temperature for that month/decade",
    x = NULL, y = NULL
  ) +
  theme_oxford() +
  theme(panel.grid = element_blank())

# -------------------------------------------------------------------------
# 6. PLOT — Annual rainfall totals with trend
# -------------------------------------------------------------------------

p_rain <- annual %>%
  filter(year >= 1827) %>%   # daily rainfall record starts here
  ggplot(aes(year, total_rain)) +
  geom_col(fill = "#2c7fb8", alpha = 0.7, width = 1) +
  geom_smooth(method = "loess", se = FALSE, color = "#08306b", linewidth = 1) +
  labs(
    title = "Oxford Annual Rainfall Total",
    subtitle = "Daily rainfall record begins 1827 (loess trend in dark blue)",
    x = NULL, y = "Rainfall (mm)"
  ) +
  theme_oxford()

# -------------------------------------------------------------------------
# 7. PLOT — Annual sunshine hours (record begins 1921 daily / 1880 monthly)
# -------------------------------------------------------------------------

p_sun <- annual %>%
  filter(year >= 1921) %>%
  ggplot(aes(year, total_sun)) +
  geom_line(color = "grey60") +
  geom_point(color = "#E69F00", size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, color = "#7F4F00", fill = "#E69F00", alpha = 0.15) +
  labs(
    title = "Oxford Annual Sunshine Duration",
    subtitle = "Daily sunshine record begins 1921",
    x = NULL, y = "Hours"
  ) +
  theme_oxford()

# -------------------------------------------------------------------------
# 8. PLOT — Distribution shift: summer (JJA) max temps by decade (boxplot)
# -------------------------------------------------------------------------

summer_box <- oxford %>%
  filter(month %in% 6:8) %>%
  mutate(decade = factor((year %/% 10) * 10)) %>%
  filter(as.numeric(as.character(decade)) >= 1820)

p_summer_box <- summer_box %>%
  ggplot(aes(decade, tmax)) +
  geom_boxplot(outlier.alpha = 0.15, fill = "#fdae6b", color = "grey30") +
  labs(
    title = "Distribution of Summer (Jun–Aug) Daily Maximum Temperatures",
    subtitle = "By decade — Oxford, Radcliffe Observatory",
    x = NULL, y = "Tmax (°C)"
  ) +
  theme_oxford() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# -------------------------------------------------------------------------
# 9. SAVE OUTPUTS
# -------------------------------------------------------------------------

dir.create("oxford_plots", showWarnings = FALSE)

ggsave("oxford_plots/01_annual_mean_temp.png",  p_annual_temp,  width = 10, height = 6, dpi = 300)
ggsave("oxford_plots/02_warming_stripes.png",   p_stripes,      width = 10, height = 5, dpi = 300)
ggsave("oxford_plots/03_monthly_heatmap.png",   p_heatmap,      width = 9,  height = 8, dpi = 300)
ggsave("oxford_plots/04_annual_rainfall.png",   p_rain,         width = 10, height = 6, dpi = 300)
ggsave("oxford_plots/05_annual_sunshine.png",   p_sun,          width = 10, height = 6, dpi = 300)
ggsave("oxford_plots/06_summer_tmax_boxplot.png", p_summer_box, width = 11, height = 6, dpi = 300)

# To view interactively instead of saving, just call e.g. p_annual_temp