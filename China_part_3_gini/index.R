# ── Inequality: income-based Gini from SWIID ─────────────────────────────────
# Source: SWIID v9.92 (Solt, 2026); World Bank (GDP per capita)
# Eduardo Fé | University of Manchester

setwd("/home/claude")

library(ggplot2)
library(dplyr)
library(tidyr)
library(jsonlite)

col_china  <- "#D62728"
col_other  <- "#4A7C94"
col_mkt    <- "#E07B39"
col_disp   <- "#2E5266"
col_bg     <- "#FAFAFA"
col_grid   <- "#E0E0E0"
col_text   <- "#2B2B2B"

keep <- c("China", "Argentina", "Brazil", "Mexico", "Malaysia",
          "Kazakhstan", "Montenegro", "Dominican Republic",
          "Belarus", "Mauritius")

# =============================================================================
# 1. DOWNLOAD SWIID FROM GITHUB
# =============================================================================

swiid_url <- "https://raw.githubusercontent.com/fsolt/swiid/master/data/swiid_summary.csv"

cat("Downloading SWIID from GitHub...\n")
swiid <- read.csv(url(swiid_url), stringsAsFactors = FALSE)
cat("SWIID loaded:", nrow(swiid), "rows,", length(unique(swiid$country)),
    "countries\n")

# Most recent year per country
swiid_latest <- swiid %>%
  filter(!is.na(gini_disp), !is.na(gini_mkt)) %>%
  arrange(country, desc(year)) %>%
  group_by(country) %>%
  slice(1) %>%
  ungroup()

# Filter to selected countries
df <- swiid_latest %>%
  filter(country %in% keep) %>%
  select(country, year, gini_disp, gini_disp_se, gini_mkt, gini_mkt_se)

cat("\nSelected countries:\n")
print(df %>% arrange(desc(gini_disp)) %>%
        select(country, year, gini_disp, gini_mkt))

# =============================================================================
# 2. DOWNLOAD GDP PER CAPITA FROM WB (for subtitle context)
# =============================================================================

get_wb_gdp <- function() {
  url <- paste0(
    "https://api.worldbank.org/v2/country/all/indicator/NY.GDP.PCAP.CD",
    "?format=json&per_page=20000&mrnev=1"
  )
  raw <- fromJSON(url)
  raw[[2]] %>%
    transmute(country = country$value, gdp_pc = value) %>%
    filter(!is.na(gdp_pc))
}

gdp_ok <- tryCatch({
  gdp <- get_wb_gdp()
  # Merge — name matching may need adjustment
  df <<- df %>%
    left_join(gdp, by = "country")
  TRUE
}, error = function(e) {
  cat("WB API unavailable, adding hardcoded GDP.\n")
  FALSE
})

if (!gdp_ok) {
  gdp_hc <- data.frame(
    country = keep,
    gdp_pc = c(13862, 14898, 10713, 13889, 13125,
               14692, 14817, 11059, 10279, 12991)
  )
  df <- df %>% left_join(gdp_hc, by = "country")
}

df$is_china <- ifelse(df$country == "China", "China", "Other")

# =============================================================================
# 3. CHART 1: DISPOSABLE INCOME GINI (post-tax, post-transfer)
# =============================================================================

df_sorted_disp <- df %>%
  arrange(gini_disp) %>%
  mutate(
    label = paste0(country, " (", year, ")"),
    label = factor(label, levels = label)
  )

p_disp <- ggplot(df_sorted_disp,
                 aes(x = gini_disp, y = label, fill = is_china)) +
  geom_col(width = 0.6) +
  geom_errorbarh(aes(xmin = gini_disp - 1.96 * gini_disp_se,
                     xmax = gini_disp + 1.96 * gini_disp_se),
                 height = 0.25, colour = col_text, linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.1f", gini_disp)),
            hjust = -0.3, size = 4.5, colour = col_text, fontface = "bold") +
  geom_vline(xintercept = 40, linetype = "dashed",
             colour = col_china, linewidth = 0.5) +
  annotate("text", x = 41, y = 1.5,
           label = "UN warning\nlevel (40)",
           colour = col_china, size = 3.2, hjust = 0,
           fontface = "italic", lineheight = 0.9) +
  scale_fill_manual(values = c("China" = col_china, "Other" = col_other),
                    guide = "none") +
  scale_x_continuous(limits = c(0, 60),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "China's income inequality: the SWIID picture",
    subtitle = paste0(
      "Gini of disposable income (post-tax, post-transfer)\n",
      "Income-based, standardised to LIS methodology. ",
      "Error bars = 95% CI."
    ),
    x = "Gini coefficient (disposable income)",
    y = NULL,
    caption = paste0(
      "Source: SWIID v9.92 (Solt, 2026). ",
      "Most recent year per country.\n",
      "Unlike World Bank figures, these are income-based, ",
      "not consumption-based. @EduardoFe"
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
    plot.title    = element_text(colour = col_text, size = 16, face = "bold",
                                 margin = margin(b = 4)),
    plot.subtitle = element_text(colour = "#555555", size = 10.5,
                                 margin = margin(b = 12), lineheight = 1.1),
    plot.caption  = element_text(colour = "#888888", size = 8, hjust = 0,
                                 margin = margin(t = 10)),
    plot.margin   = margin(20, 25, 15, 15)
  )

ggsave("gini_swiid_disp.png", p_disp,
       width = 10, height = 6, dpi = 300, bg = col_bg)

# =============================================================================
# 4. CHART 2: MARKET vs DISPOSABLE (paired bars — shows redistribution)
# =============================================================================

df_paired <- df %>%
  select(country, year, gini_disp, gini_mkt, is_china) %>%
  pivot_longer(cols = c(gini_disp, gini_mkt),
               names_to = "type", values_to = "gini") %>%
  mutate(
    type = factor(type,
                  levels = c("gini_mkt", "gini_disp"),
                  labels = c("Market income\n(pre-tax)",
                             "Disposable income\n(post-tax)"))
  )

# Order by market Gini
order_mkt <- df %>% arrange(gini_mkt) %>% pull(country)
df_paired$country <- factor(df_paired$country, levels = order_mkt)

p_paired <- ggplot(df_paired,
                   aes(x = gini, y = country, fill = type)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_text(aes(label = sprintf("%.1f", gini)),
            position = position_dodge(width = 0.75),
            hjust = -0.08, size = 3.5, colour = col_text, fontface = "bold") +
  geom_vline(xintercept = 40, linetype = "dashed",
             colour = col_china, linewidth = 0.4) +
  scale_fill_manual(values = c("Market income\n(pre-tax)" = col_mkt,
                               "Disposable income\n(post-tax)" = col_disp),
                    name = NULL) +
  scale_x_continuous(limits = c(0, 60),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "How much does the state reduce inequality?",
    subtitle = paste0(
      "Market income Gini (before taxes/transfers) vs ",
      "disposable income Gini (after)\n",
      "The gap between the two bars = redistribution."
    ),
    x = "Gini coefficient",
    y = NULL,
    caption = paste0(
      "Source: SWIID v9.92 (Solt, 2026). ",
      "Most recent year per country.\n",
      "Small gap = low redistribution. @EduardoFe"
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
    plot.title    = element_text(colour = col_text, size = 16, face = "bold",
                                 margin = margin(b = 4)),
    plot.subtitle = element_text(colour = "#555555", size = 10.5,
                                 margin = margin(b = 12), lineheight = 1.1),
    plot.caption  = element_text(colour = "#888888", size = 8, hjust = 0,
                                 margin = margin(t = 10)),
    plot.margin   = margin(20, 25, 15, 15),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 11)
  )

ggsave("gini_swiid_paired.png", p_paired,
       width = 11, height = 7, dpi = 300, bg = col_bg)

# =============================================================================
# 5. CHART 3: REDISTRIBUTION GAP
# =============================================================================

df$redistribution <- df$gini_mkt - df$gini_disp
df$redist_pct <- df$redistribution / df$gini_mkt * 100

df_redist <- df %>%
  arrange(redist_pct) %>%
  mutate(
    label = paste0(country, " (", year, ")"),
    label = factor(label, levels = label)
  )

p_redist <- ggplot(df_redist,
                   aes(x = redist_pct, y = label, fill = is_china)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%.0f%%", redist_pct)),
            hjust = -0.15, size = 4.5, colour = col_text, fontface = "bold") +
  scale_fill_manual(values = c("China" = col_china, "Other" = col_other),
                    guide = "none") +
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "China redistributes less than almost all its peers",
    subtitle = paste0(
      "Relative redistribution: % reduction in Gini from market to disposable income\n",
      "Higher = state does more to reduce inequality through taxes and transfers."
    ),
    x = "Relative redistribution (%)",
    y = NULL,
    caption = paste0(
      "Source: SWIID v9.92 (Solt, 2026).\n",
      "Redistribution = (Gini_market - Gini_disposable) / Gini_market. @EduardoFe"
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
    plot.title    = element_text(colour = col_text, size = 16, face = "bold",
                                 margin = margin(b = 4)),
    plot.subtitle = element_text(colour = "#555555", size = 10.5,
                                 margin = margin(b = 12), lineheight = 1.1),
    plot.caption  = element_text(colour = "#888888", size = 8, hjust = 0,
                                 margin = margin(t = 10)),
    plot.margin   = margin(20, 25, 15, 15)
  )

ggsave("gini_swiid_redist.png", p_redist,
       width = 10, height = 6, dpi = 300, bg = col_bg)

# =============================================================================
# 6. SUMMARY
# =============================================================================

cat("\n=== SWIID INCOME-BASED GINI (most recent year) ===\n")
cat(sprintf("%-20s Year  Mkt   Disp  Redist  Redist%%\n", "Country"))
df_print <- df %>% arrange(desc(gini_disp))
for (i in seq_len(nrow(df_print))) {
  r <- df_print[i, ]
  cat(sprintf("%-20s %4d  %4.1f  %4.1f  %5.1f   %4.0f%%\n",
              r$country, r$year, r$gini_mkt, r$gini_disp,
              r$redistribution, r$redist_pct))
}

cat("\nDone. 3 charts saved.\n")