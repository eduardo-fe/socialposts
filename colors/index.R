# =============================================================================
# Colour Words in Literature vs Economic Cycles
# =============================================================================
# Data sources:
#   - Google Books Ngram Viewer (via `ngramr` CRAN package)
#     Corpus: English, 1800–2019, ~2 trillion words
#     Michel et al. (2011), Science 331(6014):176–182
#
#   - NBER recession dates (US): https://www.nber.org/research/business-cycle-dating
#   - Maddison Project GDP data (optional extension):
#     https://www.rug.nl/ggdc/historicaldevelopment/maddison/
#
# Requires R >= 4.0.0
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Install and load packages
# -----------------------------------------------------------------------------

packages <- c(
  "ngramr",      # Query Google Books Ngram Viewer
  "dplyr",       # Data wrangling
  "tidyr",       # Reshaping data
  "ggplot2",     # Plotting
  "scales",      # Axis formatting
  "zoo",         # Rolling averages
  "corrr",       # Correlation analysis
  "patchwork",   # Combine ggplots
  "readr",       # CSV import (for Maddison data)
  "ggrepel"      # Non-overlapping text labels
)

# Install any missing packages
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if (length(new_packages)) install.packages(new_packages)

invisible(lapply(packages, library, character.only = TRUE))


# -----------------------------------------------------------------------------
# 2. Define colour words and recession periods
# -----------------------------------------------------------------------------

COLOUR_WORDS <- c(
  "black", "white", "red", "blue", "green",
  "grey", "gray", "golden", "purple", "pink" 
)

# NBER US recession periods (start year, end year)
# Source: https://www.nber.org/research/business-cycle-dating
RECESSIONS <- tibble::tribble(
  ~label,                  ~start, ~end,
  "Panic of 1819",          1819,   1822,
  "Panic of 1837",          1837,   1843,
  "Long Depression",        1873,   1879,
  "Panic of 1893",          1893,   1897,
  "Panic of 1907",          1907,   1908,
  "WW1 / Post-war",         1914,   1921,
  "Great Depression",       1929,   1939,
  "WW2",                    1939,   1945,
  "Post-war recession",     1948,   1949,
  "1953 recession",         1953,   1954,
  "1957 recession",         1957,   1958,
  "1960 recession",         1960,   1961,
  "1969 recession",         1969,   1970,
  "Oil Crisis",             1973,   1975,
  "Double-dip 1980s",       1980,   1982,
  "1990 recession",         1990,   1991,
  "Dot-com bust",           2001,   2002,
  "Global Financial Crisis",2007,   2009,
  "COVID-19",               2020,   2021
)

# Create a year-level indicator: 1 = recession year, 0 = expansion
YEAR_RANGE <- 1800:2022
recession_years <- YEAR_RANGE[sapply(YEAR_RANGE, function(y) {
  any(y >= RECESSIONS$start & y <= RECESSIONS$end)
})]


# -----------------------------------------------------------------------------
# 3. Fetch Ngram data
# -----------------------------------------------------------------------------
# ngramr scrapes the Google Ngram JSON endpoint.
# corpus options: "en" (English), "en-fiction", "en-GB", "en-US"
# smoothing = 0 gives raw annual frequencies
#
# NOTE: Google rate-limits this endpoint. If you fetch many words,
# add Sys.sleep(2) between calls or use chunk() from ngramr.

fetch_ngrams <- function(words, corpus = "en", start = 1800, end = 2019,
                         smoothing = 0) {
  message("Fetching ngrams for: ", paste(words, collapse = ", "))
  result <- ngramr::ngram(
    phrases    = words,
    corpus     = corpus,
    year_start = start,
    year_end   = end,
    smoothing  = smoothing,
    count      = FALSE   # return proportions (frequency per word in corpus)
  )
  if (is.null(result)) stop("ngramr returned NULL — check your connection or Google may have changed their API.")
  result
}

# Fetch in batches of 5 to avoid rate limits
batch1 <- fetch_ngrams(COLOUR_WORDS[1:5])
Sys.sleep(2)
batch2 <- fetch_ngrams(COLOUR_WORDS[6:10])

ngram_raw <- bind_rows(batch1, batch2)

# Rename columns for clarity
ngram_df <- ngram_raw |>
  rename(word = Phrase, year = Year, frequency = Frequency) |>
  mutate(
    word = tolower(word),
    # Convert to per-million for readability
    freq_per_million = frequency * 1e6
  )

# Combine grey/gray as one series
ngram_df <- ngram_df |>
  mutate(word = if_else(word == "gray", "grey", word)) |>
  group_by(word, year) |>
  summarise(
    freq_per_million = sum(freq_per_million),
    .groups = "drop"
  )

message("Data fetched. Years: ", min(ngram_df$year), "–", max(ngram_df$year))
message("Words: ", paste(unique(ngram_df$word), collapse = ", "))


# -----------------------------------------------------------------------------
# 4. Compute rolling averages and recession flag
# -----------------------------------------------------------------------------

ngram_df <- ngram_df |>
  group_by(word) |>
  arrange(year) |>
  mutate(
    freq_smooth_5yr  = zoo::rollmean(freq_per_million, k = 5,  fill = NA, align = "center"),
    freq_smooth_10yr = zoo::rollmean(freq_per_million, k = 10, fill = NA, align = "center"),
    pct_change_yoy   = (freq_per_million / lag(freq_per_million) - 1) * 100
  ) |>
  ungroup() |>
  mutate(in_recession = year %in% recession_years)

# Normalise as share of all colour words combined (optional view)
total_by_year <- ngram_df |>
  group_by(year) |>
  summarise(total_freq = sum(freq_per_million), .groups = "drop")

ngram_df <- ngram_df |>
  left_join(total_by_year, by = "year") |>
  mutate(pct_of_colour_total = freq_per_million / total_freq * 100)


# -----------------------------------------------------------------------------
# 5. Colour palette (accessible)
# -----------------------------------------------------------------------------

word_colours <- c(
  "black"  = "#2C2C2A",
  "white"  = "#888780",
  "red"    = "#D85A30",
  "blue"   = "#378ADD",
  "green"  = "#639922",
  "grey"   = "#B4B2A9",
  "golden" = "#EF9F27",
  "purple" = "#E03DF8",
  "pink"   = "#FF6FFF"
  
)


# -----------------------------------------------------------------------------
# 6. Plot 1: Raw frequency over time with recession shading
# -----------------------------------------------------------------------------

recession_rects <- RECESSIONS |>
  filter(start >= 1800, end <= 2022)

p1 <- ggplot(ngram_df, aes(x = year, y = freq_smooth_10yr, colour = word)) +
  
  # Recession shading (behind lines)
  geom_rect(
    data = recession_rects,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "#D85A30", alpha = 0.08
  ) +
  
  geom_line(linewidth = 0.7, na.rm = TRUE) +
  
  scale_colour_manual(values = word_colours, name = NULL) +
  scale_x_continuous(breaks = seq(1800, 2020, 20)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "Colour word frequency in English books, 1800–2019",
    subtitle = "10-year rolling average · per million words · shaded areas = recession periods",
    x        = NULL,
    y        = "Frequency (per million words)",
    caption  = "Source: Google Books Ngram Viewer (English corpus) via ngramr · NBER recession dates"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "top",
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold"),
    plot.caption     = element_text(colour = "grey50", size = 9)
  )

print(p1)


# -----------------------------------------------------------------------------
# 7. Plot 2: Recession vs expansion average frequency (bar chart)
# -----------------------------------------------------------------------------

recession_comparison <- ngram_df |>
  group_by(word, in_recession) |>
  summarise(mean_freq = mean(freq_per_million, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = in_recession, values_from = mean_freq,
              names_prefix = "rec_") |>
  rename(expansion = rec_FALSE, recession = rec_TRUE) |>
  mutate(
    pct_diff = (recession - expansion) / expansion * 100,
    direction = if_else(pct_diff > 0, "Higher in recessions", "Lower in recessions")
  ) |>
  arrange(desc(pct_diff))

p2 <- ggplot(recession_comparison,
             aes(x = reorder(word, pct_diff), y = pct_diff, fill = direction)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey40") +
  geom_text(aes(label = paste0(round(pct_diff, 1), "%"),
                hjust = if_else(pct_diff >= 0, -0.15, 1.15)),
            size = 3.2, colour = "grey20") +
  coord_flip() +
  scale_fill_manual(
    values = c("Higher in recessions" = "#D85A30", "Lower in recessions" = "#378ADD"),
    name = NULL
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     expand = expansion(mult = c(0.05, 0.15))) +
  labs(
    title    = "Word frequency: recession years vs expansion years",
    subtitle = "% difference in mean annual frequency · 1800–2019",
    x        = NULL,
    y        = "% difference from expansion baseline",
    caption  = "Source: Google Books Ngram Viewer · NBER recession dates"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(colour = "grey50", size = 9)
  )

print(p2)


# -----------------------------------------------------------------------------
# 8. Plot 3: Normalised share (% of all colour words) — removes secular trends
# -----------------------------------------------------------------------------

p3 <- ggplot(ngram_df |> filter(!is.na(pct_of_colour_total)),
             aes(x = year, y = pct_of_colour_total, colour = word)) +
  
  geom_rect(
    data = recession_rects,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "#D85A30", alpha = 0.08
  ) +
  
  geom_line(linewidth = 0.7, alpha = 0.85, na.rm = TRUE) +
  
  scale_colour_manual(values = word_colours, name = NULL) +
  scale_x_continuous(breaks = seq(1800, 2020, 20)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title    = "Colour word share of all colour words, 1800–2019",
    subtitle = "Normalised to remove secular word-frequency trends · shaded = recessions",
    x        = NULL,
    y        = "% of all colour word occurrences",
    caption  = "Source: Google Books Ngram Viewer · NBER recession dates"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "top",
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold"),
    plot.caption     = element_text(colour = "grey50", size = 9)
  )

print(p3)


# -----------------------------------------------------------------------------
# 9. Correlation analysis: word frequency vs recession indicator
# -----------------------------------------------------------------------------

# Pearson correlation for each word
cor_results <- ngram_df |>
  group_by(word) |>
  summarise(
    pearson_r   = cor(freq_per_million, as.integer(in_recession),
                      use = "complete.obs", method = "pearson"),
    spearman_r  = cor(freq_per_million, as.integer(in_recession),
                      use = "complete.obs", method = "spearman"),
    n_years     = sum(!is.na(freq_per_million)),
    .groups     = "drop"
  ) |>
  arrange(desc(pearson_r))

print(cor_results)


# -----------------------------------------------------------------------------
# 10. Cross-correlation: does frequency LEAD or LAG economic downturns?
# -----------------------------------------------------------------------------
# We check whether word frequencies predict recessions 1–5 years ahead

cross_cor_analysis <- function(word_data, recession_vec, max_lag = 5) {
  freq <- word_data$freq_per_million
  rec  <- as.integer(word_data$year %in% recession_vec)
  ccf_result <- ccf(freq, rec, lag.max = max_lag, plot = FALSE)
  tibble(
    lag = as.integer(ccf_result$lag),
    acf = as.numeric(ccf_result$acf)
  )
}

cross_cors <- ngram_df |>
  group_by(word) |>
  group_modify(~ cross_cor_analysis(.x, recession_years)) |>
  ungroup()

p4 <- ggplot(cross_cors, aes(x = lag, y = acf, fill = acf > 0)) +
  geom_col(width = 0.6) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  facet_wrap(~ word, nrow = 2) +
  scale_fill_manual(values = c("TRUE" = "#D85A30", "FALSE" = "#378ADD"),
                    guide = "none") +
  labs(
    title    = "Cross-correlation: colour word frequency vs recession indicator",
    subtitle = "Negative lag = word frequency leads recessions · Positive = lags",
    x        = "Lag (years)",
    y        = "Correlation",
    caption  = "Source: Google Books Ngram Viewer · NBER recession dates"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text       = element_text(face = "bold"),
    plot.title       = element_text(face = "bold"),
    plot.caption     = element_text(colour = "grey50", size = 9)
  )

print(p4)


# -----------------------------------------------------------------------------
# 11. (Optional) Correlate with real GDP — requires Maddison Project data
# -----------------------------------------------------------------------------
# Download from: https://www.rug.nl/ggdc/historicaldevelopment/maddison/releases/maddison-project-database-2023
# File: mpd2023.xlsx — convert to CSV or read directly with readxl

# Uncomment and adjust path to use:
#
# library(readxl)
# maddison <- read_excel("mpd2023.xlsx", sheet = "Full data") |>
#   filter(country == "USA") |>
#   select(year, gdppc) |>
#   mutate(gdp_growth = (gdppc / lag(gdppc) - 1) * 100)
#
# ngram_with_gdp <- ngram_df |>
#   left_join(maddison, by = "year")
#
# gdp_cors <- ngram_with_gdp |>
#   group_by(word) |>
#   summarise(
#     cor_with_gdp_growth = cor(freq_per_million, gdp_growth,
#                               use = "complete.obs"),
#     .groups = "drop"
#   ) |>
#   arrange(cor_with_gdp_growth)
#
# print(gdp_cors)


# -----------------------------------------------------------------------------
# 12. Save outputs
# -----------------------------------------------------------------------------

# Save combined plot
combined_plot <- p1 / p2 / p3 +
  plot_annotation(
    title   = "Colours in Literature and the Economy, 1800–2019",
    caption = "Google Books Ngram Viewer · NBER recession dates · Analysis in R"
  )

ggsave("colour_economics_plot.pdf", combined_plot,
       width = 12, height = 20, units = "in", device = cairo_pdf)

ggsave("colour_economics_plot.png", combined_plot,
       width = 12, height = 20, units = "in", dpi = 150)

# Save raw data
readr::write_csv(ngram_df, "colour_ngram_data.csv")
readr::write_csv(recession_comparison, "recession_comparison.csv")
readr::write_csv(cor_results, "correlation_results.csv")

message("\nDone. Files written:")
message("  colour_economics_plot.pdf")
message("  colour_economics_plot.png")
message("  colour_ngram_data.csv")
message("  recession_comparison.csv")
message("  correlation_results.csv")