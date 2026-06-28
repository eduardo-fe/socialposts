## ---------------------------------------------------------------------------
## analyse_market_value.R
##
## Squad market values for France, England, Spain, Portugal, Germany
## (5 squads x 26 players, scraped from transfermarkt-style listing).
##
## Focus: variation in market value, and whether England's squad value is
## more *concentrated* (i.e. driven by a smaller number of expensive players)
## than the other squads, rather than just larger on average.
##
## Concentration is operationalised three ways, since "concentration" is
## ambiguous and each measure picks up something different:
##   1. Gini coefficient of player values within each squad
##   2. Share of total squad value held by the top-3 / top-5 players
##   3. Coefficient of variation (sd/mean) of player values
## A squad can have a high CV (lots of variance) without being "concentrated"
## in the sense of star-dependence; the top-N share is the more direct test
## of the claim "talent is concentrated [in a few players] in England".
## ---------------------------------------------------------------------------

suppressMessages({
  library(dplyr)
  library(ggplot2)
})

df <- read.csv("players.csv", stringsAsFactors = FALSE)

## --- Clean ------------------------------------------------------------------

## MarketValue comes as either "€30.00m" (millions) or "€700k" (thousands) --
## both need converting to a common unit (millions of euros).
parse_value_m <- function(x) {
  x <- trimws(x)
  is_k <- grepl("k$", x)
  num <- as.numeric(gsub("[€mk]", "", x))
  ifelse(is_k, num / 1000, num)
}

df <- df %>%
  mutate(
    Country = factor(Country, levels = sort(unique(Country))),
    ValueM = parse_value_m(MarketValue),
    Age = as.numeric(Age),
    HeightM = as.numeric(gsub(",", ".", gsub("m$", "", Height))),
    Caps = suppressWarnings(as.numeric(Caps)),
    Goals = suppressWarnings(as.numeric(Goals))
  )

stopifnot(all(!is.na(df$ValueM)))  # fail loudly if parsing broke anything

## --- Gini coefficient (no external dependency: ineq pkg not assumed) -------

gini <- function(x) {
  x <- sort(x)
  n <- length(x)
  if (n <= 1 || sum(x) == 0) return(NA_real_)
  # standard discrete Gini formula
  (2 * sum(seq_len(n) * x) - (n + 1) * sum(x)) / (n * sum(x))
}

top_n_share <- function(x, n) {
  x <- sort(x, decreasing = TRUE)
  sum(x[seq_len(min(n, length(x)))]) / sum(x)
}

## --- Squad-level summary -----------------------------------------------------

squad_summary <- df %>%
  group_by(Country) %>%
  summarise(
    n_players   = n(),
    total_value = sum(ValueM),
    mean_value  = mean(ValueM),
    median_value = median(ValueM),
    sd_value    = sd(ValueM),
    cv          = sd_value / mean_value,
    gini        = gini(ValueM),
    top3_share  = top_n_share(ValueM, 3),
    top5_share  = top_n_share(ValueM, 5),
    max_value   = max(ValueM),
    .groups = "drop"
  ) %>%
  arrange(desc(gini))

cat("\n=== Squad-level market value summary (€m) ===\n")
print(as.data.frame(squad_summary), row.names = FALSE, digits = 3)

## --- Headline test of "talent is concentrated in England" -----------------
## Compare England's Gini / top-3 share against the other four squads.

cat("\n=== Is England the most concentrated squad? ===\n")
rank_tbl <- squad_summary %>%
  mutate(
    gini_rank = rank(-gini),
    top3_rank = rank(-top3_share),
    total_value_rank = rank(-total_value)
  ) %>%
  select(Country, gini, gini_rank, top3_share, top3_rank, total_value, total_value_rank)
print(as.data.frame(rank_tbl), row.names = FALSE, digits = 3)

england_gini_rank <- rank_tbl$gini_rank[rank_tbl$Country == "England"]
n_squads <- nrow(squad_summary)
cat(sprintf(
  "\nEngland ranks %s of %d on Gini concentration (1 = most concentrated).\n",
  england_gini_rank, n_squads
))
cat(sprintf(
  "England's squad total value (€%.0fm) is %s of %d (1 = largest) -- so check whether\n",
  rank_tbl$total_value[rank_tbl$Country == "England"],
  rank_tbl$total_value_rank[rank_tbl$Country == "England"], n_squads
))
cat("any 'concentration' is really just scale (bigger squads -> bigger absolute gaps).\n")

## --- Decompose: is concentration about total value or distribution shape? -

## Spearman correlation between total squad value and Gini across squads
cor_total_gini <- cor(squad_summary$total_value, squad_summary$gini, method = "spearman")
cat(sprintf("\nSpearman correlation(total squad value, Gini) across %d squads = %.2f (n=%d, indicative only)\n",
            n_squads, cor_total_gini, n_squads))

## --- Position-level check: where does England's value sit? ----------------
## "Concentration" claims about England often mean attack/creative positions.
## Collapse detailed positions into broad lines.

position_map <- c(
  "Goalkeeper" = "GK",
  "Centre-Back" = "DEF", "Left-Back" = "DEF", "Right-Back" = "DEF",
  "Defensive Midfield" = "MID", "Central Midfield" = "MID",
  "Left Midfield" = "MID", "Right Midfield" = "MID",
  "Attacking Midfield" = "MID",
  "Left Winger" = "ATT", "Right Winger" = "ATT",
  "Second Striker" = "ATT", "Centre-Forward" = "ATT"
)

df <- df %>%
  mutate(Line = factor(position_map[Position], levels = c("GK", "DEF", "MID", "ATT")))

line_summary <- df %>%
  group_by(Country, Line) %>%
  summarise(total_value = sum(ValueM), mean_value = mean(ValueM), n = n(), .groups = "drop")

cat("\n=== Total squad value by country and position line (€m) ===\n")
print(
  as.data.frame(
    tidyr_spread_fallback <- reshape(
      as.data.frame(line_summary[, c("Country", "Line", "total_value")]),
      idvar = "Country", timevar = "Line", direction = "wide"
    )
  ),
  row.names = FALSE, digits = 3
)

## --- ANOVA / Kruskal-Wallis: do squads differ in player value distribution? -
## (Sample is the entire population of 26-man squads, not a random sample,
## so treat the test as descriptive, not as formal inference to a population.)

cat("\n=== Kruskal-Wallis test: ValueM by Country (descriptive, not a random sample) ===\n")
print(kruskal.test(ValueM ~ Country, data = df))

cat("\n=== One-way ANOVA on log(ValueM) by Country (values are heavily right-skewed) ===\n")
print(summary(aov(log(ValueM) ~ Country, data = df)))

## --- Plots ------------------------------------------------------------------

dir.create("figures", showWarnings = FALSE)

## National colour palette. Three countries map onto "blue" (France, Norway,
## Argentina) and two onto "red" (Spain, Belgium), so colour alone won't
## distinguish them reliably -- linetype carries the second dimension for
## those pairs/triples. England is white, which needs a black outline to
## read against a light background; handled separately below.
country_colors <- c(
  Argentina   = "#75AADB",  # light blue
  Belgium     = "#9B1B30",  # dark red/maroon
  Brazil      = "#E0BF00",  # yellow
  England     = "#FF0000",  # white (outlined separately)
  France      = "#0055A4",  # blue
  Germany     = "#000000",  # black
  Netherlands = "#F36C21",  # orange
  Norway      = "#00205B",  # dark navy blue
  Portugal    = "#046A38",  # green
  Spain       = "#AA151B"   # red
)

country_linetypes <- c(
  Argentina   = "dotted",  # blue family member 3
  Belgium     = "dashed",  # red family member 2
  Brazil      = "solid",
  England     = "solid",
  France      = "solid",   # blue family member 1
  Germany     = "solid",
  Netherlands = "solid",
  Norway      = "dashed",  # blue family member 2
  Portugal    = "solid",
  Spain       = "solid"    # red family member 1
)

## "Economist"-style theme: pale blue-grey background, white gridlines, no
## minor gridlines, dark grey text, bold sans-serif title.
theme_economist_ish <- function(base_size = 12) {
  econ_bg <- "#D5E4EB"
  theme_minimal(base_size = base_size) +
    theme(
      plot.background  = element_rect(fill = econ_bg, colour = NA),
      panel.background  = element_rect(fill = econ_bg, colour = NA),
      legend.background = element_rect(fill = econ_bg, colour = NA),
      legend.key        = element_rect(fill = econ_bg, colour = NA),
      panel.grid.major  = element_line(colour = "white", linewidth = 0.5),
      panel.grid.minor  = element_blank(),
      text              = element_text(colour = "#3C3C3C"),
      plot.title        = element_text(face = "bold", size = base_size * 1.2, colour = "#222222"),
      plot.subtitle     = element_text(colour = "#555555"),
      axis.title        = element_text(size = base_size * 0.9),
      axis.text         = element_text(colour = "#3C3C3C")
    )
}

## 1. Boxplot of player value by country (log scale - heavy right skew)
## Coloured by national squad rather than position line; England gets a
## thin black outline so the white fill reads against the pale background.
p1 <- ggplot(df, aes(x = Country, y = ValueM, fill = Country)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1.5, colour = "#3C3C3C", linewidth = 0.4) +
  scale_fill_manual(values = country_colors, guide = "none") +
  scale_y_log10() +
  labs(
    title = "Player market value by national squad",
    subtitle = "Log scale; boxes coloured by national squad",
    x = NULL, y = "Market value (\u20acm, log scale)"
  ) +
  theme_economist_ish() +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))

ggsave("figures/boxplot_value_by_country.png", p1, width = 9, height = 5.5, dpi = 150, bg = "#D5E4EB")

## 2. Lorenz curves - the direct visual test of "concentration"
lorenz_points <- df %>%
  group_by(Country) %>%
  arrange(ValueM) %>%
  mutate(
    cum_players = row_number() / n(),
    cum_value = cumsum(ValueM) / sum(ValueM)
  ) %>%
  ungroup()

# add (0,0) origin per country for clean curves
origin <- lorenz_points %>%
  distinct(Country) %>%
  mutate(cum_players = 0, cum_value = 0)

lorenz_points <- bind_rows(origin, lorenz_points[, c("Country", "cum_players", "cum_value")]) %>%
  arrange(Country, cum_players)

p2 <- ggplot(lorenz_points, aes(x = cum_players, y = cum_value,
                                colour = Country, linetype = Country)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
  # black outline pass for England so its white line reads against the
  # pale background -- drawn first, slightly thicker, then white on top.
  geom_line(
    data = filter(lorenz_points, Country == "England"),
    colour = "black", linewidth = 1.8, linetype = "solid"
  ) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = country_colors) +
  scale_linetype_manual(values = country_linetypes) +
  labs(
    title = "Lorenz curves of squad market value",
    subtitle = "Further below the diagonal = more concentrated in a few players",
    x = "Cumulative share of players (lowest value first)",
    y = "Cumulative share of total squad value",
    colour = "Country", linetype = "Country"
  ) +
  theme_economist_ish()

ggsave("figures/lorenz_curves.png", p2, width = 8, height = 6.5, dpi = 150, bg = "#D5E4EB")

## 3. Top-3 share bar chart
p3 <- ggplot(squad_summary, aes(x = reorder(Country, top3_share), y = top3_share, fill = Country)) +
  geom_col(colour = "#3C3C3C", linewidth = 0.3) +
  geom_text(aes(label = scales::percent(top3_share, accuracy = 1)), hjust = -0.1, colour = "#3C3C3C") +
  scale_fill_manual(values = country_colors, guide = "none") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Share of total squad value held by the 3 most valuable players",
    x = NULL, y = "Top-3 share of squad value"
  ) +
  theme_economist_ish()

ggsave("figures/top3_share.png", p3, width = 8, height = 5, dpi = 150, bg = "#D5E4EB")

## 4. Total value vs Gini, with isoquants of value-adjusted-for-evenness
## (value_adj = total_value * (1 - gini)). Points on the same curve have the
## same adjusted value via different routes (high value & high inequality vs
## lower value & low inequality).

squad_summary <- squad_summary %>%
  mutate(value_adj = total_value * (1 - gini))

iso_levels <- c(300, 500, 700, 900)
x_grid <- seq(500, 1600, length.out = 200)

iso_data <- do.call(rbind, lapply(iso_levels, function(k) {
  y <- 1 - k / x_grid
  data.frame(k = k, x = x_grid, y = y)
})) %>%
  filter(y >= 0, y <= 0.65)  # keep within plotted range, avoid runaway curves

iso_labels <- iso_data %>%
  group_by(k) %>%
  slice_max(x, n = 1) %>%
  ungroup()

p4 <- ggplot() +
  geom_line(
    data = iso_data, aes(x = x, y = y, group = k),
    colour = "#9AA7AD", linewidth = 0.5, linetype = "dashed"
  ) +
  geom_text(
    data = iso_labels, aes(x = x, y = y, label = paste0("\u20ac", k, "m")),
    colour = "#6B7780", size = 4, hjust = 0, nudge_x = 15, fontface = "italic"
  ) +
  geom_point(
    data = squad_summary, aes(x = total_value, y = gini, colour = Country),
    size = 4
  ) +
  geom_text(
    data = squad_summary, aes(x = total_value, y = gini, label = Country, colour = Country),
    vjust = -1, size = 4.3, fontface = "bold", show.legend = FALSE
  ) +
  scale_colour_manual(values = country_colors, guide = "none") +
  scale_x_continuous(limits = c(450, 1750), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.30, 0.65), labels = scales::number_format(accuracy = 0.1)) +
  labs(
    title = "World Cup: Squad value vs. internal inequality",
    subtitle = "Dashed lines = constant value-adjusted-for-evenness (total value \u00d7 (1 \u2212 Gini))",
    x = "Total squad value (\u20acm)",
    y = "Gini coefficient (higher = more concentrated)"
  ) +
  theme_economist_ish()

ggsave("figures/value_vs_gini_isoquants.png", p4, width = 9.5, height = 7, dpi = 150, bg = "#D5E4EB")

cat("\nFigures written to ./figures/: boxplot_value_by_country.png, lorenz_curves.png, top3_share.png, value_vs_gini_isoquants.png\n")

## --- Save tidy summary tables for reuse -------------------------------------

write.csv(squad_summary, "squad_summary.csv", row.names = FALSE)
write.csv(line_summary, "line_summary.csv", row.names = FALSE)

cat("\nDone. Summary tables written to squad_summary.csv and line_summary.csv\n")

p4