



########################################################################
# "The Red Queen" – productivity growth since 2000
# One standalone code block; paste after the data-loading section of
# oecd_productivity_levels.R, or run independently (needs df already built).
#
# If running standalone, build df first:
#
#   library(dplyr); library(tidyr)
#   raw <- read.csv("OECD_SDD_TPS_DSD_PDB_DF_PDB_LV_1_0__A_GDPHRS______.csv",
#                   stringsAsFactors = FALSE, check.names = FALSE)
#   df <- raw |>
#     filter(PRICE_BASE == "Q", UNIT_MEASURE == "USD_PPP_H") |>
#     rename(country = `Reference area`, year = TIME_PERIOD, value = `OBS_VALUE`) |>
#     select(country, year, value) |>
#     mutate(year = as.integer(year), value = as.numeric(value)) |>
#     filter(!is.na(value))
########################################################################

library(ggplot2)
library(dplyr)
library(scales)

# ── Country groups ────────────────────────────────────────────────────────────
WESTERN  <- c("Spain", "France", "Germany", "Netherlands", "Portugal")
EASTERN  <- c("Romania", "Lithuania", "Poland", "Estonia")
FOCUS    <- c(WESTERN, EASTERN)

# ── Colours ───────────────────────────────────────────────────────────────────
COL_SPAIN   <- "#C60B1E"
COL_WESTERN <- "#6A8FBF"   # muted blue — all Western peers share this
COL_EASTERN <- "#4A9860"   # muted green — all Eastern countries share this

# ── Build indexed series (2000 = 100) ─────────────────────────────────────────
red_queen <- df |>
  filter(country %in% FOCUS, year >= 2000) |>
  group_by(country) |>
  mutate(base = value[year == 2000][1],
         idx  = value / base * 100) |>
  filter(!is.na(base)) |>
  ungroup() |>
  mutate(
    group = case_when(
      country == "Spain"           ~ "Spain",
      country %in% WESTERN         ~ "Western peers",
      country %in% EASTERN         ~ "Eastern Europe",
      TRUE                          ~ "Other"
    ),
    group = factor(group, levels = c("Spain", "Western peers", "Eastern Europe"))
  )

# End-of-line label positions
end_labels <- red_queen |>
  group_by(country) |>
  filter(year == max(year)) |>
  ungroup()

# ── Plot ──────────────────────────────────────────────────────────────────────
col_map   <- c("Spain" = COL_SPAIN, "Western peers" = COL_WESTERN, "Eastern Europe" = COL_EASTERN)
width_map <- c("Spain" = 1.6,       "Western peers" = 0.9,         "Eastern Europe" = 0.9)
alpha_map <- c("Spain" = 1.0,       "Western peers" = 0.75,        "Eastern Europe" = 0.85)

p <- ggplot(red_queen,
            aes(x = year, y = idx,
                colour    = group,
                linewidth = group,
                alpha     = group,
                group     = country)) +
  # ── faint band showing the spread of Western peers (excl. Spain) ──────────
  stat_summary(
    data     = filter(red_queen, group == "Western peers"),
    aes(x = year, y = idx, group = 1),
    fun.min  = min,
    fun.max  = max,
    geom     = "ribbon",
    alpha    = 0.08,
    fill     = COL_WESTERN,
    colour   = NA,
    inherit.aes = FALSE
  ) +
  geom_line() +
  # ── reference line ────────────────────────────────────────────────────────
  geom_hline(yintercept = 100, linetype = "dashed",
             colour = "grey60", linewidth = 0.35) +
  # ── end-of-line country labels ────────────────────────────────────────────
  geom_text(
    data        = end_labels,
    aes(label   = country, colour = group),
    hjust       = -0.08,
    size        = 2.9,
    fontface    = ifelse(end_labels$country == "Spain", "bold", "plain"),
    show.legend = FALSE
  ) +
  # ── scales ───────────────────────────────────────────────────────────────
  scale_colour_manual(
    values = col_map,
    name   = NULL,
    guide  = guide_legend(override.aes = list(linewidth = 1.4))
  ) +
  scale_linewidth_manual(values = width_map, guide = "none") +
  scale_alpha_manual(values    = alpha_map,  guide = "none") +
  scale_x_continuous(breaks = seq(2000, 2024, by = 4),
                     expand = expansion(mult = c(0.01, 0.14))) +
  scale_y_continuous(labels = label_number(accuracy = 1),
                     breaks = seq(80, 320, by = 40)) +
  coord_cartesian(clip = "off") +
  # ── labels ────────────────────────────────────────────────────────────────
  labs(
    title    = "The Red Queen: labour productivity since 2000  (2000 = 100)",
    subtitle = paste0(
      "Spain (red) grew ~20% \u2014 indistinguishable from its Western European peers.",
      "\nEastern Europe (green) surged 100\u2013200%, converging on Western levels."
    ),
    x        = NULL,
    y        = "Index  (2000 = 100)",
    caption  = paste0(
      "\u201cIt takes all the running you can do, to keep in the same place.\u201d",
      "  \u2014 Lewis Carroll\n",
      "Source: OECD Productivity Database. GDP per hour worked, USD PPP, constant 2020 prices."
    )
  ) +
  # ── theme ─────────────────────────────────────────────────────────────────
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.4),
    plot.title         = element_text(face = "bold", size = 13, margin = margin(b = 4)),
    plot.subtitle      = element_text(size = 9.5, colour = "grey35",
                                      lineheight = 1.4, margin = margin(b = 12)),
    plot.caption       = element_text(size = 8, colour = "grey50",
                                      lineheight = 1.4, hjust = 0,
                                      margin = margin(t = 10)),
    legend.position    = "top",
    legend.text        = element_text(size = 9),
    legend.key.size    = unit(0.5, "cm"),
    plot.margin        = margin(10, 80, 10, 10)
  )

ggsave("red_queen_productivity.pdf", p, width = 11, height = 6.5)
ggsave("red_queen_productivity.png", p, width = 11, height = 6.5, dpi = 300)
message("\u2713 red_queen_productivity.pdf / .png saved.")
########################################################################
