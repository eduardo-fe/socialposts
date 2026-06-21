# =============================================================================
# UK HE: Students per Academic FTE, 2015/16 → 2024/25
# Data: HESA DT025 Table 6 (Staff FTE), DT051 Table 1 (Student enrolments)
# Russell Group identified by UKPRN
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(forcats)
  library(ragg)
})

# ── Helpers ───────────────────────────────────────────────────────────────────
pn <- function(x) suppressWarnings(as.numeric(gsub(",", "", x)))

read_fte <- function(path) {
  read_csv(path, skip = 14,
           locale = locale(encoding = "UTF-8"),
           show_col_types = FALSE, name_repair = "unique") |>
    filter(!is.na(UKPRN)) |>
    transmute(UKPRN   = as.integer(UKPRN),
              provider = `HE provider`,
              fte      = pn(`Total academic staff (excluding atypical)`))
}

read_students <- function(path) {
  read_csv(path, skip = 17,
           locale = locale(encoding = "UTF-8"),
           show_col_types = FALSE, name_repair = "unique") |>
    filter(!is.na(UKPRN)) |>
    transmute(UKPRN    = as.integer(UKPRN),
              students = pn(Total))
}

# ── Load ──────────────────────────────────────────────────────────────────────
fte_1516 <- read_fte("dt025-table-6-15.csv")
fte_2425 <- read_fte("dt025-table-6-24.csv")
stu_1516 <- read_students("dt051-table-1-15.csv")
stu_2425 <- read_students("dt051-table-1-24.csv")

# ── Merge & clean ─────────────────────────────────────────────────────────────
merge_clean <- function(fte, stu, ratio_col) {
  inner_join(fte, stu, by = "UKPRN") |>
    filter(!is.na(fte), !is.na(students), fte > 0, students > 0) |>
    mutate(!!ratio_col := students / fte)
}

m16 <- merge_clean(fte_1516, stu_1516, "ratio_16") |>
  select(UKPRN, provider, fte_16 = fte, stu_16 = students, ratio_16)

m24 <- merge_clean(fte_2425, stu_2425, "ratio_24") |>
  select(UKPRN, fte_24 = fte, stu_24 = students, ratio_24)

both <- inner_join(m16, m24, by = "UKPRN") |>
  # Exclude distance-learning anomaly (Highlands & Islands, ratio > 100)
  filter(ratio_24 < 80) |>
  mutate(ratio_chg = ratio_24 - ratio_16,
         ratio_pct = ratio_chg / ratio_16 * 100)

cat(sprintf("Universities in analysis: %d\n", nrow(both)))

# ── Russell Group ─────────────────────────────────────────────────────────────
rg_ukprns <- c(
  10006840, 10007786, 10007788, 10007814, 10007143, 10007790,
  10007792, 10007794, 10003270, 10003645, 10007795, 10006842,
  10004063, 10007798, 10007799, 10007154, 10007774, 10007775,
  10005343, 10007157, 10007158, 10007784, 10007163, 10007167
)

name_map <- c(
  "10006840" = "Birmingham",        "10007786" = "Bristol",
  "10007788" = "Cambridge",         "10007814" = "Cardiff",
  "10007143" = "Durham",            "10007790" = "Edinburgh",
  "10007792" = "Exeter",            "10007794" = "Glasgow",
  "10003270" = "Imperial",          "10003645" = "King's College London",
  "10007795" = "Leeds",             "10006842" = "Liverpool",
  "10004063" = "LSE",               "10007798" = "Manchester",
  "10007799" = "Newcastle",         "10007154" = "Nottingham",
  "10007774" = "Oxford",            "10007775" = "Queen Mary",
  "10005343" = "Queen's Belfast",   "10007157" = "Sheffield",
  "10007158" = "Southampton",       "10007784" = "UCL",
  "10007163" = "Warwick",           "10007167" = "York"
)

both <- both |>
  mutate(
    is_rg = UKPRN %in% rg_ukprns,
    name  = coalesce(name_map[as.character(UKPRN)], provider)
  )

rg    <- filter(both, is_rg)
other <- filter(both, !is_rg)

cat(sprintf("Russell Group matched: %d\n\n", nrow(rg)))

# ── Audit table ───────────────────────────────────────────────────────────────
cat("=== STUDENT:FTE RATIO — RUSSELL GROUP ===\n")
rg |>
  arrange(ratio_24) |>
  transmute(
    University  = name,
    FTE_1516    = round(fte_16),
    Stu_1516    = round(stu_16),
    Ratio_1516  = round(ratio_16, 2),
    FTE_2425    = round(fte_24),
    Stu_2425    = round(stu_24),
    Ratio_2425  = round(ratio_24, 2),
    Change      = round(ratio_chg, 2),
    Pct_change  = round(ratio_pct, 1)
  ) |>
  print(n = 24, width = 120)

cat(sprintf("\nRG mean:    2015/16 = %.2f, 2024/25 = %.2f, change = %+.2f\n",
            mean(rg$ratio_16), mean(rg$ratio_24), mean(rg$ratio_chg)))
cat(sprintf("RG median:  2015/16 = %.2f, 2024/25 = %.2f, change = %+.2f\n",
            median(rg$ratio_16), median(rg$ratio_24), median(rg$ratio_chg)))
cat(sprintf("All median: 2015/16 = %.2f, 2024/25 = %.2f, change = %+.2f\n",
            median(both$ratio_16), median(both$ratio_24), median(both$ratio_chg)))

# ── Plot constants ─────────────────────────────────────────────────────────────
bg      <- "#FEFEFE"
grid_c  <- "#ECE9E3"
c_rg    <- "#1A5276"
c_other <- "#B0BEC5"
c_pos   <- "#1A5276"
c_neg   <- "#C0392B"
cap     <- 52          # x/y axis cap for scatter and histograms

n_pos <- sum(rg$ratio_chg > 0)
n_neg <- sum(rg$ratio_chg <= 0)
med   <- median(rg$ratio_chg)

# ── RG bar data (sorted by change) ────────────────────────────────────────────
rg_bar <- rg |>
  arrange(ratio_chg) |>
  mutate(
    name   = fct_reorder(name, ratio_chg),
    colour = if_else(ratio_chg >= 0, c_pos, c_neg),
    label  = sprintf("%.1f \u2192 %.1f", ratio_16, ratio_24),
    hjust  = if_else(ratio_chg >= 0, -0.08, 1.08)
  )

xlim_bar <- c(min(rg_bar$ratio_chg) * 1.6, max(rg_bar$ratio_chg) * 2.4)

# ── PANEL A: Scatter ──────────────────────────────────────────────────────────
# label nudges [x, y] to avoid overlap
label_nudge <- tribble(
  ~name,                   ~nx,    ~ny,
  "Oxford",               -0.4,   -1.4,
  "Cambridge",             0.4,   -1.3,
  "Imperial",              0.4,    0.3,
  "UCL",                  -5.5,    0.5,
  "Edinburgh",            -5.5,   -0.8,
  "Sheffield",             0.4,   -1.0,
  "King's College London", 0.4,    0.3,
  "Queen Mary",            0.4,   -1.0,
  "Bristol",               0.4,    0.3,
  "Manchester",           -6.0,    0.5,
  "LSE",                  -4.5,   -1.1,
  "Leeds",                 0.4,    0.3,
  "Southampton",           0.4,   -1.1,
  "Warwick",              -6.5,    0.5,
  "Glasgow",              -6.5,   -0.9,
  "Durham",                0.4,    0.3,
  "Newcastle",             0.4,   -1.1,
  "York",                 -5.5,    0.5,
  "Liverpool",             0.4,    0.3,
  "Birmingham",           -8.0,   -0.9,
  "Nottingham",            0.4,    0.3,
  "Cardiff",               0.4,   -1.1,
  "Exeter",                0.4,    0.3,
  "Queen's Belfast",     -10.0,    0.5
)

rg_sc <- rg |> left_join(label_nudge, by = "name")

p_scatter <- ggplot() +
  # 45° reference line
  geom_abline(slope = 1, intercept = 0, colour = "#BBBBBB",
              linewidth = 0.9, linetype = "dashed") +
  # All other HEIs
  geom_point(data = other, aes(ratio_16, ratio_24),
             colour = c_other, alpha = 0.5, size = 1.8) +
  # Russell Group
  geom_point(data = rg, aes(ratio_16, ratio_24),
             colour = c_rg, alpha = 0.9, size = 3.0) +
  # Arrows for displaced labels
  geom_segment(
    data = filter(rg_sc, abs(nx) > 1.5 | abs(ny) > 1.0),
    aes(x = ratio_16, y = ratio_24,
        xend = ratio_16 + nx, yend = ratio_24 + ny),
    colour = "#AAAAAA", linewidth = 0.4
  ) +
  # Labels
  geom_text(data = rg_sc,
            aes(x = ratio_16 + nx, y = ratio_24 + ny, label = name),
            size = 2.7, colour = "#1A3A5C", vjust = 0.5) +
  # Fake legend points
  annotate("point", x = -999, y = -999, colour = c_other, size = 2.5, alpha = 0.6,
           shape = 16) +
  annotate("point", x = -999, y = -999, colour = c_rg,    size = 3.5, alpha = 0.9,
           shape = 16) +
  annotate("segment", x = 1, xend = 4, y = 49, yend = 49,
           colour = "#BBBBBB", linewidth = 0.9, linetype = "dashed") +
  annotate("text", x = 4.5, y = 49, label = "No change (diagonal)",
           hjust = 0, size = 3.0, colour = "#555550") +
  annotate("point", x = 2, y = 46, colour = c_other, size = 2.5, alpha = 0.6) +
  annotate("text",  x = 4.5, y = 46,
           label = sprintf("Other HEIs (n=%d)", nrow(other)),
           hjust = 0, size = 3.0, colour = "#555550") +
  annotate("point", x = 2, y = 43, colour = c_rg, size = 3.5, alpha = 0.9) +
  annotate("text",  x = 4.5, y = 43,
           label = sprintf("Russell Group (n=%d)", nrow(rg)),
           hjust = 0, size = 3.0, colour = c_rg) +
  coord_cartesian(xlim = c(0, cap), ylim = c(0, cap)) +
  labs(
    title    = "A.  Student:FTE ratio 2015/16 vs 2024/25  (points above dashed line = ratio rose)",
    x        = "Students per academic FTE, 2015/16",
    y        = "Students per academic FTE, 2024/25"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = bg, colour = NA),
    panel.background = element_rect(fill = bg, colour = NA),
    panel.grid.major = element_line(colour = grid_c, linewidth = 0.4),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold", size = 11, colour = "#1C1C1C",
                                    margin = margin(b = 6)),
    axis.title       = element_text(size = 10, colour = "#3A3A3A"),
    axis.text        = element_text(size = 9,  colour = "#555550"),
    plot.margin      = margin(10, 10, 6, 10)
  )

# ── PANEL B: RG bar chart ─────────────────────────────────────────────────────
p_bar <- ggplot(rg_bar, aes(x = ratio_chg, y = name)) +
  geom_col(aes(fill = colour), width = 0.65, show.legend = FALSE) +
  geom_text(aes(x     = ratio_chg + if_else(ratio_chg >= 0, 0.04, -0.04),
                label = label,
                hjust = hjust),
            size = 2.9, colour = "#2A2A2A") +
  geom_vline(xintercept = 0, colour = "#555550", linewidth = 0.8) +
  scale_fill_identity() +
  scale_x_continuous(
    labels = \(x) paste0(ifelse(x > 0, "+", ""), round(x, 1)),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  coord_cartesian(xlim = xlim_bar) +
  labs(
    title = sprintf(
      "B.  Russell Group: change in student:FTE ratio  (%d universities up \u25b2, %d down \u25bc | median change %+.2f)",
      n_pos, n_neg, med),
    x = "Change in students per FTE (2024/25 \u2212 2015/16)",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background    = element_rect(fill = bg, colour = NA),
    panel.background   = element_rect(fill = bg, colour = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = grid_c, linewidth = 0.4),
    panel.grid.minor   = element_blank(),
    plot.title         = element_text(face = "bold", size = 11, colour = "#1C1C1C",
                                      margin = margin(b = 6)),
    axis.title.x       = element_text(size = 10, colour = "#3A3A3A"),
    axis.text.y        = element_text(size = 10, colour = "#1C1C1C"),
    axis.text.x        = element_text(size = 9,  colour = "#555550"),
    plot.margin        = margin(10, 10, 6, 10)
  )

# ── PANEL C & D: Histograms ───────────────────────────────────────────────────
make_hist <- function(data, col, year, panel_letter) {
  d_all <- data[[col]]
  d_rg  <- data[[col]][data$is_rg]
  
  med_all <- median(d_all, na.rm = TRUE)
  med_rg  <- median(d_rg,  na.rm = TRUE)
  
  # Stacked data for ggplot histogram
  hist_df <- bind_rows(
    data |> select(ratio = !!sym(col), is_rg) |>
      mutate(group = if_else(is_rg, "Russell Group", "All HEIs"))
  ) |>
    filter(ratio <= cap)
  
  ggplot(hist_df, aes(x = ratio, fill = group)) +
    geom_histogram(data = filter(hist_df, group == "All HEIs"),
                   binwidth = 2, fill = c_other, alpha = 0.75, colour = NA) +
    geom_histogram(data = filter(hist_df, group == "Russell Group"),
                   binwidth = 2, fill = c_rg,    alpha = 0.90, colour = NA) +
    geom_vline(xintercept = med_all, colour = "#666660", linewidth = 1.1,
               linetype = "dashed") +
    geom_vline(xintercept = med_rg,  colour = c_rg,      linewidth = 1.4,
               linetype = "dotted") +
    annotate("text", x = med_all + 0.5, y = Inf,
             label = sprintf("All median\n%.1f", med_all),
             hjust = 0, vjust = 1.2, size = 2.7, colour = "#555550") +
    annotate("text", x = med_rg - 0.5, y = Inf,
             label = sprintf("RG median\n%.1f", med_rg),
             hjust = 1, vjust = 1.2, size = 2.7, colour = c_rg) +
    scale_x_continuous(limits = c(0, cap)) +
    scale_fill_manual(values = c("All HEIs" = c_other, "Russell Group" = c_rg),
                      name = NULL) +
    labs(
      title = sprintf("%s  Distribution of student:FTE ratio, %s", panel_letter, year),
      x     = "Students per academic FTE",
      y     = "Number of universities"
    ) +
    guides(fill = guide_legend(override.aes = list(alpha = 0.9))) +
    theme_minimal(base_size = 12) +
    theme(
      plot.background  = element_rect(fill = bg, colour = NA),
      panel.background = element_rect(fill = bg, colour = NA),
      panel.grid.major = element_line(colour = grid_c, linewidth = 0.4),
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold", size = 11, colour = "#1C1C1C",
                                      margin = margin(b = 6)),
      axis.title       = element_text(size = 10, colour = "#3A3A3A"),
      axis.text        = element_text(size = 9,  colour = "#555550"),
      legend.position  = c(0.85, 0.85),
      legend.text      = element_text(size = 8),
      legend.key.size  = unit(0.4, "cm"),
      plot.margin      = margin(10, 10, 6, 10)
    )
}

p_hist16 <- make_hist(both, "ratio_16", "2015/16", "C.")
p_hist24 <- make_hist(both, "ratio_24", "2024/25", "D.")

# ── Assemble with patchwork ───────────────────────────────────────────────────
# (install patchwork if needed: install.packages("patchwork"))
library(patchwork)

caption_txt <- paste0(
  "Source: HESA DT025 Table 6 (Staff FTE, \u2018Total academic staff excluding atypical\u2019) ",
  "and DT051 Table 1 (Total student enrolments, all levels/modes/entrants).\n",
  "Ratio = total student enrolments \u00f7 total academic FTE (excl. atypical). ",
  "University of the Highlands and Islands excluded (distance-learning anomaly, ratio > 100). ",
  sprintf("%d HEIs with positive FTE and student counts in both years. ", nrow(both)),
  "Russell Group identified by UKPRN."
)

layout <- "
AAAA
AAAA
BBBB
BBBB
BBBB
CD
"

p_final <- (p_scatter / p_bar / (p_hist16 | p_hist24)) +
  plot_layout(heights = c(1.5, 1.15, 0.95)) +
  plot_annotation(
    title   = "Students per academic FTE: UK higher education, 2015/16 \u2192 2024/25",
    caption = caption_txt,
    theme   = theme(
      plot.title      = element_text(face = "bold", size = 17, colour = "#1C1C1C",
                                     margin = margin(b = 8)),
      plot.caption    = element_text(size = 7.5, colour = "#909090", hjust = 0,
                                     lineheight = 1.5, margin = margin(t = 10)),
      plot.background = element_rect(fill = bg, colour = NA)
    )
  )

out <- "plot_student_fte.png"
agg_png(out, width = 1800, height = 2100, res = 150, background = bg)
print(p_final)
invisible(dev.off())
cat(sprintf("\nSaved: %s\n", out))