# =============================================================================
# UK HE Full League Table: Real Income per Academic Staff — All Universities
# Data: HESA DT025 (Staff), DT031 (Finance)
# Academic staff = "Total academic staff" column (DT025 Table 1)
# Deflated to 2026 prices using ONS CPI (D7BT series, 2015=100)
# Outputs: plot_league_all.png  +  league_table.html
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(ragg)
})

source("build_html.R")   # loads build_html_page()

# ── CPI deflators ─────────────────────────────────────────────────────────────
# 2015/16: avg(Q3-15=100.5, Q4-15=100.3, Q1-16=99.9, Q2-16=100.4) = 100.28
# 2023/24: avg(Q3-23=131.4, Q4-23=132.0, Q1-24=132.3, Q2-24=133.8) = 132.38
# 2026 mid-year: Q4-2025(139.8) uprated ~3% for H1-2026 = 143.99
cpi_1516 <- 100.28;  cpi_2324 <- 132.38;  cpi_2026 <- 143.99
defl_1516 <- cpi_2026 / cpi_1516   # 1.4359
defl_2324 <- cpi_2026 / cpi_2324   # 1.0877

# ── Helpers ───────────────────────────────────────────────────────────────────
pn <- function(x) as.numeric(gsub(",", "", x))
rh <- function(f, s) read_csv(f, skip = s, locale = locale(encoding = "UTF-8"),
                               show_col_types = FALSE, name_repair = "unique")

# ── Staff ─────────────────────────────────────────────────────────────────────
staff_1516 <- rh("dt025-table-1.csv", 15) |>
  filter(!is.na(UKPRN)) |>
  transmute(UKPRN    = as.integer(UKPRN),
            provider = `HE provider`,
            acad15   = pn(`Total academic staff`))

staff_2324 <- rh("dt025-table-1-2.csv", 15) |>
  filter(!is.na(UKPRN)) |>
  transmute(UKPRN  = as.integer(UKPRN),
            acad24 = pn(`Total academic staff`))

# ── Finance ───────────────────────────────────────────────────────────────────
finance_1516 <- rh("dt031-table-1.csv", 16) |>
  filter(!is.na(UKPRN)) |>
  transmute(UKPRN = as.integer(UKPRN), inc15 = pn(`Total income`))

finance_2324 <- rh("dt031-table-1-2.csv", 16) |>
  filter(!is.na(UKPRN)) |>
  transmute(UKPRN = as.integer(UKPRN), inc24 = pn(`Total income`))

# ── Russell Group flag ────────────────────────────────────────────────────────
rg_names <- c(
  "The University of Birmingham", "The University of Bristol",
  "The University of Cambridge", "Cardiff University",
  "University of Durham", "The University of Edinburgh",
  "The University of Exeter", "The University of Glasgow",
  "Imperial College of Science, Technology and Medicine",
  "King's College London", "The University of Leeds",
  "The University of Liverpool",
  "London School of Economics and Political Science",
  "The University of Manchester", "Newcastle University",
  "University of Nottingham", "The University of Oxford",
  "Queen Mary University of London", "Queen's University Belfast",
  "The University of Sheffield", "The University of Southampton",
  "University College London", "The University of Warwick",
  "The University of York")

# ── Build dataset ─────────────────────────────────────────────────────────────
df <- staff_1516 |>
  inner_join(staff_2324,   "UKPRN") |>
  inner_join(finance_1516, "UKPRN") |>
  inner_join(finance_2324, "UKPRN") |>
  filter(!is.na(acad15), !is.na(acad24), acad15 > 0, acad24 > 0) |>
  mutate(
    ips_nom15  = inc15 / acad15,
    ips_nom24  = inc24 / acad24,
    ips_real15 = ips_nom15 * defl_1516,
    ips_real24 = ips_nom24 * defl_2324,
    real_abs   = ips_real24 - ips_real15,
    real_pct   = real_abs / ips_real15 * 100,
    acad_chg   = (acad24 - acad15) / acad15 * 100,
    russell    = provider %in% rg_names
  ) |>
  arrange(real_pct)

n_pos <- sum(df$real_pct > 0)
n_neg <- sum(df$real_pct < 0)
med   <- median(df$real_pct)

cat(sprintf("n = %d HEIs  (%d Russell Group,  %d other)\n",
            nrow(df), sum(df$russell), sum(!df$russell)))
cat(sprintf("Gained: %d   Lost: %d   Median: %+.1f%%\n", n_pos, n_neg, med))

# ── Colours ───────────────────────────────────────────────────────────────────
bg     <- "#FEFEFE"
grid_c <- "#ECE9E3"

df <- df |>
  mutate(
    bar_col = case_when(
       russell & real_pct >= 0 ~ "#C0392B",
       russell & real_pct <  0 ~ "#E8A09A",
      !russell & real_pct >= 0 ~ "#1A5276",
      TRUE                     ~ "#A9CCE3"
    ),
    name_label = if_else(russell,
                         paste0(provider, " \u25b2"),
                         provider),
    name_f = fct_reorder(name_label, real_pct)
  )

# ── Plot ──────────────────────────────────────────────────────────────────────
p <- ggplot(df, aes(real_pct, name_f)) +
  geom_vline(xintercept = 0, colour = "#555550", linewidth = 0.55) +
  geom_col(aes(fill = bar_col), width = 0.7, show.legend = FALSE) +
  geom_col(data = filter(df, russell), aes(fill = bar_col), width = 0.7,
           colour = "#7B241C", linewidth = 0.25, show.legend = FALSE) +
  scale_fill_identity() +
  scale_x_continuous(
    labels = \(x) paste0(ifelse(x > 0, "+", ""), x, "%"),
    expand = expansion(mult = c(0.01, 0.04))
  ) +
  labs(
    title    = "Real change in income per academic staff, 2015/16 \u2192 2023/24",
    subtitle = paste0(
      sprintf("All %d UK higher education institutions  \u2022  2026 prices (ONS CPI-deflated)\n",
              nrow(df)),
      sprintf("%d gained  \u2022  %d lost  \u2022  Median: %+.0f%%  \u2022  \u25b2 = Russell Group",
              n_pos, n_neg, med)),
    x        = "Real % change in income per academic staff (2026 prices)",
    y        = NULL,
    caption  = paste0(
      "Income per academic staff = total consolidated income (HESA DT031) \u00f7 ",
      "total academic staff headcount (HESA DT025), \u00a3000s.\n",
      "Deflated using ONS CPI D7BT (2015=100): 2015/16 avg=100.3, ",
      "2023/24 avg=132.4, 2026 est=144.0.\n",
      "Outlined bars = Russell Group.")
  ) +
  theme_minimal(base_size = 9) +
  theme(
    plot.background       = element_rect(fill = bg, colour = NA),
    panel.background      = element_rect(fill = bg, colour = NA),
    panel.grid.major.y    = element_blank(),
    panel.grid.major.x    = element_line(colour = grid_c, linewidth = 0.35),
    panel.grid.minor      = element_blank(),
    plot.title            = element_text(face = "bold", size = 15, colour = "#1C1C1C",
                                          margin = margin(b = 4)),
    plot.subtitle         = element_text(size = 8.5, colour = "#5A5A5A",
                                          lineheight = 1.35, margin = margin(b = 12)),
    plot.caption          = element_text(size = 7, colour = "#909090", hjust = 0,
                                          lineheight = 1.4, margin = margin(t = 8)),
    axis.text.y           = element_text(size = 6.2, colour = "#1C1C1C"),
    axis.text.x           = element_text(size = 7.5, colour = "#666660"),
    axis.title.x          = element_text(size = 8.5, colour = "#3A3A3A",
                                          margin = margin(t = 6)),
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    plot.margin           = margin(20, 24, 14, 18)
  )

agg_png("plot_league_all.png", width = 1200, height = 3200, res = 150, background = bg)
print(p)
invisible(dev.off())
cat("Saved: plot_league_all.png\n")

# ── HTML rows ─────────────────────────────────────────────────────────────────
max_abs <- max(abs(df$real_pct))

html_rows <- df |>
  arrange(desc(real_pct)) |>
  mutate(rank_n = row_number()) |>
  pmap_chr(function(rank_n, provider, russell, acad15, acad24,
                    ips_real15, ips_real24, real_pct, bar_col, ...) {
    direction <- if_else(real_pct >= 0, "pos", "neg")
    bar_w     <- abs(real_pct) / max_abs * 100
    badge     <- if_else(russell,
                         '<span class="rg-badge">RG</span>', "")
    row_cls   <- if_else(russell, "rg-row", "")
    paste0(
      '<tr class="', row_cls, '">',
      '<td class="rank">', rank_n, '</td>',
      '<td class="uni-name">', provider, ' ', badge, '</td>',
      '<td class="num">', format(round(acad15), big.mark = ","), '</td>',
      '<td class="num">', format(round(acad24), big.mark = ","), '</td>',
      '<td class="num">\u00a3', round(ips_real15), 'k</td>',
      '<td class="num">\u00a3', round(ips_real24), 'k</td>',
      '<td class="bar-cell">',
        '<div class="bar-wrap">',
          '<div class="bar ', direction, '" style="width:', round(bar_w, 1),
               '%;background:', bar_col, ';"></div>',
          '<span class="bar-label ', direction, '">',
               sprintf("%+.1f%%", real_pct), '</span>',
        '</div>',
      '</td>',
      '</tr>'
    )
  })

html_body <- paste(html_rows, collapse = "\n")

# ── Build and write HTML ──────────────────────────────────────────────────────
html <- build_html_page(df, html_body, n_pos, n_neg, med)
writeLines(html, "league_table.html")
cat("Saved: league_table.html\n")
