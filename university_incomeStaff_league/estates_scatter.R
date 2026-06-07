# =============================================================================
# UK HE Estates vs Change in Nominal Income per Academic Staff
# Data: HESA DT025 (Staff), DT031 (Finance), DT042 (Estates)
# Academic staff = "Total academic staff" column (DT025 Table 1)
# Nominal income per academic staff — no deflation
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(broom)
  library(ggtext)
  library(glue)
  library(ragg)
})

# ── Helpers ───────────────────────────────────────────────────────────────────
pn <- function(x) as.numeric(gsub(",", "", x))
rh <- function(f, s) read_csv(f, skip = s, locale = locale(encoding = "UTF-8"),
                               show_col_types = FALSE, name_repair = "unique")

# ── Staff: Total academic staff (consistently reported both years) ─────────────
staff_1516 <- rh("dt025-table-1.csv", 15) |>
  filter(!is.na(UKPRN)) |>
  transmute(UKPRN    = as.integer(UKPRN),
            provider = `HE provider`,
            acad15   = pn(`Total academic staff`))

staff_2324 <- rh("dt025-table-1-2.csv", 15) |>
  filter(!is.na(UKPRN)) |>
  transmute(UKPRN  = as.integer(UKPRN),
            acad24 = pn(`Total academic staff`))

# ── Finance: total consolidated income ────────────────────────────────────────
finance_1516 <- rh("dt031-table-1.csv", 16) |>
  filter(!is.na(UKPRN)) |>
  transmute(UKPRN = as.integer(UKPRN), inc15 = pn(`Total income`))

finance_2324 <- rh("dt031-table-1-2.csv", 16) |>
  filter(!is.na(UKPRN)) |>
  transmute(UKPRN = as.integer(UKPRN), inc24 = pn(`Total income`))

# ── Estates ───────────────────────────────────────────────────────────────────
read_estates <- function(path) {
  rh(path, 14) |>
    filter(!is.na(UKPRN), UKPRN != "UKPRN") |>
    mutate(UKPRN = as.integer(UKPRN),
           across(-c(UKPRN, `HE provider`), ~ as.numeric(gsub(",", "", .))))
}

e1516 <- read_estates("dt042-table-1-2.csv") |>
  select(UKPRN,
         bld15 = `Total number of buildings`,
         gia15 = `Total gross internal area (m2)`,
         car15 = `Total number of car parking spaces`,
         cyc15 = `Total number of cycle spaces`)

e2324 <- read_estates("dt042-table-1.csv") |>
  select(UKPRN,
         bld24 = `Total number of buildings`,
         gia24 = `Total gross internal area (m2)`,
         car24 = `Total number of car parking spaces`,
         cyc24 = `Total number of cycle spaces`)

# ── Russell Group membership ───────────────────────────────────────────────────
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

# ── Master dataset ─────────────────────────────────────────────────────────────
df <- staff_1516 |>
  inner_join(staff_2324,   "UKPRN") |>
  inner_join(finance_1516, "UKPRN") |>
  inner_join(finance_2324, "UKPRN") |>
  inner_join(e1516,        "UKPRN") |>
  inner_join(e2324,        "UKPRN") |>
  mutate(
    # Nominal income per academic staff (£000s) — no deflation
    ips15 = inc15 / acad15,
    ips24 = inc24 / acad24,
    d_ips = (ips24 - ips15) / ips15 * 100,   # % change
    # Estates % changes
    d_gia = (gia24 - gia15) / gia15 * 100,
    d_car = (car24 - car15) / car15 * 100,
    d_cyc = (cyc24 - cyc15) / cyc15 * 100,
    d_bld = (bld24 - bld15) / bld15 * 100,
    group = if_else(provider %in% rg_names, "Russell Group", "Other HEI")
  ) |>
  filter(acad15 > 0, acad24 > 0,
         is.finite(d_ips), abs(d_ips) < 200)

cat(sprintf("Sample: %d HEIs  (%d Russell Group)\n",
            nrow(df), sum(df$group == "Russell Group")))

# ── Colour palette ─────────────────────────────────────────────────────────────
c_rg   <- "#C0392B"
c_oth  <- "#1A5276"
c_all  <- "#2C3E50"
bg     <- "#FEFEFE"
grid_c <- "#ECE9E3"

# ── Regression annotation helper ──────────────────────────────────────────────
pval_str <- function(fit) {
  p <- tidy(fit)$p.value[2]
  ifelse(p < 0.001, "p<0.001",
         ifelse(p < 0.01, sprintf("p=%.3f", p), sprintf("p=%.2f", p)))
}

make_ann <- function(fit, col, lab, n) {
  b  <- round(coef(fit)[2], 3)
  r2 <- round(summary(fit)$r.squared, 3)
  glue("<span style='color:{col}'>**{lab}**</span> (n={n}):  ",
       "\u03b2={b},  R\u00b2={r2},  {pval_str(fit)}")
}

# ── Generic scatter builder ────────────────────────────────────────────────────
make_scatter <- function(data, yv, y_label, title, subtitle, filter_expr = TRUE) {

  d <- data |>
    filter({{ filter_expr }}, is.finite(.data[[yv]]), abs(.data[[yv]]) < 200)

  fit_all <- lm(reformulate("d_ips", yv), data = d)
  fit_rg  <- lm(reformulate("d_ips", yv), data = filter(d, group == "Russell Group"))
  fit_oth <- lm(reformulate("d_ips", yv), data = filter(d, group == "Other HEI"))

  ann <- paste(
    make_ann(fit_all, c_all, "All HEIs",       nrow(d)),
    make_ann(fit_rg,  c_rg,  "Russell Group",  sum(d$group == "Russell Group")),
    make_ann(fit_oth, c_oth, "Other HEIs",     sum(d$group == "Other HEI")),
    sep = "<br>")

  ox  <- filter(d, provider == "The University of Oxford")
  yr  <- range(d[[yv]], na.rm = TRUE)
  ys  <- diff(yr)
  xr  <- range(d$d_ips, na.rm = TRUE)

  ggplot(d, aes(d_ips, .data[[yv]])) +
    annotate("rect", xmin = -Inf, xmax = 0,   ymin = 0,    ymax = Inf,
             fill = "#D6EAF8", alpha = 0.25) +
    annotate("rect", xmin = 0,    xmax = Inf, ymin = -Inf, ymax = 0,
             fill = "#FADBD8", alpha = 0.22) +
    geom_hline(yintercept = 0, colour = grid_c, linewidth = 0.7) +
    geom_vline(xintercept = 0, colour = grid_c, linewidth = 0.7) +
    geom_smooth(method = "lm", se = FALSE, colour = c_all,
                linewidth = 1.0, linetype = "31", data = d) +
    geom_smooth(data = filter(d, group == "Other HEI"),
                method = "lm", se = TRUE, colour = c_oth, fill = c_oth,
                alpha = 0.09, linewidth = 1.1) +
    geom_smooth(data = filter(d, group == "Russell Group"),
                method = "lm", se = TRUE, colour = c_rg, fill = c_rg,
                alpha = 0.12, linewidth = 1.1) +
    geom_point(data = filter(d, group == "Other HEI"),
               colour = c_oth, shape = 21, fill = alpha(c_oth, 0.14),
               size = 3.0, stroke = 0.85) +
    geom_point(data = filter(d, group == "Russell Group"),
               colour = c_rg, shape = 24, fill = alpha(c_rg, 0.28),
               size = 3.8, stroke = 1.0) +
    { if (nrow(ox) > 0) list(
        geom_point(data = ox, colour = c_rg, shape = 24,
                   fill = c_rg, size = 5.2, stroke = 1.2),
        annotate("curve", curvature = -0.25,
                 x = ox$d_ips - 3,   xend = ox$d_ips - 18,
                 y = ox[[yv]] + 0.01 * ys, yend = ox[[yv]] + 0.13 * ys,
                 colour = c_rg, linewidth = 0.55,
                 arrow = arrow(length = unit(5, "pt"), type = "open")),
        annotate("richtext",
                 x = ox$d_ips - 19, y = ox[[yv]] + 0.16 * ys,
                 label = "**Oxford**", colour = c_rg, size = 3.4,
                 fill = NA, label.colour = NA, hjust = 1)
    )} +
    annotate("richtext",
             x = -Inf, y = Inf, label = ann,
             hjust = -0.02, vjust = 1.08, size = 3.1,
             fill = alpha("white", 0.88), label.colour = "#CCCCCC",
             label.padding = unit(c(0.4, 0.5, 0.4, 0.5), "lines"),
             label.r = unit(0.15, "lines")) +
    scale_x_continuous(labels = \(x) paste0(x, "%"),
                       expand = expansion(mult = c(0.04, 0.04))) +
    scale_y_continuous(labels = \(x) paste0(x, "%"),
                       expand = expansion(mult = c(0.07, 0.10))) +
    labs(
      title    = title,
      subtitle = subtitle,
      x        = "Change in nominal income per academic staff  (%)",
      y        = paste0("Change in ", y_label, "  (%)"),
      caption  = paste0(
        "Source: HESA DT025 Table 1 (Staff), DT031 Table 1 (Finance), DT042 Table 1 (Estates).\n",
        "Income per academic staff = total consolidated income \u00f7 total academic staff headcount.\n",
        "Academic staff = 'Total academic staff' column (DT025).  ",
        "n = ", nrow(d), " institutions.\n",
        "\u25b2 Russell Group     \u25cf Other HEIs     \u2015\u2015 Overall regression (dashed)")
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.background   = element_rect(fill = bg, colour = NA),
      panel.background  = element_rect(fill = bg, colour = NA),
      panel.grid.major  = element_line(colour = grid_c, linewidth = 0.45),
      panel.grid.minor  = element_blank(),
      plot.title        = element_text(face = "bold", size = 18.5,
                                        colour = "#1C1C1C", margin = margin(b = 5)),
      plot.subtitle     = element_text(size = 11, colour = "#5A5A5A",
                                        lineheight = 1.25, margin = margin(b = 16)),
      plot.caption      = element_text(size = 8.5, colour = "#909090", hjust = 0,
                                        lineheight = 1.45, margin = margin(t = 12)),
      axis.title.x      = element_text(size = 10.5, colour = "#3A3A3A",
                                        margin = margin(t = 10)),
      axis.title.y      = element_text(size = 10.5, colour = "#3A3A3A",
                                        margin = margin(r = 10), angle = 90,
                                        lineheight = 1.2),
      axis.text         = element_text(size = 9.5, colour = "#6A6A6A"),
      plot.title.position    = "plot",
      plot.caption.position  = "plot",
      plot.margin       = margin(28, 32, 20, 28),
      legend.position   = "none"
    )
}

# ── Render four plots ──────────────────────────────────────────────────────────
specs <- list(
  list(yv = "d_gia", yl = "gross internal area",
       title    = "Do richer universities shrink their campuses?",
       subtitle = "Change in nominal income per academic staff vs change in gross internal floor area  |  UK HEIs, 2015/16 \u2192 2023/24",
       filter   = quote(gia15 > 0),
       file     = "plot_estates_gia.png"),
  list(yv = "d_car", yl = "car parking spaces",
       title    = "Are richer universities cutting car parks?",
       subtitle = "Change in nominal income per academic staff vs change in car parking spaces  |  UK HEIs, 2015/16 \u2192 2023/24",
       filter   = quote(car15 > 0),
       file     = "plot_estates_car.png"),
  list(yv = "d_cyc", yl = "cycle spaces",
       title    = "Richer universities: more bikes?",
       subtitle = "Change in nominal income per academic staff vs change in cycle spaces  |  UK HEIs, 2015/16 \u2192 2023/24",
       filter   = quote(cyc15 > 0),
       file     = "plot_estates_cycle.png"),
  list(yv = "d_bld", yl = "number of buildings",
       title    = "Do richer universities consolidate their estate?",
       subtitle = "Change in nominal income per academic staff vs change in number of buildings  |  UK HEIs, 2015/16 \u2192 2023/24",
       filter   = quote(bld15 > 0),
       file     = "plot_estates_buildings.png")
)

for (sp in specs) {
  p <- make_scatter(df, sp$yv, sp$yl, sp$title, sp$subtitle,
                    filter_expr = eval(sp$filter))
  agg_png(sp$file, width = 1200, height = 900, res = 150, background = bg)
  print(p)
  invisible(dev.off())
  cat("Saved:", sp$file, "\n")
}
