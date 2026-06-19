############## OLD VERSION (see lines 231+ for the "fixed" version)


# =============================================================================
# UK HE Russell Group League Table: Real Income per Academic Staff
# Data: HESA DT025 (Staff), DT031 (Finance)
# Academic staff = "Total academic staff" column (DT025 Table 1)
# Deflated to 2026 prices using ONS CPI (D7BT series, 2015=100)
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(ragg)
})

# ── CPI deflators (ONS D7BT, academic-year averages, base 2015=100) ───────────
# 2015/16: avg of Q3-2015(100.5), Q4-2015(100.3), Q1-2016(99.9), Q2-2016(100.4) = 100.28
# 2023/24: avg of Q3-2023(131.4), Q4-2023(132.0), Q1-2024(132.3), Q2-2024(133.8) = 132.38
# 2026 mid-year: Q4-2025(139.8) uprated at ~3% for H1-2026 = 143.99
cpi_1516 <- 100.28
cpi_2324 <- 132.38
cpi_2026 <- 143.99
defl_1516 <- cpi_2026 / cpi_1516   # 1.4359
defl_2324 <- cpi_2026 / cpi_2324   # 1.0877

# ── Helpers ───────────────────────────────────────────────────────────────────
pn <- function(x) as.numeric(gsub(",", "", x))
rh <- function(f, s) read_csv(f, skip = s, locale = locale(encoding = "UTF-8"),
                               show_col_types = FALSE, name_repair = "unique")

# ── Staff: Total academic staff ───────────────────────────────────────────────
staff_1516 <- rh("dt025-table-1-15.csv", 15) |>
  filter(!is.na(UKPRN)) |>
  transmute(UKPRN    = as.integer(UKPRN),
            provider = `HE provider`,
            acad15   = pn(`Total academic staff`))

staff_2324 <- rh("dt025-table-1-23.csv", 15) |>
  filter(!is.na(UKPRN)) |>
  transmute(UKPRN  = as.integer(UKPRN),
            acad24 = pn(`Total academic staff`))

# ── Finance: total consolidated income ────────────────────────────────────────
finance_1516 <- rh("dt031-table-1-15.csv", 16) |>
  filter(!is.na(UKPRN)) |>
  transmute(UKPRN = as.integer(UKPRN), inc15 = pn(`Total income`))

finance_2324 <- rh("dt031-table-1-23.csv", 16) |>
  filter(!is.na(UKPRN)) |>
  transmute(UKPRN = as.integer(UKPRN), inc24 = pn(`Total income`))

# ── Russell Group ─────────────────────────────────────────────────────────────
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

name_map <- c(
  "The University of Birmingham"                       = "Birmingham",
  "The University of Bristol"                          = "Bristol",
  "The University of Cambridge"                        = "Cambridge",
  "Cardiff University"                                 = "Cardiff",
  "University of Durham"                               = "Durham",
  "The University of Edinburgh"                        = "Edinburgh",
  "The University of Exeter"                           = "Exeter",
  "The University of Glasgow"                          = "Glasgow",
  "Imperial College of Science, Technology and Medicine" = "Imperial",
  "King's College London"                              = "King's College London",
  "The University of Leeds"                            = "Leeds",
  "The University of Liverpool"                        = "Liverpool",
  "London School of Economics and Political Science"   = "LSE",
  "The University of Manchester"                       = "Manchester",
  "Newcastle University"                               = "Newcastle",
  "University of Nottingham"                           = "Nottingham",
  "The University of Oxford"                           = "Oxford",
  "Queen Mary University of London"                    = "Queen Mary",
  "Queen's University Belfast"                         = "Queen's Belfast",
  "The University of Sheffield"                        = "Sheffield",
  "The University of Southampton"                      = "Southampton",
  "University College London"                          = "UCL",
  "The University of Warwick"                          = "Warwick",
  "The University of York"                             = "York")

# ── Build table ───────────────────────────────────────────────────────────────
df <- staff_1516 |>
  inner_join(staff_2324,   "UKPRN") |>
  inner_join(finance_1516, "UKPRN") |>
  inner_join(finance_2324, "UKPRN") |>
  filter(provider %in% rg_names,
         !is.na(acad15), !is.na(acad24),
         acad15 > 0, acad24 > 0) |>
  mutate(
    # Nominal income per academic staff (£000s)
    ips_nom15   = inc15 / acad15,
    ips_nom24   = inc24 / acad24,
    # Real income per academic staff (2026 prices, £000s)
    ips_real15  = ips_nom15 * defl_1516,
    ips_real24  = ips_nom24 * defl_2324,
    # Changes
    real_abs    = ips_real24 - ips_real15,
    real_pct    = real_abs / ips_real15 * 100,
    nom_pct     = (ips_nom24 - ips_nom15) / ips_nom15 * 100,
    acad_chg    = (acad24 - acad15) / acad15 * 100,
    name        = recode(provider, !!!name_map)
  ) |>
  arrange(desc(real_pct))

cat(sprintf("n = %d Russell Group universities\n", nrow(df)))
cat(sprintf("CPI deflators: 2015/16 \u00d7%.4f, 2023/24 \u00d7%.4f (to 2026 prices)\n\n",
            defl_1516, defl_2324))

# ── Print audit table ─────────────────────────────────────────────────────────
cat("=== LEAGUE TABLE: Real income per academic staff (2026 prices) ===\n")
df |>
  transmute(
    University   = name,
    Acad_15      = acad15,
    Acad_24      = acad24,
    `Acad_chg_%` = round(acad_chg, 1),
    `Inc15_£m`   = round(inc15 / 1e6, 1),
    `Inc24_£m`   = round(inc24 / 1e6, 1),
    `Nom_IPS15`  = round(ips_nom15, 0),
    `Nom_IPS24`  = round(ips_nom24, 0),
    `Real_IPS15` = round(ips_real15, 0),
    `Real_IPS24` = round(ips_real24, 0),
    `Real_abs_£k` = round(real_abs, 0),
    `Real_pct_%` = round(real_pct, 1),
    `Nom_pct_%`  = round(nom_pct, 1)
  ) |>
  print(n = 24, width = 160)

# ── Plot ──────────────────────────────────────────────────────────────────────
bg     <- "#FEFEFE"
grid_c <- "#ECE9E3"
c_pos  <- "#1A5276"
c_neg  <- "#C0392B"

n_pos <- sum(df$real_pct > 0)
n_neg <- sum(df$real_pct < 0)

plot_df <- df |>
  mutate(
    name   = fct_reorder(name, real_pct),
    colour = if_else(real_pct >= 0, c_pos, c_neg),
    label  = sprintf("\u00a3%dk \u2192 \u00a3%dk",
                     round(ips_real15), round(ips_real24))
  )

sub_txt <- sprintf(
  paste0("Real income per academic staff (2026 prices, ONS CPI-deflated).\n",
         "Academic staff = 'Total academic staff' (DT025).  ",
         "%d universities gained, %d lost in real terms.  ",
         "Median change: %+.0f%%."),
  n_pos, n_neg, median(df$real_pct))

p <- ggplot(plot_df, aes(real_pct, name)) +
  geom_vline(xintercept = 0, colour = "#888880", linewidth = 0.6) +
  geom_col(aes(fill = colour), width = 0.65, show.legend = FALSE) +
  geom_text(
    aes(x     = real_pct + if_else(real_pct >= 0, 0.5, -0.5),
        label = label),
    hjust   = if_else(plot_df$real_pct >= 0, 0, 1),
    size    = 3.0,
    colour  = "#444440"
  ) +
  scale_fill_identity() +
  scale_x_continuous(
    labels  = \(x) paste0(ifelse(x > 0, "+", ""), x, "%"),
    expand  = expansion(mult = c(0.02, 0.32))
  ) +
  labs(
    title   = "Real change in income per academic staff, 2015/16 \u2192 2023/24",
    subtitle = sub_txt,
    x       = "Real % change in income per academic staff (2026 prices)",
    y       = NULL,
    caption = paste0(
      "Source: HESA DT025 Table 1 (Staff), DT031 Table 1 (Finance).\n",
      "Income per academic staff = total consolidated income (DT031) \u00f7 ",
      "total academic staff headcount (DT025), \u00a3000s.\n",
      "Deflated using ONS CPI D7BT (2015=100): ",
      "2015/16 avg=100.3, 2023/24 avg=132.4, 2026 est=144.0.\n",
      "All 24 Russell Group universities included (academic staff reporting ",
      "was mandatory in both years).")
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background        = element_rect(fill = bg, colour = NA),
    panel.background       = element_rect(fill = bg, colour = NA),
    panel.grid.major.y     = element_blank(),
    panel.grid.major.x     = element_line(colour = grid_c, linewidth = 0.4),
    panel.grid.minor       = element_blank(),
    plot.title             = element_text(face = "bold", size = 17,
                                           colour = "#1C1C1C", margin = margin(b = 4)),
    plot.subtitle          = element_text(size = 9.5, colour = "#5A5A5A",
                                           lineheight = 1.35, margin = margin(b = 14)),
    plot.caption           = element_text(size = 7.5, colour = "#909090", hjust = 0,
                                           lineheight = 1.4, margin = margin(t = 10)),
    axis.text.y            = element_text(size = 10.5, colour = "#1C1C1C"),
    axis.text.x            = element_text(size = 9,    colour = "#666660"),
    axis.title.x           = element_text(size = 10,   colour = "#3A3A3A",
                                           margin = margin(t = 8)),
    plot.title.position    = "plot",
    plot.caption.position  = "plot",
    plot.margin            = margin(24, 28, 18, 24)
  )

agg_png("plot_league_academic.png", width = 1200, height = 980, res = 150,
        background = bg)
print(p)
invisible(dev.off())
cat("\nSaved: plot_league_academic.png\n")








############## Modified version

# =============================================================================
# UK HE Russell Group League Table: Real Income per Academic Staff FTE
# HESA DT025 Table 6 (FTE), DT031 Table 1 (Finance)
# Base = avg(2017/18, 2018/19); End = 2023/24; all in 2026 prices
# =============================================================================

library(dplyr)
library(readr)
library(ggplot2)
library(forcats)
library(tidyr)

# ── CPI deflators (ONS D7BT, academic-year averages, base 2015=100) ──────────
# 2017/18: avg Q3-2017(105.9), Q4-2017(106.7), Q1-2018(107.4), Q2-2018(108.6) = 107.15
# 2018/19: avg Q3-2018(108.9), Q4-2018(109.6), Q1-2019(109.8), Q2-2019(111.2) = 109.88
# 2023/24: avg Q3-2023(131.4), Q4-2023(132.0), Q1-2024(132.3), Q2-2024(133.8) = 132.38
# 2026 est: 143.99
cpi_1718 <- 107.15
cpi_1819 <- 109.88
cpi_2324 <- 132.38
cpi_2026 <- 143.99

defl_1718 <- cpi_2026 / cpi_1718
defl_1819 <- cpi_2026 / cpi_1819
defl_2324 <- cpi_2026 / cpi_2324

cat(sprintf("CPI deflators (to 2026 prices): 2017/18 x%.4f  2018/19 x%.4f  2023/24 x%.4f\n\n",
            defl_1718, defl_1819, defl_2324))

# ── Helpers ───────────────────────────────────────────────────────────────────
pn <- function(x) suppressWarnings(as.numeric(gsub(",|\\(|\\)", "", x)))

read_finance <- function(path) {
  read_csv(path, skip = 16,
           locale = locale(encoding = "UTF-8"),
           show_col_types = FALSE,
           name_repair = "unique") |>
    filter(!is.na(UKPRN)) |>
    transmute(UKPRN    = as.integer(UKPRN),
              provider = `HE Provider`,
              inc      = pn(`Total income`))
}

read_fte <- function(path) {
  read_csv(path, skip = 14,
           locale = locale(encoding = "UTF-8"),
           show_col_types = FALSE,
           name_repair = "unique") |>
    filter(!is.na(UKPRN)) |>
    transmute(UKPRN    = as.integer(UKPRN),
              provider = `HE provider`,
              fte      = pn(`Total academic staff (excluding atypical)`))
}

# ── Load ──────────────────────────────────────────────────────────────────────
fin17 <- read_finance("dt031-table-1-17.csv")
fin18 <- read_finance("dt031-table-1-18.csv")
fin23 <- read_finance("dt031-table-1-23.csv")

fte17 <- read_fte("dt025-table-6-17.csv")
fte18 <- read_fte("dt025-table-6-18.csv")
fte23 <- read_fte("dt025-table-6-23.csv")

# ── Russell Group ─────────────────────────────────────────────────────────────
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

name_map <- c(
  "The University of Birmingham"                         = "Birmingham",
  "The University of Bristol"                            = "Bristol",
  "The University of Cambridge"                          = "Cambridge",
  "Cardiff University"                                   = "Cardiff",
  "University of Durham"                                 = "Durham",
  "The University of Edinburgh"                          = "Edinburgh",
  "The University of Exeter"                             = "Exeter",
  "The University of Glasgow"                            = "Glasgow",
  "Imperial College of Science, Technology and Medicine" = "Imperial",
  "King's College London"                                = "King's College London",
  "The University of Leeds"                              = "Leeds",
  "The University of Liverpool"                          = "Liverpool",
  "London School of Economics and Political Science"     = "LSE",
  "The University of Manchester"                         = "Manchester",
  "Newcastle University"                                 = "Newcastle",
  "University of Nottingham"                             = "Nottingham",
  "The University of Oxford"                             = "Oxford",
  "Queen Mary University of London"                      = "Queen Mary",
  "Queen's University Belfast"                           = "Queen's Belfast",
  "The University of Sheffield"                          = "Sheffield",
  "The University of Southampton"                        = "Southampton",
  "University College London"                            = "UCL",
  "The University of Warwick"                            = "Warwick",
  "The University of York"                               = "York")

# ── Build per-year income/FTE ─────────────────────────────────────────────────
yr <- function(fin, fte, defl, tag) {
  inner_join(fin, fte |> select(UKPRN, fte), by = "UKPRN") |>
    filter(!is.na(inc), !is.na(fte), fte > 0) |>
    transmute(
      UKPRN, provider,
      !!paste0("inc_", tag)      := inc,
      !!paste0("fte_", tag)      := fte,
      !!paste0("ips_nom_", tag)  := inc / fte,
      !!paste0("ips_real_", tag) := (inc / fte) * defl
    )
}

d17 <- yr(fin17, fte17, defl_1718, "17")
d18 <- yr(fin18, fte18, defl_1819, "18")
d23 <- yr(fin23, fte23, defl_2324, "23")

# ── Merge, filter RG, compute changes ─────────────────────────────────────────
df <- d17 |>
  inner_join(d18 |> select(-provider), by = "UKPRN") |>
  inner_join(d23 |> select(-provider), by = "UKPRN") |>
  filter(provider %in% rg_names) |>
  mutate(
    ips_real_base = (ips_real_17 + ips_real_18) / 2,
    ips_nom_base  = (ips_nom_17  + ips_nom_18)  / 2,
    fte_base      = (fte_17      + fte_18)       / 2,
    inc_base      = (inc_17      + inc_18)       / 2,
    
    real_abs  = ips_real_23 - ips_real_base,
    real_pct  = real_abs / ips_real_base * 100,
    nom_pct   = (ips_nom_23 - ips_nom_base) / ips_nom_base * 100,
    fte_chg   = (fte_23 - fte_base) / fte_base * 100,
    
    name = recode(provider, !!!name_map)
  ) |>
  arrange(desc(real_pct))

cat(sprintf("n = %d Russell Group universities matched across all three years\n\n", nrow(df)))

# ── Audit table ───────────────────────────────────────────────────────────────
cat("=== LEAGUE TABLE: Real income per academic FTE (2026 prices) ===\n")
cat("    Base = avg(2017/18, 2018/19)  |  End = 2023/24\n\n")

df |>
  transmute(
    University     = name,
    FTE_base       = round(fte_base),
    FTE_2324       = round(fte_23),
    FTE_chg_pct    = round(fte_chg, 1),
    Inc_base_Mm    = round(inc_base / 1e6, 1),
    Inc_2324_Mm    = round(inc_23   / 1e6, 1),
    RealIPS_base   = round(ips_real_base),
    RealIPS_2324   = round(ips_real_23),
    Real_abs_k     = round(real_abs),
    Real_pct       = round(real_pct, 1),
    Nom_pct        = round(nom_pct,  1)
  ) |>
  print(n = 24, width = 200)

# ── Plot ──────────────────────────────────────────────────────────────────────
bg     <- "#FEFEFE"
grid_c <- "#ECE9E3"
c_pos  <- "#1A5276"
c_neg  <- "#C0392B"

n_pos <- sum(df$real_pct > 0)
n_neg <- sum(df$real_pct < 0)
med   <- median(df$real_pct)




plot_df <- df |>
  mutate(
    name   = fct_reorder(name, real_pct),
    colour = ifelse(real_pct >= 0, c_pos, c_neg),
    label  = sprintf("\u00a3%dk \u2192 \u00a3%dk",
                     round(ips_real_base),
                     round(ips_real_23  ))
  )


df |>
  summarise(
    min_base = min(ips_real_base, na.rm = TRUE),
    max_base = max(ips_real_base, na.rm = TRUE),
    min_23   = min(ips_real_23, na.rm = TRUE),
    max_23   = max(ips_real_23, na.rm = TRUE)
  )

sub_txt <- sprintf(
  "Real income per academic FTE (2026 prices, ONS CPI-deflated).\nBase = average of 2017/18 and 2018/19; end year = 2023/24. Academic FTE = 'Total academic staff excl. atypical' (DT025 Table 6).\n%d universities gained in real terms, %d lost. Median change: %+.0f%%.",
  n_pos, n_neg, med)

p <- ggplot(plot_df, aes(x = real_pct, y = name)) +
  geom_vline(xintercept = 0, colour = "#888880", linewidth = 0.6) +
  geom_col(aes(fill = colour), width = 0.65, show.legend = FALSE) +
  geom_text(
  aes(
    x = real_pct + ifelse(real_pct >= 0, 0.4, -0.4),
    label = label,
    hjust = ifelse(real_pct >= 0, 0, 1)
  ),
  size = 3.0,
  colour = "#444440"
) +
  scale_fill_identity() +
  scale_x_continuous(
    labels = function(x) paste0(ifelse(x > 0, "+", ""), x, "%"),
    expand = expansion(mult = c(0.02, 0.30))
  ) +
  labs(
    title    = "Real change in income per academic FTE, avg(2017/18\u20132018/19) \u2192 2023/24",
    subtitle = sub_txt,
    x        = "Real % change in income per academic FTE (2026 prices)",
    y        = NULL,
    caption  = paste0(
      "Source: HESA DT025 Table 6 (Staff FTE), DT031 Table 1 (Finance).\n",
      "Income per FTE = total consolidated income \u00f7 total academic FTE excl. atypical, \u00a3000s.\n",
      "Deflated using ONS CPI D7BT (2015=100): 2017/18=107.2, 2018/19=109.9, 2023/24=132.4, 2026 est=144.0.\n",
      "24 Russell Group universities. Base = two-year average to mitigate single-year reporting anomalies."
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background    = element_rect(fill = bg, colour = NA),
    panel.background   = element_rect(fill = bg, colour = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = grid_c, linewidth = 0.4),
    panel.grid.minor   = element_blank(),
    plot.title         = element_text(face = "bold", size = 14, colour = "#1C1C1C",
                                      margin = margin(b = 4)),
    plot.subtitle      = element_text(size = 9, colour = "#5A5A5A", lineheight = 1.4,
                                      margin = margin(b = 14)),
    plot.caption       = element_text(size = 7.5, colour = "#909090", hjust = 0,
                                      lineheight = 1.4, margin = margin(t = 10)),
    axis.text.y        = element_text(size = 10.5, colour = "#1C1C1C"),
    axis.text.x        = element_text(size = 9, colour = "#666660"),
    axis.title.x       = element_text(size = 10, colour = "#3A3A3A", margin = margin(t = 8)),
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    plot.margin           = margin(24, 28, 18, 24)
  )

out <- "plot_league_fte.png"
png(out, width = 1200, height = 980, res = 150)
print(p)
invisible(dev.off())
cat(sprintf("\nSaved: %s\n", out))












