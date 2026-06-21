# =============================================================================
# UK HE: Rank providers by change in student numbers, 2015/16 -> 2024/25
# Purpose: identify institutions whose student growth is wildly out of line
# with their staff (FTE) growth -- a likely signature of franchise/
# subcontractual delivery (students counted at lead provider, taught
# elsewhere, so staff growth at the lead provider doesn't keep pace).
#
# Data: HESA DT025 Table 6 (Staff FTE), DT051 Table 1 (Student enrolments)
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

# ── Helpers ───────────────────────────────────────────────────────────────────
pn <- function(x) suppressWarnings(as.numeric(gsub(",", "", x)))

read_fte <- function(path) {
  read_csv(path, skip = 14,
           locale = locale(encoding = "UTF-8"),
           show_col_types = FALSE, name_repair = "unique") |>
    filter(!is.na(UKPRN)) |>
    transmute(UKPRN    = as.integer(UKPRN),
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

# ── Merge ─────────────────────────────────────────────────────────────────────
# Use student data as the base (left_join on FTE) so a provider with no FTE
# match (e.g. some franchise-heavy providers report patchy staff data) still
# shows up in the student-change ranking rather than silently disappearing.

m16 <- stu_1516 |>
  left_join(fte_1516, by = "UKPRN") |>
  select(UKPRN, provider, fte_16 = fte, stu_16 = students)

m24 <- stu_2425 |>
  left_join(fte_2425, by = "UKPRN") |>
  select(UKPRN, provider_24 = provider, fte_24 = fte, stu_24 = students)

both <- inner_join(m16, m24, by = "UKPRN") |>
  mutate(provider = coalesce(provider, provider_24)) |>
  select(-provider_24) |>
  filter(!is.na(stu_16), !is.na(stu_24), stu_16 > 0) |>
  mutate(
    stu_chg      = stu_24 - stu_16,
    stu_pct_chg  = stu_chg / stu_16 * 100,
    fte_chg      = fte_24 - fte_16,
    fte_pct_chg  = if_else(!is.na(fte_16) & fte_16 > 0,
                           fte_chg / fte_16 * 100, NA_real_),
    # Ratio of student growth rate to staff growth rate.
    # >1 means students grew faster than staff (relatively more outsourced
    # teaching, all else equal). NA where FTE data is missing/zero.
    growth_gap_ratio = if_else(
      !is.na(fte_pct_chg) & fte_pct_chg > -100,
      (1 + stu_pct_chg / 100) / (1 + fte_pct_chg / 100),
      NA_real_
    ),
    # Flag: student count grew a lot AND staff growth lagged a lot.
    # Tune the thresholds below to taste.
    likely_franchise_flag = !is.na(growth_gap_ratio) &
      stu_chg > 1000 &                 # meaningful absolute student growth
      growth_gap_ratio > 2             # students grew >2x faster than staff
  )

# ── Rank by absolute change in students ───────────────────────────────────────
ranked_abs <- both |>
  arrange(desc(stu_chg)) |>
  transmute(
    Provider        = provider,
    UKPRN           = UKPRN,
    Students_1516   = round(stu_16),
    Students_2425   = round(stu_24),
    Change_abs      = round(stu_chg),
    Change_pct      = round(stu_pct_chg, 1),
    FTE_1516        = round(fte_16),
    FTE_2425        = round(fte_24),
    FTE_change_pct  = round(fte_pct_chg, 1),
    Growth_gap_x    = round(growth_gap_ratio, 1),   # student growth / staff growth
    Flag            = if_else(likely_franchise_flag, "⚠ CHECK", "")
  )

cat("\n=== TOP 30 BY ABSOLUTE INCREASE IN STUDENTS, 2015/16 -> 2024/25 ===\n")
print(ranked_abs |> slice_head(n = 30), n = 30, width = 200)

# ── Rank by percentage change in students ─────────────────────────────────────
ranked_pct <- both |>
  filter(stu_16 >= 100) |>     # drop tiny providers where % swings are noise
  arrange(desc(stu_pct_chg)) |>
  transmute(
    Provider        = provider,
    UKPRN           = UKPRN,
    Students_1516   = round(stu_16),
    Students_2425   = round(stu_24),
    Change_pct      = round(stu_pct_chg, 1),
    FTE_1516        = round(fte_16),
    FTE_2425        = round(fte_24),
    FTE_change_pct  = round(fte_pct_chg, 1),
    Growth_gap_x    = round(growth_gap_ratio, 1),
    Flag            = if_else(likely_franchise_flag, "⚠ CHECK", "")
  )

cat("\n=== TOP 30 BY PERCENTAGE INCREASE IN STUDENTS (min 100 students in 2015/16) ===\n")
print(ranked_pct |> slice_head(n = 30), n = 30, width = 200)

# ── Providers most likely to be franchise-affected ────────────────────────────
# Sorted by growth_gap_ratio: students growing much faster than staff is the
# key signature (lead provider headcount balloons without commensurate
# in-house staff growth, because partner-delivered teaching doesn't show up
# in the lead provider's own FTE return).
flagged <- both |>
  filter(likely_franchise_flag) |>
  arrange(desc(growth_gap_ratio)) |>
  transmute(
    Provider        = provider,
    UKPRN           = UKPRN,
    Students_1516   = round(stu_16),
    Students_2425   = round(stu_24),
    Change_abs      = round(stu_chg),
    Change_pct      = round(stu_pct_chg, 1),
    FTE_1516        = round(fte_16),
    FTE_2425        = round(fte_24),
    FTE_change_pct  = round(fte_pct_chg, 1),
    Growth_gap_x    = round(growth_gap_ratio, 1)
  )

cat("\n=== LIKELY FRANCHISE-AFFECTED (student growth >1000 AND >2x staff growth rate) ===\n")
print(flagged, n = nrow(flagged), width = 200)

cat(sprintf("\n%d providers flagged out of %d total matched.\n",
            nrow(flagged), nrow(both)))

# ── Save full ranked tables to CSV for further inspection ─────────────────────
write_csv(ranked_abs, "ranked_by_absolute_change.csv")
write_csv(ranked_pct, "ranked_by_percent_change.csv")
write_csv(flagged,    "likely_franchise_flagged.csv")











# =============================================================================
# UK HE: Bar chart of change in student numbers by provider, 2015/16 -> 2024/25
# Data: HESA DT051 Table 1 (Student enrolments). FTE (DT025) merged in too,
# so bars can be coloured/flagged by likely franchise involvement (students
# growing much faster than staff -- see rank_student_changes.R for the logic).
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
    transmute(UKPRN    = as.integer(UKPRN),
              provider = `HE provider`,
              fte      = pn(`Total academic staff (excluding atypical)`))
}

read_students <- function(path) {
  read_csv(path, skip = 17,
           locale = locale(encoding = "UTF-8"),
           show_col_types = FALSE, name_repair = "unique") |>
    filter(!is.na(UKPRN)) |>
    transmute(UKPRN    = as.integer(UKPRN),
              provider = `HE provider`,
              students = pn(Total))
}

# ── Load ──────────────────────────────────────────────────────────────────────
fte_1516 <- read_fte("dt025-table-6-15.csv")
fte_2425 <- read_fte("dt025-table-6-24.csv")
stu_1516 <- read_students("dt051-table-1-15.csv")
stu_2425 <- read_students("dt051-table-1-24.csv")

# ── Merge (student data is the base; FTE joined in where available) ───────────
m16 <- stu_1516 |>
  left_join(select(fte_1516, UKPRN, fte_16 = fte), by = "UKPRN") |>
  rename(stu_16 = students)

m24 <- stu_2425 |>
  left_join(select(fte_2425, UKPRN, fte_24 = fte), by = "UKPRN") |>
  select(UKPRN, provider_24 = provider, fte_24, stu_24 = students)

both <- inner_join(m16, m24, by = "UKPRN") |>
  mutate(provider = coalesce(provider, provider_24)) |>
  select(-provider_24) |>
  filter(!is.na(stu_16), !is.na(stu_24), stu_16 > 0) |>
  mutate(
    stu_chg     = stu_24 - stu_16,
    stu_pct_chg = stu_chg / stu_16 * 100,
    fte_pct_chg = if_else(!is.na(fte_16) & fte_16 > 0,
                          (fte_24 - fte_16) / fte_16 * 100, NA_real_),
    growth_gap_ratio = if_else(
      !is.na(fte_pct_chg) & fte_pct_chg > -100,
      (1 + stu_pct_chg / 100) / (1 + fte_pct_chg / 100), NA_real_
    ),
    # Category used to colour the bars:
    #  - "No staff data"      : provider absent from DT025 (can't assess)
    #  - "Flag: outpaced staff": students grew >2x faster than staff & by 1000+
    #  - "Normal"              : everything else
    category = case_when(
      is.na(fte_16) | is.na(fte_24)                              ~ "No staff data",
      !is.na(growth_gap_ratio) & stu_chg > 1000 & growth_gap_ratio > 2 ~ "Flag: outpaced staff",
      TRUE                                                        ~ "Normal"
    )
  )

# ── Size filter ────────────────────────────────────────────────────────────────
# Keep only providers with at least MIN_STU_16 students in 2015/16. This drops
# very small/specialist providers where both absolute and percentage changes
# are dominated by base-size noise rather than telling us anything meaningful
# about sector-wide trends.
MIN_STU_16 <- 2000

n_before <- nrow(both)
both <- both |> filter(stu_16 >= MIN_STU_16)
n_after <- nrow(both)

cat(sprintf("Size filter: kept %d of %d providers with >= %s students in 2015/16.\n",
            n_after, n_before, format(MIN_STU_16, big.mark = ",")))

# ── Optional: exclude providers with no staff (FTE) data ──────────────────────
# Some providers (often private/professional providers like Arden, BPP, The
# University of Law) don't appear in the DT025 staff return, so a growth-gap
# ratio can't be computed for them -- they're tagged "No staff data" above.
# Set this to TRUE to drop them entirely rather than show them as grey bars.
EXCLUDE_NO_STAFF_DATA <- TRUE

if (EXCLUDE_NO_STAFF_DATA) {
  n_before_staff <- nrow(both)
  both <- both |> filter(category != "No staff data")
  n_after_staff <- nrow(both)
  cat(sprintf("Excluded providers with no staff data: kept %d of %d.\n",
              n_after_staff, n_before_staff))
}

# ── Shared settings (LinkedIn-optimised) ──────────────────────────────────────
TOP_N <- 20   # LinkedIn: keep this small -- 10 bars is about the limit for a
# mobile feed image to stay readable. 30 was right for analysis,
# wrong for a social post.

colour_map <- c(
  "Normal"                = "#1A5276",   # navy
  "Flag: outpaced staff"  = "#1A5276",   # #C0392Bred - likely franchise signature
  "No staff data"         = "#95A5A6"    # grey - can't assess, often the biggest movers
)

bg     <- "#FFFFFF"   # pure white reads cleaner on LinkedIn's feed than off-white
grid_c <- "#F0F0F0"

legend_note <- if (EXCLUDE_NO_STAFF_DATA) {
  "Red = student growth far outpaced staff growth."
} else {
  "Red = student growth far outpaced staff growth. Grey = no staff data available."
}

# Short caption: a small explanatory note above a one-line source credit.
# Kept out of the subtitle so the subtitle stays a single short line.
caption_txt <- paste0(
  legend_note, "\n",
  "Source: HESA DT025 & DT051, academic staff FTE vs total student enrolments"
)

# ── Reusable chart builder (LinkedIn-optimised) ───────────────────────────────
# rank_by: "abs" (change in student numbers) or "pct" (% change in student numbers)
build_chart <- function(data, rank_by = c("abs", "pct"), top_n = TOP_N) {
  rank_by <- match.arg(rank_by)
  
  d <- if (rank_by == "abs") {
    data |> arrange(desc(stu_chg)) |> slice_head(n = top_n)
  } else {
    data |> arrange(desc(stu_pct_chg)) |> slice_head(n = top_n)
  }
  
  value_col <- if (rank_by == "abs") "stu_chg" else "stu_pct_chg"
  x_label   <- if (rank_by == "abs") "Increase in student enrolments, 2015/16 \u2192 2024/25"
  else "% increase in student enrolments, 2015/16 \u2192 2024/25"
  label_fmt <- if (rank_by == "abs") {
    \(x) format(round(x), big.mark = ",")
  } else {
    \(x) paste0(round(x, 0), "%")
  }
  
  # Punchy, claim-led titles -- this is the hook for a feed scroller, not a
  # methods caption. Keep the technical detail (min size, exclusions) in the
  # LinkedIn post text instead. Title and subtitle are split into separate
  # short lines (rather than one long wrapped string) so spacing stays
  # predictable regardless of font metrics.
  title_txt <- if (rank_by == "abs") {
    "Where UK student numbers\ngrew the most"
  } else {
    "Where UK student numbers grew the fastest"
  }
  subtitle_txt <- sprintf("Top %d providers, 2015/16 \u2192 2024/25", top_n)
  
  d <- d |>
    mutate(
      # Wrap long provider names onto two lines so they don't get clipped
      # against the left edge of a narrow portrait image.
      provider_wrapped = str_wrap(provider, width = 30),
      provider_f = fct_reorder(provider_wrapped, .data[[value_col]]),
      bar_label  = sprintf("%s \u2192 %s",
                           format(round(stu_16), big.mark = ","),
                           format(round(stu_24), big.mark = ","))
    )
  
  ggplot(d, aes(x = .data[[value_col]], y = provider_f, fill = category)) +
    geom_col(width = 0.68) +
    geom_text(aes(label = bar_label),
              hjust = -0.05, size = 3.7, colour = "#2A2A2A", fontface = "bold") +
    geom_vline(xintercept = 0, colour = "#333333", linewidth = 0.8) +
    scale_fill_manual(values = colour_map, name = NULL) +
    scale_x_continuous(
      labels = label_fmt,
      expand = expansion(mult = c(0.02, 0.38))   # generous room for bold labels
    ) +
    labs(
      title    = title_txt,
      subtitle = subtitle_txt,
      x        = x_label,
      y        = NULL,
      caption  = caption_txt
    ) +
    theme_minimal(base_size = 15) +
    theme(
      plot.background    = element_rect(fill = bg, colour = NA),
      panel.background   = element_rect(fill = bg, colour = NA),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(colour = grid_c, linewidth = 0.4),
      panel.grid.minor   = element_blank(),
      plot.title.position = "plot",   # title/subtitle/caption span the full
      plot.caption.position = "plot", # figure width, not just the bar panel
      plot.title         = element_text(face = "bold", size = 20, colour = "#000000",
                                        margin = margin(b = 8), lineheight = 1.05),
      plot.subtitle      = element_text(size = 11.5, colour = "#444444",
                                        margin = margin(b = 14), lineheight = 1.4),
      axis.title.x       = element_text(size = 11, colour = "#333333", margin = margin(t = 10)),
      axis.text.y        = element_text(size = 7.5, colour = "#000000", face = "bold"),
      axis.text.x        = element_text(size = 10, colour = "#666666"),
      legend.position    = "none",   # colour meaning is carried by a small note instead
      plot.caption       = element_text(size = 8.5, colour = "#999999", hjust = 0,
                                        lineheight = 1.4, margin = margin(t = 10)),
      plot.margin        = margin(20, 24, 16, 8)
    )
}

# ── Build both charts ──────────────────────────────────────────────────────────
p_abs <- build_chart(both, rank_by = "abs")
p_pct <- build_chart(both, rank_by = "pct")

staff_suffix <- if (EXCLUDE_NO_STAFF_DATA) "_staffonly" else ""

# LinkedIn's recommended portrait image size is 1080x1350 (4:5). This renders
# crisp in-feed on both mobile and desktop without LinkedIn cropping it.
LI_WIDTH  <- 1080
LI_HEIGHT <- 1350
LI_RES    <- 150

# ── Save: absolute change chart ───────────────────────────────────────────────
out_abs <- sprintf("linkedin_student_change_abs%s.png", staff_suffix)
agg_png(out_abs, width = LI_WIDTH, height = LI_HEIGHT, res = LI_RES, background = bg)
print(p_abs)
invisible(dev.off())
cat(sprintf("\nSaved: %s (%dx%d, LinkedIn portrait)\n", out_abs, LI_WIDTH, LI_HEIGHT))

# ── Save: percentage change chart ─────────────────────────────────────────────
out_pct <- sprintf("linkedin_student_change_pct%s.png", staff_suffix)
agg_png(out_pct, width = LI_WIDTH, height = LI_HEIGHT, res = LI_RES, background = bg)
print(p_pct)
invisible(dev.off())
cat(sprintf("Saved: %s (%dx%d, LinkedIn portrait)\n", out_pct, LI_WIDTH, LI_HEIGHT))

# ── Console summary for both charts ───────────────────────────────────────────
summarise_categories <- function(data, rank_by) {
  d <- if (rank_by == "abs") {
    data |> arrange(desc(stu_chg)) |> slice_head(n = TOP_N)
  } else {
    data |> arrange(desc(stu_pct_chg)) |> slice_head(n = TOP_N)
  }
  cat(sprintf("\n[%s chart] top %d providers \u2014 %d Normal, %d Flag, %d No staff data\n",
              rank_by, TOP_N,
              sum(d$category == "Normal"),
              sum(d$category == "Flag: outpaced staff"),
              sum(d$category == "No staff data")))
}

summarise_categories(both, "abs")
summarise_categories(both, "pct")