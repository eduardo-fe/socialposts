# ============================================================================
# MACCL ANALYSIS  –  Manchester Area Cross Country League
# ============================================================================
# Sections:
#   0.  Setup & Data Loading
#   1.  Data Cleaning & Feature Engineering
#   2.  Descriptive Analysis
#   3.  Aging Curve Analysis
#   4.  Longitudinal / Panel Data Analysis
#   5.  Club Analysis
#   6.  Venue / Conditions Analysis
#   7.  Individual Career Profiles
#   8.  Export
# ============================================================================


# ── 0. SETUP ─────────────────────────────────────────────────────────────────

required <- c("readxl","dplyr","ggplot2","lubridate","tidyr","scales",
              "plm","lme4","fixest","broom","ggridges","stringr","forcats",
              "purrr","knitr","gridExtra")

to_install <- required[!sapply(required, requireNamespace, quietly = TRUE)]
if (length(to_install) > 0) install.packages(to_install)

invisible(lapply(required, library, character.only = TRUE))

theme_maccl <- function() {
  theme_bw(base_size = 11) +
    theme(
      plot.title    = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(colour = "grey40", size = 10),
      strip.background = element_rect(fill = "#1F4E79"),
      strip.text       = element_text(colour = "white", face = "bold"),
      legend.position  = "bottom"
    )
}

MALE_COL   <- "#2166ac"
FEMALE_COL <- "#d6604d"
OUT_DIR    <- "."   # change to preferred output directory


# ── 1. DATA LOADING & CLEANING ───────────────────────────────────────────────

data_path <- "maccl_full_results.xlsx"   # adjust path as needed
raw <- read_excel(data_path)

# Rename columns to safe R names
maccl <- raw %>%
  rename(
    season      = Season,
    match       = Match,
    date_approx = Date,
    venue       = Venue,
    race_date_raw = `Race Date`,
    category    = Category,
    pos         = Pos,
    num         = Num,
    name        = Name,
    club        = Club,
    age_cat     = `Age Cat`,
    cat_pos     = `Cat Pos`,
    time_str    = Time,
    non_counter = `Non-Counter`
  )


# ── 1a. Time parsing ─────────────────────────────────────────────────────────
# Handles mm:ss and h:mm:ss
time_to_seconds <- function(x) {
  x <- as.character(x)
  ifelse(is.na(x), NA_real_, {
    parts <- strsplit(x, ":")[[1]]
    n <- length(parts)
    if (n == 2) as.numeric(parts[1]) * 60 + as.numeric(parts[2])
    else if (n == 3) as.numeric(parts[1]) * 3600 + as.numeric(parts[2]) * 60 + as.numeric(parts[3])
    else NA_real_
  })
}
maccl$time_sec <- vapply(maccl$time_str, time_to_seconds, numeric(1))


# ── 1b. Race date parsing ─────────────────────────────────────────────────────
# "14th January 2023" → remove ordinal suffix, then parse
maccl$race_date <- dmy(
  gsub("(\\d+)(st|nd|rd|th)", "\\1", maccl$race_date_raw, perl = TRUE)
)

# Season as ordered factor
seasons_ordered <- sort(unique(maccl$season))
maccl$season    <- factor(maccl$season, levels = seasons_ordered, ordered = TRUE)


# ── 1c. Category normalisation ───────────────────────────────────────────────
# Exact lookup built from all 15 unique category strings confirmed in the data.
# Older seasons used completely different naming conventions:
#   "Ladies"                   = Senior Ladies (earliest seasons)
#   "Senior Women"             = Senior Ladies (mid-period)
#   "Senior Women - Ladies"    = Senior Ladies (older seasons)
#   "Senior Women - Men"       = Senior Men    (confusingly named — these ARE men)
#   "Senior Women - Under 17"  = Under 17 - Ladies (old U17 girls name)

category_lookup <- c(
  "Ladies"                  = "Senior Ladies",
  "Senior Women"            = "Senior Ladies",
  "Senior Women - Ladies"   = "Senior Ladies",
  "Senior Women - Men"      = "Senior Men",
  "Senior Women - Under 17" = "Under 17 - Ladies"
)

maccl <- maccl %>%
  mutate(
    category = str_trim(category),
    category = if_else(category %in% names(category_lookup),
                       category_lookup[category],
                       category)
  )

remaining <- intersect(unique(maccl$category), names(category_lookup))
if (length(remaining) == 0) {
  cat("Category normalisation: OK\n")
} else {
  cat("WARNING - not remapped:", paste(remaining, collapse=", "), "\n")
}


# ── 1d. Gender from Category ─────────────────────────────────────────────────
# Uses exact category names after normalisation — no regex needed.
maccl <- maccl %>%
  mutate(
    gender = case_when(
      category %in% c("Senior Ladies",
                      "Under 17 - Ladies",
                      "Under 11 - Girls",
                      "Under 13 - Girls",
                      "Under 15 - Girls")  ~ "Female",
      category %in% c("Senior Men",
                      "Under 17 - Men",
                      "Under 11 - Boys",
                      "Under 13 - Boys",
                      "Under 15 - Boys")   ~ "Male",
      str_detect(age_cat, "^L")            ~ "Female",
      str_detect(age_cat, "^V")            ~ "Male",
      TRUE                                  ~ NA_character_
    )
  )



# ── 1e. Age category coding ──────────────────────────────────────────────────
#
# MACCL category structure (post-normalisation):
#
#  JUNIORS
#   Under 11  Boys/Girls   ~10 yrs
#   Under 13  Boys/Girls   11-12
#   Under 15  Boys/Girls   13-14
#   Under 17  Men/Ladies   15-16
#
#  SENIORS
#   "Senior Men"    with blank Age Cat → open (20-39)
#   "Senior Men"    with Age Cat "U20" → 17-19
#   "Senior Men"    with Age Cat "V40" → 40-44, etc.
#   "Senior Ladies" with blank Age Cat → open (20-34)
#   "Senior Ladies" with Age Cat "L20" → 17-19
#   "Senior Ladies" with Age Cat "L35" → 35-39, etc.
#
# Note: "L" prefix = female age group; "V" = male vet.

maccl <- maccl %>%
  mutate(
    age_cat = if_else(is.na(age_cat), "", as.character(age_cat)),
    
    # ── Junior age groups from Category ──────────────────────────────────────
    age_group_label = case_when(
      str_detect(category, "Under 11")  ~ "U11",
      str_detect(category, "Under 13")  ~ "U13",
      str_detect(category, "Under 15")  ~ "U15",
      str_detect(category, "Under 17")  ~ "U17",
      
      # ── Open seniors (blank age_cat) ─────────────────────────────────────
      age_cat == "" & gender == "Male"    ~ "SM Open (20-39)",
      age_cat == "" & gender == "Female"  ~ "SF Open (20-34)",
      
      # ── Junior seniors (17-19) ────────────────────────────────────────────
      age_cat %in% c("U20","L20","V20")  ~ "17-19",
      
      # ── Veteran bands ─────────────────────────────────────────────────────
      age_cat %in% c("V35","L35")  ~ "35-39",
      age_cat %in% c("V40","L40")  ~ "40-44",
      age_cat %in% c("V45","L45")  ~ "45-49",
      age_cat %in% c("V50","L50")  ~ "50-54",
      age_cat %in% c("V55","L55")  ~ "55-59",
      age_cat %in% c("V60","L60")  ~ "60-64",
      age_cat %in% c("V65","L65")  ~ "65-69",
      age_cat %in% c("V70","L70")  ~ "70-74",
      age_cat %in% c("V75","L75")  ~ "75-79",
      age_cat %in% c("V80","L80")  ~ "80+",
      TRUE                          ~ age_cat   # catch-all
    ),
    
    # ── Numeric age (midpoint of band) for regression ────────────────────
    age_numeric = case_when(
      age_group_label == "U11"             ~ 10,
      age_group_label == "U13"             ~ 12,
      age_group_label == "U15"             ~ 14,
      age_group_label == "U17"             ~ 16,
      age_group_label == "17-19"           ~ 18,
      age_group_label == "SM Open (20-39)" ~ 29.5,
      age_group_label == "SF Open (20-34)" ~ 27.0,
      age_group_label == "35-39"           ~ 37,
      age_group_label == "40-44"           ~ 42,
      age_group_label == "45-49"           ~ 47,
      age_group_label == "50-54"           ~ 52,
      age_group_label == "55-59"           ~ 57,
      age_group_label == "60-64"           ~ 62,
      age_group_label == "65-69"           ~ 67,
      age_group_label == "70-74"           ~ 72,
      age_group_label == "75-79"           ~ 77,
      age_group_label == "80+"             ~ 82,
      TRUE                                 ~ NA_real_
    ),
    
    # ── Broad type ──────────────────────────────────────────────────────────
    runner_type = case_when(
      str_detect(age_group_label, "^U")   ~ "Junior",
      age_group_label == "17-19"          ~ "Junior Senior",
      str_detect(age_group_label, "Open") ~ "Senior Open",
      TRUE                                ~ "Senior Vet"
    ),
    
    # ── Non-counter flag ────────────────────────────────────────────────────
    is_non_counter = (!is.na(non_counter) & non_counter == "Yes")
  )


# ── 1e. Race-level normalisation ─────────────────────────────────────────────
# Normalise within the SAME category on the SAME day (correct distance/course)
maccl <- maccl %>%
  filter(!is.na(time_sec), time_sec > 60) %>%   # remove implausible times
  mutate(race_cat_id = interaction(race_date, category, drop = TRUE)) %>%
  group_by(race_cat_id) %>%
  mutate(
    n_in_race    = n(),
    winner_time  = min(time_sec, na.rm = TRUE),
    median_time  = median(time_sec, na.rm = TRUE),
    rel_time     = time_sec / winner_time,        # 1 = winner; >1 = behind
    pct_behind   = (rel_time - 1) * 100,
    log_rel_time = log(rel_time),
    race_pctile  = percent_rank(desc(time_sec))   # 0 = slowest, 1 = fastest
  ) %>%
  ungroup()

# ── 1f. Club name harmonisation ──────────────────────────────────────────────
# Maps all known spelling variants to a single canonical name per club.
# Confirmed by manual review of all 112 unique raw club name strings.

club_lookup <- c(
  # Altrincham & District AC (variants 1-5)
  "Altrincham & District"            = "Altrincham & District AC",
  "Altrincham & District Ac"         = "Altrincham & District AC",
  "Altrincham & District Athletic"   = "Altrincham & District AC",
  "Altrincham&district AC"           = "Altrincham & District AC",
  
  # Congleton Harriers (11-12)
  "Congleton H & Ac"                 = "Congleton Harriers",
  
  # Dragons Running Club Sale (14-15)
  "Dragons Rc Sale"                  = "Dragons Running Club (Sale)",
  "Dragons Running Club (sale)"      = "Dragons Running Club (Sale)",
  
  # East Cheshire Harriers & Tameside AC (16-19)
  "E Cheshire H & Tameside AC"       = "East Cheshire Harriers & Tameside AC",
  "E Cheshire H&tameside AC"         = "East Cheshire Harriers & Tameside AC",
  "East Cheshire Harriers"           = "East Cheshire Harriers & Tameside AC",
  "East Cheshire Harriers & Tames"   = "East Cheshire Harriers & Tameside AC",
  
  # Halton & Frodsham Harriers (22-25)
  "Halton & Frodsham"                = "Halton & Frodsham Harriers",
  "Halton&frodsham"                  = "Halton & Frodsham Harriers",
  "Halton&frodsham H"                = "Halton & Frodsham Harriers",
  
  # Horwich RMI Harriers (27-28)
  "Horwich R M I Harriers"           = "Horwich RMI Harriers",
  "Horwich Rmi"                      = "Horwich RMI Harriers",
  
  # Hyde Village Striders (29-30)
  "Hyde Village Striders Rc"         = "Hyde Village Striders",
  
  # Knutsford Harriers (31-32)
  "Knutsford Harriers & Athletics"   = "Knutsford Harriers",
  
  # Knutsford Tri Club (33-34)
  "Knutsford Tri"                    = "Knutsford Tri Club",
  
  # Liverpool Harriers & AC (37-40)
  "Liverpool H & AC"                 = "Liverpool Harriers & AC",
  "Liverpool H&ac"                   = "Liverpool Harriers & AC",
  "Liverpool Harriers & Ac"          = "Liverpool Harriers & AC",
  
  # Macclesfield Harriers & AC (41-43)
  "Macclesfield H&ac"                = "Macclesfield Harriers & AC",
  "Macclesfield Harriers & Ac"       = "Macclesfield Harriers & AC",
  
  # Manchester Harriers & AC (45-47)
  "Manchester H&ac"                  = "Manchester Harriers & AC",
  "Manchester Harriers & Ac"         = "Manchester Harriers & AC",
  
  # Manchester Metropolitan University AC (48-50, 61-63)
  "Manchester Met Uni AC"            = "Manchester Metropolitan University AC",
  "Manchester Metropolitan Uni"      = "Manchester Metropolitan University AC",
  "Manchester Metropolitan Univer"   = "Manchester Metropolitan University AC",
  "Manchester University"            = "Manchester Metropolitan University AC",
  "MMU"                              = "Manchester Metropolitan University AC",
  "Mmu Athletics"                    = "Manchester Metropolitan University AC",
  "Mmu H"                            = "Manchester Metropolitan University AC",
  
  # Manchester Triathlon Club (52-53)
  "Manchester Triathlon"             = "Manchester Triathlon Club",
  
  # Manchester YMCA Harriers (55-56)
  "Manchester Ymca H"                = "Manchester YMCA Harriers",
  "Manchester Ymca Harriers"         = "Manchester YMCA Harriers",
  
  # Middleton Harriers AC (58-60)
  "Middleton H AC"                   = "Middleton Harriers AC",
  "Middleton Harriers Ac"            = "Middleton Harriers AC",
  
  # Oldham & Royton Harriers & AC (65-67)
  "Oldham & Royton H & Ac"           = "Oldham & Royton Harriers & AC",
  "Oldham & Royton H & AC"           = "Oldham & Royton Harriers & AC",
  "Oldham & Royton H&ac"             = "Oldham & Royton Harriers & AC",
  
  # Quays Running Club (68-69)
  "Quay Runners"                     = "Quays Running Club",
  
  # Sale Harriers Manchester (73-74)
  "Sale H Manchester"                = "Sale Harriers Manchester",
  
  # Salford Harriers & AC (75-76)
  "Salford H&ac"                     = "Salford Harriers & AC",
  
  # Salford Metropolitan AC (77-79)
  "Salford Met AC"                   = "Salford Metropolitan AC",
  "Salford Metropolitan Ac"          = "Salford Metropolitan AC",
  
  # St Helens Sutton AC (80-81)
  "St Helens Sutton Ac"              = "St Helens Sutton AC",
  
  # Stockport Harriers & AC (85-86)
  "Stockport H&ac"                   = "Stockport Harriers & AC",
  "Stockport Harriers & Ac"          = "Stockport Harriers & AC",
  
  # Swinton Running Club (89-90)
  "Swinton RC"                       = "Swinton Running Club",
  
  # Trafford Athletic Club (92-93)
  "Trafford AC"                      = "Trafford Athletic Club",
  
  # Vale Royal AC (96-97)
  "Vale Royal Ac"                    = "Vale Royal AC",
  
  # Warrington AC (98-100)
  "Warrington A C"                   = "Warrington AC",
  "Warrington Athletic Club"         = "Warrington AC",
  
  # West Cheshire AC (104-105)
  "West Cheshire Athletic Club"      = "West Cheshire AC",
  
  # Wilmslow Running Club (106-107)
  "Wilmslow RC"                      = "Wilmslow Running Club",
  
  # Wirral AC (111-112)
  "Wirral Ac"                        = "Wirral AC"
)

maccl <- maccl %>%
  mutate(club = if_else(club %in% names(club_lookup),
                        club_lookup[club],
                        club))

remaining <- intersect(unique(maccl$club), names(club_lookup))
if (length(remaining) == 0) {
  cat("Club harmonisation: OK -", length(club_lookup),
      "raw names mapped to", n_distinct(club_lookup), "canonical names.\n")
} else {
  cat("WARNING: these names were not remapped:\n")
  print(remaining)
}


cat("Rows after cleaning:", nrow(maccl), "\n")
cat("Seasons:", paste(levels(maccl$season), collapse = ", "), "\n")
cat("Date range:", format(min(maccl$race_date, na.rm=TRUE)),
    "to", format(max(maccl$race_date, na.rm=TRUE)), "\n\n")


# ── 2. DESCRIPTIVE ANALYSIS ──────────────────────────────────────────────────

## 2.1 Overall participation ─────────────────────────────────────────────────
participation <- maccl %>%
  group_by(season, match, race_date, category) %>%
  summarise(n_finishers = n(), .groups = "drop")

participation_summary <- participation %>%
  group_by(season, match, race_date) %>%
  summarise(
    total_finishers = sum(n_finishers),
    n_categories    = n_distinct(category),
    .groups = "drop"
  )

cat("=== Participation by Season ===\n")
maccl %>%
  group_by(season) %>%
  summarise(
    matches     = n_distinct(match),
    entries     = n(),
    unique_runners = n_distinct(name),
    unique_clubs   = n_distinct(club),
    .groups = "drop"
  ) %>%
  print()

## 2.2 Participation trend plot ───────────────────────────────────────────────
p_participation <- participation_summary %>%
  mutate(label = paste0(season, " M", match)) %>%
  ggplot(aes(x = race_date, y = total_finishers)) +
  geom_col(aes(fill = season), show.legend = FALSE) +
  geom_smooth(method = "lm", se = TRUE, colour = "black", linetype = "dashed", size = 0.7) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = comma) +
  labs(title = "Total Finishers per Race",
       subtitle = "All categories combined",
       x = NULL, y = "Finishers") +
  theme_maccl()
print(p_participation)
ggsave(file.path(OUT_DIR, "01_participation_trend.png"), p_participation,
       width = 12, height = 5, dpi = 300)

## 2.3 Category breakdown ─────────────────────────────────────────────────────
cat_summary <- maccl %>%
  group_by(category, gender, runner_type) %>%
  summarise(entries = n(), unique_runners = n_distinct(name), .groups = "drop") %>%
  arrange(desc(entries))

p_cat <- cat_summary %>%
  mutate(category = fct_reorder(category, entries)) %>%
  ggplot(aes(x = entries, y = category, fill = gender)) +
  geom_col() +
  scale_fill_manual(values = c(Male = MALE_COL, Female = FEMALE_COL),
                    na.value = "grey60") +
  scale_x_continuous(labels = comma) +
  labs(title = "Total Entries by Category", x = "Total entries", y = NULL, fill = "Gender") +
  theme_maccl() +
  theme(legend.position = "right")
print(p_cat)
ggsave(file.path(OUT_DIR, "02_entries_by_category.png"), p_cat,
       width = 10, height = 8, dpi = 300)

## 2.4 Time distributions by category ────────────────────────────────────────
p_ridges <- maccl %>%
  filter(!is.na(gender)) %>%
  mutate(time_min = time_sec / 60,
         age_group_label = fct_reorder(age_group_label, ifelse(is.na(age_numeric), Inf, age_numeric))) %>%
  ggplot(aes(x = time_min, y = age_group_label, fill = gender)) +
  geom_density_ridges(alpha = 0.6, scale = 0.9, rel_min_height = 0.01) +
  scale_fill_manual(values = c(Male = MALE_COL, Female = FEMALE_COL)) +
  scale_x_continuous(labels = comma) +
  facet_wrap(~gender, scales = "free_x") +
  labs(title = "Finishing Time Distributions by Age Group",
       x = "Time (minutes)", y = "Age group", fill = "Gender") +
  theme_maccl() +
  theme(legend.position = "none")
print(p_ridges)
ggsave(file.path(OUT_DIR, "03_time_distributions.png"), p_ridges,
       width = 12, height = 8, dpi = 300)

## 2.5 Gender participation ratio over seasons ────────────────────────────────
gender_ratio <- maccl %>%
  filter(!is.na(gender)) %>%
  group_by(season, gender) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = gender, values_from = n, values_fill = 0) %>%
  mutate(pct_female = Female / (Male + Female) * 100)

cat("\n=== Female Participation % by Season ===\n")
print(gender_ratio)

p_gender <- gender_ratio %>%
  ggplot(aes(x = as.numeric(season), y = pct_female)) +
  geom_line(colour = FEMALE_COL, size = 1.2) +
  geom_point(colour = FEMALE_COL, size = 3) +
  scale_x_continuous(breaks = seq_along(levels(maccl$season)),
                     labels = levels(maccl$season)) +
  scale_y_continuous(limits = c(0, 60), labels = function(x) paste0(x, "%")) +
  labs(title = "Female Participation Rate Over Seasons",
       x = NULL, y = "% Female finishers") +
  theme_maccl() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
print(p_gender)
ggsave(file.path(OUT_DIR, "04_gender_ratio.png"), p_gender, width = 10, height = 5, dpi = 300)

## 2.6 Venue summary ──────────────────────────────────────────────────────────
venue_summary <- maccl %>%
  filter(!is.na(venue), venue != "") %>%
  group_by(venue) %>%
  summarise(races = n_distinct(race_cat_id),
            avg_entrants = mean(n_in_race),
            .groups = "drop") %>%
  arrange(desc(races))

cat("\n=== Venue Summary ===\n")
print(venue_summary)


# ── 3. AGING CURVE ANALYSIS ──────────────────────────────────────────────────
#
# Strategy: we have two distinct populations that should NOT be mixed
#   A) Juniors: U11 → U17 (compulsory age-group bands)
#   B) Seniors: 17-19, SM/SF Open, V35/L35 → V80/L80
#
# For the SENIOR aging curve we use the CAT midpoints as proxies for age.
# The quadratic curve captures the inverted-U shape of athletic performance.

## 3.1 Junior aging curve ─────────────────────────────────────────────────────
junior_data <- maccl %>%
  filter(runner_type == "Junior", !is.na(age_numeric), !is.na(gender),
         !is_non_counter)

p_junior_curve <- junior_data %>%
  ggplot(aes(x = age_numeric, y = rel_time, colour = gender)) +
  geom_jitter(alpha = 0.08, width = 0.3) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2),
              se = TRUE, size = 1.2) +
  scale_colour_manual(values = c(Male = MALE_COL, Female = FEMALE_COL)) +
  scale_y_continuous(limits = c(0.9, 2.5)) +
  scale_x_continuous(breaks = c(10, 12, 14, 16),
                     labels = c("U11","U13","U15","U17")) +
  labs(title = "Junior Aging Curve",
       subtitle = "Relative time (1 = winner in same race & category)",
       x = "Age group", y = "Relative time", colour = "Gender") +
  theme_maccl()
print(p_junior_curve)
ggsave(file.path(OUT_DIR, "05_junior_aging_curve.png"), p_junior_curve,
       width = 9, height = 6, dpi = 300)

## 3.2 Senior aging curve ─────────────────────────────────────────────────────
senior_data <- maccl %>%
  filter(runner_type %in% c("Junior Senior","Senior Open","Senior Vet"),
         !is.na(age_numeric), !is.na(gender), !is_non_counter)

# Quadratic model with gender interaction
aging_model_senior <- lm(
  log_rel_time ~ age_numeric * gender + I(age_numeric^2),
  data = senior_data
)
cat("\n=== Senior Aging Curve Model ===\n")
print(summary(aging_model_senior))

# Grid of predictions for plot
age_grid <- expand.grid(
  age_numeric = seq(17, 82, by = 0.5),
  gender      = c("Male","Female")
)
age_grid$pred_log  <- predict(aging_model_senior, newdata = age_grid)
age_grid$pred_rel  <- exp(age_grid$pred_log)

# Peak age per gender (d/d_age = 0: -b / 2c, ignoring interaction term)
peak_ages <- senior_data %>%
  group_by(gender) %>%
  group_modify(~ {
    m <- lm(log_rel_time ~ age_numeric + I(age_numeric^2), data = .x)
    b <- coef(m)["age_numeric"]
    cq <- coef(m)["I(age_numeric^2)"]
    tibble(peak_age = -b / (2 * cq),
           min_pred = exp(predict(m, newdata = data.frame(age_numeric = -b/(2*cq)))))
  }) %>%
  ungroup()

cat("\n=== Estimated peak ages ===\n")
print(peak_ages)

p_senior_curve <- ggplot() +
  geom_jitter(data = senior_data,
              aes(x = age_numeric, y = rel_time, colour = gender),
              alpha = 0.06, width = 1) +
  geom_line(data = age_grid,
            aes(x = age_numeric, y = pred_rel, colour = gender),
            size = 1.4) +
  geom_vline(data = peak_ages,
             aes(xintercept = peak_age, colour = gender),
             linetype = "dashed", size = 0.8, show.legend = FALSE) +
  scale_colour_manual(values = c(Male = MALE_COL, Female = FEMALE_COL)) +
  scale_x_continuous(breaks = c(18,29.5,37,42,47,52,57,62,67,72,77,82),
                     labels = c("17-19","Open","35-39","40-44","45-49",
                                "50-54","55-59","60-64","65-69","70-74","75-79","80+")) +
  scale_y_continuous(limits = c(0.9, 2.8)) +
  labs(title = "Senior Aging Curve",
       subtitle = "Relative to winner in same race. Dashed = estimated peak.",
       x = "Age group", y = "Relative time (1 = winner)", colour = "Gender") +
  theme_maccl() +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))
print(p_senior_curve)
ggsave(file.path(OUT_DIR, "06_senior_aging_curve.png"), p_senior_curve,
       width = 11, height = 6, dpi = 300)

## 3.3 Age group mean performance table ───────────────────────────────────────
age_group_perf <- maccl %>%
  filter(!is.na(age_group_label), !is_non_counter) %>%
  group_by(age_group_label, gender) %>%
  summarise(
    n              = n(),
    mean_rel_time  = mean(rel_time, na.rm = TRUE),
    sd_rel_time    = sd(rel_time,  na.rm = TRUE),
    median_time_min = median(time_sec, na.rm = TRUE) / 60,
    .groups = "drop"
  ) %>%
  arrange(gender, mean_rel_time)

cat("\n=== Age Group Performance Summary ===\n")
print(age_group_perf, n = 40)


# ── 4. LONGITUDINAL / PANEL DATA ANALYSIS ───────────────────────────────────
#
# Build a runner × race panel.  We use counter runners only to avoid
# guest/non-scoring appearances distorting trajectories.

## 4.1 Identify multi-race runners ────────────────────────────────────────────
# Standardise name (trim whitespace, collapse multiple spaces)
maccl <- maccl %>%
  mutate(name_clean = str_squish(str_to_upper(name)))

runner_freq <- maccl %>%
  filter(!is_non_counter) %>%
  group_by(name_clean) %>%
  summarise(
    n_races    = n(),
    n_seasons  = n_distinct(season),
    first_race = min(race_date, na.rm = TRUE),
    last_race  = max(race_date, na.rm = TRUE),
    career_span_days = as.numeric(max(race_date,na.rm=TRUE) - min(race_date,na.rm=TRUE)),
    gender     = first(gender),
    primary_club = names(sort(table(club), decreasing = TRUE))[1],
    .groups = "drop"
  ) %>%
  arrange(desc(n_races))

cat("\n=== Runner Frequency Distribution ===\n")
print(summary(runner_freq$n_races))
cat("Runners with 5+ races:", sum(runner_freq$n_races >= 5), "\n")
cat("Runners with 10+ races:", sum(runner_freq$n_races >= 10), "\n")
cat("Runners with 20+ races:", sum(runner_freq$n_races >= 20), "\n")

## 4.2 Build panel ────────────────────────────────────────────────────────────
MIN_RACES <- 5    # minimum appearances for panel inclusion

panel_runners <- runner_freq %>%
  filter(n_races >= MIN_RACES) %>%
  pull(name_clean)

panel <- maccl %>%
  filter(name_clean %in% panel_runners, !is_non_counter, !is.na(time_sec)) %>%
  arrange(name_clean, race_date) %>%
  group_by(name_clean) %>%
  mutate(
    first_race_date  = min(race_date, na.rm = TRUE),
    race_number      = row_number(),
    time_years       = as.numeric(race_date - first_race_date) / 365.25,
    # Age evolving: use category midpoint at entry + time elapsed
    age_at_entry     = first(age_numeric),
    age_evolving     = age_at_entry + time_years,
    # Performance trend (z-score within runner)
    z_reltime        = (rel_time - mean(rel_time, na.rm=TRUE)) / sd(rel_time, na.rm=TRUE),
    # Career phase
    career_phase     = case_when(
      race_number <= 3            ~ "Early",
      time_years >= max(time_years) - 0.5 ~ "Late",
      TRUE                        ~ "Mid"
    )
  ) %>%
  ungroup()

cat("\n=== Panel Dataset ===\n")
cat("Runners:", n_distinct(panel$name_clean), "\n")
cat("Observations:", nrow(panel), "\n")
cat("Avg races per runner:", round(mean(runner_freq$n_races[runner_freq$n_races >= MIN_RACES]),1), "\n")

## 4.3 Within-runner improvement/decline ───────────────────────────────────────
# Compute OLS slope per runner (improvement = negative slope in rel_time)
runner_slopes <- panel %>%
  group_by(name_clean) %>%
  filter(n_distinct(race_date) >= 3) %>%          # need 3+ points for slope
  group_by(name_clean, gender) %>%
  summarise(
    n_obs         = n(),
    slope_per_year = tryCatch(
      coef(lm(rel_time ~ time_years))[["time_years"]], error = function(e) NA_real_),
    mean_rel_time  = mean(rel_time, na.rm = TRUE),
    cv_time        = sd(time_sec, na.rm=TRUE) / mean(time_sec, na.rm=TRUE),  # consistency
    .groups = "drop"
  ) %>%
  filter(!is.na(slope_per_year))

cat("\n=== Individual slope distribution (per year) ===\n")
print(summary(runner_slopes$slope_per_year))
cat("Improving (negative slope):", sum(runner_slopes$slope_per_year < 0), "\n")
cat("Declining (positive slope):", sum(runner_slopes$slope_per_year > 0), "\n")

p_slopes <- runner_slopes %>%
  filter(!is.na(gender)) %>%
  ggplot(aes(x = slope_per_year, fill = gender)) +
  geom_histogram(binwidth = 0.02, alpha = 0.7, position = "identity") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c(Male = MALE_COL, Female = FEMALE_COL)) +
  facet_wrap(~gender, ncol = 1) +
  labs(title = "Distribution of Individual Performance Trends",
       subtitle = "Negative = improving over time; positive = slowing",
       x = "Change in relative time per year", y = "Runners", fill = NULL) +
  theme_maccl() + theme(legend.position = "none")
print(p_slopes)
ggsave(file.path(OUT_DIR, "07_individual_slopes.png"), p_slopes,
       width = 9, height = 6, dpi = 300)

## 4.4 Panel models ───────────────────────────────────────────────────────────

# Convert to plm panel (runner × race_date)
pdata <- pdata.frame(
  panel %>% mutate(runner_id = as.numeric(factor(name_clean))),
  index = c("runner_id", "race_date")
)

cat("\n--- MODEL 1: Pooled OLS ---\n")
m_pooled <- plm(rel_time ~ age_evolving + I(age_evolving^2) + time_years + gender,
                data = pdata, model = "pooling")
print(summary(m_pooled))

cat("\n--- MODEL 2: Runner Fixed Effects ---\n")
m_fe <- plm(rel_time ~ age_evolving + I(age_evolving^2) + time_years,
            data = pdata, model = "within")
print(summary(m_fe))

cat("\n--- MODEL 3: Random Effects ---\n")
m_re <- plm(rel_time ~ age_evolving + I(age_evolving^2) + time_years + gender,
            data = pdata, model = "random")
print(summary(m_re))

# Hausman test: requires SAME specification (same RHS) for FE and RE
m_fe_haus <- plm(rel_time ~ age_evolving + I(age_evolving^2) + time_years,
                 data = pdata, model = "within")
m_re_haus <- plm(rel_time ~ age_evolving + I(age_evolving^2) + time_years,
                 data = pdata, model = "random")
cat("\n--- Hausman Test (FE vs RE) ---\n")
print(phtest(m_fe_haus, m_re_haus))

cat("\n--- MODEL 4: Two-Way FE (runner + race) via fixest ---\n")
# fixest handles large factor sets efficiently
panel$race_fe_id <- as.numeric(factor(paste(panel$race_date, panel$category)))
m_twoway <- feols(rel_time ~ age_evolving + I(age_evolving^2) + time_years |
                    name_clean + race_fe_id,
                  data = panel, cluster = ~name_clean)
print(summary(m_twoway))

cat("\n--- MODEL 5: Mixed Effects (random intercepts + slopes per runner) ---\n")
m_mixed <- lmer(
  rel_time ~ age_evolving + I(age_evolving^2) + time_years + gender +
    (1 + time_years | name_clean),
  data  = panel,
  REML  = TRUE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
print(summary(m_mixed))

# Model comparison table
model_summary <- bind_rows(
  tidy(m_pooled) %>% mutate(model = "Pooled OLS"),
  tidy(m_fe)     %>% mutate(model = "FE (runner)"),
  tidy(m_re)     %>% mutate(model = "RE"),
  tidy(m_twoway) %>% mutate(model = "Two-Way FE")
) %>%
  filter(term %in% c("age_evolving","I(age_evolving^2)","time_years","genderMale")) %>%
  select(model, term, estimate, std.error, p.value)

cat("\n=== Model Comparison (key coefficients) ===\n")
print(model_summary, n = 40)

## 4.5 Longitudinal trajectory plots ─────────────────────────────────────────

# Top 30 most prolific runners
top_runners <- runner_freq %>%
  filter(n_races >= MIN_RACES) %>%
  arrange(desc(n_races)) %>%
  slice_head(n = 30) %>%
  pull(name_clean)

p_trajectories <- panel %>%
  filter(name_clean %in% top_runners, !is.na(gender)) %>%
  ggplot(aes(x = time_years, y = rel_time, group = name_clean, colour = gender)) +
  geom_line(alpha = 0.5, size = 0.6) +
  geom_point(alpha = 0.4, size = 0.9) +
  geom_smooth(aes(group = gender), method = "loess", se = TRUE,
              colour = "black", size = 1.1, linetype = "dashed") +
  scale_colour_manual(values = c(Male = MALE_COL, Female = FEMALE_COL)) +
  facet_wrap(~gender) +
  labs(title = "Performance Trajectories: Top 30 Most Prolific Runners",
       subtitle = "Relative time vs years since first race. Dashed = group trend.",
       x = "Years since first race", y = "Relative time (1 = winner)", colour = NULL) +
  theme_maccl() + theme(legend.position = "none")
print(p_trajectories)
ggsave(file.path(OUT_DIR, "08_longitudinal_trajectories.png"), p_trajectories,
       width = 12, height = 6, dpi = 300)

# Career phase performance
p_career_phase <- panel %>%
  filter(!is.na(gender)) %>%
  ggplot(aes(x = career_phase, y = rel_time, fill = gender)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.1) +
  scale_fill_manual(values = c(Male = MALE_COL, Female = FEMALE_COL)) +
  scale_x_discrete(limits = c("Early","Mid","Late")) +
  labs(title = "Performance by Career Phase",
       subtitle = "Early = first 3 races; Late = last 6 months",
       x = "Career phase", y = "Relative time", fill = NULL) +
  theme_maccl()
print(p_career_phase)
ggsave(file.path(OUT_DIR, "09_career_phase.png"), p_career_phase,
       width = 8, height = 5, dpi = 300)

# Within-runner consistency
p_consistency <- runner_slopes %>%
  filter(!is.na(gender), n_obs >= MIN_RACES) %>%
  ggplot(aes(x = mean_rel_time, y = cv_time, colour = gender)) +
  geom_point(aes(size = n_obs), alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, size = 1) +
  scale_colour_manual(values = c(Male = MALE_COL, Female = FEMALE_COL)) +
  scale_size_continuous(range = c(1, 6)) +
  labs(title = "Performance Level vs Consistency",
       subtitle = "CV = within-runner coefficient of variation; lower = more consistent",
       x = "Mean relative time (ability level)", y = "CV of finish time",
       colour = "Gender", size = "Races") +
  theme_maccl()
print(p_consistency)
ggsave(file.path(OUT_DIR, "10_consistency.png"), p_consistency,
       width = 9, height = 6, dpi = 300)


# ── 5. CLUB ANALYSIS ─────────────────────────────────────────────────────────

## 5.1 Club size & dominance ──────────────────────────────────────────────────
club_summary <- maccl %>%
  filter(!is_non_counter) %>%
  group_by(club) %>%
  summarise(
    total_entries   = n(),
    unique_runners  = n_distinct(name_clean),
    seasons_active  = n_distinct(season),
    mean_rel_time   = mean(rel_time, na.rm = TRUE),
    median_pos      = median(pos,    na.rm = TRUE),
    pct_top10       = mean(pos <= 10, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(total_entries))

cat("\n=== Top 20 Clubs by Entries ===\n")
print(head(club_summary, 20))

## 5.2 Club performance plot ──────────────────────────────────────────────────
p_clubs <- club_summary %>%
  filter(total_entries >= 50) %>%
  mutate(club = fct_reorder(club, mean_rel_time)) %>%
  ggplot(aes(x = mean_rel_time, y = club, size = total_entries,
             colour = mean_rel_time)) +
  geom_point() +
  scale_colour_gradient(low = "#2166ac", high = "#d6604d", guide = "none") +
  scale_size_continuous(range = c(2, 8)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey40") +
  labs(title = "Club Performance Profile",
       subtitle = "Clubs with 50+ entries. Mean relative time vs all categories.",
       x = "Mean relative time (lower = faster)", y = NULL, size = "Entries") +
  theme_maccl() + theme(legend.position = "right")
print(p_clubs)
ggsave(file.path(OUT_DIR, "11_club_performance.png"), p_clubs,
       width = 11, height = 9, dpi = 300)

## 5.3 Club participation trend ───────────────────────────────────────────────
top_clubs <- club_summary %>% slice_head(n = 8) %>% pull(club)

club_season_trend <- maccl %>%
  filter(club %in% top_clubs) %>%
  group_by(club, season) %>%
  summarise(entries = n(), .groups = "drop")

p_club_trend <- club_season_trend %>%
  ggplot(aes(x = as.numeric(season), y = entries,
             colour = club, group = club)) +
  geom_line(size = 1) +
  geom_point() +
  scale_x_continuous(breaks = seq_along(levels(maccl$season)),
                     labels = levels(maccl$season)) +
  labs(title = "Participation Trend: Top 8 Clubs",
       x = NULL, y = "Entries per season", colour = "Club") +
  theme_maccl() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
print(p_club_trend)
ggsave(file.path(OUT_DIR, "12_club_trend.png"), p_club_trend,
       width = 12, height = 6, dpi = 300)


# ── 6. VENUE ANALYSIS ────────────────────────────────────────────────────────

# Average pace per venue & category to detect course difficulty
venue_effect <- maccl %>%
  filter(!is.na(venue), venue != "") %>%
  group_by(venue, category) %>%
  summarise(
    races        = n_distinct(race_date),
    mean_time    = mean(time_sec, na.rm = TRUE),
    median_time  = median(time_sec, na.rm = TRUE),
    entries      = n(),
    .groups      = "drop"
  )

# Standardise: z-score within category to isolate venue effect
venue_z <- venue_effect %>%
  group_by(category) %>%
  mutate(z_time = (mean_time - mean(mean_time)) / sd(mean_time)) %>%
  ungroup()

p_venue <- venue_z %>%
  filter(races >= 2) %>%
  ggplot(aes(x = z_time, y = venue, fill = z_time > 0)) +
  geom_col() +
  scale_fill_manual(values = c("FALSE" = "#2166ac", "TRUE" = "#d6604d"),
                    labels = c("Faster than avg", "Slower than avg"),
                    guide = "none") +
  facet_wrap(~category, scales = "free_x", ncol = 3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Venue Difficulty Effect",
       subtitle = "Z-score of mean time relative to category average across venues",
       x = "Standardised time (positive = slower course)", y = NULL) +
  theme_maccl() +
  theme(axis.text.y = element_text(size = 7))
print(p_venue)
ggsave(file.path(OUT_DIR, "13_venue_effect.png"), p_venue,
       width = 14, height = 10, dpi = 300)


# ── 7. INDIVIDUAL CAREER PROFILES ────────────────────────────────────────────

# Top performers by mean relative time (min 10 races)
top_performers <- runner_freq %>%
  filter(n_races >= 10) %>%
  left_join(
    panel %>% group_by(name_clean) %>%
      summarise(mean_rel = mean(rel_time, na.rm = TRUE), .groups = "drop"),
    by = "name_clean"
  ) %>%
  arrange(mean_rel) %>%
  slice_head(n = 20)

cat("\n=== Top 20 Performers (min 10 races, ranked by mean relative time) ===\n")
print(top_performers %>% select(name_clean, gender, primary_club, n_races,
                                n_seasons, mean_rel, career_span_days))

# Career trajectory facet for top 12
top12 <- top_performers$name_clean[1:min(12, nrow(top_performers))]

p_top12 <- panel %>%
  filter(name_clean %in% top12) %>%
  mutate(display_name = str_to_title(name_clean)) %>%
  ggplot(aes(x = race_date, y = rel_time, colour = gender)) +
  geom_line(size = 0.7, alpha = 0.8) +
  geom_point(size = 1.5, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, size = 0.6, linetype = "dashed",
              colour = "grey30") +
  scale_colour_manual(values = c(Male = MALE_COL, Female = FEMALE_COL)) +
  scale_x_date(date_labels = "%Y") +
  facet_wrap(~display_name, scales = "free_y", ncol = 3) +
  labs(title = "Career Trajectories: Top 12 Performers",
       x = NULL, y = "Relative time", colour = NULL) +
  theme_maccl() +
  theme(legend.position = "none",
        strip.text = element_text(size = 8))
print(p_top12)
ggsave(file.path(OUT_DIR, "14_top12_careers.png"), p_top12,
       width = 14, height = 10, dpi = 300)


# ── 8. SEASON-ON-SEASON PERFORMANCE TRENDS ───────────────────────────────────

# Are times improving over the years within each category?
season_perf <- maccl %>%
  filter(!is_non_counter, !is.na(gender)) %>%
  group_by(season, category, gender) %>%
  summarise(
    mean_rel_time  = mean(rel_time, na.rm = TRUE),
    entries        = n(),
    .groups = "drop"
  )

# Senior Men and Senior Ladies only for clarity
p_season_trend <- season_perf %>%
  filter(str_detect(category, "Senior")) %>%
  ggplot(aes(x = as.numeric(season), y = mean_rel_time,
             colour = category, group = category)) +
  geom_line(size = 1) +
  geom_point() +
  scale_x_continuous(breaks = seq_along(levels(maccl$season)),
                     labels = levels(maccl$season)) +
  labs(title = "Mean Relative Time Trend: Senior Races",
       subtitle = "Relative to within-race winner. Lower = field getting faster.",
       x = NULL, y = "Mean relative time", colour = "Category") +
  facet_wrap(~gender, ncol = 1) +
  theme_maccl() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
print(p_season_trend)
ggsave(file.path(OUT_DIR, "15_season_trend.png"), p_season_trend,
       width = 11, height = 8, dpi = 300)


# ── 9. EXPORT SUMMARY TABLES ─────────────────────────────────────────────────

write.csv(club_summary,    file.path(OUT_DIR, "club_summary.csv"),    row.names = FALSE)
write.csv(age_group_perf,  file.path(OUT_DIR, "age_group_perf.csv"),  row.names = FALSE)
write.csv(runner_slopes,   file.path(OUT_DIR, "runner_slopes.csv"),   row.names = FALSE)
write.csv(top_performers,  file.path(OUT_DIR, "top_performers.csv"),  row.names = FALSE)
write.csv(venue_summary,   file.path(OUT_DIR, "venue_summary.csv"),   row.names = FALSE)
write.csv(gender_ratio,    file.path(OUT_DIR, "gender_ratio.csv"),    row.names = FALSE)
write.csv(model_summary,   file.path(OUT_DIR, "model_comparison.csv"),row.names = FALSE)

cat("\n\n=== Analysis complete. Files written to:", OUT_DIR, "===\n")