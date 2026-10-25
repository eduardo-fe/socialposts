# ============================================================
# REF 2021 × CUG 2027 Psychology — merged analysis
# ============================================================
# Sources:
#   REF 2021: Unit of Assessment 4 (Psychology, Psychiatry & Neuroscience)
#             Native file: REF_2021_Results_-_4_-_2022-05-06.xlsx
#   CUG 2027: Complete University Guide subject table, Psychology
#             https://www.thecompleteuniversityguide.co.uk/league-tables/rankings/psychology
# ============================================================

library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(ggrepel)

# ---- 1. REF 2021 data (from native Excel) ------------------------------

ref_raw <- read_xlsx(
  "REF_2021_Results.xlsx",
  skip = 6  # header row is row 7 in the file
)

# Keep only the Overall profile rows and compute GPA
ref <- ref_raw %>%
  filter(Profile == "Overall") %>%
  transmute(
    university = `Institution name`,
    ref_fte    = `FTE of submitted staff`,
    ref_pct_eligible = `% of eligible staff submitted`,
    ref_4star  = `4*`,
    ref_3star  = `3*`,
    ref_2star  = `2*`,
    ref_1star  = `1*`,
    ref_gpa    = (4 * ref_4star + 3 * ref_3star + 2 * ref_2star + 1 * ref_1star) / 4
  )

ref <- ref_raw %>%
  filter(Profile == "Overall") %>%
  transmute(
    university = `Institution name`,
    ref_fte = `FTE of submitted staff`,
    ref_pct_eligible = `% of eligible staff submitted`,
    ref_4star = `4*`,
    ref_3star = `3*`,
    ref_2star = `2*`,
    ref_1star = `1*`,
    ref_gpa = (
      4 * ref_4star +
        3 * ref_3star +
        2 * ref_2star +
        1 * ref_1star
    ) / (
      ref_4star +
        ref_3star +
        ref_2star +
        ref_1star
    )
  )

cat(sprintf("REF UoA 4 institutions loaded: %d\n", nrow(ref)))

# ---- 2. CUG 2027 data -------------------------------------------------
# entry_std: entry standards (max 205, based on UCAS tariff of admitted students)
# sat:       student satisfaction (max 4)
# res_q:     research quality score (max 4, CUG-derived from REF)
# cont:      continuation rate (%)
# grad_out:  graduate outcomes (%)
# grad_trk:  graduate on track (%)

cug <- tibble::tribble(
  ~university,                           ~cug_rank, ~entry_std, ~sat,  ~res_q, ~cont,  ~grad_out, ~grad_trk,
  "London School of Economics",          1,  205, NA,    NA,    94.9, 88, 84,
  "University of Cambridge",             2,  200, 3.21,  3.64,  99.2, 86, 86,
  "University of St Andrews",            3,  200, 3.13,  3.51,  98.7, 86, 86,
  "University of Glasgow",               4,  196, 3.18,  3.49,  95.6, 84, 73,
  "University of Strathclyde",           5,  192, 3.15,  3.62,  91.3, 75, 69,
  "University of Oxford",                6,  190, NA,    3.64,  97.9, 80, 84,
  "University of Edinburgh",             7,  185, 2.88,  3.91,  95.8, 80, 80,
  "University of Bath",                  8,  172, 3.16,  3.03,  97.2, 79, 75,
  "King's College London",               9,  166, 3.12,  3.50,  93.8, 75, 80,
  "University of Exeter",                10, 164, 3.07,  3.44,  97.8, 80, 80,
  "Cardiff University",                  11, 163, 2.95,  3.55,  95.7, 76, 78,
  "University of Bristol",               12, 162, 3.07,  3.33,  99.1, 76, 76,
  "University College London",           13, 161, 3.21,  3.57,  98.6, 78, 78,
  "Durham University",                   13, 161, 3.14,  3.22,  97.7, 66, 66,
  "Glasgow Caledonian University",       15, 160, 3.18,  3.41,  91.9, 65, 65,
  "University of Stirling",              16, 157, 3.09,  3.18,  88.8, 66, 69,
  "University of Dundee",                16, 157, 3.12,  3.80,  89.2, 64, 72,
  "University of Manchester",            18, 156, 2.96,  3.38,  96.1, 79, 77,
  "University of Warwick",               19, 155, 3.21,  3.35,  97.6, 68, 76,
  "University of Aberdeen",              20, 154, 3.09,  3.28,  93.1, 71, 79,
  "University of York",                  21, 153, 3.13,  3.53,  98.5, 71, 73,
  "Edinburgh Napier University",         21, 153, 3.24,  3.08,  84.6, 63, 51,
  "Queen Margaret University",           23, 151, 3.03,  2.30,  86.1, 52, 75,
  "University of Leeds",                 23, 151, 3.06,  3.28,  96.2, 70, 70,
  "Queen Mary University of London",     25, 149, 2.97,  2.41,  97.7, 53, 81,
  "Heriot-Watt University",              26, 148, 3.08,  2.68,  89.9, 59, 68,
  "Abertay University",                  26, 148, 3.36,  3.53,  87.7, 80, 81,
  "Loughborough University",             28, 147, 3.07,  3.45,  97.3, 64, 71,
  "University of Sheffield",             28, 147, 3.25,  2.64,  98.6, 58, 68,
  "Queen's University Belfast",          30, 146, 3.22,  3.21,  93.8, 70, 68,
  "Newcastle University",                31, 145, 2.96,  3.29,  96.7, 62, 76,
  "University of Nottingham",            32, 143, 3.03,  3.23,  99.0, 68, 77,
  "Lancaster University",                33, 140, 3.03,  3.48,  97.0, 73, 76,
  "University of Birmingham",            34, 138, 3.13,  3.46,  97.2, 68, 75,
  "University of Liverpool",             35, 137, 3.06,  3.37,  97.8, 66, 71,
  "University of Southampton",           36, 136, 3.19,  3.47,  96.3, 68, 70,
  "Royal Holloway",                      37, 126, 3.18,  3.35,  92.9, 68, 73,
  "University of Surrey",                38, 125, 3.13,  3.32,  93.5, 67, 71,
  "University of Reading",               38, 125, 3.09,  3.38,  94.7, 63, 69,
  "University of Sussex",                40, 123, 3.06,  2.46,  92.2, 60, 65,
  "Liverpool John Moores University",    41, 122, 3.14,  3.02,  95.7, 60, 77,
  "University of East Anglia",           41, 122, 3.12,  3.05,  91.8, 69, 74,
  "Manchester Metropolitan University",  43, 121, 3.18,  2.89,  92.9, 58, 69,
  "University of Plymouth",              43, 121, 3.05,  2.90,  95.0, 62, 74,
  "City St George's London",             45, 120, 3.14,  2.45,  92.3, 62, 76,
  "Swansea University",                  46, 119, 3.07,  2.85,  92.3, 68, 66,
  "Edge Hill University",                46, 119, 3.07,  3.10,  91.7, 65, 73,
  "Northumbria University",              46, 119, 3.19,  3.17,  92.2, 63, 72,
  "University of Hull",                  49, 118, 3.04,  2.09,  92.0, 67, 69,
  "Bangor University",                   49, 118, 3.12,  2.00,  93.2, 61, 59,
  "University of Bradford",              49, 118, 2.98,  3.03,  82.6, 62, 76,
  "Ulster University",                   52, 117, 3.24,  3.32,  91.0, 64, 70,
  "Aston University",                    54, 116, 3.19,  1.39,  93.8, 69, 72,
  "University of Leicester",             54, 116, 3.15,  2.40,  97.4, 66, 66,
  "Univ of Wales Trinity Saint David",   54, 116, 3.53,  2.35,  85.1, 55, 63,
  "University of Huddersfield",          57, 115, 3.12,  3.01,  94.6, 52, 70,
  "University of Chester",               57, 115, 3.18,  3.01,  88.4, 70, 72,
  "Goldsmiths College",                  60, 114, 2.86,  2.92,  87.3, 74, 74,
  "University of Salford",               61, 113, 2.98,  2.89,  88.4, 73, 73,
  "University of Hertfordshire",         61, 113, 3.08,  2.96,  90.3, 65, 65,
  "Sheffield Hallam University",         64, 112, 2.98,  3.17,  94.0, 72, 75,
  "University of Kent",                  64, 112, 2.91,  2.52,  83.7, 71, 71,
  "Brunel University London",            67, 110, 3.10,  2.67,  94.1, 60, 73,
  "York St John University",             69, 109, 3.12,  3.02,  91.8, 60, 68,
  "Nottingham Trent University",         71, 108, 3.30,  2.92,  92.7, 61, 67,
  "Liverpool Hope University",           71, 108, 3.30,  2.83,  93.2, 54, 64,
  "Coventry University",                 71, 108, 3.19,  2.54,  92.7, 57, 68,
  "University of Brighton",              71, 108, 3.15,  2.73,  83.9, 45, 63,
  "Aberystwyth University",              79, 107, 3.26,  2.65,  89.7, 59, 68,
  "University of Essex",                 79, 107, 3.18,  2.19,  83.5, 62, 70,
  "Bath Spa University",                 79, 107, 3.21,  2.64,  92.5, 63, 72,
  "University of Derby",                 85, 105, 2.94,  2.95,  85.6, 60, 63,
  "Leeds Trinity University",            85, 105, 3.01,  2.93,  90.5, 59, 63,
  "University of Worcester",             89, 104, 3.17,  2.37,  78.7, 57, 55,
  "University of Westminster",           89, 104, 2.94,  2.17,  87.3, 68, 56,
  "University of Winchester",            91, 103, 3.17,  1.40,  92.7, 63, 46,
  "Leeds Beckett University",            91, 103, 3.04,  1.63,  87.6, 58, 51,
  "Bournemouth University",              91, 103, 3.11,  2.63,  87.2, 70, 61,
  "University of Portsmouth",            91, 103, 3.28,  2.61,  88.1, 66, 70,
  "Birmingham City University",          91, 103, 3.02,  2.92,  90.3, 64, 72,
  "London South Bank University",        91, 103, 2.83,  2.43,  87.0, 70, 74,
  "Anglia Ruskin University",            110, 95,  NA,    NA,    90.7, 63, 70,
  "University of Roehampton",            112, 92,  NA,    NA,    93.9, 56, 67,
  "University of Northampton",           113, 91,  NA,    NA,    85.4, 46, 62,
  "De Montfort University",              113, 91,  NA,    NA,    84.7, 51, 63,
  "Middlesex University",                115, 89,  NA,    NA,    86.8, 58, 62,
)

# ---- 3. Harmonise REF names to CUG names and merge --------------------
#
# The native REF file uses legal/formal names that diverge from CUG's
# trading names in several systematic ways:
#   - "The University of X" prefix
#   - Welsh bilingual suffixes ("/ Prifysgol ...")
#   - Trailing ", The" or ", Edinburgh"
#   - Full legal names (e.g. "Royal Holloway and Bedford New College")
#   - Post-2021 rebrands (City → City St George's London)
#
# We handle this in two steps:
#   (a) a case_when for names that need manual mapping
#   (b) a programmatic strip of "The " prefix for the rest

ref_clean <- ref %>%
  mutate(university = case_when(
    # ---- Welsh bilingual suffixes ----
    university == "Aberystwyth University / Prifysgol Aberystwyth"
    ~ "Aberystwyth University",
    university == "Bangor University / Prifysgol Bangor"
    ~ "Bangor University",
    university == "Cardiff University / Prifysgol Caerdydd"
    ~ "Cardiff University",
    university == "Swansea University / Prifysgol Abertawe"
    ~ "Swansea University",
    university == "University of Wales Trinity Saint David / Prifysgol Cymru Y Drindod Dewi Sant"
    ~ "Univ of Wales Trinity Saint David",
    
    # ---- Legal → trading name changes ----
    university == "Anglia Ruskin University Higher Education Corporation"
    ~ "Anglia Ruskin University",
    university == "Royal Holloway and Bedford New College"
    ~ "Royal Holloway",
    university == "Goldsmiths' College"
    ~ "Goldsmiths College",
    university == "Queen Margaret University, Edinburgh"
    ~ "Queen Margaret University",
    university == "Queen's University of Belfast"
    ~ "Queen's University Belfast",
    
    # ---- Post-2021 rebrands ----
    university == "City, University of London"
    ~ "City St George's London",
    university == "Roehampton University"
    ~ "University of Roehampton",
    
    # ---- REF formal → CUG common name ----
    university == "University of Durham"
    ~ "Durham University",
    university == "The University of Lancaster"
    ~ "Lancaster University",
    university == "University of Newcastle upon Tyne"
    ~ "Newcastle University",
    university == "University of Northumbria at Newcastle"
    ~ "Northumbria University",
    university == "University of Ulster"
    ~ "Ulster University",
    
    # ---- Trailing ", The" ----
    university == "University of Northampton, The"
    ~ "University of Northampton",
    university == "University of Nottingham, The"
    ~ "University of Nottingham",
    
    # ---- "The University of X" → "University of X" ----
    # (handled programmatically below for the bulk, but these are
    #  listed explicitly so the mapping is auditable)
    university == "The University of Bath"          ~ "University of Bath",
    university == "The University of Birmingham"    ~ "University of Birmingham",
    university == "The University of Bradford"      ~ "University of Bradford",
    university == "The University of East Anglia"   ~ "University of East Anglia",
    university == "The University of Essex"         ~ "University of Essex",
    university == "The University of Huddersfield"  ~ "University of Huddersfield",
    university == "The University of Hull"          ~ "University of Hull",
    university == "The University of Kent"          ~ "University of Kent",
    university == "The University of Leeds"         ~ "University of Leeds",
    university == "The University of Leicester"     ~ "University of Leicester",
    university == "The University of Liverpool"     ~ "University of Liverpool",
    university == "The University of Manchester"    ~ "University of Manchester",
    university == "The University of Reading"       ~ "University of Reading",
    university == "The University of Sheffield"     ~ "University of Sheffield",
    university == "The University of Warwick"       ~ "University of Warwick",
    university == "The University of Westminster"   ~ "University of Westminster",
    
    # ---- Catch-all: strip any remaining "The " prefix ----
    grepl("^The ", university) ~ sub("^The ", "", university),
    
    TRUE ~ university
  ))

# Join
combined <- cug %>%
  left_join(ref_clean, by = "university") %>%
  mutate(
    in_ref    = !is.na(ref_gpa),
    entry_pct = entry_std / 205 * 100
  )

cat(sprintf("Matched %d / %d CUG institutions to REF 2021\n",
            sum(combined$in_ref), nrow(combined)))

# Show which CUG institutions have no REF match (genuinely absent from UoA 4)
no_ref <- combined %>%
  filter(!in_ref) %>%
  select(university, cug_rank, entry_std)

cat(sprintf("\nCUG institutions with no REF UoA 4 submission (%d):\n", nrow(no_ref)))
print(no_ref, n = Inf)

# Show which REF institutions have no CUG match
ref_unmatched <- ref_clean %>%
  anti_join(cug, by = "university") %>%
  arrange(desc(ref_gpa)) %>%
  select(university, ref_gpa, ref_4star, ref_fte)

cat(sprintf("\nREF UoA 4 institutions not in CUG 2027 Psychology (%d):\n",
            nrow(ref_unmatched)))
print(ref_unmatched, n = Inf)

# ---- 4. Value index (residual of REF GPA ~ entry standards) -----------

fit_data <- combined %>% filter(in_ref, !is.na(entry_std))

fit <- lm(ref_gpa ~ entry_std, data = fit_data)
cat("\n--- OLS: REF GPA ~ CUG entry standards ---\n")
print(summary(fit))

fit_data <- fit_data %>%
  mutate(
    pred_gpa    = predict(fit),
    residual    = ref_gpa - pred_gpa,
    value_index = as.numeric(scale(residual))
  )

cat(sprintf("\nCorrelation (REF GPA vs CUG entry standards): r = %.3f\n",
            cor(fit_data$ref_gpa, fit_data$entry_std)))

# ---- 5. Plot p1: REF GPA vs CUG entry standards -----------------------

p1 <- ggplot(fit_data, aes(x = entry_std, y = ref_gpa)) +
  geom_point(aes(colour = ref_4star), size = 3) +
  geom_smooth(method = "lm", se = FALSE,
              colour = "grey40", linetype = "dashed", linewidth = 0.8) +
  scale_colour_viridis_c(name = "% 4-star\n(REF)", option = "plasma") +
  geom_text_repel(
    data = fit_data %>%
      filter(ref_gpa > 85 | entry_std > 170 | value_index > 1.5),
    aes(label = university),
    size = 3, max.overlaps = 20, box.padding = 0.4
  ) +
  labs(
    title    = "REF 2021 research quality vs CUG 2027 entry standards",
    subtitle = "Psychology departments — each point one institution",
    x        = "CUG 2027 entry standards (out of 205)",
    y        = "REF 2021 overall GPA"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right", panel.grid.minor = element_blank())

print(p1)



# ---- 6. Plot p2: Value index (residual plot) --------------------------

p2 <- ggplot(fit_data, aes(x = entry_std, y = residual)) +
  geom_hline(yintercept = 0, colour = "grey50", linetype = "dashed") +
  geom_point(aes(colour = residual > 0), size = 3, show.legend = FALSE) +
  scale_colour_manual(values = c("TRUE" = "#2a78d6", "FALSE" = "#eb6834")) +
  geom_text_repel(
    data = fit_data %>% filter(abs(value_index) > 1.0),
    aes(label = university),
    size = 3, max.overlaps = 20, box.padding = 0.4
  ) +
  labs(
    title    = "Research quality 'value' relative to entry demands",
    subtitle = "Blue = over-delivers on REF quality for its entry standards | Orange = under-delivers",
    x        = "CUG 2027 entry standards (out of 205)",
    y        = "Residual: actual REF GPA − predicted REF GPA"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

print(p2)


 

# ---- 7. Plot p3: Student satisfaction vs REF GPA ----------------------

p3 <- combined %>%
  filter(in_ref, !is.na(sat)) %>%
  ggplot(aes(x = sat, y = ref_gpa)) +
  geom_point(aes(colour = entry_std), size = 3) +
  geom_smooth(method = "lm", se = FALSE,
              colour = "grey40", linetype = "dashed", linewidth = 0.8) +
  scale_colour_viridis_c(name = "Entry\nstandards", option = "mako", direction = -1) +
  geom_text_repel(
    data = combined %>%
      filter(in_ref, !is.na(sat)) %>%
      filter(ref_gpa > 0 | sat > 0),
    aes(label = university),
    size = 3, max.overlaps = 15, box.padding = 0.4
  ) +
  labs(
    title    = "Student satisfaction vs REF research quality",
    subtitle = "Colour = CUG 2027 entry standards",
    x        = "Student satisfaction (CUG 2027, out of 4)",
    y        = "REF 2021 overall GPA"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

print(p3)

# ---- 8. Top 20 by value index (best research for lowest entry bar) ----

cat("\n=== Top 20 'value' departments (best REF quality for their entry demands) ===\n\n")
fit_data %>%
  arrange(desc(value_index)) %>%
  select(university, cug_rank, entry_std, ref_gpa, ref_4star, ref_fte,
         residual, value_index) %>%
  print(n = 20)

# ---- 9. AAB-or-below tier: best research quality ----------------------
# CUG entry standard of 136 ≈ ABB (48+40+40); 144 ≈ AAB (48+48+40)

aab_tier <- combined %>%
  filter(!is.na(entry_std), entry_std <= 144, in_ref) %>%
  arrange(desc(ref_gpa)) %>%
  select(university, cug_rank, entry_std, sat, grad_out, ref_gpa, ref_4star, ref_fte)

cat("\n=== Best REF research quality within the AAB-or-below entry tier ===\n\n")
print(aab_tier, n = 20)










# ---- 5. Plot p1: OWiD-style REF GPA vs CUG entry standards -----------

plot_data <- fit_data %>%
  filter(
    !is.na(ref_gpa),
    entry_std >= 120,
    ref_gpa >= 3
  ) %>%
  mutate(
    # Shorten university names for labels
    label = university %>%
      stringr::str_remove("^University of (?=[A-Z])") %>%
      stringr::str_remove("^The ") %>%
      stringr::str_trim(),
    
   
    # Universities worth highlighting/annotating
    notable = university %in% c(
      "University of Dundee",
      "University of Sheffield",
      "University of East Anglia",
      "University of Cambridge",
      "University of Glasgow",
      "Durham University",
      "University of Strathclyde",
      "University of Bath",
      "University of St Andrews",
      "University of Manchester",
      "University of York",
      "University of Southampton"
    ),
    
    # Position relative to the regression line
    above_line = residual > 0
 
    
  )

# Headline numbers for possible annotations

n_above <- sum(plot_data$above_line, na.rm = TRUE)
n_total <- nrow(plot_data)
n_below <- n_total - n_above

# Plot

p1 <- ggplot(plot_data, aes(x = entry_std, y = ref_gpa)) +
  
  # Background
  
  theme_minimal(base_size = 14) +
  
  # Regression line
  
  geom_smooth(
    method    = "lm",
    se        = FALSE,
    colour    = "grey65",
    linetype  = "solid",
    linewidth = 0.8
  ) +
  
  # Universities
  
  geom_point(
    colour = "#C9956A",
    fill   = "#C9956A",
    size   = 3,
    alpha  = 0.75,
    shape  = 21,
    stroke = 0.4
  ) +
  
  # Labels for all universities
  
  geom_text_repel(
    aes(label = label),
    size              = 3.3,
    colour            = "grey25",
    segment.colour    = "grey75",
    segment.size      = 0.3,
    box.padding       = 0.3,
    point.padding     = 0.2,
    max.overlaps      = Inf,
    min.segment.length = 0.2,
    seed              = 42
  ) +
  
  # X-axis
  
  scale_x_continuous(
    labels = function(x) paste0(x, " pts"),
    breaks = seq(120, 200, 20),
    expand = expansion(mult = c(0.08, 0.1))
  ) +
  
  # Y-axis
  
  scale_y_continuous(
    labels = function(y) sprintf("%.1f", y),
    breaks = seq(2.0, 4.0, 0.5),
    expand = expansion(mult = c(0.08, 0.08))
  ) +
  
  # Titles and labels
  
  labs(
    title = paste0(
      "Research Quality vs Entry Standards\n",
      "in UK Psychology"
    ),
    subtitle = paste0(
      "REF 2021 research quality (GPA, 4-star scale) vs CUG 2027 entry standards (UCAS points). ",
      "Universities above the line produce stronger research than their typical entrant profile suggests."
    ),
    x = "Entry standards — CUG 2027 (UCAS points)",
    y = "Research quality — REF 2021 GPA",
    caption = paste0(
      "Data: REF 2021 (UKRI); Complete University Guide 2027. ",
      "Restricted to institutions with entry standards ≥ 120 and REF GPA ≥ 2.0."
    )
  ) +
  
  # Theme
  
  theme(
    plot.title = element_text(
      size = 18,
      face = "bold",
      colour = "grey10",
      lineheight = 1.1,
      margin = margin(b = 6)
    ),
    plot.subtitle = element_text(
      size = 12,
      colour = "grey40",
      lineheight = 1.3,
      margin = margin(b = 16)
    ),
    plot.caption = element_text(
      size = 9.5,
      colour = "grey55",
      hjust = 0,
      margin = margin(t = 10)
    ),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(
      colour = "grey92",
      linewidth = 0.5
    ),
    panel.grid.minor = element_blank(),
    axis.title = element_text(
      size = 12,
      colour = "grey30"
    ),
    axis.text = element_text(
      size = 11,
      colour = "grey40"
    ),
    plot.margin = margin(20, 20, 14, 20)
  )

ggsave(
  "scatter_owid_psychology.png",
  p1,
  width  = 11,
  height = 9,
  dpi    = 300,
  bg     = "white"
)

print(p1)

message("Saved: scatter_owid_psychology.png")











p1 <- ggplot(fit_data, aes(x = entry_std, y = ref_gpa)) +
  geom_point(aes(colour = ref_4star), size = 3) +
  geom_smooth(
    method = "loess",
    method.args = list(degree = 2),
    span = 0.5,
    se = FALSE,
    colour = "grey40",
    linetype = "dashed",
    linewidth = 0.8
  ) +
  scale_colour_viridis_c(name = "% 4-star\n(REF)", option = "plasma") +
  geom_text_repel(
    data = fit_data %>%
      filter(ref_gpa > 85 | entry_std > 170 | value_index > 1.5),
    aes(label = university),
    size = 3, max.overlaps = 20, box.padding = 0.4
  ) +
  labs(
    title    = "REF 2021 research quality vs CUG 2027 entry standards",
    subtitle = "Psychology departments — each point one institution",
    x        = "CUG 2027 entry standards (out of 205)",
    y        = "REF 2021 overall GPA"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

print(p1)




library(KernSmooth)

lp <- locpoly(
  fit_data$entry_std,
  fit_data$ref_gpa,
  degree = 5,
  bandwidth = 15
)

p1 <- ggplot(fit_data, aes(x = entry_std, y = ref_gpa)) +
  geom_point(aes(colour = ref_4star), size = 3) +
  
  geom_line(
    data = data.frame(x = lp$x, y = lp$y),
    aes(x = x, y = y),
    inherit.aes = FALSE,
    colour = "grey40",
    linetype = "dashed",
    linewidth = 0.8
  ) +
  
  scale_colour_viridis_c(
    name = "% 4-star\n(REF)",
    option = "plasma"
  ) +
  
  geom_text_repel(
    data = fit_data %>%
      filter(ref_gpa > 85 | entry_std > 170 | value_index > 1.5),
    aes(label = university),
    size = 3,
    max.overlaps = 20,
    box.padding = 0.4
  ) +
  
  labs(
    title = "REF 2021 research quality vs CUG 2027 entry standards",
    subtitle = "Psychology departments — each point one institution",
    x = "CUG 2027 entry standards (out of 205)",
    y = "REF 2021 overall GPA"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

print(p1)



fit_data_120 <- fit_data %>%
  filter(entry_std >= 120)

lp <- locpoly(
  fit_data_120$entry_std,
  fit_data_120$ref_gpa,
  degree = 2,
  bandwidth = 20
)

p1 <- ggplot(fit_data_120, aes(x = entry_std, y = ref_gpa)) +
  geom_point(aes(colour = ref_4star), size = 3) +
  
  geom_line(
    data = data.frame(x = lp$x, y = lp$y),
    aes(x = x, y = y),
    inherit.aes = FALSE,
    colour = "grey40",
    linetype = "dashed",
    linewidth = 0.8
  ) +
  
  scale_colour_viridis_c(
    name = "% 4-star\n(REF)",
    option = "plasma"
  ) +
  
  geom_text_repel(
    data = fit_data %>%
      filter(ref_gpa > 85 | entry_std > 170 | value_index > 1.5),
    aes(label = university),
    size = 3,
    max.overlaps = 20,
    box.padding = 0.4
  ) +
  
  labs(
    title = "REF 2021 research quality vs CUG 2027 entry standards",
    subtitle = "Psychology departments — each point one institution",
    x = "CUG 2027 entry standards (out of 205)",
    y = "REF 2021 overall GPA"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

print(p1)





 