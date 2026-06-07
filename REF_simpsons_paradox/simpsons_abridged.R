# ══════════════════════════════════════════════════════════════════════════════
# plots.R
# Publication-ready static figures: Income per Staff vs REF / Deficit
#
# Usage:
#   1. Set working directory to the folder containing the four data files.
#   2. Run the whole script: source("plots.R")
#   3. Figures are saved as high-resolution PNGs in ./figures/
#      and also left in your R environment as ggplot objects:
#        p_ips_ref      — income/staff vs REF GPA (linear x)
#        p_ips_ref_log  — income/staff vs REF GPA (log x)
#        p_ips_def      — income/staff vs deficit (linear x)
#        p_ips_def_log  — income/staff vs deficit (log x)
#        p_three        — three-way view (log x, colour = financial band)
#
# Adjust the PARAMETERS section below before running.
# Key exclusion parameters: FEE_FLOOR_M (low-income floor) and
# IPS_UPPER_PCT (upper percentile ceiling on income per staff).
# ══════════════════════════════════════════════════════════════════════════════

library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(scales)

# ── PARAMETERS ────────────────────────────────────────────────────────────────
REF_PATH    <- "ref2021.xlsx"
FIN_PATH    <- "dt031-table-14-4.csv"
FEE_PATH    <- "dt031-table-6-4.csv"
STAFF_PATH  <- "dt025-table-1.csv"

FEE_FLOOR_M   <- 20        # exclude institutions with total fees below this (£m)
IPS_UPPER_PCT <- 0.99      # exclude institutions above this income-per-staff percentile
BAND_LO     <- -2        # deficit/surplus band lower threshold (% of income)
BAND_HI     <-  2        # deficit/surplus band upper threshold (% of income)

OUT_DIR     <- "figures" # output folder for saved PNGs
DPI         <- 300       # resolution for saved files
FIG_W       <- 10        # figure width  (inches)
FIG_H       <- 7         # figure height (inches)
BASE_SIZE   <- 13        # ggplot base font size

# ── COLOURS & SHAPES ──────────────────────────────────────────────────────────
rg_colours   <- c("Russell Group" = "#E87722", "Other" = "#003D5C")
rg_shapes    <- c("Russell Group" = 17,        "Other" = 16)
rg_alphas    <- c("Russell Group" = 1.0,        "Other" = 0.5)
rg_sizes     <- c("Russell Group" = 3.0,        "Other" = 2.0)

band_colours <- c(
  "Deficit"         = "#CC2222",
  "Near break-even" = "grey75",
  "Surplus"         = "#4A9E6B"
)

# ══════════════════════════════════════════════════════════════════════════════
# 1. LOAD & CLEAN DATA
# ══════════════════════════════════════════════════════════════════════════════

# ── REF 2021 outputs GPA ──────────────────────────────────────────────────────
ref_raw <- read_excel(REF_PATH, skip = 6, col_names = TRUE)
colnames(ref_raw) <- c(
  "ukprn", "institution", "sort_order", "main_panel",
  "uoa_number", "uoa_name", "multi_letter", "multi_name",
  "joint", "profile", "fte", "fte_joint", "pct_eligible",
  "star4", "star3", "star2", "star1", "unclassified"
)
ref_inst <- ref_raw %>%
  filter(profile == "Outputs") %>%
  mutate(across(c(star4, star3, star2, star1, unclassified), as.numeric),
         fte = as.numeric(fte),
         gpa = (star4 * 4 + star3 * 3 + star2 * 2 + star1 * 1) / 100) %>%
  group_by(ukprn, institution) %>%
  summarise(ref_score = weighted.mean(gpa, w = fte, na.rm = TRUE),
            total_fte = sum(fte, na.rm = TRUE),
            .groups   = "drop")

# ── Financial surplus/deficit ─────────────────────────────────────────────────
fin_raw <- read.csv(FIN_PATH, skip = 15, header = TRUE,
                    stringsAsFactors = FALSE, check.names = FALSE)
fin_col <- "Surplus/(deficit) excl. pension adjustment as a % of total income"
fin_clean <- fin_raw %>%
  select(ukprn = 1, surplus_excl_pension = all_of(fin_col)) %>%
  mutate(
    ukprn = as.numeric(ukprn),
    surplus_excl_pension = as.numeric(
      str_replace_all(str_replace_all(surplus_excl_pension, "\\(", "-"), "\\)", "")
    ),
    deficit = -surplus_excl_pension
  ) %>%
  filter(!is.na(deficit))

# ── Fee income ────────────────────────────────────────────────────────────────
fee_raw <- read.csv(FEE_PATH, skip = 17, header = TRUE,
                    stringsAsFactors = FALSE, check.names = FALSE)
fee_col <- "Total tuition fees and education contracts"
fee_clean <- fee_raw %>%
  select(ukprn = 1, total_fees_str = all_of(fee_col)) %>%
  mutate(
    ukprn        = as.numeric(ukprn),
    total_fees_m = as.numeric(str_remove_all(total_fees_str, ",")) / 1000
  ) %>%
  filter(!is.na(total_fees_m), total_fees_m > 0)

# ── Academic staff headcount ──────────────────────────────────────────────────
staff_raw <- read.csv(STAFF_PATH, skip = 15, header = TRUE,
                      stringsAsFactors = FALSE, check.names = FALSE)
staff_clean <- staff_raw %>%
  select(ukprn = 1, total_staff_str = all_of("Total academic staff")) %>%
  mutate(
    ukprn       = as.numeric(ukprn),
    total_staff = as.numeric(str_remove_all(total_staff_str, ","))
  ) %>%
  filter(!is.na(total_staff), total_staff > 0)

# ── Merge, derive variables, apply exclusions ─────────────────────────────────
russell_group_ukprns <- c(
  10006840, 10007786, 10007788, 10007814, 10007143,
  10007790, 10007792, 10007794, 10003270, 10003645,
  10007795, 10006842, 10007798, 10007799, 10007154,
  10007774, 10005343, 10007537, 10007157, 10007158,
  10007784, 10007163, 10007167, 10004082
)

df <- ref_inst %>%
  inner_join(fin_clean,   by = "ukprn") %>%
  inner_join(fee_clean,   by = "ukprn") %>%
  inner_join(staff_clean, by = "ukprn") %>%
  mutate(
    income_per_staff = total_fees_m / total_staff * 1000,
    log_ips          = log(income_per_staff)
  )

# Compute ceiling from full distribution before filtering
ips_ceiling <- quantile(df$income_per_staff, IPS_UPPER_PCT, na.rm = TRUE)
n_low  <- sum(df$total_fees_m < FEE_FLOOR_M, na.rm = TRUE)
n_high <- sum(df$income_per_staff > ips_ceiling, na.rm = TRUE)

df <- df %>%
  filter(
    total_fees_m     >= FEE_FLOOR_M,
    income_per_staff <= ips_ceiling
  ) %>%
  mutate(
    russell = ifelse(ukprn %in% russell_group_ukprns, "Russell Group", "Other"),
    fin_band = case_when(
      deficit < BAND_LO ~ "Deficit",
      deficit > BAND_HI ~ "Surplus",
      TRUE              ~ "Near break-even"
    ),
    fin_band = factor(fin_band, levels = c("Deficit", "Near break-even", "Surplus"))
  )

message(sprintf(
  "Exclusions: %d below fee floor (< £%dm total fees); %d above IPS ceiling (top %g%%, > £%.1fk/staff)",
  n_low, FEE_FLOOR_M, n_high, (1 - IPS_UPPER_PCT) * 100, ips_ceiling
))
message(sprintf("Institutions in analysis: %d  (Russell Group: %d)",
                nrow(df), sum(df$russell == "Russell Group")))
message(sprintf("Income per staff range: £%.1fk – £%.1fk",
                min(df$income_per_staff), max(df$income_per_staff)))

# ══════════════════════════════════════════════════════════════════════════════
# 2. SHARED THEME
# ══════════════════════════════════════════════════════════════════════════════

theme_hesa <- theme_minimal(base_size = BASE_SIZE) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(size = BASE_SIZE - 4, colour = "grey40"),
    legend.position = "bottom"
  )

# ══════════════════════════════════════════════════════════════════════════════
# 3. FIGURES
# ══════════════════════════════════════════════════════════════════════════════

 
# ══════════════════════════════════════════════════════════════════════════════
# 5. CLUSTER ANALYSIS
#    K-means on three standardised variables:
#      - log(income per staff)
#      - REF GPA
#      - surplus/deficit %
#
#    Workflow:
#      (a) Inspect elbow + silhouette plots to choose k
#      (b) Adjust K below and re-run from this section
#      (c) Two cluster scatter plots are saved: vs REF and vs deficit
# ══════════════════════════════════════════════════════════════════════════════

library(cluster)   # silhouette() — base R, no extra install needed

K <- 2   # ← CHANGE THIS after inspecting the diagnostic plots below

# ── (a) Standardise clustering variables ──────────────────────────────────────
clust_vars <- df %>%
  select(log_ips, ref_score, deficit) %>%
  mutate(across(everything(), scale))

# ── (b) Diagnostic: elbow + silhouette ───────────────────────────────────────
set.seed(42)
wss <- sapply(1:10, function(k) {
  kmeans(clust_vars, centers = k, nstart = 25, iter.max = 100)$tot.withinss
})
sil_width <- sapply(2:10, function(k) {
  km  <- kmeans(clust_vars, centers = k, nstart = 25, iter.max = 100)
  sil <- silhouette(km$cluster, dist(clust_vars))
  mean(sil[, "sil_width"])
})

png(file.path(OUT_DIR, "fig6_cluster_diagnostics.png"),
    width = FIG_W * DPI, height = 4 * DPI, res = DPI)
par(mfrow = c(1, 2))
plot(1:10, wss, type = "b", pch = 19, col = "#003D5C",
     xlab = "Number of clusters k", ylab = "Total within-cluster SS",
     main = "Elbow Plot")
plot(2:10, sil_width, type = "b", pch = 19, col = "#E87722",
     xlab = "Number of clusters k", ylab = "Average silhouette width",
     main = "Silhouette Width")
abline(v = which.max(sil_width) + 1, lty = 2, col = "grey50")
par(mfrow = c(1, 1))
dev.off()
message("Saved: ", file.path(OUT_DIR, "fig6_cluster_diagnostics.png"))
message(sprintf("Silhouette suggests k = %d", which.max(sil_width) + 1))

# ── (c) Fit final k-means ─────────────────────────────────────────────────────
set.seed(42)
km <- kmeans(clust_vars, centers = K, nstart = 50, iter.max = 200)
df <- df %>% mutate(cluster = factor(km$cluster))

# ── (d) Auto-label clusters by REF + income profile ──────────────────────────
cluster_profile <- df %>%
  group_by(cluster) %>%
  summarise(
    mean_ips    = mean(income_per_staff),
    mean_ref    = mean(ref_score),
    mean_def    = mean(deficit),
    n_rg        = sum(russell == "Russell Group"),
    n           = n(),
    .groups     = "drop"
  )

# Labels derived from rank of REF and income — edit freely
cluster_labels <- cluster_profile %>%
  mutate(
    ref_rank = rank(-mean_ref),
    ips_rank = rank(-mean_ips),
    label = case_when(
      ref_rank == 1 & ips_rank == 1 ~ "High REF / High income",
      ref_rank == 1                  ~ "High REF / Lower income",
      ips_rank == 1                  ~ "Lower REF / High income",
      TRUE                           ~ "Lower REF / Lower income"
    )
  ) %>%
  select(cluster, label)

df <- df %>% left_join(cluster_labels, by = "cluster")

# Print profile to console
message("\nCluster profiles:")
print(
  df %>%
    group_by(cluster, label) %>%
    summarise(N = n(), RG = sum(russell == "Russell Group"),
              `Income/staff £k` = round(mean(income_per_staff), 1),
              `REF GPA`         = round(mean(ref_score), 3),
              `Deficit %`       = round(mean(deficit), 2),
              .groups = "drop"),
  n = Inf
)

# ── (e) Cluster colour palette ────────────────────────────────────────────────
cluster_pal <- c("1" = "#003D5C", "2" = "#E87722",
                 "3" = "#4A9E6B", "4" = "#8B3A8B")
if (K > 4) cluster_pal <- setNames(scales::hue_pal()(K), as.character(1:K))

# ── (f) Figure 7: clusters in income/staff vs REF space ──────────────────────
centroids_ref <- df %>%
  group_by(cluster, label) %>%
  summarise(x = exp(mean(log_ips)), y = mean(ref_score), .groups = "drop")

p_clust_ref <- ggplot(df,
                      aes(x = income_per_staff, y = ref_score,
                          colour = cluster, shape = russell)) +
  geom_point(aes(size = russell, alpha = russell)) +
  geom_label(data = centroids_ref,
             aes(x = x, y = y, label = label, colour = cluster),
             size = 2.8, fontface = "bold", fill = "white",
             alpha = 0.85, label.size = 0.3,
             inherit.aes = FALSE, show.legend = FALSE) +
  scale_x_log10(labels = comma_format(suffix = "k", prefix = "£")) +
  scale_colour_manual(values = cluster_pal, name = "Cluster") +
  scale_shape_manual(values = rg_shapes, name = NULL) +
  scale_size_manual(values  = rg_sizes,  guide = "none") +
  scale_alpha_manual(values = rg_alphas, guide = "none") +
  labs(
    title    = "Institution Clusters: Income per Staff vs REF GPA",
    subtitle = paste0("K-means (k = ", K, ") on standardised log(income/staff),",
                      " REF GPA, and surplus/deficit\n",
                      "Labels at cluster centroids; triangles = Russell Group"),
    x        = "Fee income per academic staff member (£000s, log scale)",
    y        = "REF Outputs GPA (FTE-weighted)"
  ) +
  theme_minimal(base_size = BASE_SIZE) +
  theme(plot.title      = element_text(face = "bold"),
        plot.subtitle   = element_text(size = BASE_SIZE - 4, colour = "grey40"),
        legend.position = "bottom")

# ── (g) Figure 8: clusters in income/staff vs deficit space ──────────────────
centroids_def <- df %>%
  group_by(cluster, label) %>%
  summarise(x = exp(mean(log_ips)), y = mean(deficit), .groups = "drop")

 

# ══════════════════════════════════════════════════════════════════════════════
# 6. SIMPSON'S PARADOX: WITHIN-CLUSTER vs OVERALL REGRESSION
#    Shows whether the overall negative slope reverses within clusters.
#    Requires the cluster objects from Section 5 (km, df$cluster, df$label).
# ══════════════════════════════════════════════════════════════════════════════

# ── Overall regression ────────────────────────────────────────────────────────
m_overall <- lm(ref_score ~ log_ips, data = df)
b_overall <- coef(m_overall)[["log_ips"]]

# ── Per-cluster regressions ───────────────────────────────────────────────────
cluster_coefs <- df %>%
  group_by(cluster, label) %>%
  group_modify(~ {
    m <- lm(ref_score ~ log_ips, data = .x)
    tibble(
      slope  = coef(m)[["log_ips"]],
      se     = summary(m)$coef["log_ips", "Std. Error"],
      pval   = summary(m)$coef["log_ips", "Pr(>|t|)"],
      n      = nrow(.x)
    )
  }) %>%
  ungroup() %>%
  mutate(
    sig_star    = case_when(pval < 0.001 ~ "***", pval < 0.01 ~ "**",
                            pval < 0.05  ~ "*",   TRUE        ~ ""),
    slope_label = paste0(label, ":  β = ", sprintf("%.3f", slope),
                         sig_star, "  (n=", n, ")")
  )

# Print table to console
message("\nWithin-cluster slopes (β on log IPS scale):")
print(cluster_coefs %>%
        select(cluster, label, n, slope, se, pval, sig_star),
      n = Inf)
message(sprintf("Overall β = %.3f", b_overall))

# ── Prediction lines on original scale ───────────────────────────────────────
cluster_lines <- df %>%
  group_by(cluster) %>%
  group_modify(~ {
    m     <- lm(ref_score ~ log_ips, data = .x)
    x_seq <- seq(min(.x$log_ips), max(.x$log_ips), length.out = 80)
    tibble(x_orig = exp(x_seq),
           y_hat  = predict(m, newdata = data.frame(log_ips = x_seq)))
  }) %>%
  ungroup()

x_all        <- seq(min(df$log_ips), max(df$log_ips), length.out = 200)
line_overall <- data.frame(
  x_orig = exp(x_all),
  y_hat  = predict(m_overall, newdata = data.frame(log_ips = x_all))
)

# ── Stacked annotation block in lower-left ────────────────────────────────────
overall_line      <- paste0("Overall (dashed):  β = ", sprintf("%.3f", b_overall))
cluster_lines_txt <- cluster_coefs %>% arrange(cluster) %>% pull(slope_label)
legend_text       <- paste(c(overall_line, cluster_lines_txt), collapse = "\n")

ann_x <- exp(min(df$log_ips) + 0.05 * diff(range(df$log_ips)))
ann_y <- min(df$ref_score)   + 0.02 * diff(range(df$ref_score))

# ── Figure 9: Simpson's paradox plot ─────────────────────────────────────────
p_simpson <- ggplot(df,
                    aes(x = income_per_staff, y = ref_score, colour = cluster)) +
  geom_point(aes(shape = russell, size = russell)) +
  geom_line(data = cluster_lines,
            aes(x = x_orig, y = y_hat, colour = cluster),
            linewidth = 2.1, inherit.aes = FALSE) +
  geom_line(data = line_overall,
            aes(x = x_orig, y = y_hat),
            colour = "black", linewidth = 1, linetype = "dashed",
            inherit.aes = FALSE) +
  
  scale_x_log10(labels = comma_format(suffix = "k", prefix = "£")) +
  scale_colour_manual(values = cluster_pal, name = "Cluster") +
  scale_shape_manual(values  = rg_shapes,  name = NULL) +
  scale_size_manual(values   = rg_sizes,   guide = "none") +
  scale_alpha_manual(values  = rg_alphas,  guide = "none") +
  labs(
    title    = "REF2021 Results vs Income per Staff", 
    subtitle = 
      "Coloured lines = within-cluster OLS; black dashed = overall OLS\n",
    x = "Fee income per academic staff member (£000s, log scale)",
    y = "REF Outputs GPA (FTE-weighted)"
  ) +
  theme_minimal(base_size = BASE_SIZE) +
  theme(plot.title      = element_text(face = "bold"), 
        plot.subtitle   = element_text(size = BASE_SIZE - 4, colour = "black"),
        legend.position = "bottom")


p_simpson
 