# --------------------------------------------------
# Libraries
# --------------------------------------------------
library(tidyverse)
library(cluster)
library(factoextra)

# --------------------------------------------------
# 1. Read data
# --------------------------------------------------
men <- read.csv("~/Desktop/tweets/hesa/dt025-table-men.csv", stringsAsFactors = FALSE)
women <- read.csv("~/Desktop/tweets/hesa/dt025-table-women.csv", stringsAsFactors = FALSE)

# --------------------------------------------------
# 2. Clean column names
# --------------------------------------------------
clean_names <- function(df) {
  names(df) <- names(df) |>
    str_replace_all("[^[:alnum:]]+", "_") |>
    str_replace_all("^_|_$", "") |>
    str_replace_all("_+", "_")
  df
}

men <- clean_names(men)
women <- clean_names(women)

# --------------------------------------------------
# 3. Identify salary band columns
# --------------------------------------------------
salary_cols <- names(men)[
  str_detect(names(men), "^Contract_salary") &
    !names(men) %in% c("Contract_salary", "Total")
]

# --------------------------------------------------
# 4. Convert salary columns and Total to numeric
# --------------------------------------------------
numeric_clean <- function(x) as.numeric(str_replace_all(x, "[^0-9.-]", ""))
men[salary_cols] <- lapply(men[salary_cols], numeric_clean)
women[salary_cols] <- lapply(women[salary_cols], numeric_clean)
men$Total <- numeric_clean(men$Total)
women$Total <- numeric_clean(women$Total)

# --------------------------------------------------
# 5. Drop institutions with Total < 100
# --------------------------------------------------
men <- men |> filter(Total >= 100)
women <- women |> filter(Total >= 100)

# --------------------------------------------------
# 6. Define salary band midpoints (adjust top band)
# --------------------------------------------------
band_midpoints <- c(
  Contract_salary_22_681 = 22681 / 2,
  Contract_salary_22_681_and_28_759 = mean(c(22681, 28759)),
  Contract_salary_28_759_and_38_205 = mean(c(28759, 38205)),
  Contract_salary_38_205_and_51_283 = mean(c(38205, 51283)),
  Contract_salary_51_283_and_68_857 = mean(c(51283, 68857)),
  Contract_salary_68_857 = 68857 + 10000
)

# keep only columns actually in the data
band_midpoints <- band_midpoints[names(band_midpoints) %in% salary_cols]

# --------------------------------------------------
# 7. Compute weighted average salary safely
# --------------------------------------------------
weighted_salary <- function(df) {
  df |>
    rowwise() |>
    mutate(
      actual_total = sum(c_across(all_of(names(band_midpoints))), na.rm = TRUE),
      avg_salary = sum(c_across(all_of(names(band_midpoints))) *
                         band_midpoints[names(band_midpoints)], na.rm = TRUE) / actual_total,
      size = actual_total
    ) |>
    ungroup() |>
    select(UKPRN, HE_provider, avg_salary, size)
}

men_avg <- weighted_salary(men)
women_avg <- weighted_salary(women)

# --------------------------------------------------
# 8. Compute Gender difference in average salariess
# --------------------------------------------------
gender_gap <- men_avg |>
  left_join(
    women_avg,
    by = c("UKPRN", "HE_provider"),
    suffix = c("_men", "_women")
  ) |>
  mutate(
    gap_salary = avg_salary_men - avg_salary_women,
    avg_salary_overall = (avg_salary_men + avg_salary_women)/2,
    women_men_ratio = size_women / size_men
  ) |>
  mutate(row_id = row_number())

# --------------------------------------------------
# 9. Size vs Gender difference in average salaries plot (LOESS)
# --------------------------------------------------
ggplot(gender_gap,
       aes(x = size_men + size_women, y = gap_salary)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE, color = "darkred", span = 0.6) +
  scale_x_log10() +
  labs(
    x = "University size (log staff)",
    y = "Difference (men-women) (£)",
    title = "University size and Gender difference in average salaries"
  ) +
  theme_minimal()

# --------------------------------------------------
# 10. Cluster analysis (4 clusters)
# --------------------------------------------------
cluster_input <- gender_gap |>
  select(row_id, size_men, gap_salary) |>
  drop_na() |>
  mutate(size_men = log(size_men))

cluster_scaled <- scale(cluster_input |> select(size_men, gap_salary))
set.seed(123)
km4 <- kmeans(cluster_scaled, centers = 4, nstart = 50)

cluster_labels <- tibble(row_id = cluster_input$row_id, cluster = factor(km4$cluster))
gender_gap <- gender_gap |> left_join(cluster_labels, by = "row_id")

# Cluster plot
ggplot(gender_gap |> filter(!is.na(cluster)),
       aes(x = log(size_men), y = gap_salary, color = cluster)) +
  geom_point(size = 2.5, alpha = 0.7) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "University size (log staff)", y = "Difference (men-women) (£)", color = "Cluster",
       title = "Clusters of universities by size and Gender difference in average salaries") +
  theme_minimal()

# --------------------------------------------------
# 11. Average salary vs Gender difference in average salaries
# --------------------------------------------------
ggplot(gender_gap,
       aes(x = avg_salary_overall, y = gap_salary)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE, color = "darkred") +
  labs(x = "Average salary (£)", y = "Difference (men-women) (£)",
       title = "Average salary and Difference (men-women)") +
  theme_minimal()

# --------------------------------------------------
# 12. Gender difference in average salaries vs women/men ratio (LOESS, extremes removed)
# --------------------------------------------------
ratio_q <- quantile(gender_gap$women_men_ratio, probs = c(0.05, 0.95), na.rm = TRUE)
gender_gap_filtered <- gender_gap |>
  filter(women_men_ratio >= ratio_q[1], women_men_ratio <= ratio_q[2])

ggplot(gender_gap_filtered,
       aes(x = women_men_ratio, y = gap_salary)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE, color = "darkred", span = 0.6) +
  labs(x = "Ratio of women to men (staff)", y = "Difference (men-women) (£)",
       title = "Difference (men-women) vs women–men staff ratio (LOESS, extremes removed)") +
  theme_minimal()

# --------------------------------------------------
# 13. Kernel density of Difference (men-women)
# --------------------------------------------------
ggplot(gender_gap, aes(x = gap_salary)) +
  geom_density(fill = "grey70", alpha = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.6) +
  labs(x = "Difference (men-women) (£, men − women)", y = "Density",
       title = "Distribution of Difference (men-women)s across universities") +
  theme_minimal()

ggplot(gender_gap, aes(x = gap_salary, weight = size_men + size_women)) +
  geom_density(fill = "grey70", alpha = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.6) +
  labs(x = "Difference (men-women) (£)", y = "Employment-weighted density",
       title = "Employment-weighted distribution of Difference (men-women)s") +
  theme_minimal()

# --------------------------------------------------
# 14. Top/bottom band ratios and gaps
# --------------------------------------------------
top_bands <- tail(salary_cols, 2)
bottom_bands <- head(salary_cols, 1)

compute_band_ratio <- function(df, bands) {
  df |>
    rowwise() |>
    mutate(band_ratio = sum(c_across(all_of(bands)), na.rm = TRUE) /
             sum(c_across(all_of(salary_cols)), na.rm = TRUE)) |>
    ungroup() |>
    select(UKPRN, HE_provider, band_ratio)
}

top_men <- compute_band_ratio(men, top_bands)
top_women <- compute_band_ratio(women, top_bands)
bottom_men <- compute_band_ratio(men, bottom_bands)
bottom_women <- compute_band_ratio(women, bottom_bands)

# Merge and compute gaps
top_gap_df <- top_men |>
  left_join(top_women, by = c("UKPRN","HE_provider"), suffix = c("_men","_women")) |>
  mutate(top_gap = band_ratio_men - band_ratio_women) |>
  left_join(men |> select(UKPRN, HE_provider, Total), by = c("UKPRN","HE_provider"))

bottom_gap_df <- bottom_men |>
  left_join(bottom_women, by = c("UKPRN","HE_provider"), suffix = c("_men","_women")) |>
  mutate(bottom_gap = band_ratio_men - band_ratio_women) |>
  left_join(men |> select(UKPRN, HE_provider, Total), by = c("UKPRN","HE_provider"))

# Plot top/bottom gaps
ggplot(top_gap_df, aes(x = top_gap)) +
  geom_density(fill = "skyblue", alpha = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Gap in top salary band ratio (men - women)", y = "Density",
       title = "Distribution of top salary band Gender difference in average salariess") +
  theme_minimal()

ggplot(bottom_gap_df, aes(x = bottom_gap)) +
  geom_density(fill = "pink", alpha = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Gap in bottom salary band ratio (men - women)", y = "Density",
       title = "Distribution of bottom salary band Gender difference in average salariess") +
  theme_minimal()
