library(dplyr)
library(ggplot2)
library(readr)

# Example: read your data (replace with your actual file path)
hesa <- read.csv("~/Desktop/universities/unisalaries/dt025-table-17.csv")
# Check data
head(hesa)

# Define approximate midpoints for each salary band
midpoints <- c(20000, 25720, 33482, 44744, 60070, 80000)

# Compute proportions and weighted average (proxy salary)
hesa_processed <- hesa %>%
  rename(
    salary1 = Contract.salary....22.681.,
    salary2 = Contract.salary....22.681.and....28.759.,
    salary3 = Contract.salary....28.759.and....38.205.,
    salary4 = Contract.salary....38.205.and....51.283.,
    salary5 = Contract.salary....51.283.and....68.857.,
    salary6 = Contract.salary....68.857.
  )  %>%
  mutate(across(
    c(salary1:salary6, Total),
    ~ as.numeric(gsub(",", "", .))
  )) %>%   # remove commas and convert to numeric
  mutate(
    prop1 = salary1 / Total,
    prop2 = salary2 / Total,
    prop3 = salary3 / Total,
    prop4 = salary4 / Total,
    prop5 = salary5 / Total,
    prop6 = salary6 / Total,
    mean_salary_proxy = (salary1*midpoints[1] + salary2*midpoints[2] + salary3*midpoints[3] +
                           salary4*midpoints[4] + salary5*midpoints[5] + salary6*midpoints[6]) / Total
  )

hesa_filtered <- hesa_processed %>%
  filter(Total > 200)

# Scatter: average salary vs total staff
ggplot(hesa_filtered, aes(x = Total, y = mean_salary_proxy)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Average Salary vs Total Staff per University",
    x = "Total Staff",
    y = "Estimated Average Salary (£)"
  ) +
  theme_minimal()



# Distribution of average salaries
ggplot(hesa_filtered, aes(x = mean_salary_proxy)) +
  geom_histogram(binwidth = 2000, fill = "steelblue", color = "white", alpha = 0.8) +
  geom_density(aes(y = 2000*..count..), color = "red", size = 1) +  # scale density to histogram
  labs(
    title = "Distribution of Average Salaries Across Universities",
    x = "Estimated Average Salary (£)",
    y = "Number of Universities"
  ) +
  theme_minimal()

library(quantreg)


# Scatter: average salary vs total staff
ggplot(hesa_filtered, aes(x = Total, y = prop6)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "darkgreen", span=0.5) +  # nonparametric fit
  geom_quantile(quantiles = c(0.25, 0.5, 0.75), color = "red", linetype = "dashed") +  # conditional quantiles
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Average Salary vs Total Staff per University",
    x = "Total Staff",
    y = "Estimated Average Salary (£)"
  ) +
  theme_minimal()


# Find clusters

library(factoextra)  # for nice clustering plots
library(cluster)

# Select features for clustering
hesa_cluster <- hesa_filtered %>%
  select(Total, mean_salary_proxy, prop6) %>%  # you can add other proportions too
  scale()  # standardize features

# Determine optimal number of clusters (Elbow method)
fviz_nbclust(hesa_cluster, kmeans, method = "wss") +
  labs(title = "Elbow Method for Choosing k")



# Suppose we choose k = 3
set.seed(123)
kmeans_res <- kmeans(hesa_cluster, centers = 4, nstart = 25)

# Add cluster labels to data
hesa_filtered$cluster <- factor(kmeans_res$cluster)

# Plot clusters
ggplot(hesa_filtered, aes(x = Total, y = mean_salary_proxy, color = cluster)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "darkgreen", span=0.5) +  # nonparametric fit
  geom_quantile(quantiles = c(0.25, 0.5, 0.75), color = "red", linetype = "dashed") +  # conditional quantiles
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "University Average Salary vs Staff Size",
    x = "Total Staff",
    y = "Estimated Average Salary (£)"
  ) +
  theme_minimal()



# Interactive graph
library(dplyr)
library(ggplot2)
library(plotly)

# Assume your data already has cluster info
# hesa_filtered$cluster <- factor(kmeans_res$cluster)

# Basic scatter with clusters
p <- ggplot(hesa_filtered, aes(x = Total, y = mean_salary_proxy, color = cluster, text = HE.provider)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "black") +  # nonparametric regression
  labs(
    title = "Average Salary vs Total Staff per University",
    x = "Total Staff",
    y = "Estimated Average Salary (£)"
  ) +
  theme_minimal()

# Convert to interactive plotly graph
p_interactive <- ggplotly(p, tooltip = c("text", "x", "y"))

# Save as HTML for GitHub deployment
htmlwidgets::saveWidget(p_interactive, "university_salary_clusters.html")






# Discipliens
library(dplyr)
library(readr)

# Example: read your discipline-level data (replace path)
disciplines <- read.csv("~/Desktop/universities/unisalaries/dt025-table-13.csv")

# Define approximate midpoints for each salary band
midpoints <- c(20000, 25720, 33482, 44744, 60070, 80000)

# Process data
disciplines_processed <- disciplines %>%
  rename(
    salary1 = Contract.salary....22.681.,
    salary2 = Contract.salary....22.681.and....28.759.,
    salary3 = Contract.salary....28.759.and....38.205.,
    salary4 = Contract.salary....38.205.and....51.283.,
    salary5 = Contract.salary....51.283.and....68.857.,
    salary6 = Contract.salary....68.857.
  ) %>%
  mutate(across(
    c(salary1:salary6, Total),
    ~ as.numeric(gsub(",", "", .))
  )) %>%
  mutate(
    prop1 = salary1 / Total,
    prop2 = salary2 / Total,
    prop3 = salary3 / Total,
    prop4 = salary4 / Total,
    prop5 = salary5 / Total,
    prop6 = salary6 / Total,
    mean_salary_proxy = (salary1*midpoints[1] + salary2*midpoints[2] + salary3*midpoints[3] +
                           salary4*midpoints[4] + salary5*midpoints[5] + salary6*midpoints[6]) / Total
  )

# Ranking by mean salary
top_salary <- disciplines_processed %>%
  arrange(desc(mean_salary_proxy)) %>%
  select(Cost.centre, mean_salary_proxy)

# Ranking by proportion of staff in highest band
top_high_paid <- disciplines_processed %>%
  arrange(desc(prop6)) %>%
  select(Cost.centre, prop6)

# Print results
print("Top disciplines by average salary:")
print(top_salary)

print("Top disciplines by proportion of staff ≥ £68,857:")
print(top_high_paid)



#Financials

# Load financial dataset
fin <- read.csv("~/Desktop/universities/unisalaries/dt031-table-14.csv")

# Merge datasets by UKPRN
merged_data <- hesa_filtered %>%
  inner_join(fin, by = "UKPRN")

# Prepare data
fin_plot <- merged_data %>%
  select(HE.Provider, Surplus..deficit..as.a...of.total.income, Surplus..deficit..excl..pension.adjustment.as.a...of.total.income) %>%
  filter(!is.na(Surplus..deficit..as.a...of.total.income)) %>%
  mutate(Surplus = as.numeric(gsub(",", "", Surplus..deficit..as.a...of.total.income))) %>%  # convert to numeric
  mutate(Surplus_ex = as.numeric(gsub(",", "", Surplus..deficit..excl..pension.adjustment.as.a...of.total.income))) %>%  # convert to numeric
  arrange(Surplus) %>%
  mutate(rank = row_number())  # create an index for x-axis


g1<- ggplot(fin_plot, aes(x = rank, y = Surplus)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_y_continuous(breaks = pretty_breaks(n = 8)) +  # fewer ticks
  labs(
    x = "Universities (ordered by surplus/deficit)",
    y = "Surplus / Deficit (% of total income)",
    title = "Universities Ordered by Surplus / Deficit"
  ) +
  theme_minimal()

fin_plot <- fin_plot %>%
  arrange(Surplus_ex) %>%
  mutate(rank2 = row_number())  # create an index for x-axis

g2<- ggplot(fin_plot, aes(x = rank2, y = Surplus_ex)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_y_continuous(breaks = pretty_breaks(n = 8)) +  # fewer ticks
  labs(
    x = "Universities (ordered by surplus/deficit)",
    y = "Surplus / Deficit (% of total income)",
    title = "Universities Ordered by Surplus / Deficit"
  ) +
  theme_minimal()



ggplot() +
  # Surplus vs rank
  geom_line(data = fin_plot, aes(x = rank, y = Surplus, color = "With Pension Adjustment"), size = 1) +
  geom_point(data = fin_plot, aes(x = rank, y = Surplus, color = "With Pension Adjustment"), size = 2) +
  
  # Surplus_ex vs rank2
  geom_line(data = fin_plot, aes(x = rank2, y = Surplus_ex, color = "Excl. Pension Adjustment"), size = 1, linetype = "dashed") +
  geom_point(data = fin_plot, aes(x = rank2, y = Surplus_ex, color = "Excl. Pension Adjustment"), size = 2, shape = 1) +
  
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  
  scale_color_manual(values = c("With Pension Adjustment" = "steelblue", 
                                "Excl. Pension Adjustment" = "orange")) +
  scale_y_continuous(breaks = pretty_breaks(n = 8)) +
  
  labs(
    x = "Universities (ordered by series)",
    y = "Surplus / Deficit (% of total income)",
    title = "Universities Ordered by Surplus / Deficit",
    color = "Series"
  ) +
  theme_minimal()

