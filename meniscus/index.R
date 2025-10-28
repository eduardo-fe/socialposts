# Load required packages
library(tidyverse)
library(lubridate)
library(rdrobust)
library(ggplot2)



# Load the data
data <- read.csv("~/Desktop/meniscusData.csv")

# Convert month column to Date
data <- data %>%
  mutate(Date = as.Date(paste0(month, "-01")))

# Define cutoff
cutoff_date <- as.Date("2020-03-01")

# Create running variable (numeric date) and post indicator
data <- data %>%
  mutate(
    running = as.numeric(Date),         # numeric days since 1970-01-01
    post = ifelse(Date >= cutoff_date, 1, 0)
  )

# Estimate RDD (local linear regression)
rdd_result <- rdrobust(y = data$interest, x = data$running, c = as.numeric(cutoff_date))
summary(rdd_result)

# Plot the discontinuity
ggplot(data, aes(x = Date, y = interest)) +
  geom_point(color = "grey50") +
  geom_vline(xintercept = cutoff_date, linetype = "dashed", color = "red") +
  geom_smooth(data = subset(data, Date < cutoff_date), method = "lm", se = FALSE, color = "blue") +
  geom_smooth(data = subset(data, Date >= cutoff_date), method = "lm", se = FALSE, color = "green") +
  labs(
    title = "RDD on Google Trends: 'Meniscal tear'",
    subtitle = "Cutoff: March 2020",
    x = "Date",
    y = "Interest (Google Trends index)"
  ) +
  theme_minimal()

