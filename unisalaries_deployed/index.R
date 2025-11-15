library(dplyr)
library(ggplot2)
library(readr)

# Example: read your data (replace with your actual file path)
hesa <- read_tsv("hesa_salaries.tsv")

# Check data
head(hesa)
