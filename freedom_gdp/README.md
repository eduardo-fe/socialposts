# Economic Freedom vs GDP per Capita (2025 data)

Interactive and static visualization exploring the relationship between **economic freedom** (Heritage Foundation Index) and **GDP per capita (PPP)** (World Bank), colored by **regime type** according to V-Dem liberal democracy index (2023).

## Main Findings (at a glance)

- Positive correlation between economic freedom and GDP per capita (usually r ≈ 0.60–0.75 depending on year)
- Very clear separation: countries with **high liberal democracy** tend to have both higher economic freedom **and** much higher income levels
- Many **low-democracy** countries appear in the middle/lower economic freedom range, even with some resource-rich outliers

## Features

- Automatic download of latest available **World Bank GDP per capita PPP** data
- Country name harmonization (many tricky cases: Korea, Congo, Hong Kong, Macao, Kosovo, etc.)
- Three-category regime classification based on V-Dem `v2x_libdem` (2023):
  - High democracy ≥ 0.7
  - Middle 0.4–0.699
  - Low democracy < 0.4
- Static ggplot2 visualization + **interactive plotly** HTML widget
- Logarithmic Y-axis for better spread of poor ↔ rich countries

## Requirements

```r
# Core packages
library(tidyverse)     # dplyr, ggplot2, readr, etc.
library(countrycode)
library(ggrepel)       # optional – nice label repelling if you add labels
library(plotly)

# Optional – only if you load V-Dem yourself from .dta
# library(haven)
