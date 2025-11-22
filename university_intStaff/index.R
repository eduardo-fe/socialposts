df <- read.csv("~/Desktop/universities/university_staff/dt025-table-12-2023.csv")
df20 <- read.csv("~/Desktop/universities/university_staff/dt025-table-12-2020.csv")
df16 <- read.csv("~/Desktop/universities/university_staff/dt025-table-12-2016.csv")

# Load packages
library(tidyverse)
library(viridis)
library(FactoMineR)
library(factoextra)
library(countrycode)


df <- df %>%
  filter(!Nationality %in% c("United Kingdom", "Total", "Not known"))

df <- df %>%
  mutate(
    Continent = countrycode(Nationality, origin = "country.name", destination = "continent")
  )

# --- 1. Identify columns ---
# Subject columns: X101–X145
subject_cols <- grep("^X\\d{3}", colnames(df), value = TRUE)

# Service / support columns: use keywords to match names robustly

# --- 2. Convert all to numeric safely ---
df <- df %>%
  mutate(across(all_of(subject_cols), ~ as.numeric(str_replace_all(., ",", "")))) %>%
  mutate(across(c(all_of(subject_cols)), ~ replace_na(., 0)))

# Optional: clean subject names for readability
clean_subject_names <- subject_cols %>%
  str_remove("^X\\d{3}\\.") %>%       # remove prefix like X101.
  str_replace_all("\\.+", " ")        # replace multiple dots with space


# --- 3. Compute totals per nationality ---
df_totals <- df %>%
  rowwise() %>%
  mutate(
    TotalAcademic = sum(c_across(all_of(subject_cols))),
    TotalAll = TotalAcademic 
  ) %>%
  ungroup() %>%
  arrange(desc(TotalAll))



# Bar chart: top 10 nationalities by total students / costs
df_totals %>%
  top_n(15, TotalAll) %>%
  ggplot(aes(x = reorder(Nationality, TotalAll), y = TotalAll)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Nationalities by Total Students / Costs",
       x = "Nationality", y = "Total Count / Cost") +
  theme_minimal()



# Top 10 nationalities by total students / costs
top10 <- df_totals %>%
  filter(!Nationality %in% c("UK", "Total", "Not known")) %>%  # remove unwanted rows
  mutate(
    Nationality = str_replace(Nationality, " \\[.*\\]$", "")   # remove brackets and text after
  ) %>%
  top_n(20, TotalAll) %>%
  arrange(TotalAll)  # ascending for coord_flip

ggplot(top10, aes(x = reorder(Nationality, TotalAll), y = TotalAll)) +
  geom_col(fill = "#1f78b4", width = 0.7) +                  # vibrant steel-blue bars
  geom_text(aes(label = scales::comma(TotalAll)),            # add values with commas
            hjust = -0.1, size = 4, fontface = "bold") +
  coord_flip(clip = "off") +                                 # allow text outside
  labs(title = "UK Universities: Top 10 Staff Nationalities",
       x = "Nationality", y = "Total Count / Cost") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.text.y = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))    





## Part 2

library(dplyr)
library(purrr)
library(stringr)

# List of files and years
dfs <- list(
  "2016" = "~/Desktop/universities/university_staff/dt025-table-12-2016.csv",
  "2020" = "~/Desktop/universities/university_staff/dt025-table-12-2020.csv",
  "2023" = "~/Desktop/universities/university_staff/dt025-table-12-2023.csv"
)

# Function to load and clean one file
load_clean <- function(file, year) {
  df <- read.csv(file, stringsAsFactors = FALSE)
  
  # Subject columns: X101–X145
  subject_cols <- grep("^X\\d{3}", colnames(df), value = TRUE)
  
  # Convert all subject columns to numeric (remove commas)
  df <- df %>%
    mutate(across(all_of(subject_cols), ~ as.numeric(str_replace_all(., ",", "")))) %>%
    mutate(
      Year = year,
      Nationality = str_replace(Nationality, " \\[.*\\]$", "")
    ) %>%
    filter(!Nationality %in% c("United Kingdom", "Total", "Not known"))
  
  return(df)
}

# Load all years and combine safely
df_all <- map2_df(dfs, names(dfs), load_clean)


df_all <- df_all %>%
  rowwise() %>%
  mutate(TotalAll = sum(c_across(starts_with("X")), na.rm = TRUE)) %>%
  ungroup()


subject_cols <- grep("^X\\d{3}", colnames(df_all), value = TRUE)

df_all <- df_all %>%
  mutate(across(all_of(subject_cols), ~ as.numeric(str_replace_all(., ",", "")))) %>%
  rowwise() %>%
  mutate(TotalAll = sum(c_across(all_of(subject_cols)), na.rm = TRUE)) %>%
  ungroup()


df_prop <- df_all %>%
  group_by(Year) %>%
  mutate(TotalStaffYear = sum(TotalAll)) %>%
  ungroup() %>%
  mutate(Proportion = TotalAll / TotalStaffYear)

top_nationalities <- df_prop %>%
  group_by(Nationality) %>%
  summarise(MaxStaff = max(TotalAll)) %>%
  slice_max(MaxStaff, n = 10) %>%
  pull(Nationality)

df_plot <- df_prop %>%
  filter(Nationality %in% top_nationalities)



library(viridis)

ggplot(df_plot, aes(x = as.numeric(Year), y = Proportion, color = Nationality)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_text(aes(label = scales::percent(Proportion, accuracy = 0.1)), 
            vjust = -0.5, size = 3, show.legend = FALSE) +
  scale_color_viridis_d(option = "turbo") +   # colorblind-friendly discrete palette
  scale_x_continuous(breaks = c(2016, 2020, 2023)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Composition of Foreign Academic Staff ", subtitle="in UK Universities (2016–2023)",
       x = "Year", y = "Share of Foreign Academic Staff (%)", color = "Nationality") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )






library(dplyr)
library(readr)

# 2016
df16 <- read_csv("~/Desktop/universities/university_staff/dt025-table-12-2016.csv")
total16 <- df16 %>%
  filter(!Nationality %in% c("Total", "Not known")) %>%
  summarise(TotalAcademic = sum(as.numeric(gsub(",", "", `Total academic cost centres`)), na.rm = TRUE))

# 2020
df20 <- read_csv("~/Desktop/universities/university_staff/dt025-table-12-2020.csv")
total20 <- df20 %>%
  filter(!Nationality %in% c("Total", "Not known")) %>%
  summarise(TotalAcademic = sum(as.numeric(gsub(",", "", `Total academic cost centres`)), na.rm = TRUE))

# 2023
df23 <- read_csv("~/Desktop/universities/university_staff/dt025-table-12-2023.csv")
total23 <- df23 %>%
  filter(!Nationality %in% c("Total", "Not known")) %>%
  summarise(TotalAcademic = sum(as.numeric(gsub(",", "", `Total academic cost centres`)), na.rm = TRUE))

# Combine into a single table
total_academic <- tibble(
  Year = c(2016, 2020, 2023),
  TotalAcademic = c(total16$TotalAcademic, total20$TotalAcademic, total23$TotalAcademic)
)

total_academic





library(dplyr)
library(readr)

# 2016
df16 <- read_csv("~/Desktop/universities/university_staff/dt025-table-12-2016.csv")
nonUK16 <- df16 %>%
  filter(!Nationality %in% c("United Kingdom", "Total", "Not known")) %>%
  summarise(NonUKAcademic = sum(as.numeric(gsub(",", "", `Total academic cost centres`)), na.rm = TRUE))

# 2020
df20 <- read_csv("~/Desktop/universities/university_staff/dt025-table-12-2020.csv")
nonUK20 <- df20 %>%
  filter(!Nationality %in% c("United Kingdom", "Total", "Not known")) %>%
  summarise(NonUKAcademic = sum(as.numeric(gsub(",", "", `Total academic cost centres`)), na.rm = TRUE))

# 2023
df23 <- read_csv("~/Desktop/universities/university_staff/dt025-table-12-2023.csv")
nonUK23 <- df23 %>%
  filter(!Nationality %in% c("United Kingdom", "Total", "Not known")) %>%
  summarise(NonUKAcademic = sum(as.numeric(gsub(",", "", `Total academic cost centres`)), na.rm = TRUE))

# Combine into a single table
nonUK_totals <- tibble(
  Year = c(2016, 2020, 2023),
  NonUKAcademic = c(nonUK16$NonUKAcademic, nonUK20$NonUKAcademic, nonUK23$NonUKAcademic)
)

nonUK_totals




 