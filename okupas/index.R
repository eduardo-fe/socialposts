# Load packages
library(tidyverse)
library(sf)
library(ggplot2)

# -----------------------------
# Step 1: Load okupation data
# -----------------------------
 


# Assume okupation CSV is already in long format or wide
okup <- read_csv("okupation.csv", ) %>%
  pivot_longer(cols = `2010`:`2024`, names_to = "year", values_to = "okupations") %>%
  mutate(year = as.integer(year))

# Load population data (province + population_2024)
pop <- read_csv("population.csv")

# Merge okupation data with population
okup_long <- okup %>%
  left_join(pop, by = c("NAME_2"))

# -----------------------------
# Step 2: Total okupations trend
# -----------------------------
total_okup <- okup_long %>%
  group_by(year) %>%
  summarise(total_okup = sum(okupations, na.rm = TRUE)) %>%
  arrange(year) %>%
  mutate(pct_change = (total_okup / lag(total_okup) - 1) * 100)

# Plot total okupations
ggplot(total_okup, aes(x = year, y = total_okup)) +
  geom_line(size = 1.2, color = "steelblue") +
  geom_point(size = 2) +
  labs(title = "Total Okupations in Spain (2010-2024)",
       x = "Year",
       y = "Total Okupations") +
  theme_minimal()

# Plot % change
ggplot(total_okup[-1,], aes(x = year, y = pct_change)) + # remove first NA
  geom_col(fill = "orange") +
  labs(title = "Year-on-Year % Change in Total Okupations",
       x = "Year",
       y = "% Change") +
  theme_minimal()

# -----------------------------
# Step 3: Map per capita okupations (2024)
# -----------------------------
# Load shapefile
esp <- st_read("gadm41_ESP_shp/gadm41_ESP_2.shp")

# Prepare 2024 data
okup_2024 <- okup_long %>%
  filter(year == 2024) %>%
  mutate(okup_per_capita = 100000* okupations / population)

# Join with shapefile
# Adjust column name in esp that contains province names, usually "NAME_1"
esp_okup <- esp %>%
  left_join(okup_2024, by = c("NAME_2" = "NAME_2"))

# Plot map
avg_val <- mean(esp_okup$okup_per_capita, na.rm = TRUE)
med_val <- median(esp_okup$okup_per_capita, na.rm = TRUE)


ggplot(esp_okup) +
  geom_sf(aes(fill = okup_per_capita), color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = med_val, name = "Okupations per Capita") +
  labs(title = "Okupations per Capita by Province (2024)",
       fill = "Okupations per Capita") +
  theme_minimal()




# Transform to metric projection
# Transform to metric projection
# Project to metric CRS
esp_okup_proj <- st_transform(esp_okup, 3857)  # Web Mercator

# Separate Canarias and mainland
canarias <- esp_okup_proj %>% filter(NAME_1 == "Islas Canarias")
mainland <- esp_okup_proj %>% filter(NAME_1 != "Islas Canarias")

# Shift Canarias safely
new_geom <- st_geometry(canarias) + c(500000, 800000)
canarias <- st_set_geometry(canarias, new_geom)

# Explicitly set CRS (copy from mainland)
st_crs(canarias) <- st_crs(mainland)

# Recombine
esp_okup_shifted <- rbind(mainland, canarias)
  

 


ggplot(esp_okup_shifted) +
  geom_sf(aes(fill = okup_per_capita), color = "white") +
  scale_fill_gradientn(colors = c("darkblue", "white", "darkred"),
                       values = scales::rescale(c(min(esp_okup_shifted$okup_per_capita, na.rm = TRUE),
                                                  median(esp_okup_shifted$okup_per_capita, na.rm = TRUE),
                                                  max(esp_okup_shifted$okup_per_capita, na.rm = TRUE))),
                       name = "Okupations per Capita") +
  labs(title = "Okupations per Capita by Province (2024)") +
  theme_minimal()
