library(tidyverse)
library(readr)
library(ggrepel)

# ── Data prep (unchanged) ────────────────────────────────────────────────────
foreign_students <- read_csv("chart-6.csv", skip = 12)
colnames(foreign_students) <- c("Permanent_address", "Academic_Year", "Number")

foreign_students <- foreign_students %>%
  filter(!is.na(Number)) %>%
  mutate(Academic_Year = trimws(Academic_Year))

total_by_year <- foreign_students %>%
  group_by(Academic_Year) %>%
  summarise(Total_Foreign = sum(Number))

composition_data <- foreign_students %>%
  left_join(total_by_year, by = "Academic_Year") %>%
  mutate(Percentage_of_Foreign = (Number / Total_Foreign) * 100)

# ── Palette: LinkedIn-friendly, high contrast ────────────────────────────────
custom_colors <- c(
  "China"          = "#0A66C2",   # LinkedIn blue
  "India"          = "#E84545",   # Strong red
  "Nigeria"        = "#2DBD6E",   # Vivid green
  "Total EU"       = "#F5A623",   # Warm amber
  "Other Asia"     = "#9B59B6",   # Purple
  "Middle East"    = "#1ABC9C",   # Teal
  "Other Africa"   = "#E67E22",   # Burnt orange
  "North America"  = "#C0392B",   # Dark red
  "Other Europe"   = "#2980B9",   # Sky blue
  "South America"  = "#7F8C8D",   # Slate grey
  "Australasia"    = "#16A085"    # Dark teal
)

# ── Identify end-point labels (last year only, to avoid clutter) ─────────────
label_data <- composition_data %>%
  group_by(Permanent_address) %>%
  slice_max(order_by = Academic_Year, n = 1) %>%
  ungroup()

# ── X-axis: show every other year to avoid overcrowding ─────────────────────
all_years   <- sort(unique(composition_data$Academic_Year))
shown_years <- all_years[seq(1, length(all_years), by = 2)]

# ── Plot ─────────────────────────────────────────────────────────────────────
line_plot <- ggplot(
  composition_data,
  aes(x     = Academic_Year,
      y     = Percentage_of_Foreign,
      color = Permanent_address,
      group = Permanent_address)
) +
  
  # Subtle background bands for readability
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0,  ymax = 10, fill = "#F7F9FB", alpha = 0.5) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 20, ymax = 30, fill = "#F7F9FB", alpha = 0.5) +  
  
  geom_line(linewidth = 1.1, lineend = "round", linejoin = "round") +
  geom_point(size = 1.8, stroke = 0.8, shape = 21, fill = "white") +
  
  # End-of-line labels instead of a legend (cleaner on LinkedIn)
  geom_text_repel(
    data          = label_data,
    aes(label     = Permanent_address),
    size          = 3.1,
    fontface      = "bold",
    direction     = "y",
    hjust         = 0,
    nudge_x       = 0.3,
    segment.size  = 0.3,
    segment.color = "grey70",
    box.padding   = 0.2,
    show.legend   = FALSE
  ) +
  
  scale_color_manual(values = custom_colors) +
  
  scale_x_discrete(
    breaks = shown_years,
    expand = expansion(mult = c(0.02, 0.18))  # extra right margin for labels
  ) +
  
  scale_y_continuous(
    limits = c(0, 35),
    breaks = seq(0, 35, 5),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0.01, 0.05))
  )  +
  
  labs(
    title    = "Where do the UK's foreign students come from?",
    subtitle = "Share of total foreign student enrolments by country/region of permanent address",
    caption  = "Source: HESA",
    x        = NULL,
    y        = NULL
  ) +
  
  theme_minimal(base_family = "sans", base_size = 13) +
  theme(
    # Canvas
    plot.background  = element_rect(fill = "#FFFFFF", color = NA),
    panel.background = element_rect(fill = "#FFFFFF", color = NA),
    plot.margin      = margin(20, 80, 16, 20),   # wide right margin for labels
    
    # Grid: keep only major horizontal
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(color = "#E2E8F0", linewidth = 0.4),
    
    # Axes
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 9,  color = "#475569"),
    axis.text.y  = element_text(size = 9, color = "#475569"),
    axis.ticks   = element_blank(),
    
    # Titles
    plot.title    = element_text(face = "bold",  size = 16, color = "#0F172A",
                                 margin = margin(b = 4)),
    plot.subtitle = element_text(size = 10.5, color = "#475569",
                                 margin = margin(b = 14)),
    plot.caption  = element_text(size = 8, color = "#94A3B8", hjust = 0,
                                 margin = margin(t = 12)),
    
    # No legend — labels do the job
    legend.position = "none"
  )

# ── Save at LinkedIn's ideal image ratio (1200 × 627 px) ────────────────────
ggsave(
  "foreign_students_linkedin.png",
  plot   = line_plot,
  width  = 12,
  height = 6.27,
  dpi    = 150,
  bg     = "white"
)

line_plot





# Filter to only the 5 groups of interest
composition_data <- composition_data %>%
  filter(Permanent_address %in% c("India", "China", "Nigeria", "Other Asia", "Total EU"))

# Recalculate percentages based on TOTAL (not just these 5)
# so shares still reflect % of all foreign students
# (already done via total_by_year join — no change needed)

custom_colors <- c(
  "China"     = "#0A66C2",   # LinkedIn blue
  "India"     = "#E84545",   # Strong red
  "Nigeria"   = "#2DBD6E",   # Vivid green
  "Other Asia" = "#F5A623",  # Warm amber
  "Total EU"  = "#9B59B6"    # Purple
)

label_data <- composition_data %>%
  group_by(Permanent_address) %>%
  slice_max(order_by = Academic_Year, n = 1) %>%
  ungroup()

all_years   <- sort(unique(composition_data$Academic_Year))
shown_years <- all_years[seq(1, length(all_years), by = 2)]

line_plot <- ggplot(
  composition_data,
  aes(x     = Academic_Year,
      y     = Percentage_of_Foreign,
      color = Permanent_address,
      group = Permanent_address)
) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0,  ymax = 10, fill = "#F7F9FB", alpha = 0.5) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 20, ymax = 30, fill = "#F7F9FB", alpha = 0.5) +
  
  geom_line(linewidth = 1.1, lineend = "round", linejoin = "round") +
  geom_point(size = 1.8, stroke = 0.8, shape = 21, fill = "white") +
  
  geom_text_repel(
    data          = label_data,
    aes(label     = Permanent_address),
    size          = 3.2,
    fontface      = "bold",
    direction     = "y",
    hjust         = 0,
    nudge_x       = 0.3,
    segment.size  = 0.3,
    segment.color = "grey70",
    box.padding   = 0.25,
    show.legend   = FALSE
  ) +
  
  scale_color_manual(values = custom_colors) +
  
  scale_x_discrete(
    breaks = shown_years,
    expand = expansion(mult = c(0.02, 0.18))
  ) +
  
  scale_y_continuous(
    limits = c(0, 35),
    breaks = seq(0, 35, 5),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0.01, 0.05))
  ) +
  
  labs(
    title    = "Where do the UK's foreign students come from?",
    subtitle = "Share of total foreign student enrolments by country/region of permanent address",
    caption  = "Source: HESA  ",
    x        = NULL,
    y        = NULL
  ) +
  
  theme_minimal(base_family = "sans", base_size = 13) +
  theme(
    plot.background  = element_rect(fill = "#FFFFFF", color = NA),
    panel.background = element_rect(fill = "#FFFFFF", color = NA),
    plot.margin      = margin(20, 80, 16, 20),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(color = "#E2E8F0", linewidth = 0.4),
    
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 9,  color = "#475569"),
    axis.text.y  = element_text(size = 9, color = "#475569"),
    axis.ticks   = element_blank(),
    
    plot.title    = element_text(face = "bold", size = 16, color = "#0F172A",
                                 margin = margin(b = 4)),
    plot.subtitle = element_text(size = 10.5, color = "#475569",
                                 margin = margin(b = 14)),
    plot.caption  = element_text(size = 8, color = "#94A3B8", hjust = 0,
                                 margin = margin(t = 12)),
    
    legend.position = "none"
  )

ggsave(
  "foreign_students_linkedin.png",
  plot   = line_plot,
  width  = 12,
  height = 6.27,
  dpi    = 150,
  bg     = "white"
)

line_plot


line_plot
# Create comparison table
comparison_table <- composition_data %>%
  filter(Academic_Year %in% c("2006/07", "2024/25")) %>%
  select(Permanent_address, Academic_Year, Percentage_of_Foreign) %>%
  pivot_wider(
    names_from = Academic_Year,
    values_from = Percentage_of_Foreign,
    names_prefix = "Pct_"
  ) %>%
  mutate(Change = `Pct_2024/25` - `Pct_2006/07`) %>%
  arrange(desc(`Pct_2024/25`))
 
cat("\n\nComposition Change (2006/07 vs 2024/25):\n")
print(comparison_table)

# Key insights
max_increase <- comparison_table %>% filter(Change == max(Change))
max_decrease <- comparison_table %>% filter(Change == min(Change))

cat("\n\nKey Insights:\n")
cat("-------------\n")
cat(sprintf("Biggest increase in share: %s (+%.2f pp)\n", 
            max_increase$Permanent_address, max_increase$Change))
cat(sprintf("Biggest decrease in share: %s (%.2f pp)\n", 
            max_decrease$Permanent_address, max_decrease$Change))

# Total foreign students
total_2006 <- composition_data %>% 
  filter(Academic_Year == "2006/07") %>% 
  pull(Total_Foreign) %>% 
  first()

total_2024 <- composition_data %>% 
  filter(Academic_Year == "2024/25") %>% 
  pull(Total_Foreign) %>% 
  first()

cat(sprintf("\nTotal foreign students:\n"))
cat(sprintf("  2006/07: %s\n", format(total_2006, big.mark = ",")))
cat(sprintf("  2024/25: %s\n", format(total_2024, big.mark = ",")))
cat(sprintf("  Growth: %.1f%%\n", ((total_2024/total_2006 - 1) * 100)))
