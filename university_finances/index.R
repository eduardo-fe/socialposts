# You need to run this alonside the code in "unisalaries_deployed"
#Financials

# Load financial dataset
fin <- read.csv("~/Desktop/universities/salareis/dt031-table-14.csv")

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




ggplot(fin_plot, aes(x = Surplus, y = Surplus_ex)) +
  geom_point(color = "steelblue", alpha = 0.7, size = 2) +
  geom_smooth(method = "loess", color = "orange", se = TRUE, span = 0.6) + # nonparametric smooth
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") + # 45-degree reference
  labs(
    title = "Surplus vs Surplus Excluding Pension Adjustment",
    x = "Surplus (% of total income, with pension adjustment)",
    y = "Surplus Ex (% of total income, excl. pension adjustment)"
  ) +
  theme_minimal()
