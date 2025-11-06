df <- read.csv("~/Desktop/premierleague/premier.csv")

df$Height_num <- as.numeric(gsub("m", "", df$Height))

df$MarketValue_num <- df$Market.Value
df$MarketValue_num <- gsub("€", "", df$MarketValue_num)
df$MarketValue_num <- gsub("m", "e6", df$MarketValue_num)
df$MarketValue_num <- gsub("k", "e3", df$MarketValue_num)
df$MarketValue_num <- trimws(df$MarketValue_num)   # remove spaces
df$MarketValue_num[df$MarketValue_num %in% c("", "-", "NA")] <- NA  # clean blanks
df$MarketValue_num <- as.numeric(sapply(df$MarketValue_num, function(x) {
  if (!is.na(x)) eval(parse(text = x)) else NA
}))

df$JoinDate <- as.Date(df$Join.Date, format = "%d/%m/%Y")
df$ContractUntil <- as.Date(df$Contract.Until, format = "%d/%m/%Y")

df$ContractRemaining <- as.numeric(difftime(df$ContractUntil, Sys.Date(), units = "days")) / 365

df$DualNationality <- ifelse(df$Nationality2 != "", 1, 0)

df$PositionGroup <- case_when(
  grepl("Goalkeeper", df$Position, ignore.case = TRUE) ~ "Goalkeeper",
  grepl("Back", df$Position, ignore.case = TRUE) ~ "Defender",
  grepl("Midfield", df$Position, ignore.case = TRUE) ~ "Midfielder",
  grepl("Wing|Forward|Striker", df$Position, ignore.case = TRUE) ~ "Forward",
  TRUE ~ "Other"
)

library(dplyr)
library(stringr)

df <- df %>%
  mutate(
    # 1️⃣ Trim leading/trailing spaces
    Nationality  = str_trim(Nationality),
    Nationality2 = str_trim(Nationality2),
    
    # 2️⃣ Merge home nations into "United Kingdom"
    Nationality = ifelse(Nationality %in% c("England", "Scotland", "Wales", "Northern Ireland"),
                         "United Kingdom", Nationality),
    Nationality2 = ifelse(Nationality2 %in% c("England", "Scotland", "Wales", "Northern Ireland"),
                          "United Kingdom", Nationality2),
    
    # 3️⃣ If both nationalities are "United Kingdom", clear the second
    Nationality2 = ifelse(Nationality == "United Kingdom" & Nationality2 == "United Kingdom", "", Nationality2)
  )


library(stringr)

df$Date_of_Birth <- str_extract(df$Date.of.Birth..Age., "\\d{2}/\\d{2}/\\d{4}")
df$Age <- as.numeric(str_extract(df$Date.of.Birth..Age., "(?<=\\()\\d+(?=\\))"))


# 
df <- df %>%
  rowwise() %>%
  mutate(
    nats = list(sort(unique(c(Nationality, Nationality2)[c(Nationality, Nationality2) != ""]))),
    Nationality  = nats[1],
    Nationality2 = ifelse(length(nats) > 1, nats[2], "")
  ) %>%
  ungroup() %>%
  dplyr::select(-nats)


# Height vs market value
ggplot(df, aes(x = Age, y = MarketValue_num)) +
  geom_point(alpha = 0.6, color = "steelblue") +  # scatter points
  geom_smooth(method = "loess", se = TRUE, color = "darkred") +  # smooth line with confidence interval
  scale_y_continuous(labels = scales::comma) +  # makes large numbers easier to read
  labs(
    title = "Player Height vs Market Value",
    x = "Age (y)",
    y = "Market Value (€)"
  ) +
  theme_minimal(base_size = 14)

library(ggplot2)
library(quantreg)

ggplot(df, aes(x = Age, y = MarketValue_num)) +
  geom_point(alpha = 0.6, color = "steelblue") +  # scatter points
  geom_quantile(quantiles = c(0.25, 0.5, 0.75), color = "darkred", size = 1) +  
  geom_smooth(method = "loess", se = TRUE, color = "darkred") +  # smooth line with confidence interval
  scale_y_continuous(labels = scales::comma) +  # format large numbers
  labs(
    title = "Player Age vs Market Value with Quantile Regression",
    x = "Age (y)",
    y = "Market Value (€)"
  ) +
  theme_minimal(base_size = 14)

library(ggplot2)
library(dplyr)

# Use the categorical MarketValue_quantile we created before
df <- df %>%
  group_by(Age) %>%
  mutate(
    q25 = quantile(MarketValue_num, 0.25, na.rm = TRUE),
    q50 = quantile(MarketValue_num, 0.50, na.rm = TRUE),
    q75 = quantile(MarketValue_num, 0.75, na.rm = TRUE),
    MarketValue_quantile = case_when(
      MarketValue_num <= q25 ~ "25th",
      MarketValue_num <= q50 ~ "50th",
      MarketValue_num <= q75 ~ "75th",
      TRUE ~ "Above 75th"
    )
  ) %>%
  ungroup()



get_loess_max <- function(data, y_var, x_var = "Age", span = 0.75) {
  # Fit LOESS smooth
  formula <- as.formula(paste(y_var, "~", x_var))
  loess_fit <- loess(formula, data = data, span = span)
  
  # Predict on fine grid
  x_grid <- seq(min(data[[x_var]]), max(data[[x_var]]), length.out = 200)
  y_pred <- predict(loess_fit, newdata = data.frame(Age = x_grid))
  
  # Find max
  max_idx <- which.max(y_pred)
  tibble(
    x_max = x_grid[max_idx],
    y_max = y_pred[max_idx]
  )
}


library(dplyr)

max_25 <- get_loess_max(filter(df, MarketValue_quantile == "25th"), "MarketValue_num")
max_50 <- get_loess_max(filter(df, MarketValue_quantile == "50th"), "MarketValue_num")
max_75 <- get_loess_max(filter(df, MarketValue_quantile == "75th"), "MarketValue_num")

max_25
max_50
max_75

max_table <- bind_rows(
  max_25 %>% mutate(Quantile = "25th"),
  max_50 %>% mutate(Quantile = "50th"),
  max_75 %>% mutate(Quantile = "75th")
)

max_table


# Plot with LOESS smooths and vertical lines at maxima
ggplot(df, aes(x = Age, y = MarketValue_num)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  
  geom_smooth(
    data = filter(df, MarketValue_quantile == "25th"),
    aes(y = MarketValue_num),
    method = "loess", color = "red", size = 1.2, se = FALSE
  ) +
  geom_smooth(
    data = filter(df, MarketValue_quantile == "50th"),
    aes(y = MarketValue_num),
    method = "loess", color = "darkgreen", size = 1.2, se = FALSE
  ) +
  geom_smooth(
    data = filter(df, MarketValue_quantile == "75th"),
    aes(y = MarketValue_num),
    method = "loess", color = "purple", size = 1.2, se = FALSE
  ) +
  
  # Add vertical lines at LOESS maxima
  geom_vline(data = max_table, aes(xintercept = x_max, color = Quantile), linetype = "dashed", size = 1) +
  
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c("25th" = "red", "50th" = "darkgreen", "75th" = "purple")) +
  
  labs(
    title = "Player Age vs Market Value",
    subtitle = "Vertical line = Peak market value",
    x = "Age (years)",
    y = "Market Value (€)",
    color = "Quantile"
  ) +
  theme_minimal(base_size = 14)



# Enhanced, cleaner plot: Reduced point opacity and size for less clutter,
# switched to theme_classic for crisper lines, added minor grid removal,
# and subtle adjustments to line styles for better distinction

library(ggplot2)
library(dplyr)
library(scales)  # For comma labels

# Assuming df and max_table are prepared as before

ggplot(df, aes(x = Age, y = MarketValue_num)) +
  geom_point(alpha = 0.15, size = 0.6, color = "steelblue", shape = 16) +  # Smaller, fainter points
  
  geom_smooth(
    data = filter(df, MarketValue_quantile == "25th"),
    aes(y = MarketValue_num),
    method = "loess", color = "red", size = 1.1, se = FALSE, linetype = "solid"
  ) +
  geom_smooth(
    data = filter(df, MarketValue_quantile == "50th"),
    aes(y = MarketValue_num),
    method = "loess", color = "darkgreen", size = 1.1, se = FALSE, linetype = "solid"
  ) +
  geom_smooth(
    data = filter(df, MarketValue_quantile == "75th"),
    aes(y = MarketValue_num),
    method = "loess", color = "purple", size = 1.1, se = FALSE, linetype = "solid"
  ) +
  
  # Vertical lines at maxima, with matching colors but thinner for subtlety
  geom_vline(data = max_table, aes(xintercept = x_max, color = Quantile), 
             linetype = "dashed", size = 0.8, alpha = 0.8) +
  
  scale_y_continuous(labels = comma) +
  scale_color_manual(values = c("25th" = "red", "50th" = "darkgreen", "75th" = "purple")) +
  
  labs(
    title = "Player Age vs Market Value",
    subtitle = "Vertical line = Peak market value",
    x = "Age (years)",
    y = "Market Value (€)",
    color = "Quantile market value"
  ) +
  theme_classic(base_size = 14) +  # Cleaner theme
  theme(
    panel.grid.minor = element_blank(),  # Remove minor grids for simplicity
    legend.position = "top",             # Move legend to top to save space
    plot.title = element_text(hjust = 0.5),  # Center title
    plot.subtitle = element_text(hjust = 0.5) # Center subtitle
  )

library(randomForest)

# 1️⃣ Remove rows with missing key variables
df_rf <- df %>%
  filter(!is.na(MarketValue_num) & !is.na(Age) & !is.na(Height_num) & !is.na(PositionGroup) & !is.na(Current.Club))

# 2️⃣ One-hot encode PositionGroup
position_dummies <- model.matrix(~ PositionGroup - 1, data = df_rf)
df_rf <- cbind(df_rf, position_dummies)
df_rf$PositionGroup <- NULL  # remove original factor
df_rf <- df_rf %>%
  mutate(Age2 = Age^2, Age3 = Age^3)

# 3️⃣ Fit random forest on all predictors
set.seed(123)
rf_model <- randomForest(MarketValue_num ~ Age+Age2+Age3 + Height_num+Current.Club + PositionGroupDefender+PositionGroupForward+PositionGroupGoalkeeper+PositionGroupMidfielder,  # all other columns as predictors
                         data = df_rf, ntree = 1000,
                         importance = TRUE)

# 4️⃣ Predict market value
df_rf$PredictedValue <- predict(rf_model, newdata = df_rf)

# 5️⃣ Flag "cheap" players (actual < 80% of predicted)
df_rf <- df_rf %>%
  mutate(CheapFlag = MarketValue_num < 0.02 * PredictedValue)

# 6️⃣ Inspect cheap players
cheap_players <- df_rf %>%
  filter(CheapFlag == TRUE) %>%
  select(Full.Name, Current.Club, Age, Height_num, starts_with("PositionGroup"), MarketValue_num, PredictedValue)

print(cheap_players)

# Optional: visualize actual vs predicted
library(ggplot2)

ggplot(df_rf, aes(x = PredictedValue, y = MarketValue_num, color = CheapFlag)) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "Predicted Market Value (€)", y = "Actual Market Value (€)",
       title = "Identifying 'Cheap' Premier League Players") +
  theme_minimal()

library(scales)  # for comma formatting

ggplot(df_rf, aes(x = PredictedValue, y = MarketValue_num, color = CheapFlag)) +
  geom_point(alpha = 0.7) +
  
  # 45-degree reference line
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "blue", size = 1) +
  
  # Regression line
  #geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "solid", size = 1) +
  
  # Format axes in millions
  scale_x_continuous(labels = function(x) paste0(x/1e6, "M")) +
  scale_y_continuous(labels = function(x) paste0(x/1e6, "M")) +
  
  labs(
    x = "Predicted Market Value (€M)", 
    y = "Actual Market Value (€M)",
    title = "Actual vs Predicted Market Value",
    subtitle = "with 45° and Regression Line",
    color = "Cheap Player"
  ) +
  theme_minimal(base_size = 14)

