# You need the meniscusData.csv file.

# Load required libraries
library(ggplot2)
library(readr)
library(rdrobust)
library(tseries)
library(urca)

# --- Load and prepare data ---
df <- read_csv("meniscusData.csv")
df$month <- as.Date(paste0(df$month, "-01"))

# Define cutoff (March 2020)
cutoff <- as.Date("2020-03-01")

# Create running variable: time in months relative to cutoff
df$running <- as.numeric(difftime(df$month, cutoff, units = "days")) / 30.44

# Plot with vertical cutoff line
ggplot(df, aes(x = month, y = interest)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", span = 0.3, color = "blue", se = FALSE) +
  geom_vline(xintercept = cutoff, linetype = "dashed", color = "red", size = 1) +
  theme_minimal() +
  labs(
    title = "Google Searches for 'Meniscal Tear' Over Time (RDD cutoff: March 2020)",
    x = "Month",
    y = "Search Interest"
  )

# --- Regression Discontinuity Design ---
# rdrobust estimates the treatment effect at the cutoff
rdd_result <- rdrobust(y = df$interest, x = df$running, c = 0)

# Print results
summary(rdd_result)

# --- Optional: RDD plot ---
rdplot(y = df$interest, x = df$running, c = 0,
       x.label = "Months from cutoff (March 2020)",
       y.label = "Search Interest",
       title = "RDD plot: Google Search Interest around March 2020")


# --- AR(1) Analysis ---
# Convert to monthly time series
ts_data <- ts(df$interest,
              start = c(as.numeric(format(min(df$month), "%Y")),
                        as.numeric(format(min(df$month), "%m"))),
              frequency = 12)

# Fit AR(1) model
fit_ar1 <- arima(ts_data, order = c(1, 0, 0))

# Print model results
print(fit_ar1)

# Extract AR(1) coefficient
phi1 <- fit_ar1$coef[1]
cat("\nEstimated AR(1) coefficient:", round(phi1, 3), "\n")

# Optional diagnostic plots
acf(ts_data, main = "Autocorrelation Function (ACF) of Search Interest")
pacf(ts_data, main = "Partial Autocorrelation Function (PACF) of Search Interest")


adf_result <- adf.test(ts_data, alternative = "stationary")
print(adf_result)

# --- Phillips-Perron test (urca package) ---
pp_result <- ur.pp(ts_data, type = "Z-tau", model = "trend", lags = "short")
summary(pp_result)

# --- Optional: KPSS test (stationarity test, null = stationary) ---
kpss_result <- ur.kpss(ts_data, type = "tau", use.lag = NULL)
summary(kpss_result)
