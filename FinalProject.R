#####################
### Final Project ###
#####################

# Author: Keeley Kuru
# Date: 11/13/25
# Course: Zoo800

########################################
## NTL-LTER Trout Bog Lake Color Data ##
########################################

# Hypothesis:
# Trout Bog Lake has become increasingly brown (darker water color)
# over time due to rising dissolved organic carbon (DOC) inputs.

# Ecological expectation:
# Mean absorbance (normalized to 1 cm path length) should increase
# linearly with year, indicating long-term browning.

# Statistical model:
# Linear regression with mean absorbance as the response
# and year as the continuous predictor variable.

#######################################
## 1A. Find two continuous variables ##
#######################################

# -> Variables:
# 1. Year (from sampledate)
# 2. Mean absorbance normalized to 1 cm path length (value / cuvette)

required_packages <- c("tidyverse", "lubridate", "here", "lmtest", "viridis")  ### NEW
install_if_missing <- function(pkg){
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
}
invisible(sapply(required_packages, install_if_missing))

# Load libraries
library(tidyverse)
library(lubridate)
library(lmtest)
library(viridis)

# Import data
data <- read_csv(here::here("data", "ntl87_v13.csv"))

# Filter for Trout Bog lake
tb_data <- data %>%
  filter(lakeid == "TB")

# Ensure date format and extract year/month
tb_data <- tb_data %>%
  mutate(
    sampledate = as.Date(sampledate),
    year = year(sampledate),
    month = month(sampledate)
  )

# Filter for summer months: May (5) to August (8)
summer_tb <- tb_data %>%
  filter(month %in% 5:8)

# Normalize absorbance to 1 cm path length
summer_tb <- summer_tb %>%
  mutate(value_1cm = value / cuvette)

# Summarize by year
summer_summary <- summer_tb %>%
  group_by(year) %>%
  summarise(
    mean_value = mean(value_1cm, na.rm = TRUE),
    sd_value = sd(value_1cm, na.rm = TRUE),
    se_value = sd_value / sqrt(n()),
    n_samples = n()
  ) %>%
  arrange(year)

print(summer_summary)

###############################
## 1B. Linear Regression Fit ##
###############################

# Fit linear model: mean absorbance (Y) vs. year (X)
lm_model <- lm(mean_value ~ year, data = summer_summary)
summary(lm_model)

# Extract model stats
model_summary <- summary(lm_model)
slope <- round(coef(lm_model)[2], 4)
p_value <- signif(model_summary$coefficients[2, 4], 2)
r_squared <- round(model_summary$r.squared, 2)

annotation_text <- paste0(
  "p = ", p_value, "\n",
  "R² = ", r_squared, "\n",
  "Slope = ", slope, " absorbance units/yr"
)

decadal_change <- slope * 10
cat("Estimated rate of browning:", round(decadal_change, 3), "absorbance units per decade\n")

##############################
## 1C. Evaluate Assumptions ##
##############################

# Residual diagnostics
par(mfrow = c(1, 2))
plot(lm_model, which = 1)
plot(lm_model, which = 2)
par(mfrow = c(1, 1))

# Breusch-Pagan test for homoscedasticity
bptest(lm_model)  # formal variance test
# -> If p > 0.05, fail to reject null hypothesis of homoscedasticity
# -> If p < 0.05, reject null hypothesis, indicating heteroscedasticity
# --> p = 0.05799 --> There’s no strong evidence that your model’s residuals have unequal variance; they’re probably homoscedastic.
# ---> There may be a weak trend toward heteroscedasticity

# Shapiro-Wilk test for normality
shapiro.test(resid(lm_model))
# -> If p > 0.05, fail to reject null hypothesis of normality
# -> If p < 0.05, reject null hypothesis, indicating non-normality
# --> p = 0.7433 -> There’s no strong evidence that your model’s residuals deviate from normality; they’re probably normal.

# Extract residuals
residuals_lm <- resid(lm_model)

# Histogram of residuals
ggplot(data = data.frame(residuals = residuals_lm), aes(x = residuals)) +
  geom_histogram(bins = 15, fill = "#2C7BB6", color = "black", alpha = 0.8) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Histogram of Residuals for Trout Bog Lake Regression",
    x = "Residuals",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14)

#####################
## 1D. Predictions ##
#####################

# Find median and 95th percentile of year
median_year <- median(summer_summary$year)
perc95_year <- quantile(summer_summary$year, 0.95)

# Predict mean absorbance and 95% prediction intervals
predictions <- predict(
  lm_model,
  newdata = data.frame(year = c(median_year, perc95_year)),
  interval = "prediction"
)

pred_df <- data.frame(
  year = c(median_year, perc95_year),
  predictions
)
print(pred_df)

# --- Interpretation summary ---
cat("Predicted mean absorbance (median year ~2008):", round(pred_df$fit[1], 3), "\n")
cat("Predicted mean absorbance (95th percentile year ~2021):", round(pred_df$fit[2], 3), "\n")
cat("Change over period:", round(pred_df$fit[2] - pred_df$fit[1], 3), "absorbance units\n")
cat("This represents a", round(((pred_df$fit[2] - pred_df$fit[1]) / pred_df$fit[1]) * 100, 1), "% increase over time.\n")

##############
## Plotting ##
##############

# === Enhanced visualization ===
abs_trends_over_years <- ggplot(summer_summary, aes(x = year, y = mean_value)) +
  geom_line(color = "#2C7BB6", linewidth = 1) +
  geom_point(aes(color = mean_value), size = 3, alpha = 0.9) +  ### NEW: color by intensity
  scale_color_viridis(option = "C", direction = 1) +             ### NEW: color palette
  geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value),
                width = 0.2, color = "gray50", alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "#FDAE61", fill = "#FDAE61", alpha = 0.2, linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, color = "black", linetype = "dotted", linewidth = 0.8) +  ### NEW
  geom_label(
    aes(x = min(year) + 2, y = max(mean_value)),
    label = annotation_text,
    hjust = 0, size = 4,
    fill = "white", color = "black", alpha = 0.8
  ) +
  labs(
    title = "Browning of Trout Bog Lake (May–August)",
    subtitle = "Mean absorbance normalized to 1 cm path length, 1990–2020",
    x = "Year",
    y = "Mean Absorbance (1 cm)",
    color = "Absorbance"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

abs_trends_over_years

# ============================================================
# ===== Objective 2 =====
# ============================================================
# Goal: Evaluate how non-normal (lognormal) residuals affect
#       regression uncertainty and model robustness.
# ============================================================

# --- Setup ---
set.seed(123)
true_intercept <- 2
true_slope <- 3
n <- 100           # observations per simulation
n_sims <- 1000     # number of simulated datasets

##############################################################
## 2A. Simulate regression under lognormal residuals
##############################################################

# Each simulation: generate data with right-skewed lognormal errors,
# fit linear model, store slope estimate.

sims <- numeric(n_sims)

for (i in 1:n_sims) {
  X <- seq(0, 10, length.out = n)
  error <- rlnorm(n, meanlog = 0, sdlog = 0.5) - exp(0.5^2 / 2)  # centered skewed error
  Y <- true_intercept + true_slope * X + error
  model <- lm(Y ~ X)
  sims[i] <- coef(model)[2]  # store slope estimate
}

sims_df <- data.frame(slope = sims)

##############################################################
## 2B. Visualize slope variation across 1000 simulations
##############################################################

library(ggplot2)

ggplot(sims_df, aes(x = slope)) +
  geom_histogram(bins = 30, fill = "#FDAE61", color = "black", alpha = 0.8) +
  geom_vline(xintercept = true_slope, color = "red", linetype = "dashed") +
  labs(
    title = "Distribution of Estimated Slopes Across 1000 Simulations",
    subtitle = "Lognormal residuals (non-normal error structure)",
    x = "Estimated slope",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14)

# -> Expect slope estimates to cluster tightly around 3.0 (true value).
# -> Demonstrates that OLS regression is unbiased even under non-normal residuals.

##############################################################
## 2C. Single-model example and coefficient summary
##############################################################

# Generate one representative dataset
set.seed(321)
X <- seq(0, 10, length.out = n)
error <- rlnorm(n, meanlog = 0, sdlog = 0.5) - exp(0.5^2 / 2)
Y <- true_intercept + true_slope * X + error
data <- data.frame(X, Y)

# Fit model
model <- lm(Y ~ X, data = data)
summary(model)

##############################################################
## 2D. Repeat simulation using function for slope/intercept
##############################################################

simulate_once <- function() {
  X <- seq(0, 10, length.out = n)
  error <- rlnorm(n, meanlog = 0, sdlog = 0.5) - exp(0.5^2 / 2)
  Y <- true_intercept + true_slope * X + error
  fit <- lm(Y ~ X)
  c(intercept = coef(fit)[1], slope = coef(fit)[2])
}

set.seed(42)
sims <- replicate(n_sims, simulate_once())
sims_df <- as.data.frame(t(sims))

# Summarize results
mean_intercept <- mean(sims_df$intercept)
mean_slope <- mean(sims_df$slope)

cat("True intercept:", true_intercept, "\nEstimated mean intercept:", mean_intercept, "\n")
cat("True slope:", true_slope, "\nEstimated mean slope:", mean_slope, "\n")

# -> Should be nearly identical, confirming unbiased OLS estimates.

##############################################################
## 2E. Prediction intervals and coverage
##############################################################

preds <- predict(model, newdata = data.frame(X = X),
                 interval = "prediction", level = 0.95)
pred_df <- cbind(data, preds)

# Compute coverage fraction
inside <- mean(data$Y >= pred_df$lwr & data$Y <= pred_df$upr)
cat("Fraction of Y within 95% prediction interval:", inside, "\n")

##############################################################
## 2F. Interpretation
##############################################################
# -> The mean slope/intercept match their true values → regression is *unbiased*.
# -> The coverage fraction (≈ 0.95) shows prediction intervals are reliable
#   even with non-normal residuals.
# -> If the fraction were < 0.95, it would mean that skewed errors cause
#   OLS prediction intervals to underestimate uncertainty.
# -> Conclusion: OLS remains robust to moderate non-normality,
#   but caution is needed if residuals are highly asymmetric or heteroscedastic.
