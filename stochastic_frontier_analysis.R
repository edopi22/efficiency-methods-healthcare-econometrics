# Stochastic Frontier Analysis for Healthcare Efficiency
# Demonstrates cost efficiency analysis for residential care facilities
# Author: E.R. Piombini
# Date: February 2025

# Load required packages
library(frontier)  # For SFA estimation
library(tidyverse) # Data manipulation and visualization
library(ggplot2)   # Advanced plotting

# Set seed for reproducibility
set.seed(123)

# ============================================================================
# 1. Simulate Healthcare Facility Data
# ============================================================================

# Number of facilities
n <- 150

# Simulate facility characteristics
facilities <- tibble(
  facility_id = 1:n,
  
  # Output: Total care days provided
  care_days = rpois(n, lambda = 8000),
  
  # Inputs
  staff_fte = rnorm(n, mean = 40, sd = 10),
  capital_expenditure = rlnorm(n, meanlog = 12, sdlog = 0.5),
  operating_costs = rlnorm(n, meanlog = 13, sdlog = 0.4),
  
  # Facility characteristics
  ownership = sample(c("For-Profit", "Non-Profit", "Public"), 
                     n, replace = TRUE, prob = c(0.4, 0.4, 0.2)),
  beds = round(rnorm(n, mean = 80, sd = 20)),
  urban = sample(c(0, 1), n, replace = TRUE, prob = c(0.3, 0.7)),
  
  # Quality proxy
  staff_per_bed = staff_fte / beds
)

# ============================================================================
# 2. Estimate Cost Frontier
# ============================================================================

# Cost function specification: log(Cost) = f(Output, Input Prices)
# Using Cobb-Douglas functional form

# Create log-transformed variables
facilities <- facilities %>%
  mutate(
    log_cost = log(operating_costs),
    log_care_days = log(care_days),
    log_staff = log(staff_fte),
    log_capital = log(capital_expenditure),
    log_beds = log(beds)
  )

# Estimate Stochastic Frontier (Half-Normal distribution)
sfa_model <- sfa(
  log_cost ~ log_care_days + log_staff + log_capital + log_beds + urban,
  data = facilities,
  ineffDecrease = FALSE,  # Cost frontier: higher values = inefficiency
  truncNorm = FALSE       # Half-normal distribution
)

# Display results
summary(sfa_model)

# ============================================================================
# 3. Extract Efficiency Scores
# ============================================================================

# Calculate technical efficiency scores
facilities$efficiency <- efficiencies(sfa_model)

# Summary statistics by ownership type
efficiency_summary <- facilities %>%
  group_by(ownership) %>%
  summarise(
    mean_efficiency = mean(efficiency),
    median_efficiency = median(efficiency),
    sd_efficiency = sd(efficiency),
    n = n()
  ) %>%
  arrange(desc(mean_efficiency))

print(efficiency_summary)

# ============================================================================
# 4. Visualization
# ============================================================================

# Distribution of efficiency scores by ownership
p1 <- ggplot(facilities, aes(x = efficiency, fill = ownership)) +
  geom_density(alpha = 0.6) +
  labs(
    title = "Cost Efficiency Distribution by Ownership Type",
    x = "Technical Efficiency Score",
    y = "Density",
    fill = "Ownership"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

print(p1)

# Boxplot comparison
p2 <- ggplot(facilities, aes(x = ownership, y = efficiency, fill = ownership)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  labs(
    title = "Cost Efficiency by Ownership Type",
    x = "Ownership Type",
    y = "Technical Efficiency Score"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "none"
  )

print(p2)

# ============================================================================
# 5. Statistical Testing
# ============================================================================

# ANOVA to test differences across ownership types
anova_result <- aov(efficiency ~ ownership, data = facilities)
summary(anova_result)

# Post-hoc Tukey HSD test
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

# ============================================================================
# 6. Export Results
# ============================================================================

# Save efficiency scores
write_csv(facilities %>% select(facility_id, ownership, efficiency),
          "facility_efficiency_scores.csv")

# Save summary statistics
write_csv(efficiency_summary, "efficiency_summary_by_ownership.csv")

cat("\nAnalysis complete. Results saved to CSV files.\n")
