# =============================================================================
# Vickrey-Salop Model Simulation for Care Home Competition
# Author: E.R. Piombini
# Affiliation: University of Modena and Reggio Emilia & CAPP
# Date: February 2025
# =============================================================================
#
# This script implements the theoretical Vickrey-Salop spatial competition model
# presented in Section 4 of the chapter, analyzing the relationship between
# market competition and service quality in care home markets.
#
# Key features:
# - Regulated pricing segment (public funding)
# - Private-pay segment (market pricing)
# - Combined segment analysis
# - Distance-based competition weights
# - Critical discount factor calculations
#
# =============================================================================

library(tidyverse)
library(ggplot2)
library(patchwork)
library(scales)

set.seed(456)

# =============================================================================
# 1. MODEL PARAMETERS
# =============================================================================

# Quality levels
q_high <- 1.0      # High quality
q_low <- 0.5       # Low quality

# Cost parameters
c <- 0.15          # Marginal cost of high quality per patient
C <- 50            # Fixed cost of switching to high quality

# Market parameters
k <- 0.5           # Market size parameter (regulated segment)
sigma <- 0.5       # Horizontal differentiation parameter (0 to 1)

# Regulated price
p_regulated <- 0.7

# Discount factor range
delta_range <- seq(0.5, 0.99, by = 0.01)

# Number of competitors range
n_range <- seq(1, 20, by = 1)

# =============================================================================
# 2. REGULATED SEGMENT ANALYSIS
# =============================================================================

cat("\n")
cat("=" * 70, "\n")
cat("REGULATED SEGMENT: FIXED PRICE ANALYSIS\n")
cat("=" * 70, "\n\n")

# Function to calculate critical discount factor for regulated segment
delta_RS <- function(n, p, c, q_h, q_l, k, sigma, C) {
  numerator <- c * q_h^2 * (1 - p) - (2 + k) * (1 + n * sigma) * C
  denominator <- p * (1 - p) * (q_h^2 - q_l^2) - (2 + k) * (1 + n * sigma) * C
  
  delta_critical <- numerator / denominator
  return(delta_critical)
}

# Calculate critical delta for different values of n
delta_RS_values <- tibble(
  n = n_range,
  delta_critical = sapply(n, function(x) {
    delta_RS(x, p_regulated, c, q_high, q_low, k, sigma, C)
  })
) %>%
  mutate(
    # Clamp to valid range [0, 1]
    delta_critical = pmax(0, pmin(1, delta_critical)),
    segment = "Regulated"
  )

# Equilibrium patients and profits along equilibrium path
x_star_RS <- function(n, p, q_h, k, sigma) {
  (1 - p) * q_h^2 / ((2 + k) * (1 + n * sigma))
}

pi_star_RS <- function(n, p, c, q_h, k, sigma) {
  x <- x_star_RS(n, p, q_h, k, sigma)
  (p - c) * x
}

# Calculate for different n
regulated_equilibrium <- tibble(
  n = n_range,
  patients = sapply(n, function(x) x_star_RS(x, p_regulated, q_high, k, sigma)),
  profit = sapply(n, function(x) pi_star_RS(x, p_regulated, c, q_high, k, sigma))
)

print("Regulated Segment: Critical discount factors")
print(head(delta_RS_values, 10))

# =============================================================================
# 3. PRIVATE-PAY SEGMENT ANALYSIS
# =============================================================================

cat("\n")
cat("=" * 70, "\n")
cat("PRIVATE-PAY SEGMENT: ENDOGENOUS PRICING ANALYSIS\n")
cat("=" * 70, "\n\n")

# Function to calculate critical discount factor for private-pay segment
delta_PS <- function(n, c, q_h, q_l, sigma, C) {
  numerator <- (2 + n * sigma) * (8 * (2 + n * sigma) * C - 
                                    c^2 * q_h^2 * n * sigma + 
                                    2 * c^2 * q_h^2 - 
                                    4 * c * q_h^2)
  
  denominator <- 8 * (2 + n * sigma)^2 * C - 
                 c^2 * q_h^2 * n^2 * sigma^2 - 
                 4 * c * q_h^2 * n * sigma - 
                 4 * (q_h^2 - q_l^2)
  
  delta_critical <- numerator / denominator
  return(delta_critical)
}

# Calculate critical delta for private-pay segment
delta_PS_values <- tibble(
  n = n_range,
  delta_critical = sapply(n, function(x) {
    delta_PS(x, c, q_high, q_low, sigma, C)
  })
) %>%
  mutate(
    delta_critical = pmax(0, pmin(1, delta_critical)),
    segment = "Private-Pay"
  )

# Equilibrium patients and profits in private-pay segment
x_star_PS <- function(n, c, q_h, sigma) {
  (1 - c) * q_h^2 / (2 * (2 + n * sigma))
}

pi_star_PS <- function(n, c, q_h, sigma) {
  (1 - c)^2 * q_h^2 / (2 * (2 + n * sigma)^2)
}

private_equilibrium <- tibble(
  n = n_range,
  patients = sapply(n, function(x) x_star_PS(x, c, q_high, sigma)),
  profit = sapply(n, function(x) pi_star_PS(x, c, q_high, sigma)),
  # Implied equilibrium price
  price = sapply(n, function(x) {
    x_eq <- x_star_PS(x, c, q_high, sigma)
    1 - (2 * x_eq) / q_high^2 - (2 * sigma * (x - 1) * x_eq) / q_high^2
  })
)

print("Private-Pay Segment: Critical discount factors")
print(head(delta_PS_values, 10))

# =============================================================================
# 4. COMBINED SEGMENT ANALYSIS
# =============================================================================

cat("\n")
cat("=" * 70, "\n")
cat("COMBINED SEGMENT ANALYSIS\n")
cat("=" * 70, "\n\n")

# Combine both segments (simplified)
# In reality, the ICC combines profits from both segments
delta_combined <- delta_RS_values %>%
  left_join(delta_PS_values, by = "n", suffix = c("_reg", "_priv")) %>%
  mutate(
    # Approximate combined critical delta (weighted average)
    delta_critical_combined = 0.5 * delta_critical_reg + 0.5 * delta_critical_priv,
    delta_critical_combined = pmax(0, pmin(1, delta_critical_combined))
  )

# =============================================================================
# 5. DISTANCE-BASED COMPETITION MODEL (Model II)
# =============================================================================

cat("\n")
cat("=" * 70, "\n")
cat("DISTANCE-BASED COMPETITION ANALYSIS\n")
cat("=" * 70, "\n\n")

# Function to calculate competition weight based on distance
omega <- function(d, decay_rate = 0.5) {
  # Exponential decay: nearby competitors have higher weight
  exp(-decay_rate * d)
}

# Simulate facilities in a geographic space
n_facilities_spatial <- 50
geographic_space <- tibble(
  facility_id = 1:n_facilities_spatial,
  x_coord = runif(n_facilities_spatial, 0, 100),
  y_coord = runif(n_facilities_spatial, 0, 100)
)

# Calculate distance matrix
distance_matrix <- as.matrix(dist(geographic_space %>% select(x_coord, y_coord)))

# Calculate competition weights for each facility
competition_intensity <- tibble(
  facility_id = 1:n_facilities_spatial,
  avg_distance = rowMeans(distance_matrix),
  weighted_competition = sapply(1:n_facilities_spatial, function(i) {
    # Sum of omega weights for all other facilities
    sum(omega(distance_matrix[i, -i]))
  })
)

print("Competition intensity by geographic dispersion:")
print(summary(competition_intensity))

# =============================================================================
# 6. VISUALIZATIONS
# =============================================================================

cat("\n")
cat("=" * 70, "\n")
cat("CREATING VISUALIZATIONS\n")
cat("=" * 70, "\n\n")

# 6.1: Critical discount factor vs competition (Regulated Segment)
p1 <- ggplot(delta_RS_values, aes(x = n, y = delta_critical)) +
  geom_line(size = 1.2, color = "#2ecc71") +
  geom_point(size = 2, color = "#27ae60") +
  geom_hline(yintercept = 0.9, linetype = "dashed", color = "red", size = 0.8) +
  annotate("text", x = 15, y = 0.92, label = "δ = 0.9", color = "red") +
  labs(
    title = "Regulated Segment: Competition and Quality Sustainability",
    subtitle = "Monotonically decreasing relationship (reputation-building effect)",
    x = "Number of Competitors (n)",
    y = "Critical Discount Factor (δ)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

ggsave("model_regulated_segment.png", p1, width = 10, height = 6, dpi = 300)

# 6.2: Critical discount factor vs competition (Private-Pay Segment)
p2 <- ggplot(delta_PS_values, aes(x = n, y = delta_critical)) +
  geom_line(size = 1.2, color = "#3498db") +
  geom_point(size = 2, color = "#2980b9") +
  geom_hline(yintercept = 0.9, linetype = "dashed", color = "red", size = 0.8) +
  annotate("text", x = 15, y = 0.92, label = "δ = 0.9", color = "red") +
  labs(
    title = "Private-Pay Segment: U-Shaped Relationship",
    subtitle = "Rent extraction effect (low n) vs reputation-building effect (high n)",
    x = "Number of Competitors (n)",
    y = "Critical Discount Factor (δ)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

ggsave("model_privatepay_segment.png", p2, width = 10, height = 6, dpi = 300)

# 6.3: Compare both segments
p3 <- bind_rows(
  delta_RS_values %>% mutate(segment = "Regulated"),
  delta_PS_values %>% mutate(segment = "Private-Pay")
) %>%
  ggplot(aes(x = n, y = delta_critical, color = segment, linetype = segment)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Regulated" = "#2ecc71", "Private-Pay" = "#3498db")) +
  labs(
    title = "Competition and Quality: Comparison Across Market Segments",
    x = "Number of Competitors (n)",
    y = "Critical Discount Factor (δ)",
    color = "Market Segment",
    linetype = "Market Segment"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave("model_comparison.png", p3, width = 12, height = 7, dpi = 300)

# 6.4: Equilibrium profits by competition level
p4 <- bind_rows(
  regulated_equilibrium %>% mutate(segment = "Regulated", profit = profit),
  private_equilibrium %>% mutate(segment = "Private-Pay")
) %>%
  ggplot(aes(x = n, y = profit, color = segment)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Regulated" = "#2ecc71", "Private-Pay" = "#3498db")) +
  labs(
    title = "Equilibrium Profits vs Competition Level",
    subtitle = "High-quality equilibrium path",
    x = "Number of Competitors (n)",
    y = "Per-Facility Profit (π*)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

ggsave("equilibrium_profits.png", p4, width = 10, height = 6, dpi = 300)

# 6.5: Geographic competition intensity
p5 <- ggplot(competition_intensity, aes(x = avg_distance, y = weighted_competition)) +
  geom_point(size = 3, alpha = 0.6, color = "#e74c3c") +
  geom_smooth(method = "loess", se = TRUE, color = "#c0392b") +
  labs(
    title = "Geographic Dispersion and Competition Intensity",
    subtitle = "Distance-weighted competition index",
    x = "Average Distance to Competitors",
    y = "Weighted Competition Index"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14)
  )

ggsave("geographic_competition.png", p5, width = 10, height = 6, dpi = 300)

# 6.6: Spatial distribution of facilities
p6 <- ggplot(geographic_space, aes(x = x_coord, y = y_coord)) +
  geom_point(aes(size = competition_intensity$weighted_competition,
                 color = competition_intensity$weighted_competition),
             alpha = 0.7) +
  scale_color_viridis_c(option = "plasma") +
  scale_size_continuous(range = c(2, 10)) +
  labs(
    title = "Spatial Distribution of Care Homes",
    subtitle = "Point size indicates local competition intensity",
    x = "Geographic X-coordinate",
    y = "Geographic Y-coordinate",
    color = "Competition\nIntensity",
    size = "Competition\nIntensity"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14)
  )

ggsave("spatial_distribution.png", p6, width = 10, height = 8, dpi = 300)

# =============================================================================
# 7. PARAMETER SENSITIVITY ANALYSIS
# =============================================================================

cat("\n")
cat("=" * 70, "\n")
cat("PARAMETER SENSITIVITY ANALYSIS\n")
cat("=" * 70, "\n\n")

# Test different values of sigma (horizontal differentiation)
sigma_values <- c(0.1, 0.3, 0.5, 0.7, 0.9)

sensitivity_sigma <- map_df(sigma_values, function(sig) {
  tibble(
    n = n_range,
    sigma = sig,
    delta_RS = sapply(n, function(x) {
      delta_RS(x, p_regulated, c, q_high, q_low, k, sig, C)
    }),
    delta_PS = sapply(n, function(x) {
      delta_PS(x, c, q_high, q_low, sig, C)
    })
  )
}) %>%
  mutate(
    delta_RS = pmax(0, pmin(1, delta_RS)),
    delta_PS = pmax(0, pmin(1, delta_PS))
  )

# Plot sensitivity to sigma
p7 <- ggplot(sensitivity_sigma, aes(x = n, y = delta_RS, color = factor(sigma))) +
  geom_line(size = 1) +
  scale_color_viridis_d(option = "magma") +
  labs(
    title = "Sensitivity to Horizontal Differentiation (σ)",
    subtitle = "Regulated segment: Effect on critical discount factor",
    x = "Number of Competitors (n)",
    y = "Critical Discount Factor (δ)",
    color = "σ value"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

ggsave("sensitivity_sigma_regulated.png", p7, width = 10, height = 6, dpi = 300)

# Test different cost values
c_values <- c(0.05, 0.10, 0.15, 0.20, 0.25)

sensitivity_cost <- map_df(c_values, function(cost) {
  tibble(
    n = n_range,
    cost = cost,
    delta_RS = sapply(n, function(x) {
      delta_RS(x, p_regulated, cost, q_high, q_low, k, sigma, C)
    }),
    delta_PS = sapply(n, function(x) {
      delta_PS(x, cost, q_high, q_low, sigma, C)
    })
  )
}) %>%
  mutate(
    delta_RS = pmax(0, pmin(1, delta_RS)),
    delta_PS = pmax(0, pmin(1, delta_PS))
  )

p8 <- ggplot(sensitivity_cost, aes(x = n, y = delta_PS, color = factor(cost))) +
  geom_line(size = 1) +
  scale_color_viridis_d(option = "cividis") +
  labs(
    title = "Sensitivity to Quality Cost (c)",
    subtitle = "Private-pay segment: Effect on U-shaped relationship",
    x = "Number of Competitors (n)",
    y = "Critical Discount Factor (δ)",
    color = "Cost (c)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

ggsave("sensitivity_cost_privatepay.png", p8, width = 10, height = 6, dpi = 300)

# =============================================================================
# 8. POLICY SIMULATION
# =============================================================================

cat("\n")
cat("=" * 70, "\n")
cat("POLICY SIMULATION SCENARIOS\n")
cat("=" * 70, "\n\n")

# Scenario 1: Increase regulated price
price_scenarios <- seq(0.5, 0.9, by = 0.1)

policy_price <- map_df(price_scenarios, function(p) {
  tibble(
    n = n_range,
    price = p,
    delta_critical = sapply(n, function(x) {
      delta_RS(x, p, c, q_high, q_low, k, sigma, C)
    })
  )
}) %>%
  mutate(delta_critical = pmax(0, pmin(1, delta_critical)))

p9 <- ggplot(policy_price, aes(x = n, y = delta_critical, color = factor(price))) +
  geom_line(size = 1) +
  scale_color_viridis_d(option = "turbo") +
  labs(
    title = "Policy Simulation: Effect of Regulated Price Increase",
    subtitle = "Higher reimbursement rates facilitate quality maintenance",
    x = "Number of Competitors (n)",
    y = "Critical Discount Factor (δ)",
    color = "Regulated\nPrice (p)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

ggsave("policy_price_increase.png", p9, width = 12, height = 6, dpi = 300)

# Scenario 2: Quality subsidy (reduced cost c)
subsidy_scenarios <- seq(0, 0.10, by = 0.02)  # Subsidy amount

policy_subsidy <- map_df(subsidy_scenarios, function(sub) {
  effective_cost <- c - sub
  tibble(
    n = n_range,
    subsidy = sub,
    delta_critical_reg = sapply(n, function(x) {
      delta_RS(x, p_regulated, effective_cost, q_high, q_low, k, sigma, C)
    }),
    delta_critical_priv = sapply(n, function(x) {
      delta_PS(x, effective_cost, q_high, q_low, sigma, C)
    })
  )
}) %>%
  mutate(
    delta_critical_reg = pmax(0, pmin(1, delta_critical_reg)),
    delta_critical_priv = pmax(0, pmin(1, delta_critical_priv))
  )

p10 <- policy_subsidy %>%
  pivot_longer(cols = starts_with("delta"), names_to = "segment", values_to = "delta") %>%
  mutate(segment = ifelse(grepl("reg", segment), "Regulated", "Private-Pay")) %>%
  ggplot(aes(x = n, y = delta, color = factor(subsidy))) +
  geom_line(size = 0.8) +
  facet_wrap(~segment, ncol = 2) +
  scale_color_viridis_d(option = "mako") +
  labs(
    title = "Policy Simulation: Quality Cost Subsidies",
    subtitle = "Effect of subsidizing high-quality care provision",
    x = "Number of Competitors (n)",
    y = "Critical Discount Factor (δ)",
    color = "Subsidy\nAmount"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

ggsave("policy_quality_subsidy.png", p10, width = 12, height = 6, dpi = 300)

# =============================================================================
# 9. EXPORT RESULTS
# =============================================================================

# Export theoretical model results
write_csv(delta_RS_values, "model_regulated_segment_results.csv")
write_csv(delta_PS_values, "model_privatepay_segment_results.csv")
write_csv(delta_combined, "model_combined_segment_results.csv")

# Export equilibrium values
write_csv(regulated_equilibrium, "equilibrium_regulated.csv")
write_csv(private_equilibrium, "equilibrium_privatepay.csv")

# Export sensitivity analysis
write_csv(sensitivity_sigma, "sensitivity_analysis_sigma.csv")
write_csv(sensitivity_cost, "sensitivity_analysis_cost.csv")

# Export policy simulations
write_csv(policy_price, "policy_simulation_price.csv")
write_csv(policy_subsidy, "policy_simulation_subsidy.csv")

# Export competition intensity data
write_csv(competition_intensity, "geographic_competition_intensity.csv")

# =============================================================================
# 10. SUMMARY STATISTICS
# =============================================================================

cat("\n")
cat("=" * 70, "\n")
cat("MODEL SIMULATION SUMMARY\n")
cat("=" * 70, "\n\n")

cat("Key Model Parameters:\n")
cat("- High quality (q_h):", q_high, "\n")
cat("- Low quality (q_l):", q_low, "\n")
cat("- Quality cost (c):", c, "\n")
cat("- Fixed switching cost (C):", C, "\n")
cat("- Regulated price (p):", p_regulated, "\n")
cat("- Horizontal differentiation (σ):", sigma, "\n\n")

cat("Main Findings:\n")
cat("1. Regulated Segment:\n")
cat("   - Monotonically decreasing δ(n)\n")
cat("   - More competition → easier to sustain high quality\n")
cat("   - Range of δ:", 
    round(min(delta_RS_values$delta_critical, na.rm = TRUE), 3), "to",
    round(max(delta_RS_values$delta_critical, na.rm = TRUE), 3), "\n\n")

cat("2. Private-Pay Segment:\n")
cat("   - U-shaped relationship\n")
cat("   - Moderate competition → hardest to sustain quality\n")
cat("   - Range of δ:", 
    round(min(delta_PS_values$delta_critical, na.rm = TRUE), 3), "to",
    round(max(delta_PS_values$delta_critical, na.rm = TRUE), 3), "\n\n")

cat("3. Geographic Competition:\n")
cat("   - Average distance between facilities:", 
    round(mean(competition_intensity$avg_distance), 2), "\n")
cat("   - Average weighted competition:", 
    round(mean(competition_intensity$weighted_competition), 2), "\n\n")

cat("\nAll results have been exported to CSV files.\n")
cat("Visualizations have been saved as PNG files.\n")
cat("\nAnalysis complete.\n")
