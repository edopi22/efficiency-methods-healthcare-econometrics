# Spatial Analysis of Healthcare Facility Efficiency
# Demonstrates spatial autocorrelation and spatial econometric methods
# Author: E.R. Piombini
# Date: February 2025

# Load required packages
library(tidyverse)
library(sf)           # Spatial data handling
library(spdep)        # Spatial econometrics
library(spatialreg)   # Spatial regression models
library(ggplot2)
library(viridis)

set.seed(456)

# ============================================================================
# 1. Simulate Spatial Facility Data
# ============================================================================

n <- 120

# Create facilities with spatial coordinates (simulating a region)
facilities_spatial <- tibble(
  facility_id = 1:n,
  
  # Coordinates (simulating facilities across a geographic area)
  longitude = runif(n, min = -2, max = 2),
  latitude = runif(n, min = 51, max = 53),
  
  # Efficiency score (with spatial clustering)
  efficiency = rnorm(n, mean = 0.85, sd = 0.08),
  
  # Facility characteristics
  staff_ratio = rnorm(n, mean = 0.6, sd = 0.1),
  beds = round(rnorm(n, mean = 70, sd = 20)),
  urban = sample(c(0, 1), n, replace = TRUE, prob = c(0.4, 0.6)),
  ownership = sample(c("For-Profit", "Non-Profit"), n, replace = TRUE)
)

# Introduce spatial autocorrelation
# Facilities close to each other tend to have similar efficiency
coords <- as.matrix(facilities_spatial[, c("longitude", "latitude")])
dist_matrix <- as.matrix(dist(coords))

# Weight matrix based on inverse distance
W <- 1 / (dist_matrix + 0.1)  # +0.1 to avoid division by zero
diag(W) <- 0  # No self-influence

# Add spatial lag effect
spatial_effect <- 0.3 * (W %*% facilities_spatial$efficiency) / rowSums(W)
facilities_spatial$efficiency <- facilities_spatial$efficiency + as.vector(spatial_effect)

# Rescale efficiency to [0,1]
facilities_spatial$efficiency <- (facilities_spatial$efficiency - min(facilities_spatial$efficiency)) /
  (max(facilities_spatial$efficiency) - min(facilities_spatial$efficiency))

# ============================================================================
# 2. Create Spatial Objects
# ============================================================================

# Convert to sf object
facilities_sf <- st_as_sf(facilities_spatial, 
                         coords = c("longitude", "latitude"),
                         crs = 4326)

# Create spatial weights matrix using k-nearest neighbors
coords_sf <- st_coordinates(facilities_sf)
knn <- knearneigh(coords_sf, k = 5)
nb <- knn2nb(knn)
listw <- nb2listw(nb, style = "W")

# ============================================================================
# 3. Test for Spatial Autocorrelation
# ============================================================================

# Moran's I test
moran_test <- moran.test(facilities_spatial$efficiency, listw)

cat("\n")
cat(rep("=", 70), "\n")
cat("SPATIAL AUTOCORRELATION TEST\n")
cat(rep("=", 70), "\n")
print(moran_test)

# Local Moran's I (LISA)
local_moran <- localmoran(facilities_spatial$efficiency, listw)
facilities_spatial$local_moran_I <- local_moran[, 1]
facilities_spatial$local_moran_p <- local_moran[, 5]

# ============================================================================
# 4. Spatial Regression Models
# ============================================================================

# OLS model (baseline)
ols_model <- lm(efficiency ~ staff_ratio + beds + urban + ownership,
                data = facilities_spatial)

# Spatial Lag Model (SAR)
sar_model <- lagsarlm(efficiency ~ staff_ratio + beds + urban + ownership,
                      data = facilities_spatial, listw = listw)

# Spatial Error Model (SEM)
sem_model <- errorsarlm(efficiency ~ staff_ratio + beds + urban + ownership,
                        data = facilities_spatial, listw = listw)

# Model comparison
cat("\n")
cat(rep("=", 70), "\n")
cat("MODEL COMPARISON\n")
cat(rep("=", 70), "\n")

cat("\nOLS Model:\n")
print(summary(ols_model))

cat("\nSpatial Lag Model (SAR):\n")
print(summary(sar_model))

cat("\nSpatial Error Model (SEM):\n")
print(summary(sem_model))

# AIC comparison
aic_comparison <- data.frame(
  Model = c("OLS", "Spatial Lag", "Spatial Error"),
  AIC = c(AIC(ols_model), AIC(sar_model), AIC(sem_model))
) %>% arrange(AIC)

cat("\nAIC Comparison:\n")
print(aic_comparison)

# ============================================================================
# 5. Visualization
# ============================================================================

# Map of efficiency scores
p1 <- ggplot(facilities_spatial, aes(x = longitude, y = latitude, 
                                     color = efficiency, size = beds)) +
  geom_point(alpha = 0.7) +
  scale_color_viridis(option = "plasma") +
  labs(
    title = "Spatial Distribution of Facility Efficiency",
    x = "Longitude",
    y = "Latitude",
    color = "Efficiency\nScore",
    size = "Beds"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

print(p1)

# Moran's I scatterplot
moran_plot <- moran.plot(facilities_spatial$efficiency, listw,
                        labels = FALSE,
                        xlab = "Efficiency Score",
                        ylab = "Spatially Lagged Efficiency")

# ============================================================================
# 6. Export Results
# ============================================================================

# Save spatial data
write_csv(facilities_spatial, "spatial_efficiency_data.csv")

# Save model results
sink("spatial_regression_results.txt")
cat("SPATIAL REGRESSION ANALYSIS RESULTS\n")
cat(rep("=", 70), "\n\n")
cat("Moran's I Test:\n")
print(moran_test)
cat("\n\nSpatial Lag Model:\n")
print(summary(sar_model))
cat("\n\nSpatial Error Model:\n")
print(summary(sem_model))
sink()

cat("\nSpatial analysis complete. Results saved.\n")
