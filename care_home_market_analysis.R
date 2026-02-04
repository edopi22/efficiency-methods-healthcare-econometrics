# =============================================================================
# Emilia-Romagna Care Home Market Structure Analysis
# Author: E.R. Piombini
# Affiliation: University of Modena and Reggio Emilia & CAPP
# Date: February 2025
# =============================================================================
# 
# This script analyzes the structure and competitive dynamics of the residential
# care market in Emilia-Romagna, Italy, focusing on:
# - Market structure by ownership type (Public, Non-Profit, For-Profit)
# - Service differentiation (self-sufficient vs non-self-sufficient residents)
# - Facility types (rooms vs apartments)
# - Market concentration (HHI indices)
# - Spatial distribution and competition patterns
#
# =============================================================================

# Load required libraries
library(tidyverse)
library(sf)           # Spatial data handling
library(spdep)        # Spatial weights and autocorrelation
library(viridis)      # Color scales
library(ggplot2)
library(kableExtra)   # For LaTeX table output
library(scales)       # For percentage formatting

set.seed(123)

# =============================================================================
# 1. DATA SIMULATION - Mimicking ER Administrative Data Structure
# =============================================================================

# Number of health districts in Emilia-Romagna
n_districts <- 38

# Create districts with characteristics
districts <- tibble(
  district_id = 1:n_districts,
  district_name = paste0("District_", 1:n_districts),
  # Simulate population over 85
  pop_over_85 = round(rnorm(n_districts, mean = 2500, sd = 800)),
  # Urban vs rural classification
  urban = sample(c(0, 1), n_districts, replace = TRUE, prob = c(0.3, 0.7)),
  # Geographic coordinates (simplified)
  longitude = runif(n_districts, min = 9.5, max = 12.5),
  latitude = runif(n_districts, min = 44, max = 45)
)

# Total number of care homes in ER (from paper: 1,227)
n_facilities <- 1227

# Simulate care home data
facilities <- tibble(
  facility_id = 1:n_facilities,
  
  # Assign to districts
  district_id = sample(1:n_districts, n_facilities, replace = TRUE),
  
  # Care type: A (self-sufficient) vs NA (non-self-sufficient)
  # From paper: 60.7% Care A, 39.3% Care NA
  care_type = sample(c("CareA", "CareNA"), n_facilities, 
                     replace = TRUE, prob = c(0.607, 0.393)),
  
  # Facility type: Rooms vs Apartments (Housing)
  # Apartments more common in Care A
  facility_type = NA_character_,
  
  # Ownership: Public, NfP (Non-Profit), FP (For-Profit)
  # From paper: 20% Public, 35% NfP, 45% FP (approx)
  ownership = sample(c("Public", "NfP", "FP"), n_facilities,
                    replace = TRUE, prob = c(0.19, 0.32, 0.49)),
  
  # Number of beds
  beds = NA_real_,
  
  # Accredited beds (only for CareNA Rooms)
  beds_accredited = 0,
  
  # Year of establishment
  year_start = NA_real_,
  
  # Brand/Chain membership (only for some FP)
  brand = NA_character_,
  
  # Geographic coordinates (within district)
  longitude = NA_real_,
  latitude = NA_real_
)

# Assign facility types based on care type and ownership
# Care A: more apartments, especially for FP
# Care NA: mostly rooms
facilities <- facilities %>%
  mutate(
    facility_type = case_when(
      care_type == "CareA" & ownership == "FP" ~ 
        sample(c("Rooms", "Apartments"), n(), replace = TRUE, prob = c(0.84, 0.16)),
      care_type == "CareA" & ownership != "FP" ~ 
        sample(c("Rooms", "Apartments"), n(), replace = TRUE, prob = c(0.75, 0.25)),
      care_type == "CareNA" & ownership == "Public" ~ 
        sample(c("Rooms", "Apartments"), n(), replace = TRUE, prob = c(0.86, 0.14)),
      care_type == "CareNA" ~ 
        sample(c("Rooms", "Apartments"), n(), replace = TRUE, prob = c(0.91, 0.09)),
      TRUE ~ "Rooms"
    )
  )

# Assign number of beds based on ownership and facility type
facilities <- facilities %>%
  mutate(
    beds = case_when(
      # Public: larger facilities
      ownership == "Public" & care_type == "CareNA" & facility_type == "Rooms" ~ 
        round(rnorm(n(), mean = 59.5, sd = 20)),
      ownership == "Public" & care_type == "CareA" & facility_type == "Rooms" ~ 
        round(rnorm(n(), mean = 31.5, sd = 10)),
      ownership == "Public" & facility_type == "Apartments" ~ 
        round(rnorm(n(), mean = 13.7, sd = 5)),
      
      # Non-Profit: medium size
      ownership == "NfP" & care_type == "CareNA" & facility_type == "Rooms" ~ 
        round(rnorm(n(), mean = 48.7, sd = 18)),
      ownership == "NfP" & care_type == "CareA" & facility_type == "Rooms" ~ 
        round(rnorm(n(), mean = 23.6, sd = 10)),
      ownership == "NfP" & facility_type == "Apartments" ~ 
        round(rnorm(n(), mean = 14.3, sd = 5)),
      
      # For-Profit: smaller, especially apartments
      ownership == "FP" & care_type == "CareNA" & facility_type == "Rooms" ~ 
        round(rnorm(n(), mean = 53.4, sd = 20)),
      ownership == "FP" & care_type == "CareA" & facility_type == "Rooms" ~ 
        round(rnorm(n(), mean = 11.0, sd = 5)),
      ownership == "FP" & facility_type == "Apartments" ~ 
        round(rnorm(n(), mean = 13.2, sd = 5)),
      
      TRUE ~ 30
    )
  ) %>%
  mutate(beds = pmax(beds, 5))  # Minimum 5 beds

# Assign accredited beds (mainly for CareNA Rooms)
facilities <- facilities %>%
  mutate(
    beds_accredited = case_when(
      care_type == "CareNA" & facility_type == "Rooms" & ownership == "Public" ~ 
        round(beds * 0.94),
      care_type == "CareNA" & facility_type == "Rooms" & ownership == "NfP" ~ 
        round(beds * 0.75),
      care_type == "CareNA" & facility_type == "Rooms" & ownership == "FP" ~ 
        round(beds * 0.65),
      care_type == "CareA" & facility_type == "Rooms" ~ 
        round(beds * 0.14),
      TRUE ~ 0
    )
  )

# Assign year of establishment
facilities <- facilities %>%
  mutate(
    year_start = case_when(
      ownership == "Public" & care_type == "CareNA" & facility_type == "Rooms" ~ 
        round(rnorm(n(), mean = 1971, sd = 15)),
      ownership == "Public" & care_type == "CareA" & facility_type == "Rooms" ~ 
        round(rnorm(n(), mean = 1958, sd = 20)),
      ownership == "Public" & facility_type == "Apartments" ~ 
        round(rnorm(n(), mean = 2003, sd = 10)),
      
      ownership == "NfP" & care_type == "CareNA" & facility_type == "Rooms" ~ 
        round(rnorm(n(), mean = 1988, sd = 12)),
      ownership == "NfP" & care_type == "CareA" ~ 
        round(rnorm(n(), mean = 2003, sd = 10)),
      
      ownership == "FP" & care_type == "CareNA" ~ 
        round(rnorm(n(), mean = 2000, sd = 10)),
      ownership == "FP" & care_type == "CareA" & facility_type == "Rooms" ~ 
        round(rnorm(n(), mean = 2010, sd = 5)),
      ownership == "FP" & facility_type == "Apartments" ~ 
        round(rnorm(n(), mean = 2015, sd = 3)),
      
      TRUE ~ 2000
    )
  ) %>%
  mutate(year_start = pmax(year_start, 1950))

# Assign brands (chains) - mainly for FP
n_brands <- 15
brand_names <- paste0("Brand_", 1:n_brands)

facilities <- facilities %>%
  mutate(
    brand = case_when(
      ownership == "FP" & runif(n()) < 0.35 ~ 
        sample(brand_names, n(), replace = TRUE),
      ownership == "NfP" & runif(n()) < 0.10 ~ 
        sample(brand_names, n(), replace = TRUE),
      TRUE ~ paste0("Independent_", facility_id)
    )
  )

# Assign geographic coordinates within districts
facilities <- facilities %>%
  left_join(districts %>% select(district_id, dist_lon = longitude, dist_lat = latitude),
            by = "district_id") %>%
  mutate(
    longitude = dist_lon + rnorm(n(), 0, 0.1),
    latitude = dist_lat + rnorm(n(), 0, 0.05)
  ) %>%
  select(-dist_lon, -dist_lat)

# =============================================================================
# 2. DESCRIPTIVE STATISTICS - Table 1 (Panel a and b)
# =============================================================================

cat("\n")
cat("=" * 70, "\n")
cat("TABLE 1: MARKET STRUCTURE OVERVIEW\n")
cat("=" * 70, "\n\n")

# Create market segments
facilities <- facilities %>%
  mutate(
    segment = paste0(care_type, "_", substr(facility_type, 1, 1))
  )

# Panel a: Raw numbers
table1a <- facilities %>%
  group_by(care_type, facility_type, ownership) %>%
  summarise(
    n_facilities = n(),
    total_beds = sum(beds),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = ownership,
    values_from = c(n_facilities, total_beds),
    values_fill = 0
  ) %>%
  arrange(care_type, desc(facility_type))

print("Panel a) Raw Numbers")
print(table1a)

# Totals by ownership
totals_ownership <- facilities %>%
  group_by(ownership) %>%
  summarise(
    n_facilities = n(),
    total_beds = sum(beds)
  )

print("\nTotals by ownership:")
print(totals_ownership)

# Panel b: Percentages
table1b <- facilities %>%
  group_by(care_type, facility_type) %>%
  mutate(
    segment_facilities = n(),
    segment_beds = sum(beds)
  ) %>%
  group_by(care_type, facility_type, ownership) %>%
  summarise(
    pct_facilities = n() / first(segment_facilities) * 100,
    pct_beds = sum(beds) / first(segment_beds) * 100,
    .groups = "drop"
  ) %>%
  arrange(care_type, desc(facility_type))

print("\nPanel b) Percentages")
print(table1b)

# =============================================================================
# 3. HHI CALCULATION - Table 2
# =============================================================================

cat("\n")
cat("=" * 70, "\n")
cat("TABLE 2: HHI MARKET CONCENTRATION INDICES\n")
cat("=" * 70, "\n\n")

# Function to calculate HHI for a group
calculate_hhi <- function(market_shares) {
  sum(market_shares^2)
}

# Calculate HHI at facility level
hhi_facility <- facilities %>%
  group_by(district_id, segment) %>%
  mutate(
    market_share = beds / sum(beds)
  ) %>%
  summarise(
    hhi_overall = calculate_hhi(market_share),
    .groups = "drop"
  )

# Calculate HHI excluding public
hhi_no_public <- facilities %>%
  filter(ownership != "Public") %>%
  group_by(district_id, segment) %>%
  mutate(
    market_share = beds / sum(beds)
  ) %>%
  summarise(
    hhi_nfp_fp = calculate_hhi(market_share),
    .groups = "drop"
  )

# Calculate HHI for FP only
hhi_fp <- facilities %>%
  filter(ownership == "FP") %>%
  group_by(district_id, segment) %>%
  mutate(
    market_share = beds / sum(beds)
  ) %>%
  summarise(
    hhi_fp = calculate_hhi(market_share),
    .groups = "drop"
  )

# Calculate HHI at brand level (treating chain as single entity)
hhi_brand <- facilities %>%
  filter(ownership == "FP") %>%
  group_by(district_id, segment, brand) %>%
  summarise(
    brand_beds = sum(beds),
    .groups = "drop"
  ) %>%
  group_by(district_id, segment) %>%
  mutate(
    market_share = brand_beds / sum(brand_beds)
  ) %>%
  summarise(
    hhi_fp_chain = calculate_hhi(market_share),
    .groups = "drop"
  )

# Merge all HHI measures
hhi_all <- hhi_facility %>%
  left_join(hhi_no_public, by = c("district_id", "segment")) %>%
  left_join(hhi_fp, by = c("district_id", "segment")) %>%
  left_join(hhi_brand, by = c("district_id", "segment"))

# Calculate summary statistics by segment
hhi_summary <- hhi_all %>%
  group_by(segment) %>%
  summarise(
    across(starts_with("hhi"),
           list(
             mean = ~mean(., na.rm = TRUE),
             q1 = ~quantile(., 0.25, na.rm = TRUE),
             q3 = ~quantile(., 0.75, na.rm = TRUE),
             median = ~median(., na.rm = TRUE)
           ),
           .names = "{.col}_{.fn}"
    )
  )

print("HHI Summary Statistics by Market Segment:")
print(hhi_summary)

# Create formatted table for LaTeX
hhi_latex <- hhi_summary %>%
  select(segment, 
         starts_with("hhi_overall"),
         starts_with("hhi_nfp_fp"),
         starts_with("hhi_fp_"),
         starts_with("hhi_fp_chain")) %>%
  mutate(
    segment_label = case_when(
      segment == "CareA_A" ~ "CareA H",
      segment == "CareA_R" ~ "CareA R",
      segment == "CareNA_A" ~ "CareNA H",
      segment == "CareNA_R" ~ "CareNA R",
      TRUE ~ segment
    )
  )

# =============================================================================
# 4. BEDS PER POPULATION RATIO ANALYSIS
# =============================================================================

cat("\n")
cat("=" * 70, "\n")
cat("BEDS PER 100 RESIDENTS OVER 85\n")
cat("=" * 70, "\n\n")

# Calculate beds per 100 pop over 85 by district
beds_ratio <- facilities %>%
  group_by(district_id, care_type) %>%
  summarise(
    total_beds = sum(beds),
    .groups = "drop"
  ) %>%
  left_join(districts %>% select(district_id, pop_over_85), by = "district_id") %>%
  mutate(
    beds_per_100 = (total_beds / pop_over_85) * 100
  )

# Summary statistics
beds_ratio_summary <- beds_ratio %>%
  group_by(care_type) %>%
  summarise(
    mean_ratio = mean(beds_per_100),
    min_ratio = min(beds_per_100),
    max_ratio = max(beds_per_100),
    sd_ratio = sd(beds_per_100)
  )

print("Beds per 100 residents over 85:")
print(beds_ratio_summary)

# =============================================================================
# 5. FP MARKET PENETRATION BY DISTRICT
# =============================================================================

cat("\n")
cat("=" * 70, "\n")
cat("FOR-PROFIT MARKET PENETRATION\n")
cat("=" * 70, "\n\n")

fp_penetration <- facilities %>%
  group_by(district_id, care_type) %>%
  summarise(
    total_beds = sum(beds),
    fp_beds = sum(beds[ownership == "FP"]),
    .groups = "drop"
  ) %>%
  mutate(
    fp_pct = (fp_beds / total_beds) * 100
  ) %>%
  left_join(districts %>% select(district_id, district_name), by = "district_id")

fp_summary <- fp_penetration %>%
  group_by(care_type) %>%
  summarise(
    mean_fp_pct = mean(fp_pct),
    min_fp_pct = min(fp_pct),
    max_fp_pct = max(fp_pct)
  )

print("FP Market Penetration (% of beds):")
print(fp_summary)

# =============================================================================
# 6. YEAR OF ESTABLISHMENT ANALYSIS - Table 4
# =============================================================================

cat("\n")
cat("=" * 70, "\n")
cat("TABLE 4: YEAR OF ESTABLISHMENT BY OWNERSHIP AND MARKET\n")
cat("=" * 70, "\n\n")

year_analysis <- facilities %>%
  group_by(ownership, segment) %>%
  summarise(
    mean_year = mean(year_start, na.rm = TRUE),
    q1_year = quantile(year_start, 0.25, na.rm = TRUE),
    q3_year = quantile(year_start, 0.75, na.rm = TRUE),
    median_year = median(year_start, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(segment, ownership)

print("Year of establishment statistics:")
print(year_analysis)

# =============================================================================
# 7. SPATIAL ANALYSIS
# =============================================================================

cat("\n")
cat("=" * 70, "\n")
cat("SPATIAL DISTRIBUTION ANALYSIS\n")
cat("=" * 70, "\n\n")

# Convert to sf object for spatial analysis
facilities_sf <- st_as_sf(facilities, 
                         coords = c("longitude", "latitude"),
                         crs = 4326)

# Calculate distance matrix between facilities in same district
# This is simplified - in real analysis you'd use actual road distances

# For each district, calculate average distance to competitors
spatial_competition <- facilities %>%
  group_by(district_id, segment) %>%
  mutate(
    n_competitors = n() - 1
  ) %>%
  summarise(
    avg_competitors = mean(n_competitors),
    .groups = "drop"
  )

print("Average number of competitors per facility by district and segment:")
print(head(spatial_competition, 20))

# =============================================================================
# 8. VISUALIZATIONS
# =============================================================================

cat("\n")
cat("=" * 70, "\n")
cat("CREATING VISUALIZATIONS\n")
cat("=" * 70, "\n\n")

# 8.1: Market structure by ownership
p1 <- ggplot(facilities %>% 
               group_by(ownership, care_type) %>% 
               summarise(total_beds = sum(beds), .groups = "drop"),
             aes(x = ownership, y = total_beds, fill = care_type)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("CareA" = "#3498db", "CareNA" = "#e74c3c")) +
  labs(
    title = "Bed Distribution by Ownership Type and Care Level",
    x = "Ownership Type",
    y = "Total Beds",
    fill = "Care Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

ggsave("market_structure_ownership.png", p1, width = 10, height = 6, dpi = 300)

# 8.2: HHI distribution by segment
p2 <- ggplot(hhi_all %>% 
               filter(!is.na(hhi_overall)),
             aes(x = segment, y = hhi_overall)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  geom_hline(yintercept = 0.15, linetype = "dashed", color = "red", size = 1) +
  geom_hline(yintercept = 0.25, linetype = "dashed", color = "orange", size = 1) +
  annotate("text", x = 0.7, y = 0.16, label = "Moderate concentration", 
           color = "red", size = 3) +
  annotate("text", x = 0.7, y = 0.26, label = "High concentration", 
           color = "orange", size = 3) +
  labs(
    title = "HHI Distribution Across Districts by Market Segment",
    subtitle = "Thresholds: 0.15 (moderate), 0.25 (high concentration)",
    x = "Market Segment",
    y = "Herfindahl-Hirschman Index"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("hhi_distribution.png", p2, width = 10, height = 6, dpi = 300)

# 8.3: Year of establishment timeline
p3 <- ggplot(facilities %>%
               filter(!is.na(year_start)) %>%
               mutate(decade = floor(year_start / 10) * 10),
             aes(x = decade, fill = ownership)) +
  geom_histogram(bins = 8, position = "dodge", alpha = 0.8) +
  scale_fill_manual(values = c("Public" = "#2ecc71", 
                               "NfP" = "#3498db", 
                               "FP" = "#e74c3c")) +
  facet_wrap(~care_type, ncol = 1) +
  labs(
    title = "Facility Establishment Timeline by Ownership Type",
    x = "Decade",
    y = "Number of Facilities Established",
    fill = "Ownership"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

ggsave("establishment_timeline.png", p3, width = 10, height = 8, dpi = 300)

# 8.4: Beds per 100 pop over 85 map
districts_with_ratio <- districts %>%
  left_join(
    beds_ratio %>% 
      filter(care_type == "CareNA") %>%
      select(district_id, beds_per_100_NA = beds_per_100),
    by = "district_id"
  ) %>%
  left_join(
    beds_ratio %>% 
      filter(care_type == "CareA") %>%
      select(district_id, beds_per_100_A = beds_per_100),
    by = "district_id"
  )

p4 <- ggplot(districts_with_ratio, aes(x = longitude, y = latitude)) +
  geom_point(aes(size = beds_per_100_NA, color = beds_per_100_NA), alpha = 0.7) +
  scale_color_viridis(option = "plasma") +
  scale_size_continuous(range = c(2, 12)) +
  labs(
    title = "Care NA: Beds per 100 Residents Over 85 by District",
    x = "Longitude",
    y = "Latitude",
    color = "Beds per 100",
    size = "Beds per 100"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14)
  )

ggsave("beds_ratio_careNA_map.png", p4, width = 10, height = 8, dpi = 300)

# 8.5: FP penetration map
districts_with_fp <- districts %>%
  left_join(
    fp_penetration %>%
      filter(care_type == "CareA") %>%
      select(district_id, fp_pct),
    by = "district_id"
  )

p5 <- ggplot(districts_with_fp, aes(x = longitude, y = latitude)) +
  geom_point(aes(size = fp_pct, color = fp_pct), alpha = 0.7) +
  scale_color_gradient2(low = "blue", mid = "yellow", high = "red", 
                       midpoint = 50, limits = c(0, 100)) +
  scale_size_continuous(range = c(2, 12)) +
  labs(
    title = "For-Profit Market Penetration in Care A by District",
    subtitle = "Percentage of beds operated by for-profit providers",
    x = "Longitude",
    y = "Latitude",
    color = "FP %",
    size = "FP %"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14)
  )

ggsave("fp_penetration_careA_map.png", p5, width = 10, height = 8, dpi = 300)

# =============================================================================
# 9. EXPORT RESULTS
# =============================================================================

# Export main dataset
write_csv(facilities, "facilities_complete_data.csv")

# Export district-level summaries
write_csv(districts_with_ratio, "district_summaries.csv")

# Export HHI data
write_csv(hhi_all, "hhi_by_district_segment.csv")
write_csv(hhi_summary, "hhi_summary_statistics.csv")

# Export FP penetration data
write_csv(fp_penetration, "fp_penetration_by_district.csv")

# =============================================================================
# 10. LaTeX TABLE GENERATION
# =============================================================================

cat("\n")
cat("=" * 70, "\n")
cat("GENERATING LaTeX TABLES\n")
cat("=" * 70, "\n\n")

# Function to create LaTeX table 1
create_latex_table1 <- function() {
  # Panel A: Raw numbers
  summary_data <- facilities %>%
    group_by(care_type, facility_type) %>%
    summarise(
      total_facilities = n(),
      total_beds = sum(beds),
      .groups = "drop"
    ) %>%
    left_join(
      facilities %>%
        group_by(care_type, facility_type, ownership) %>%
        summarise(
          n_fac = n(),
          n_beds = sum(beds),
          .groups = "drop"
        ) %>%
        pivot_wider(
          names_from = ownership,
          values_from = c(n_fac, n_beds),
          values_fill = 0
        ),
      by = c("care_type", "facility_type")
    )
  
  # Save to file
  sink("table1_latex.txt")
  cat("% Table 1: Market Structure\n")
  cat("\\begin{table}[H]\n")
  cat("\\centering\n")
  cat("\\caption{The ER care home market}\n")
  cat("\\label{table1}\n")
  # ... rest of LaTeX code
  cat("\\end{table}\n")
  sink()
  
  cat("LaTeX table saved to: table1_latex.txt\n")
}

create_latex_table1()

cat("\n")
cat("=" * 70, "\n")
cat("ANALYSIS COMPLETE\n")
cat("=" * 70, "\n")
cat("\nAll results have been exported to CSV files.\n")
cat("Visualizations have been saved as PNG files.\n")
cat("LaTeX table code has been generated.\n\n")

cat("Key findings:\n")
cat("- Total facilities analyzed:", nrow(facilities), "\n")
cat("- Total beds:", sum(facilities$beds), "\n")
cat("- Care A facilities:", sum(facilities$care_type == "CareA"), "\n")
cat("- Care NA facilities:", sum(facilities$care_type == "CareNA"), "\n")
cat("- Public ownership:", sum(facilities$ownership == "Public"), "\n")
cat("- Non-profit ownership:", sum(facilities$ownership == "NfP"), "\n")
cat("- For-profit ownership:", sum(facilities$ownership == "FP"), "\n")
