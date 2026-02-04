# Emilia-Romagna Care Home Market Analysis - R Scripts

## Overview

This repository contains R scripts for empirical and theoretical analysis of the care home market structure in Emilia-Romagna, Italy. The analysis accompanies Chapter 1 of the dissertation on "Market competition dynamics in Emilia-Romagna's care home sector."

**Author**: E.R. Piombini  
**Affiliation**: University of Modena and Reggio Emilia - CAPP - CPEC (LSE)  

## Files

### 1. `care_home_market_analysis.R`
**Main empirical analysis script**

Performs comprehensive analysis of care home market structure using administrative data:
- Market structure by ownership type (Public, Non-Profit, For-Profit)
- Service differentiation (self-sufficient vs non-self-sufficient)
- Facility types (rooms vs apartments)
- HHI market concentration indices
- Spatial distribution patterns
- FP market penetration analysis

**Key Outputs:**
- Descriptive statistics (Table 1)
- HHI concentration measures (Table 2)
- Year of establishment analysis (Table 4)
- Geographic visualizations
- LaTeX table code

### 2. `vickrey_salop_model_analysis.R`
**Theoretical model simulation script**

Implements the Vickrey-Salop spatial competition model (Section 4):
- Regulated pricing segment analysis
- Private-pay segment with endogenous pricing
- Combined segment analysis
- Distance-based competition weights (Model II)
- Parameter sensitivity analysis
- Policy simulation scenarios

**Key Outputs:**
- Critical discount factor calculations
- Equilibrium profit analysis
- U-shaped relationship visualizations
- Policy impact simulations
- Geographic competition intensity measures

## Requirements

### R Packages
```r
# Data manipulation
install.packages("tidyverse")
install.packages("dplyr")
install.packages("tidyr")

# Spatial analysis
install.packages("sf")
install.packages("spdep")

# Visualization
install.packages("ggplot2")
install.packages("viridis")
install.packages("scales")
install.packages("patchwork")

# Table output
install.packages("kableExtra")
```

## Usage

### Running the Empirical Analysis

```r
# Set working directory
setwd("path/to/your/project")

# Run the main analysis
source("care_home_market_analysis.R")
```

**Output files created:**
- `facilities_complete_data.csv` - Full facility dataset
- `district_summaries.csv` - District-level aggregates
- `hhi_by_district_segment.csv` - HHI values by district and segment
- `hhi_summary_statistics.csv` - Summary statistics of HHI
- `fp_penetration_by_district.csv` - FP market share by district
- `table1_latex.txt` - LaTeX code for Table 1
- Multiple PNG visualizations

### Running the Theoretical Model

```r
# Run the Vickrey-Salop model simulations
source("vickrey_salop_model_analysis.R")
```

**Output files created:**
- `model_regulated_segment_results.csv`
- `model_privatepay_segment_results.csv`
- `model_combined_segment_results.csv`
- `equilibrium_regulated.csv`
- `equilibrium_privatepay.csv`
- `sensitivity_analysis_*.csv`
- `policy_simulation_*.csv`
- `geographic_competition_intensity.csv`
- Multiple PNG visualizations

## Data Structure

### For Empirical Analysis

If using real administrative data, your dataset should contain:

```
facility_id         : Unique facility identifier
district_id         : Health district (1-38)
care_type          : "CareA" (self-sufficient) or "CareNA" (non-self-sufficient)
facility_type      : "Rooms" or "Apartments"
ownership          : "Public", "NfP", or "FP"
beds               : Number of beds
beds_accredited    : Number of accredited beds
year_start         : Year facility began operations
brand              : Brand/chain name (if applicable)
longitude          : Geographic coordinate
latitude           : Geographic coordinate
```

### Data Sources

The scripts currently use **simulated data** that mimics the structure of:
- Emilia-Romagna's Flusso Assistenza Residenziale (FAR) database
- Regional administrative records
- Municipal accreditation data

**To use real data:**
1. Replace the simulation section (Section 1) with your data import
2. Ensure column names match the expected structure
3. Verify data types and coding schemes

## Key Findings Replicated

### Market Structure (Table 1)
- Total facilities: ~1,227 (60.7% Care A, 39.3% Care NA)
- Ownership: 20% Public, 35% NfP, 45% FP
- Public facilities: larger, focus on Care NA
- FP facilities: smaller, focus on Care A (especially apartments)

### Market Concentration (Table 2)
- Care A apartments: moderate concentration (HHI ~0.49)
- Care NA rooms: low concentration overall (HHI ~0.12)
- Concentration increases substantially when excluding public providers
- Chain structures further increase HHI values

### Theoretical Model
- **Regulated segment**: Monotonically decreasing δ(n) - more competition facilitates quality
- **Private-pay segment**: U-shaped relationship - moderate competition problematic
- **Distance matters**: Geographic proximity affects both price competition and reputation effects

## Customization

### Adjust Model Parameters

In `vickrey_salop_model_analysis.R`:

```r
# Quality levels
q_high <- 1.0      # High quality level
q_low <- 0.5       # Low quality level

# Cost parameters
c <- 0.15          # Marginal cost of high quality
C <- 50            # Fixed switching cost

# Market parameters
sigma <- 0.5       # Horizontal differentiation
p_regulated <- 0.7 # Regulated price
```

### Modify HHI Calculations

In `care_home_market_analysis.R`:

```r
# Change HHI grouping levels
# Current: district_id + segment
# Alternative: province_id + ownership + care_type

hhi_custom <- facilities %>%
  group_by(your_grouping_vars) %>%
  mutate(market_share = beds / sum(beds)) %>%
  summarise(hhi = sum(market_share^2))
```

## Visualization Examples

All scripts produce publication-ready visualizations:

1. **Market structure bar charts** - ownership and care type distribution
2. **HHI boxplots** - concentration across districts
3. **Geographic maps** - beds per capita, FP penetration
4. **Timeline charts** - facility establishment patterns
5. **Theoretical curves** - δ(n) relationships
6. **Policy simulations** - counterfactual scenarios

## References

Key papers informing the methodology:

- Bardey & Siciliani (2021) - Two-sided market model
- Espuny Pujol et al. (2021) - Chain-adjusted HHI in care homes
- Forder & Allan (2014) - Competition and quality in English nursing homes
- Hackmann (2019) - Structural estimation in nursing home markets
- Hirth et al. (2019) - Chain ownership and market concentration
- Lu et al. (2020) - Non-linear competition-quality relationship
- Zhao (2016) - Five-star rating and competition effects

## Troubleshooting

### Common Issues

**Error: Package not found**
```r
# Install missing packages
install.packages("package_name")
```

**Error: File not found**
```r
# Check working directory
getwd()
setwd("path/to/scripts")
```

**Warning: Missing values in HHI calculation**
```r
# This is expected in districts with <2 facilities in a segment
# HHI is undefined for monopolies (or set to 1)
```

**Plots not displaying**
```r
# Ensure you have a graphics device
# If running in terminal, plots save to PNG files automatically
```


## Contact

**Edoardo Renato Piombini**  
PhD Candidate, Researcher
University of Modena and Reggio Emilia - Fondazione Marco Biagi  
CAPP – Centro di Analisi delle Politiche Pubbliche  
CPEC - Cae Policy and Evaluation Centre (LSE)

For questions or collaboration: edoardorenato.piombini@unimore.it

## License

These scripts are provided for academic and research purposes.

---

**Last updated**: February 2025
