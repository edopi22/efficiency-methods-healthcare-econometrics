# Healthcare Efficiency Econometrics

Econometric methods for healthcare efficiency analysis, with focus on residential care markets, spatial competition, and quality measurement.

## About

I am a PhD candidate inHealth and social care economics and policy at the University of Modena and Reggio Emilia - Fondazione Marco Biagi, researching efficiency and market dynamics in residential care facilities. My research combines advanced econometric methods with policy-relevant analysis.
I am a Research member affiliate at CAPP - Centre for Public Policy Analysis (Unimore), CPEC - Care Policy and Evaluation Centre (London S. of Economics), and G. Brodolini Foundation.

**Research interests:**
- Healthcare efficiency measurement and cost analysis
- Market structure and competition in care markets
- Spatial econometrics and geographic access
- Technology adoption in healthcare settings

## Repository Structure

### General Methods (`/`)
Core econometric methods applicable across healthcare settings:

- **`stochastic_frontier_analysis.R`** - SFA implementation for cost efficiency measurement with ownership comparisons
- **`technology_adoption_analysis.py`** - Technology adoption index construction and regression analysis
- **`spatial_efficiency_analysis.R`** - Spatial autocorrelation testing and spatial econometric models

### Care Home Market Analysis (`/`)
Specialized analysis for residential care market research:

- **`care_home_market_analysis.R`** - Comprehensive market structure analysis (HHI, ownership patterns, geographic distribution)
- **`vickrey_salop_model_analysis.R`** - Theoretical spatial competition model with policy simulations
- **`README_analysis_scripts.md`** - Detailed documentation for care home analysis

## Key Features

**Stochastic Frontier Analysis**
- Half-normal and truncated-normal inefficiency distributions
- Multi-ownership comparisons (Public, Non-Profit, For-Profit)
- Technical efficiency decomposition

**Spatial Methods**
- Moran's I autocorrelation tests
- Spatial lag (SAR) and spatial error (SEM) models
- Distance-based competition weights

**Market Structure Analysis**
- Herfindahl-Hirschman Index (HHI) calculations
- Facility-level and chain-adjusted concentration measures
- Geographic market definition and sub-market analysis

**Theoretical Modeling**
- Vickrey-Salop spatial competition framework
- Quality-competition dynamics
- Policy counterfactual simulations

## Technical Stack

- **R**: frontier, spatialreg, spdep, sf, tidyverse, ggplot2
- **Python**: pandas, numpy, statsmodels, matplotlib, seaborn, scipy
- **Stata**: panel data methods, advanced econometrics

## Data

All code samples use **simulated data** to preserve confidentiality of administrative records. Data structures mirror real healthcare datasets including:
- Facility characteristics and ownership
- Geographic coordinates and market boundaries
- Quality indicators and cost measures
- Patient volumes and service types

## Applications

These methods have been applied to study:
- Cost efficiency in Italian residential care facilities
- Technology adoption patterns in care homes
- Market concentration and competition dynamics
- Spatial distribution of healthcare services
- Public-private ownership comparisons

## Contact

**Edoardo Renato Piombini**  
PhD Candidate, University of Modena and Reggio Emilia 
CPEC - Care Policy and Evaluation Centre 
CAPP – Centro di Analisi delle Politiche Pubbliche

---

*All code samples use simulated data. Methods and analytical frameworks are fully transferable to real healthcare datasets.*
