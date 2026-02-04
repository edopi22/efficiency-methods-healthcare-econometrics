"""
Technology Adoption Analysis in Healthcare Facilities
Demonstrates index construction and regression analysis
Author: E.R. Piombini
Date: February 2025
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from scipy import stats
import statsmodels.api as sm
from statsmodels.formula.api import ols

# Set random seed for reproducibility
np.random.seed(42)

# Set plotting style
sns.set_style("whitegrid")
plt.rcParams['figure.figsize'] = (12, 8)

# ============================================================================
# 1. Simulate Facility Data
# ============================================================================

n_facilities = 180

# Create base facility characteristics
facilities = pd.DataFrame({
    'facility_id': range(1, n_facilities + 1),
    'ownership': np.random.choice(['For-Profit', 'Non-Profit', 'Public'], 
                                   n_facilities, p=[0.45, 0.40, 0.15]),
    'size': np.random.choice(['Small', 'Medium', 'Large'], 
                            n_facilities, p=[0.3, 0.5, 0.2]),
    'urban': np.random.binomial(1, 0.65, n_facilities),
    'years_operating': np.random.randint(5, 50, n_facilities),
    'beds': np.random.normal(75, 25, n_facilities).clip(20, 200)
})

# Technology categories (binary adoption: 0 or 1)
tech_categories = [
    'electronic_records',
    'telehealth',
    'remote_monitoring',
    'medication_management',
    'fall_detection',
    'staff_communication',
    'resident_engagement',
    'sensor_technology',
    'ai_scheduling'
]

# Simulate technology adoption with realistic correlations
# Non-profit facilities tend to adopt more technologies
adoption_probability = 0.4 + (facilities['ownership'] == 'Non-Profit') * 0.15 + \
                       (facilities['size'] == 'Large') * 0.1

for tech in tech_categories:
    facilities[tech] = np.random.binomial(1, adoption_probability)

# ============================================================================
# 2. Construct Technology Adoption Index
# ============================================================================

# Simple additive index (count of technologies adopted)
facilities['tech_index_simple'] = facilities[tech_categories].sum(axis=1)

# Weighted index (some technologies weighted more heavily)
weights = {
    'electronic_records': 1.2,
    'telehealth': 1.1,
    'remote_monitoring': 1.3,
    'medication_management': 1.0,
    'fall_detection': 1.1,
    'staff_communication': 0.9,
    'resident_engagement': 0.8,
    'sensor_technology': 1.2,
    'ai_scheduling': 1.4
}

facilities['tech_index_weighted'] = sum(
    facilities[tech] * weights[tech] for tech in tech_categories
)

# Normalize to 0-100 scale
max_weighted = sum(weights.values())
facilities['tech_index_normalized'] = (facilities['tech_index_weighted'] / max_weighted) * 100

# ============================================================================
# 3. Descriptive Statistics
# ============================================================================

print("=" * 70)
print("TECHNOLOGY ADOPTION DESCRIPTIVE STATISTICS")
print("=" * 70)

print("\nOverall Technology Adoption:")
print(f"Mean technologies adopted: {facilities['tech_index_simple'].mean():.2f}")
print(f"Median technologies adopted: {facilities['tech_index_simple'].median():.2f}")
print(f"Mean normalized index: {facilities['tech_index_normalized'].mean():.2f}")

print("\nAdoption by Ownership Type:")
adoption_by_ownership = facilities.groupby('ownership').agg({
    'tech_index_simple': ['mean', 'std'],
    'tech_index_normalized': ['mean', 'std'],
    'facility_id': 'count'
})
adoption_by_ownership.columns = ['Simple_Mean', 'Simple_SD', 
                                  'Normalized_Mean', 'Normalized_SD', 'N']
print(adoption_by_ownership.round(2))

print("\nIndividual Technology Adoption Rates:")
adoption_rates = facilities[tech_categories].mean() * 100
print(adoption_rates.sort_values(ascending=False).round(1))

# ============================================================================
# 4. Statistical Analysis
# ============================================================================

# ANOVA: Test differences across ownership types
print("\n" + "=" * 70)
print("STATISTICAL TESTS")
print("=" * 70)

model = ols('tech_index_normalized ~ C(ownership)', data=facilities).fit()
anova_table = sm.stats.anova_lm(model, typ=2)
print("\nANOVA Results:")
print(anova_table)

# Regression analysis: Determinants of technology adoption
facilities['size_large'] = (facilities['size'] == 'Large').astype(int)
facilities['ownership_nonprofit'] = (facilities['ownership'] == 'Non-Profit').astype(int)

X = facilities[['urban', 'years_operating', 'beds', 'size_large', 'ownership_nonprofit']]
X = sm.add_constant(X)
y = facilities['tech_index_normalized']

regression_model = sm.OLS(y, X).fit()
print("\n" + "=" * 70)
print("REGRESSION RESULTS: Determinants of Technology Adoption")
print("=" * 70)
print(regression_model.summary())

# ============================================================================
# 5. Visualizations
# ============================================================================

fig, axes = plt.subplots(2, 2, figsize=(15, 12))

# 1. Distribution of adoption index by ownership
sns.boxplot(data=facilities, x='ownership', y='tech_index_normalized', 
            ax=axes[0, 0], palette='Set2')
axes[0, 0].set_title('Technology Adoption Index by Ownership Type', 
                     fontsize=14, fontweight='bold')
axes[0, 0].set_xlabel('Ownership Type')
axes[0, 0].set_ylabel('Normalized Technology Index (0-100)')

# 2. Individual technology adoption rates
adoption_rates_df = pd.DataFrame({
    'Technology': tech_categories,
    'Adoption_Rate': facilities[tech_categories].mean() * 100
}).sort_values('Adoption_Rate', ascending=True)

axes[0, 1].barh(adoption_rates_df['Technology'], adoption_rates_df['Adoption_Rate'],
                color='steelblue')
axes[0, 1].set_xlabel('Adoption Rate (%)')
axes[0, 1].set_title('Technology Adoption Rates', fontsize=14, fontweight='bold')
axes[0, 1].grid(axis='x', alpha=0.3)

# 3. Scatter: Facility size vs adoption
sns.scatterplot(data=facilities, x='beds', y='tech_index_normalized', 
                hue='ownership', ax=axes[1, 0], alpha=0.6, s=100)
axes[1, 0].set_title('Facility Size vs Technology Adoption', 
                     fontsize=14, fontweight='bold')
axes[1, 0].set_xlabel('Number of Beds')
axes[1, 0].set_ylabel('Normalized Technology Index')

# 4. Histogram of adoption index
axes[1, 1].hist(facilities['tech_index_normalized'], bins=20, 
                color='teal', alpha=0.7, edgecolor='black')
axes[1, 1].set_title('Distribution of Technology Adoption Index', 
                     fontsize=14, fontweight='bold')
axes[1, 1].set_xlabel('Normalized Technology Index (0-100)')
axes[1, 1].set_ylabel('Frequency')
axes[1, 1].axvline(facilities['tech_index_normalized'].mean(), 
                   color='red', linestyle='--', linewidth=2, label='Mean')
axes[1, 1].legend()

plt.tight_layout()
plt.savefig('technology_adoption_analysis.png', dpi=300, bbox_inches='tight')
print("\nVisualization saved as 'technology_adoption_analysis.png'")

# ============================================================================
# 6. Export Results
# ============================================================================

# Save facility-level data
facilities.to_csv('facility_technology_data.csv', index=False)

# Save summary statistics
summary_stats = facilities.groupby('ownership')[['tech_index_simple', 
                                                  'tech_index_normalized']].describe()
summary_stats.to_csv('technology_adoption_summary.csv')

print("\nAnalysis complete. Results exported to CSV files.")
