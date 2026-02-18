# PCA Saturation Curve Analysis Guide

## Overview

The PCA Saturation Curve feature allows you to assess whether your morphospace is saturated by analyzing how variance metrics change with increasing sample size. This helps determine if adding new specimens will likely expand your morphospace or if you have already captured the full range of morphological variation.

## What is Morphospace Saturation?

A saturated morphospace means that the current sample captures the full range of morphological variation. In a saturation curve:

- **Saturated morphospace**: The curve plateaus, indicating that additional specimens are unlikely to expand the morphospace significantly
- **Unsaturated morphospace**: The curve continues to increase, suggesting that more specimens could expand the morphospace

## How it Works

The analysis uses **bootstrap resampling** to simulate different sample sizes:

1. For each target sample size (e.g., 10%, 20%, ..., 100% of your specimens):
   - Randomly resample specimens with replacement (bootstrap)
   - Compute variance metrics for the subsample
   - Repeat multiple times (default: 200 iterations)
   
2. Calculate summary statistics (mean, median, confidence intervals) across bootstrap iterations

3. Plot variance metrics against sample size to visualize the saturation pattern

## Accessing the Feature

1. Launch the HaugShape v2 app
2. Navigate to **3. Shape Analysis** → **PCA Saturation Curve**
3. The interface has three main sections:
   - **Left panel**: File input and parameters
   - **Right panel**: Saturation curve plot and results

## Step-by-Step Workflow

### Step 1: Load PCA Results

**Input File Types:**
- **Excel file (.xlsx)**: **Most common** - PC scores from "Run Analysis" tab
  - This is the Excel file saved by the shape analysis feature
  - Contains columns: ID, PC1, PC2, PC3, ...
  - The module automatically detects and loads PC columns
- **RDS file (.rds)**: A saved PCA object (advanced users)
  - Must be a PCA object with `$x` component containing scores

**How to get PCA results:**
1. Go to **Shape Analysis** → **Run Analysis** tab
2. Run your morphometric analysis (EFA + PCA)
3. Download the results as Excel file (default output)
4. Navigate to **Shape Analysis** → **PCA Saturation Curve**
5. Upload the Excel file you just saved

**Excel File Structure** (from Run Analysis):
```
| ID        | PC1     | PC2     | PC3     | ... |
|-----------|---------|---------|---------|-----|
| specimen1 | -0.234  | 0.156   | -0.089  | ... |
| specimen2 | 0.456   | -0.234  | 0.123   | ... |
| specimen3 | -0.123  | 0.089   | -0.456  | ... |
```
The module automatically:
- Identifies PC columns (numeric columns)
- Excludes the ID column
- Uses IDs as specimen names (optional)

### Step 2: Configure Parameters

| Parameter | Description | Default | Recommendation |
|-----------|-------------|---------|----------------|
| **Bootstrap Iterations** | Number of resampling iterations per sample size | 200 | 200-500 for reliable estimates; increase for more precision |
| **Minimum Sample Size** | Smallest subsample to test | 5 | At least 5 to ensure meaningful variance calculation |
| **Number of Steps** | How many sample sizes to test | 10 | 10-20 for smooth curves |
| **Number of PCs** | How many principal components to include | 10 | Depends on your data; typically 5-20 PCs capture most variation |
| **Variance Metrics** | Which metrics to compute | Both | See below for metric descriptions |
| **Use Parallel Processing** | Speed up computation using multiple CPU cores | No | Enable for large datasets (>100 specimens) |
| **Random Seed** | Ensures reproducibility | 42 | Use consistent seed for reproducible results |

**Variance Metrics Explained:**

- **Total Variance**: Sum of variances across all analyzed PCs
  - Represents total morphospace size
  - Use to assess if morphospace volume is saturated
  
- **Cumulative Variance**: Same as total variance (kept for clarity)
  - Shows cumulative explained variation
  - Useful for understanding overall morphospace structure

### Step 3: Run Analysis

1. Click **"Compute Saturation Curve"**
2. Wait for the analysis to complete (progress bar shows status)
3. A notification will appear when finished

**Processing Time:**
- Small datasets (<50 specimens): <1 minute
- Medium datasets (50-200 specimens): 1-5 minutes
- Large datasets (>200 specimens): 5-15 minutes
- Enable parallel processing to reduce computation time

### Step 4: Interpret Results

#### The Saturation Curve Plot

**X-axis options:**
- **Absolute**: Number of specimens
- **Proportion**: Percentage of total specimens (0-1)

**Y-axis**: Variance metric value

**Plot elements:**
- **Line**: Mean variance across bootstrap iterations
- **Ribbon (shaded area)**: 95% confidence interval
- **Points**: Mean values at each sample size (optional)

#### Interpretation Guide

**Pattern 1: Clear Plateau (Saturated)**
```
Variance |     _______________
         |   /
         | /
         +-------------------
            Sample Size
```
- Curve flattens at higher sample sizes
- Confidence intervals narrow
- **Interpretation**: Morphospace is saturated; additional specimens unlikely to expand it significantly
- **Action**: Current sample is sufficient for analysis

**Pattern 2: Continuous Increase (Unsaturated)**
```
Variance |                 /
         |              /
         |           /
         |        /
         +-------------------
            Sample Size
```
- Curve continues increasing even at maximum sample size
- Wide confidence intervals at high sample sizes
- **Interpretation**: Morphospace may not be saturated
- **Action**: Consider collecting more specimens to capture full variation

**Pattern 3: Gradual Approach (Approaching Saturation)**
```
Variance |          _____/
         |       __/
         |   __/
         +-------------------
            Sample Size
```
- Curve shows decreasing rate of increase
- Narrowing confidence intervals
- **Interpretation**: Morphospace approaching saturation
- **Action**: Current sample is likely adequate, but a few more specimens could help

#### Summary Statistics Table

The table shows detailed statistics for each sample size and metric:

| Column | Description |
|--------|-------------|
| `sample_size` | Number of specimens in subsample |
| `metric_type` | Which variance metric |
| `mean` | Mean variance across bootstrap iterations |
| `median` | Median variance |
| `sd` | Standard deviation |
| `q025` | 2.5th percentile (lower bound of 95% CI) |
| `q975` | 97.5th percentile (upper bound of 95% CI) |
| `sample_proportion` | Proportion of total specimens |

**Key values to check:**
- Compare `mean` values at 50% vs 100% of specimens
  - Small difference (<5%): Likely saturated
  - Large difference (>20%): Likely unsaturated
  
- Check confidence interval width (`q975 - q025`)
  - Narrow intervals: Stable estimates
  - Wide intervals: High variability, may need more bootstrap iterations

### Step 5: Customize Plot Appearance

**X-axis Type:**
- **Absolute**: Shows actual number of specimens (easier for small datasets)
- **Proportion**: Shows percentage (better for comparing across studies)

**Plot Theme:**
- **Haug**: Light theme with white background (default)
- **inverted_Haug**: Dark theme with black background (for presentations)
- **publication**: Minimal theme for publications

**Visual Options:**
- **Show Confidence Intervals**: Display 95% CI ribbon (recommended)
- **Show Points**: Display individual mean points (optional, can reduce clutter)

### Step 6: Export Results

**Export Options:**

1. **Download Plot (PDF)**
   - Vector format, publication-quality
   - Scalable without quality loss
   - Recommended for manuscripts

2. **Download Plot (PNG)**
   - Raster format at 300 DPI
   - Easy to insert in presentations
   - Good for web sharing

3. **Download Data (CSV)**
   - Raw saturation curve data
   - Use for custom plotting or further analysis
   - Includes all statistics (mean, SD, quantiles)

## Advanced Tips

### Optimizing Bootstrap Iterations

- Start with 200 iterations (fast, reasonable precision)
- If confidence intervals seem unstable, increase to 500
- For publication-quality results, use 500-1000 iterations
- More iterations = longer computation time but more stable estimates

### Choosing Sample Size Steps

- **10 steps** (default): Good balance of detail and computation speed
- **15-20 steps**: Smoother curves, better for visualization
- **5 steps**: Quick exploratory analysis

### Analyzing Specific PCs

If you're interested in specific PC axes:

1. Set "Number of PCs to Analyze" to include your PCs of interest
2. The analysis computes total variance across selected PCs
3. For individual PC analysis, you may need to extract and analyze separately

### Parallel Processing

Enable parallel processing for datasets with:
- More than 100 specimens
- High bootstrap iterations (>500)
- Many PCs analyzed (>20)

**Note:** Parallel processing uses multiple CPU cores. The speedup depends on your computer's hardware.

### Troubleshooting

**Problem: Analysis is very slow**
- **Solution**: Enable parallel processing, reduce bootstrap iterations, or reduce sample size steps

**Problem: Confidence intervals are very wide**
- **Solution**: Increase bootstrap iterations or check if your dataset is too small

**Problem: Curve doesn't plateau**
- **Interpretation**: Morphospace may genuinely be unsaturated
- **Alternative**: Check if you're analyzing enough PCs (may need to increase)

**Problem: File won't load**
- **Solution**: Ensure file is a valid PCA object (RDS) or has numeric PC columns (Excel)
- Check file format and structure

**Problem: Error: "No valid sample sizes"**
- **Solution**: Reduce minimum sample size or increase total specimens
- Your dataset may be too small for the current parameters

## Example Use Cases

### Use Case 1: Planning Fieldwork

**Scenario**: You have 50 specimens and wonder if you need to collect more.

**Workflow:**
1. Run saturation analysis on your current 50 specimens
2. If curve plateaus → Current sample is sufficient
3. If curve still increasing → Plan to collect more specimens
4. Use the slope at 100% to estimate how many more you might need

### Use Case 2: Comparing Sampling Completeness Across Groups

**Scenario**: You have morphometric data for 3 species with different sample sizes.

**Workflow:**
1. Run saturation analysis for each species separately
2. Compare curves using "Proportion" x-axis
3. Species with plateaued curve = well-sampled
4. Species with increasing curve = needs more specimens

### Use Case 3: Validating Taxonomic Conclusions

**Scenario**: You want to ensure your morphospace analysis is based on adequate sampling.

**Workflow:**
1. Run saturation analysis before final interpretation
2. If saturated → Confident that morphospace reflects true variation
3. If unsaturated → Acknowledge sampling limitations in discussion
4. Include saturation curve in supplementary materials

## Technical Details

### Bootstrap Resampling Method

- Sampling **with replacement** (specimens can be selected multiple times)
- Each bootstrap sample has same size as target sample size
- Variance is calculated for each bootstrap sample
- Summary statistics aggregate across all bootstrap samples

### Variance Calculation

For each subsample:
1. Extract PC scores for selected specimens
2. Compute variance for each PC: `var(PC_i)`
3. Total variance = sum of variances across all PCs
4. Repeat for all bootstrap iterations

### Statistical Considerations

- **Confidence intervals**: Based on quantiles of bootstrap distribution (non-parametric)
- **Minimum sample size**: Recommended ≥5 to avoid unstable variance estimates
- **Reproducibility**: Set random seed for consistent results across runs

## References

For theoretical background on morphospace saturation:

- Hopkins, M. J., & Gerber, S. (2017). Morphological disparity. In *Evolutionary Developmental Biology* (pp. 1-12). Springer.
- Villier, L., & Korn, D. (2004). Morphological disparity of ammonoids and the mark of Permian mass extinctions. *Science*, 306(5694), 264-266.

## Citation

If you use the PCA saturation curve feature in your research, please cite HaugShape v2 and mention the specific feature used for transparency.

---

**Last updated**: December 26, 2025  
**HaugShape v2** - Morphometric Analysis Toolkit
