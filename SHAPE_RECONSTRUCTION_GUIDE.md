# Shape Reconstruction Module - User Guide

## Overview

The Shape Reconstruction module provides an interactive interface for reconstructing shapes from PCA models. You can load reconstruction models created by the Shape Analysis module and generate shape outlines by specifying PC scores.

## How to Access

In the HaugShape v2 app:
1. Navigate to **"3. Shape Analysis"** in the sidebar
2. Select **"Reconstruct Shapes"** from the submenu

## Workflow

### 1. Load a Reconstruction Model

**Steps:**
1. Click "Choose reconstruction model file"
2. Navigate to your shape analysis output directory
3. Select the `*_reconstruction_model.rds` file
4. Click "Load Model"

**What you'll see:**
- Model information (specimens, PCs, harmonics)
- Analysis parameters (normalization, start point)
- Variance explained by each PC

### 2. Set PC Scores

**Interactive inputs for each PC:**
- Enter values in **standard deviation units**
- `0` = mean shape
- `+1` = one SD above mean
- `-1` = one SD below mean
- Range typically: `-3` to `+3`

**What each PC represents:**
- Check the variance % shown in parentheses
- Higher % = more shape variation captured
- PC1 typically captures the most variation

### 3. Reconstruct a Single Shape

**Steps:**
1. Set desired PC scores in the input fields
2. Click "Reconstruct Shape"
3. View the reconstructed outline

**Options:**
- Adjust plot width/height (200-1200 pixels)
- Show original shape (if available in model)

**Downloads:**
- **CSV**: Outline coordinates (x, y points)
- **PNG**: High-resolution plot

### 4. Batch Reconstruction (Morphospace Exploration)

**Use case:** See how shapes vary along a PC axis

**Steps:**
1. Expand "Batch Reconstruction" box
2. Select which PC axis to vary
3. Set minimum value (e.g., `-3`)
4. Set maximum value (e.g., `+3`)
5. Set number of steps (e.g., `7`)
6. Choose whether to hold other PCs at zero or current values
7. Click "Generate Batch"

**Output:**
- Grid of shapes showing variation along the PC
- Each shape labeled with its PC value
- Download entire grid as PNG

## Understanding PC Scores

### Standard Deviation Units

PC scores are in units of standard deviation from the mean:

```
PC Score    Meaning
--------    -------
   0        Mean shape (average)
  +1        One SD above mean (larger/different)
  -1        One SD below mean (smaller/different)
  +2        Two SD above mean
  -2        Two SD below mean
  ±3        Extreme shapes (edge of distribution)
```

### Variance Explained

- **PC1**: Typically 40-60% - Major shape variation
- **PC2**: Typically 15-30% - Secondary variation
- **PC3+**: Increasingly subtle variations

### Example Interpretations

If PC1 explains 50% variance:
- `PC1 = +2`: Shape is elongated/larger in dimension captured by PC1
- `PC1 = -2`: Shape is compressed/smaller in that dimension

## Tips and Best Practices

### 1. Start with Simple Combinations
- Set most PCs to `0`
- Vary one PC at a time
- Observe how each PC affects shape

### 2. Use Batch Mode for Exploration
- Generate sequences: `-3, -2, -1, 0, 1, 2, 3`
- See smooth transitions between extreme shapes
- Helps understand what each PC captures

### 3. Combine Multiple PCs
- After understanding individual PCs
- Set PC1 and PC2 to non-zero values
- Explore interactions between PCs

### 4. Stay Within Reasonable Ranges
- **Conservative**: `-2` to `+2` (95% of specimens)
- **Exploratory**: `-3` to `+3` (99.7% of specimens)
- **Extreme**: Beyond `±3` (outside normal variation, may produce unrealistic shapes)

### 5. Document Your Reconstructions
- Download coordinates for quantitative analysis
- Download plots for presentations
- CSV files include PC scores in header comments

## Technical Details

### Reconstruction Process

1. **Load Model**: Reads eigenvectors, center, and EFA coefficients
2. **Transform PC Scores**: `fourier_coefs = center + (PC_scores × eigenvectors)`
3. **Inverse Fourier**: Converts coefficients back to outline coordinates
4. **Display**: Plots the reconstructed outline

### File Formats

**Input:**
- `*_reconstruction_model.rds`: Binary R object with all reconstruction data

**Output:**
- `reconstructed_shape_*.csv`: Coordinates (point, x, y) with PC scores in header
- `reconstructed_shape_*.png`: High-resolution plot (800x800px, 150 DPI)
- `batch_reconstruction_*.png`: Grid plot (1600x1600px, 150 DPI)

### Coordinate System

- Outlines are typically 120 points
- Coordinates are in the same units as original analysis
- Origin (0,0) is at the centroid (shapes were centered during analysis)
- Aspect ratio is 1:1 (equal x and y scaling)

## Troubleshooting

### "Model file does not exist"
- Check file path is correct
- Ensure the RDS file is in the selected location
- File must end in `.rds`

### "Reconstruction failed"
- Check that PC scores are numeric
- Very extreme values (>±5) may cause issues
- Try resetting PCs to `0` and vary one at a time

### Shapes look distorted
- This is normal for extreme PC values (>±3)
- Represents shapes outside the observed range
- May be biologically unrealistic

### "Package Momocs is required"
- The Momocs package will auto-install
- If issues persist, manually install: `install.packages("Momocs")`
- Restart the app after installation

## Example Use Cases

### 1. Visualizing Size Gradient
```
Scenario: PC1 captures size variation (60% variance)

Setup:
- PC1 = -3, -2, -1, 0, 1, 2, 3
- All other PCs = 0

Result: Series of shapes from small to large
```

### 2. Exploring Shape vs. Size
```
Scenario: PC1 = size (50%), PC2 = shape (25%)

Setup:
- Generate batch along PC1 (size gradient)
- Then generate batch along PC2 (shape gradient)
- Compare how each PC affects morphology
```

### 3. Reconstructing Specific Specimens
```
Scenario: Want to visualize a specimen's shape from PC scores

Setup:
- Load specimen's PC scores from Excel output
- Enter values into PC input fields
- Reconstruct to see the shape

Use: Verify analysis results or create figures
```

### 4. Morphospace Exploration
```
Scenario: Explore 2D morphospace (PC1 vs PC2)

Setup:
- Create grid: PC1 = -2, 0, 2; PC2 = -2, 0, 2
- Generate 9 reconstructions manually
- Shows shape variation across morphospace

Advanced: Use batch mode twice for each axis
```

## Integration with Other Modules

### Shape Analysis → Reconstruction
1. Run shape analysis on images
2. Analysis automatically saves reconstruction model
3. Load model in reconstruction module
4. Explore results interactively

### Reconstruction → Plotting
1. Reconstruct shapes at specific PC scores
2. Download coordinates
3. Import to Data Import module (if formatted appropriately)
4. Overlay on PCA plots in Plotting module

### Reconstruction → Complete Halved Shapes
Future enhancement:
- Use PCA model to complete halved shapes
- Ensures completed shapes stay within observed variation
- More biologically realistic than simple mirroring

## Keyboard Shortcuts

- **Tab**: Navigate between PC input fields
- **Enter**: Trigger reconstruction (when in input field)
- **Arrow keys**: Fine-tune numeric inputs

## Additional Resources

- **Momocs package**: https://CRAN.R-project.org/package=Momocs
- **EFA theory**: Elliptical Fourier Analysis for shape description
- **PCA interpretation**: Principal Components capture major axes of variation

## Support

For issues or questions:
1. Check the reconstruction info text file (`*_reconstruction_info.txt`)
2. Verify model was created with compatible version
3. Ensure Momocs package is installed and loaded
4. Review shape analysis parameters used
