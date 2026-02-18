# Shape Reconstruction Feature Implementation

## Overview
Extended `shape_analysis()` to save all data needed for shape reconstruction from PC scores.

## What Was Implemented

### 1. Reconstruction Data Saving (`shape_analysis.R`)

**New helper function: `.save_reconstruction_data()`**
- Extracts and saves PCA components (eigenvectors, center, sdev)
- Saves full EFA results object with Fourier coefficients
- Saves analysis parameters (norm, harmonics, start_point, etc.)
- Calculates and saves variance explained by each PC
- Includes metadata with format versioning (v1.0)
- Saves as binary RDS file for exact R object preservation
- Creates human-readable text summary with usage instructions

**Files created by shape_analysis():**
- `*_reconstruction_model.rds` - Binary model file with all reconstruction data
- `*_reconstruction_info.txt` - Human-readable summary and usage guide

### 2. Model Loader Function

**New exported function: `load_reconstruction_model()`**
- Loads reconstruction model from RDS file
- Validates model structure and required components
- Checks format version compatibility
- Verifies dimension consistency
- Returns properly formatted list ready for reconstruction functions

**Parameters:**
- `model_path` - Path to RDS file
- `validate` - Whether to validate structure (default: TRUE)
- `verbose` - Print loading messages (default: TRUE)

### 3. Updated Return Structure

`shape_analysis()` now returns:
- `pca_results` - PCA object (existing)
- `efa_results` - EFA coefficients object (NEW)
- `scores` - Data frame of PC scores (existing)
- `summary` - PCA summary text (existing)
- `output_path` - Excel file path (existing)
- `summary_txt_path` - Summary text file (existing)
- `pc_plot_jpg_path` - Plot JPG file (existing)
- `reconstruction_model_path` - RDS model file (NEW)
- `reconstruction_info_path` - Info text file (NEW)
- `metadata` - Analysis metadata (existing)
- `pc_contribution_plot` - Plot object (existing)

### 4. Shiny Module Updates

**shape_analysis_module.R:**
- Updated results display to show all output file paths
- Lists reconstruction model and info files
- Shows directory where all files are saved

### 5. Documentation Updates

- Updated `shape_analysis()` documentation with new return values
- Added comprehensive documentation for `load_reconstruction_model()`
- Updated NAMESPACE to export new function
- Updated print method to display reconstruction file paths

## Reconstruction Model Contents

The saved RDS file contains:

```r
list(
  rotation = matrix,           # Eigenvectors (loadings) from PCA
  center = numeric,            # Centering values from PCA
  sdev = numeric,              # Standard deviations from PCA
  efa_results = OutCoe,        # Full EFA object from Momocs
  parameters = list(
    norm = logical,            # EFA normalization setting
    harmonics = character/int, # Number of harmonics
    start_point = character,   # Alignment start point
    n_components = integer,    # Number of PCs
    n_specimens = integer      # Number of specimens
  ),
  variance_explained = numeric, # % variance per PC
  metadata = list(
    format_version = "1.0",
    created_date = POSIXct,
    momocs_version = character,
    r_version = character
  )
)
```

## Usage Example

```r
# Run shape analysis (automatically saves reconstruction model)
result <- shape_analysis(
  shape_dir = "path/to/shapes",
  output_file = "my_analysis.xlsx"
)

# Reconstruction model saved to:
# - my_analysis_reconstruction_model.rds
# - my_analysis_reconstruction_info.txt

# Later, load the model for reconstruction
model <- load_reconstruction_model("my_analysis_reconstruction_model.rds")

# Check model information
print(model$parameters)
print(model$metadata)

# Use with reconstruction function (to be implemented next)
# reconstructed <- reconstruct_shape_from_pca(
#   reconstruction_model = model,
#   pc_scores = c(PC1 = 2.0, PC2 = -1.5, PC3 = 0.5)
# )
```

## Next Steps

To complete the shape reconstruction feature, implement:

1. **`reconstruct_shape_from_pca()`** - Core reconstruction function
   - Takes reconstruction model and PC scores
   - Reconstructs Fourier coefficients from PCA
   - Uses `Momocs::efourier_i()` for inverse Fourier transform
   - Returns outline coordinates

2. **`reconstruct_shapes_batch()`** - Batch reconstruction
   - Process multiple PC score combinations
   - Export results as images or coordinate files

3. **Integration with shape completion** - Option to use PCA model for completing half shapes

4. **Shiny module for interactive reconstruction** - UI for exploring morphospace with real-time shape preview

## Technical Notes

- **Format version**: Currently v1.0, allows future compatibility handling
- **Storage format**: RDS preserves complex R objects (Momocs objects) without loss
- **Validation**: Model loader checks structure and dimensions
- **File naming**: Consistent with other outputs (uses same stem as Excel file)
- **Text summary**: Human-readable reference with variance explained and usage instructions

## Files Modified

1. `R/shape_analysis.R` - Core implementation
2. `R/shape_analysis_module.R` - UI updates
3. `NAMESPACE` - Export new function

## Testing Checklist

- [x] Syntax validation passed
- [ ] Test with sample shape dataset
- [ ] Verify RDS file can be loaded
- [ ] Check text summary formatting
- [ ] Validate model structure after loading
- [ ] Test in Shiny app
- [ ] Verify file paths are correct in module display
