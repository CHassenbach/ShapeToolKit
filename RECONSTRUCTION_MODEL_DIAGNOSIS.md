# Reconstruction Model Diagnosis

## Current Status (Based on Debug Output)

From your debug output:
```
Model components: rotation, center, sdev, efa_object, shape_metadata, parameters, variance_explained, metadata
Parameters: norm=FALSE, harmonics=automatic, n_harmonics_used=11
EFA norm flag: NULL
EFA method: NULL
```

## Issues Identified

### 1. Missing OutCoe Attributes
The `efa_object` is being saved, but its key attributes are not accessible:
- `efa_object$norm` should be `FALSE` (based on parameters) but shows as `NULL`
- `efa_object$method` should be `"efourier"` but shows as `NULL`

This suggests the OutCoe object structure is not being properly preserved during the RDS save/load process.

### 2. Why This Matters

The reconstruction code needs these attributes to:
1. **Check normalization**: `is_normalized <- !is.null(efa_object$norm) && efa_object$norm == TRUE`
2. **Know the method**: To apply the correct inverse transform

Without these attributes, the reconstruction code cannot properly determine how to process the coefficients.

## Root Cause

The issue is that when `efa_results` (an OutCoe object) is saved to RDS and then loaded, the S3 class attributes and structure might not be fully preserved, especially if:
- The object uses special attributes beyond the list structure
- The class methods expect certain environment variables
- The serialization doesn't capture all necessary metadata

## Solution Options

### Option 1: Save Raw Components (RECOMMENDED)

Instead of saving the complete `OutCoe` object, extract and save its essential components explicitly:

```r
reconstruction_model <- list(
  # PCA components
  rotation = pca_results$rotation,
  center = pca_results$center,
  sdev = pca_results$sdev,
  
  # EFA components (extracted explicitly)
  efa_coe = efa_results$coe,           # Coefficient matrix
  efa_method = efa_results$method,     # "efourier"
  efa_norm = efa_results$norm,         # TRUE or FALSE
  efa_fac = efa_results$fac,           # Factor data frame (if any)
  
  # Keep the full object as backup
  efa_object = efa_results,
  
  # ... rest of the model
)
```

### Option 2: Verify OutCoe Structure After Loading

Add diagnostic code to check what's actually in the efa_object after loading and potentially reconstruct it.

## Recommended Fix

Modify `.save_reconstruction_data()` in `shape_analysis.R` to explicitly extract and save the OutCoe components:
