# CSV Reconstruction Model Implementation

## Summary

Replaced RDS-based reconstruction model export/import with a simpler CSV-based approach to fix shape reconstruction issues and improve reliability.

## Problem

Shape reconstruction was producing garbled output (random lines) due to:
1. RDS serialization losing OutCoe object attributes ($norm, $method showing as NULL)
2. efourier_i() requiring simple list structure, not OutCoe objects
3. Lack of transparency in binary RDS format making debugging difficult

## Solution

**Export 4 CSV files instead of 1 RDS file:**

1. `*_pca_rotation.csv` - Eigenvector matrix (coefficients × PCs)
2. `*_pca_center.csv` - Mean coefficient values (coefficient, value columns)
3. `*_pca_sdev.csv` - Standard deviations with variance % (PC, sdev, variance_pct columns)
4. `*_reconstruction_metadata.txt` - Plain text parameters (norm, harmonics, start_point)

## Changes Made

### 1. R/shape_analysis.R

**New function: `.save_reconstruction_csv()`** (lines ~450-550)
- Replaces `.save_reconstruction_data()`
- Exports 4 CSV files with proper error handling
- Creates human-readable, debuggable output files
- Returns list of file paths for user feedback

**Updated function: `load_reconstruction_csv()`** (previously `load_reconstruction_model()`)
- Accepts folder path or any CSV file from the folder
- Reads and validates all 4 CSV files
- Parses metadata text file for parameters
- Returns list(rotation, center, sdev, variance_explained, parameters)
- Comprehensive dimension validation

**Modified function: `shape_analysis()`**
- Updated return value documentation to show `reconstruction_files` list
- Changed example code to use `load_reconstruction_csv(dirname(files[1]))`

### 2. R/shape_reconstruction_module.R

**Updated `shape_reconstruction_ui()`** (line ~20)
- Help text now mentions "folder or CSV file" instead of "RDS file"

**Updated `shape_reconstruction_server()`** (line ~216)
- Changed function call from `load_reconstruction_model()` to `load_reconstruction_csv()`

**Updated `output$model_info_ui`** (lines ~230-260)
- Removed references to `metadata$created_date` and `metadata$format_version`
- Changed display to use actual CSV model structure
- Made parameter display conditional based on availability

### 3. NAMESPACE

**Added export:**
```r
export(load_reconstruction_csv)
export(load_reconstruction_model)  # Keep for backward compatibility
```

## File Format Details

### rotation CSV structure:
```
"","PC1","PC2","PC3",...
"an1",0.123,-0.045,0.067,...
"bn1",-0.034,0.089,-0.012,...
...
```

### center CSV structure:
```
coefficient,value
an1,0.0012
bn1,-0.0034
...
```

### sdev CSV structure:
```
PC,sdev,variance_pct
1,2.456,45.23
2,1.789,23.45
...
```

### metadata TXT structure:
```
Reconstruction Model Metadata
Generated: 2024-01-15 14:30:45

Normalization: FALSE
Harmonics Used: 11
Start Point: auto
Total Coefficients: 44
```

## Benefits

1. **Transparency**: CSV files are human-readable and easily inspected
2. **Debuggability**: Can manually verify values, check for corruption
3. **Reliability**: No S3 class serialization issues
4. **Flexibility**: Easy to import into other analysis tools (Excel, Python, etc.)
5. **Maintainability**: Plain text format is future-proof

## Testing Checklist

- [ ] Run shape_analysis() on dataset
- [ ] Verify 4 CSV files are created
- [ ] Check CSV files contain correct data
- [ ] Load model using load_reconstruction_csv()
- [ ] Verify model structure is correct
- [ ] Reconstruct shape at PC scores = 0 (mean shape)
- [ ] Verify output is proper outline, not random lines
- [ ] Test in reconstruction module UI
- [ ] Test batch reconstruction

## Backward Compatibility

The old `load_reconstruction_model()` function is still exported in NAMESPACE for backward compatibility, but users should migrate to `load_reconstruction_csv()` for new work.

## Next Steps

1. Test the complete workflow
2. Update documentation files (RECONSTRUCTION_FEATURE.md, MORPH_SHAPES_GUIDE.md)
3. Consider adding CSV import to shape_analysis_module.R display
4. Update vignettes if any reference RDS models
