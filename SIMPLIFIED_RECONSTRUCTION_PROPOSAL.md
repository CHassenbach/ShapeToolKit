# Simplified Reconstruction Model Export

## The Problem with Current Approach

The RDS file approach has several issues:
1. R objects (especially S3 classes) don't always serialize/deserialize cleanly
2. Requires preserving complex object structure
3. Hard to debug what's actually saved
4. Not human-readable

## What We Actually Need

For shape reconstruction, we only need **3 simple pieces of data**:

### 1. PCA Model (to go from PC scores → Fourier coefficients)
- `rotation` matrix: eigenvectors [n_coefs × n_PCs]
- `center` vector: mean coefficients [n_coefs]
- `sdev` vector: standard deviations [n_PCs]

### 2. Metadata
- Number of harmonics
- Normalization flag (TRUE/FALSE)
- Number of PCs
- Number of specimens

### 3. That's it!

We don't need:
- The full OutCoe object
- Shape metadata
- Factor data
- Complex R objects

## Simplified Solution: CSV Files

Save 3 simple files:

```
analysis_name_pca_rotation.csv    # Eigenvector matrix
analysis_name_pca_center.csv      # Mean coefficient vector  
analysis_name_pca_sdev.csv        # Standard deviations
analysis_name_metadata.txt        # Plain text parameters
```

### Why This Works Better

1. **Simple**: Just matrices and vectors
2. **Portable**: Can be loaded in any software
3. **Debuggable**: Can open in Excel/text editor
4. **Reliable**: No object serialization issues
5. **Transparent**: See exactly what's saved

## Reconstruction Process

```r
# 1. Load the 3 CSV files
rotation <- read.csv("pca_rotation.csv", row.names = 1)
center <- read.csv("pca_center.csv")$value
sdev <- read.csv("pca_sdev.csv")$value

# 2. User provides PC scores
pc_scores <- c(PC1 = 2.0, PC2 = -1.5, PC3 = 0, ...)

# 3. Reconstruct coefficients
scaled_scores <- pc_scores * sdev[1:length(pc_scores)]
reconstructed_coefs <- center + as.vector(scaled_scores %*% t(rotation))

# 4. Split into coefficient list
coef_list <- coeff_split(reconstructed_coefs, nb.h = n_harmonics, cph = 4)
coef_list$ao <- 0
coef_list$co <- 0

# 5. Reconstruct shape
coords <- efourier_i(coef_list, nb.h = n_harmonics, nb.pts = 120)
```

That's it! No complex objects, no serialization issues.

## Implementation

Would you like me to:
1. Update `shape_analysis.R` to save these 3 CSV files?
2. Update the reconstruction module to load CSV files instead of RDS?

This approach is much simpler and more reliable!
