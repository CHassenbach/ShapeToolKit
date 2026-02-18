# Shape Reconstruction Fix

## Problem Identified

The shape reconstruction feature was producing garbled shapes (random assembly of lines) instead of proper shape outlines. The debug output showed the reconstruction was failing.

## Root Cause Analysis

Based on the debug output:
```
Parameters: norm=FALSE, harmonics=automatic, n_harmonics_used=11
Attempting Momocs efourier_i with complete object...
Momocs efourier_i failed: Ersetzung hat Länge 0
```

The problem was:

1. **Incorrect Input Type**: The code was trying to pass an `OutCoe` object to `efourier_i()`, but `efourier_i()` expects a **simple list** with components: `an`, `bn`, `cn`, `dn`, `ao`, `co`

2. **Missing ao/co Components**: The coefficient list created by `coeff_split()` didn't include the DC offset components (`ao` and `co`) that `efourier_i` requires

## Technical Details

### efourier_i Function Requirements

From `momocs_core-out-efourier.R` (lines 273-300), `efourier_i` expects:

```r
efourier_i <- function(ef, nb.h, nb.pts = 120) {
  if (is.null(ef$ao))
    ef$ao <- 0
  if (is.null(ef$co))
    ef$co <- 0
  an <- ef$an  # <-- Needs these as list components
  bn <- ef$bn
  cn <- ef$cn
  dn <- ef$dn
  ao <- ef$ao
  co <- ef$co
  # ... reconstruction logic
}
```

### The coeff_split Function

The `coeff_split()` function (from `momocs_core-utils.R`) converts a coefficient vector into a list:
- Input: Vector like `[A1, A2, ..., An, B1, ..., Bn, C1, ..., Cn, D1, ..., Dn]`
- Output: List with `$an`, `$bn`, `$cn`, `$dn` (but NOT `$ao` or `$co`)

### Previous Bug

The old code tried:
```r
reconstructed_coe <- model$efa_object
reconstructed_coe$coe <- matrix(reconstructed_coefs, nrow = 1)
result <- efourier_i(reconstructed_coe, ...)  # WRONG! OutCoe object, not a list
```

## Solution Implemented

### 1. Use coeff_split Properly

Split the reconstructed coefficient vector into the required list structure:
```r
coef_list <- coeff_split(reconstructed_coefs, nb.h = n_harmonics, cph = 4)
```

### 2. Add Missing ao/co Components

Before calling `efourier_i`, ensure the list has all required components:
```r
if (is.null(coef_list$ao)) coef_list$ao <- 0
if (is.null(coef_list$co)) coef_list$co <- 0
```

These DC offset values are typically 0 after centering, which is standard in morphometric analysis.

### 3. Handle Both Normalized and Non-Normalized Cases

The code now checks `model$efa_object$norm`:
- **If `norm=TRUE`**: Denormalizes A, B, C, D → an, bn, cn, dn before reconstruction
- **If `norm=FALSE`**: Uses coefficients directly (they're already in raw form)

### Code Changes

**File**: `R/shape_reconstruction_module.R`

**Key changes**:
1. Properly use `coeff_split()` to create the list structure
2. Add `ao` and `co` components (set to 0)
3. Pass the simple list (not OutCoe object) to `efourier_i()`
4. Added detailed debug messages to track the reconstruction process

## Why This Matters

The `efourier_i` function is the **inverse Elliptical Fourier Transform** - it converts Fourier coefficients back into (x, y) coordinates. It requires:
- **Correct input structure**: Simple list with named components
- **Complete data**: All 6 components (an, bn, cn, dn, ao, co)
- **Raw coefficients**: If normalized, they must be denormalized first

The function reconstructs the shape by summing harmonics:
```r
x(θ) = ao/2 + Σ[an*cos(nθ) + bn*sin(nθ)]
y(θ) = co/2 + Σ[cn*cos(nθ) + dn*sin(nθ)]
```

## Testing

To verify the fix:
1. Load a reconstruction model (created by `shape_analysis()`)
2. Set all PC scores to zero (should reconstruct the mean shape)
3. Check that the shape outline appears correct (not random lines)
4. Verify the aspect ratio is reasonable (not extreme like 0.064)

## Debug Output Interpretation

The successful reconstruction should show:
```
Using complete EFA object for reconstruction
Number of harmonics: 11
Coefficients normalized: FALSE
Calling efourier_i with list components: an, bn, cn, dn, ao, co
efourier_i succeeded! Result dimensions: 120 x 2
Shape reconstruction complete!
```

## References

- `efourier()`: Lines 88-186 in `R/momocs_core-out-efourier.R`
- `efourier_norm()`: Lines 197-240 in `R/momocs_core-out-efourier.R`  
- `efourier_i()`: Lines 273-300 in `R/momocs_core-out-efourier.R`
- `coeff_split()`: Lines 59-70 in `R/momocs_core-utils.R`
- Claude, J. (2008). *Morphometrics with R*. Springer.

## Date
December 10, 2025
