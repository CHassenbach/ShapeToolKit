[![Project Status: WIP - The project is being actively developed, but not yet appropriate for public use.](https://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)

ShapeToolKit: Morphometric Shape Analysis and Visualization Tools
========================================================


## Overview

ShapeToolKit is a comprehensive R package for morphometric shape analysis, providing tools for:

- **Shape Analysis**: Elliptical Fourier Analysis (EFA) and Principal Component Analysis (PCA)
- **Image Processing**: Advanced image splitting and morphing capabilities
- **Visualization**: Advanced plotting functions for hull plots, contour plots, and boxplots  
- **Data Processing**: Shape preprocessing, normalization, and statistical analysis
- **Interactive Tools**: Shiny application for interactive shape analysis and morphing

## Installation

You can install ShapeToolKit from GitHub using:

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install ShapeToolKit
devtools::install_github("CHassenbach/ShapeToolKit")
```

## Quick Start

```r
library(ShapeToolKit)

# Run the interactive Shiny app (development)
# Option 1: From the project root
# source('app.R')

# Option 2: Using the helper function
ShapeToolKit::run_shape_app()

# Analyze shapes in a directory
result <- shape_analysis(
  shape_dir = "path/to/your/shapes",
  output_file = "analysis_results.xlsx",
  seed = 42
)

# Create overview plots
overview <- Haug_overview(
  data = your_data,
  cols = c("PC1", "PC2", "PC3", "PC4"),
  group_col = "species",
  export_pdf = TRUE
)
```

## Main Functions

### Shape Analysis
The app's **Random seed** setting and the R API's `seed` argument default to 42.
The seed is reset for each analysis, making automatic foreground searching
reproducible without changing the caller's random state. Use identical images,
settings, file order, R/package versions, and RNG configuration when comparing
numerical results. Timestamps and output-file bytes may differ. The selected seed
is recorded in the results and reconstruction metadata. `seed_points` specifies
image pixels, not a random-number seed; use `seed = NULL` in R to retain the
previous unseeded behavior.

Manual foreground points are reused across runs. To compare upper and lower
foreground selections, click **Clear saved foreground points**, choose a different
output file name, then click **Run Shape Analysis** and select the new points.
Clearing points cancels pending clicks and clears the Shape Checks report, but
keeps completed results. Foreground clicks guide outline extraction; the separate
**Start point for alignment** setting controls contour alignment.

After an analysis completes, **Imported outline overlay** displays all extracted
outlines together, centered without rotation. Outlines are scaled to the same
centroid size by default; disable scaling to compare their original sizes.
Select an image in **Highlight an imported shape** to draw its outline in red.
The overlay uses the actual imported coordinates, not Fourier reconstructions.

- `shape_analysis()`: Perform comprehensive shape analysis with EFA and PCA
- `Haug_overview()`: Generate multi-panel overview plots
- `shape_plot()`: Create customizable shape plots
- `cluster_plot()`: Generate cluster analysis plots

### Image Processing & Morphing
- `split_image()`: Split images at specified positions with mirroring options
- `morph_shapes()`: Generate morphed shapes using advanced algorithms
- `morph_shapes_ui()` / `morph_shapes_server()`: Interactive Shiny module for image morphing

### Interactive Application
- `run_shape_app()`: Launch interactive Shiny application with all features

### Data Explorer: disparity analysis

The disparity panel offers all 73 coordinate-based metric/reducer combinations
from the supplied `dispRity` workflow, grouped into size, density, position and
other metrics. Select the same PC axes for all groups; coordinates are neither
standardized nor refitted. Both metric selectors start empty; choose only the
metrics you need. SOV, SOR, MPD and **median centroid distance** remain available. Centroid
distances describe spread around each group's own centroid, not the distance
between group centroids or absolute specimen body size.

- **Calculate** produces one row per group and selected metric, with observed
  values, optional rarefied means, sample sizes, and explicit status/error columns.
  Rarefaction subsamples without replacement to the smallest retained group.
  Missing/non-finite coordinate rows are excluded and counted; every included
  group must have at least two retained specimens.
- **Pairwise permutation tests** compare every selected metric, shuffling labels
  within each pair while preserving its sample sizes. Two-sided p-values use
  `(1 + extreme permutations) / (1 + permutations)`. Unlike the previous primary
  metric-only test, adjustment now covers all successful metric-by-pair tests.
- **Wilcoxon rank-sum** is available for mean/median centroid distance only. It
  compares specimen-level distances, never bootstrap or rarefaction replicates.
  It tests distributions, not necessarily median differences. Because the group
  centroids are estimated from those same specimens, distances are dependent and
  these p-values are approximate; label permutations are preferable for inference.
  Other selected metrics receive an explicit unsupported-test status.
- **Screen metrics** independently runs `dispRity::test.metric` on pooled retained
  specimens, optionally limited by the selected groups and a sample cap. No group
  column is required. Choose metrics, simulated shifts, replicates and steps.
  Each metric uses the same sampled coordinates and random seed for its simulated
  reductions. Results include unrounded R-squared, adjusted R-squared, slope,
  p-value, model sample size and warnings.

Screening criteria use unadjusted R-squared: random response must be at most
0.1 by default; size response must be at least 0.7 and exceed hollowing response;
density/position response must be at least 0.7 and exceed size response. The
thresholds are editable. Missing required shifts or undefined fits yield `NA`;
the "other" category has no rule. R-squared measures response to a simulated
shift, not overall metric quality: inspect slopes and warnings too.

Pairwise tests use full groups; rarefaction changes descriptive summaries only.
All tests assume exchangeable specimens and do not account for phylogeny,
repeated measures or other sampling dependence. Hull metrics need enough
non-degenerate points, including after reductions; undersized hull reductions
and undefined reduced-space values are explicitly warned about and omitted from
shift models. Other metric failures
remain visible rather than being silently dropped. Large distance/hull runs can
be expensive. Tree-dependent and between-group primitives are not offered because
they require inputs beyond a single coordinate matrix.

Separate CSV downloads preserve full precision for summaries, pairwise tests
and screening results. Re-running a stage clears its previous outputs, including
on validation failure, so an earlier result is not presented as the new run.

## New Features: Image Morphing

The package now includes a powerful image morphing module accessible through the Shiny app:

1. **Upload and Preview**: Load images with automatic format detection
2. **Smart Splitting**: Split images vertically or horizontally at any position
3. **Mirroring Options**: Mirror left/right or top/bottom halves
4. **Advanced Morphing**: Three morphing algorithms (Distance Transform, Linear, Spline)
5. **Interactive Preview**: Animate through morphing sequences

For detailed instructions, see `MORPH_SHAPES_GUIDE.md` or the quick reference in `MORPH_SHAPES_QUICKREF.md`.

## Requirements

- R >= 4.1.0
- Required packages: ggplot2, dplyr, tidyr, scales, readxl, openxlsx, magick, shiny, shinydashboard, DT, plotly, and others (see DESCRIPTION)

### Optional 3D rotation video export

Ordinary plotting and interactive HTML export do not require the video packages.
To export rotation videos, install them explicitly:

```r
install.packages(c("av", "webshot2", "chromote"))
```

Video export also requires Pandoc and a Chromium-based browser such as Chrome or
Edge. If the browser is not detected, set `CHROMOTE_CHROME` to its executable path.
The app reports missing video packages without installing them automatically.

## Citation

If you use ShapeToolKit in publications, please cite:

> Hassenbach, C. (2025). ShapeToolKit: Morphometric Shape Analysis and Visualization Tools. R package version 0.1.0. https://github.com/CHassenbach/ShapeToolKit

**Important:** ShapeToolKit incorporates code adapted from the Momocs package. Please also cite:

> Bonhomme, V., Picq, S., Gaucherel, C., & Claude, J. (2014). Momocs: Outline Analysis Using R. Journal of Statistical Software, 56(13), 1-24. https://doi.org/10.18637/jss.v056.i13

For Elliptical Fourier Analysis methods, also cite:

> Claude, J. (2008). Morphometrics with R. Springer-Verlag, New York.

You can get citation information in R with:
```r
citation("ShapeToolKit")
```

For detailed attribution information, see `ATTRIBUTION.md`.

## License

GPL (>= 3); see [LICENSE.md](LICENSE.md).

This package incorporates code adapted from Momocs (https://github.com/MomX/Momocs), which is licensed under GPL-2 | GPL-3.

## Contact

For questions and bug reports, please visit: https://github.com/CHassenbach/ShapeToolKit/issues
