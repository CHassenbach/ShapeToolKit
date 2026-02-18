HaugShape: Morphometric Shape Analysis and Visualization Tools
========================================================

## Overview

HaugShape is a comprehensive R package for morphometric shape analysis, providing tools for:

- **Shape Analysis**: Elliptical Fourier Analysis (EFA) and Principal Component Analysis (PCA)
- **Image Processing**: Advanced image splitting and morphing capabilities
- **Visualization**: Advanced plotting functions for hull plots, contour plots, and boxplots  
- **Data Processing**: Shape preprocessing, normalization, and statistical analysis
- **Interactive Tools**: Shiny application for interactive shape analysis and morphing

## Installation

You can install HaugShape from GitHub using:

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install HaugShape
devtools::install_github("TestoKlaus/HaugShape_v2")
```

## Quick Start

```r
library(HaugShape)

# Run the interactive Shiny app (development)
# Option 1: From the project root
# source('app.R')

# Option 2: Using the helper function
source('R/run_haug_app.R')
run_haug_app()

# Analyze shapes in a directory
result <- shape_analysis(
  shape_dir = "path/to/your/shapes",
  output_file = "analysis_results.xlsx"
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
- `shape_analysis()`: Perform comprehensive shape analysis with EFA and PCA
- `Haug_overview()`: Generate multi-panel overview plots
- `shape_plot()`: Create customizable shape plots
- `cluster_plot()`: Generate cluster analysis plots

### Image Processing & Morphing
- `split_image()`: Split images at specified positions with mirroring options
- `morph_shapes()`: Generate morphed shapes using advanced algorithms
- `morph_shapes_ui()` / `morph_shapes_server()`: Interactive Shiny module for image morphing

### Interactive Application
- `run_haug_app()`: Launch interactive Shiny application with all features

## New Features: Image Morphing

The package now includes a powerful image morphing module accessible through the Shiny app:

1. **Upload and Preview**: Load images with automatic format detection
2. **Smart Splitting**: Split images vertically or horizontally at any position
3. **Mirroring Options**: Mirror left/right or top/bottom halves
4. **Advanced Morphing**: Three morphing algorithms (Distance Transform, Linear, Spline)
5. **Interactive Preview**: Animate through morphing sequences

For detailed instructions, see `MORPH_SHAPES_GUIDE.md` or the quick reference in `MORPH_SHAPES_QUICKREF.md`.

## Requirements

- R >= 4.0.0
- Required packages: ggplot2, dplyr, tidyr, scales, readxl, openxlsx, magick, shiny, shinydashboard, DT, plotly, and others (see DESCRIPTION)

## Citation

If you use HaugShapeV2 in publications, please cite:

> Hassenbach, C. (2025). HaugShapeV2: Morphometric Shape Analysis and Visualization Tools. R package version 0.1.0. https://github.com/TestoKlaus/HaugShape_v2

**Important:** HaugShapeV2 incorporates code adapted from the Momocs package. Please also cite:

> Bonhomme, V., Picq, S., Gaucherel, C., & Claude, J. (2014). Momocs: Outline Analysis Using R. Journal of Statistical Software, 56(13), 1-24. https://doi.org/10.18637/jss.v056.i13

For Elliptical Fourier Analysis methods, also cite:

> Claude, J. (2008). Morphometrics with R. Springer-Verlag, New York.

You can get citation information in R with:
```r
citation("HaugShapeV2")
```

For detailed attribution information, see `ATTRIBUTION.md`.

## License

MIT License

This package incorporates code adapted from Momocs (https://github.com/MomX/Momocs), which is licensed under GPL-2 | GPL-3.

## Contact

For questions and bug reports, please visit: https://github.com/TestoKlaus/HaugShape_v2/issues