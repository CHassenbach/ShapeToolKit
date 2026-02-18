# ShapeToolKit - Installation Guide

This guide will help you install ShapeToolKit with all required packages for morphometric analysis.

## Quick Setup (Recommended)

### Option 1: Automatic Setup (Windows)
1. Double-click `setup.bat`
2. Wait for installation to complete
3. Run the app: `ShapeToolKit::run_shape_app()`

### Option 2: Manual R Setup
1. Open R/RStudio
2. Navigate to the ShapeToolKit directory
3. Run: `source('setup.R')`
4. Run the app: `ShapeToolKit::run_shape_app()`

### Install from GitHub (alternative)
If you prefer installing directly from GitHub, ensure some build tools are present first, then run the install:

```r
# 1) Install prerequisites used by the installer/build chain
install.packages(c("remotes", "processx", "callr", "pkgbuild"))

# 2) Install this package from GitHub (without building vignettes for speed)
remotes::install_github(
   "CHassenbach/ShapeToolKit",
   ref = "develop",
   dependencies = TRUE,
   build_vignettes = FALSE
)
```

Notes for Windows:
- Make sure Rtools is installed (required for compiling some packages from source).
- If you see an error like "there is no package called 'processx'", run:
   `install.packages(c("processx", "callr", "pkgbuild"))` and retry the GitHub install.

## Required Packages

ShapeToolKit requires the following R packages:

### Core Analysis
- **Momocs** - Morphometric analysis (main dependency)
- **dplyr** - Data manipulation
- **ggplot2** - Plotting
- **readxl** - Excel file reading

### Shiny Web Interface
- **shiny** - Core web framework
- **shinydashboard** - Dashboard interface
- **DT** - Interactive data tables
- **shinyWidgets** - Enhanced UI components
- **shinycssloaders** - Loading animations
- **colourpicker** - Color selection tools

### Optional Enhancements
- **shapes** - Additional shape analysis
- **MASS** - Statistical functions
- **cluster** - Clustering algorithms
- **vegan** - Ecological analysis

## Manual Installation

If automatic setup fails, install packages manually:

```r
# Install from R console
install.packages(c(
  "Momocs", "dplyr", "ggplot2", "readxl",
  "shiny", "shinydashboard", "DT", 
  "shinyWidgets", "shinycssloaders", "colourpicker"
))
```

## Troubleshooting

### Momocs Installation Issues
Momocs can sometimes be tricky to install. Try these solutions:

1. **Update R**: Make sure you have R 4.0 or newer
2. **Install dependencies first**:
   ```r
   install.packages(c("sp", "rgeos", "maptools", "dplyr", "magrittr"))
   install.packages("Momocs")
   ```
3. **Install from source**:
   ```r
   install.packages("Momocs", type = "source")
   ```

### Package Loading Errors
If packages fail to load:
1. Restart R session
2. Check package versions: `packageVersion("package_name")`
3. Reinstall problematic packages: `install.packages("package_name", force = TRUE)`

### Windows-Specific Issues
- Make sure Rtools is installed for package compilation
- Run R as administrator if installation fails
- Check antivirus software isn't blocking downloads

## Verification

Check if everything is installed correctly:

```r
source('R/install_packages.R')
check_shapetoolkit_packages()
```

This will show you which packages are available and which are missing.

## Getting Started

Once installation is complete:

1. **Start the app**: `source('app.R')`
2. **Upload Excel data**: Your morphometric measurements
3. **Upload shape files**: JPEG images (optional)
4. **Configure analysis**: Select columns and options
5. **Analyze**: Use the processed data for morphometric analysis

## File Structure

```
ShapeToolKit/
├── setup.R              # Main setup script
├── setup.bat            # Windows batch installer
├── app.R                # Main Shiny application
├── R/
│   ├── install_packages.R    # Package installation functions
│   └── map_shapes_to_data.R  # Shape mapping functionality
└── README_install.md    # This file
```

## Support

If you encounter issues:
1. Check this README for common solutions
2. Run `check_shapetoolkit_packages()` to diagnose missing packages
3. Try the manual installation steps above
4. Ensure you have a recent version of R (4.0+)

## System Requirements

- **R**: Version 4.0 or newer
- **Operating System**: Windows, macOS, or Linux
- **Memory**: 4GB RAM minimum (8GB recommended)
- **Storage**: 1GB free space for packages and data
- **Internet**: Required for package installation