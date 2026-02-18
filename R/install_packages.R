# HaugShape v2 Package Installer
# Automatically installs required packages for HaugShape_v2

#' Install Required Packages for HaugShape v2
#'
#' This function checks for and installs all packages required by HaugShape_v2
#' including dependencies for morphometric analysis and Shiny applications.
#'
#' @param force_reinstall Logical. If TRUE, reinstalls packages even if already present
#' @param verbose Logical. If TRUE, shows detailed installation progress
#' @return Invisible TRUE if all packages installed successfully
#' @export
install_haugshape_packages <- function(force_reinstall = FALSE, verbose = TRUE) {
  
  if (verbose) {
    cat("=== HaugShape v2 Package Installation ===\n")
    cat("Checking and installing required packages...\n\n")
  }
  
  # Define required packages
  required_packages <- list(
    # Core R packages (usually available)
    base_packages = c("tools", "utils", "grDevices", "graphics", "stats"),
    
    # CRAN packages for data analysis
    cran_packages = c(
      "dplyr",           # Data manipulation
      "ggplot2",         # Plotting
      "readxl",          # Excel file reading (some code may reference it)
      "openxlsx",        # Excel read/write used in data_import.R & shape_analysis.R
      "DT",              # Data tables
      "plotly"           # Interactive plots
    ),
    
    # Shiny ecosystem
    shiny_packages = c(
      "shiny",           # Core Shiny
      "shinydashboard",  # Dashboard framework
      "shinyWidgets",    # Enhanced widgets
      "shinycssloaders", # Loading animations
      "colourpicker",    # Color selection
      "shinyFiles"       # File/Dir chooser used in modules
    ),
    
    # Morphometric analysis packages
    morphometric_packages = c(
      "Momocs",          # Morphometric analysis (main dependency)
      "shapes"           # Additional shape analysis
    ),
    
    # Optional enhancement packages
    optional_packages = c(
      "MASS",            # Statistical functions
      "cluster",         # Clustering analysis
      "vegan"            # Ecological analysis
    )
  )
  
  # Install function
  install_package_group <- function(packages, group_name, required = TRUE) {
    if (verbose) cat(paste("Installing", group_name, "packages:\n"))
    
    success_count <- 0
    total_count <- length(packages)
    
    for (pkg in packages) {
      if (verbose) cat(paste("  Checking", pkg, "..."))
      
      # Check if package is already installed
      if (!force_reinstall && requireNamespace(pkg, quietly = TRUE)) {
        if (verbose) cat(" already installed\n")
        success_count <- success_count + 1
        next
      }
      
      # Install package
      tryCatch({
        if (verbose) cat(" installing...")
        
        # Special handling for Momocs (can be tricky to install)
        if (pkg == "Momocs") {
          install_momocs_with_dependencies(verbose)
        } else {
          install.packages(pkg, repos = "https://cran.r-project.org", quiet = !verbose)
        }
        
        # Verify installation
        if (requireNamespace(pkg, quietly = TRUE)) {
          if (verbose) cat(" SUCCESS\n")
          success_count <- success_count + 1
        } else {
          if (verbose) cat(" FAILED (but continuing)\n")
          if (required) {
            warning(paste("Required package", pkg, "failed to install"))
          }
        }
        
      }, error = function(e) {
        if (verbose) cat(" ERROR:", e$message, "\n")
        if (required) {
          warning(paste("Error installing", pkg, ":", e$message))
        }
      })
    }
    
    if (verbose) {
      cat(paste("  Group", group_name, ":", success_count, "/", total_count, "packages installed\n\n"))
    }
    
    return(success_count)
  }
  
  # Install all package groups
  total_installed <- 0
  
  # Skip base packages (should already be available)
  if (verbose) cat("Skipping base R packages (assumed available)\n\n")
  
  # Install CRAN packages
  total_installed <- total_installed + 
    install_package_group(required_packages$cran_packages, "CRAN data analysis", TRUE)
  
  # Install Shiny packages
  total_installed <- total_installed + 
    install_package_group(required_packages$shiny_packages, "Shiny ecosystem", TRUE)
  
  # Install morphometric packages
  total_installed <- total_installed + 
    install_package_group(required_packages$morphometric_packages, "morphometric analysis", TRUE)
  
  # Install optional packages (don't fail if these don't install)
  total_installed <- total_installed + 
    install_package_group(required_packages$optional_packages, "optional enhancement", FALSE)
  
  # Final summary
  total_expected <- length(unlist(required_packages)) - length(required_packages$base_packages)
  
  if (verbose) {
    cat("=== Installation Summary ===\n")
    cat(paste("Total packages installed/verified:", total_installed, "/", total_expected, "\n"))
    
    if (total_installed >= (total_expected - length(required_packages$optional_packages))) {
      cat("✓ HaugShape v2 is ready to use!\n")
    } else {
      cat("⚠ Some required packages failed to install. Check warnings above.\n")
    }
  }
  
  return(invisible(TRUE))
}

# Note: Momocs installation functions removed as we now use vendored Momocs code
# #' Install Momocs with special handling
# #' @noRd
# install_momocs_with_dependencies <- function(verbose = TRUE) {
#   ... (function removed)
# }

#' Check HaugShape Package Status
#'
#' Quickly check which packages are available for HaugShape v2
#' @return A data frame showing package status
#' @export
check_haugshape_packages <- function() {
  
  required_packages <- c(
    "dplyr", "ggplot2", "readxl", "DT", "plotly",
    "shiny", "shinydashboard", "shinyWidgets", "shinycssloaders", "colourpicker",
    "shapes", "MASS", "cluster", "vegan"
  )
  
  status <- data.frame(
    Package = required_packages,
    Installed = sapply(required_packages, function(x) requireNamespace(x, quietly = TRUE)),
    stringsAsFactors = FALSE
  )
  
  status$Status <- ifelse(status$Installed, "✓ Available", "✗ Missing")
  
  cat("HaugShape v2 Package Status:\n")
  cat("============================\n")
  print(status[, c("Package", "Status")], row.names = FALSE)
  
  missing_count <- sum(!status$Installed)
  if (missing_count > 0) {
    cat("\nTo install missing packages, run: install_haugshape_packages()\n")
  } else {
    cat("\n✓ All packages are available!\n")
  }
  
  return(invisible(status))
}

# Export the functions
if (exists(".GlobalEnv")) {
  assign("install_haugshape_packages", install_haugshape_packages, envir = .GlobalEnv)
  assign("check_haugshape_packages", check_haugshape_packages, envir = .GlobalEnv)
}