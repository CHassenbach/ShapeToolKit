# HaugShape v2 Installation and Setup Script
# Run this script to install HaugShape_v2 with all required packages

cat("=== HaugShape v2 Setup and Installation ===\n")
cat("This script will install all required packages for HaugShape v2\n")
cat("============================================\n\n")

# Check if we're in the right directory
if (!file.exists("R/install_packages.R")) {
  cat("ERROR: Please run this script from the HaugShape_v2 project directory\n")
  cat("Current directory:", getwd(), "\n")
  stop("Installation script not found", call. = FALSE)
}

# Source the package installer
source("R/install_packages.R")

# Install all required packages
cat("Starting package installation...\n\n")
install_haugshape_packages(force_reinstall = FALSE, verbose = TRUE)

cat("\n=== Testing Installation ===\n")

# Test essential packages for the basic app
essential_packages <- c("shiny", "shinydashboard")
optional_packages <- c("DT", "readxl", "Momocs")

# Test essential packages
all_essential_ok <- TRUE
for (pkg in essential_packages) {
  cat("Testing", pkg, "...")
  if (requireNamespace(pkg, quietly = TRUE)) {
    cat(" OK\n")
  } else {
    cat(" MISSING\n")
    all_essential_ok <- FALSE
  }
}

# Test optional packages
cat("\nOptional packages:\n")
for (pkg in optional_packages) {
  cat("Testing", pkg, "...")
  if (requireNamespace(pkg, quietly = TRUE)) {
    cat(" OK\n")
  } else {
    cat(" Not installed\n")
  }
}

if (all_essential_ok) {
  cat("\n✓ Essential packages are ready! The basic app should work.\n")
} else {
  cat("\n✗ Some essential packages are missing. Try running the setup again.\n")
}

cat("\n=== Installation Complete ===\n")
cat("HaugShape v2 is ready to use!\n\n")

cat("To start the application, run:\n")
cat("  source('launch.R')\n\n")

cat("To check package status later, run:\n")
cat("  source('R/install_packages.R')\n")
cat("  check_haugshape_packages()\n\n")