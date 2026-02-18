# HaugShape Package Development Script
# This script helps you build, check, and install your package

# Load required libraries for development
library(devtools)
library(roxygen2)
library(usethis)

# Set working directory to package root
setwd("C:/Projects/HaugShape_v2")

# 1. Document the package (generates man files and updates NAMESPACE)
cat("Documenting package...\n")
document()

# 2. Check the package for errors
cat("Checking package...\n")
check()

# 3. Build the package
cat("Building package...\n")
build()

# 4. Install the package locally
cat("Installing package locally...\n")
install()

# 5. Load the package for testing
cat("Loading HaugShape package...\n")
library(HaugShape)

# Test basic functionality
cat("Testing basic functions...\n")
try({
  # Test if run_app function exists
  if(exists("run_app")) {
    cat("✓ run_app function found\n")
  }
  
  # You can add more tests here as you develop functions
  cat("✓ Package loaded successfully!\n")
})

cat("\n=== Development Workflow ===\n")
cat("1. Add your functions to the R/ directory\n")
cat("2. Run devtools::document() to update documentation\n") 
cat("3. Run devtools::check() to check for errors\n")
cat("4. Run devtools::install() to install locally\n")
cat("5. Test your functions\n")
cat("6. Repeat!\n\n")

cat("To install from GitHub later:\n")
cat("devtools::install_github('TestoKlaus/HaugShape_v2')\n")