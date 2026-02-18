# Test Script for Morph Shapes Module
# This script helps verify that the morph shapes feature is working correctly

# Load the package
library(HaugShapeV2)

cat("Testing Morph Shapes Module\n")
cat("===========================\n\n")

# 1. Check required packages
cat("1. Checking required packages...\n")
required_pkgs <- c("shiny", "shinydashboard", "magick", "imager", "shinyFiles")
missing_pkgs <- c()

for (pkg in required_pkgs) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    cat("  ✓", pkg, "is installed\n")
  } else {
    cat("  ✗", pkg, "is NOT installed\n")
    missing_pkgs <- c(missing_pkgs, pkg)
  }
}

if (length(missing_pkgs) > 0) {
  cat("\n⚠ Missing packages detected. Install them with:\n")
  cat("  install.packages(c(", paste0("'", missing_pkgs, "'", collapse = ", "), "))\n\n")
} else {
  cat("\n✓ All required packages are installed!\n\n")
}

# 2. Check exported functions
cat("2. Checking exported functions...\n")
exported_funcs <- c("morph_shapes_ui", "morph_shapes_server", "morph_shapes", "split_image")

for (func in exported_funcs) {
  if (exists(func, where = asNamespace("HaugShapeV2"), mode = "function")) {
    cat("  ✓", func, "is exported\n")
  } else {
    cat("  ✗", func, "is NOT exported\n")
  }
}

cat("\n")

# 3. Test creating output directory
cat("3. Testing file system access...\n")
test_dir <- file.path(tempdir(), "morph_test")
tryCatch({
  dir.create(test_dir, recursive = TRUE, showWarnings = FALSE)
  if (dir.exists(test_dir)) {
    cat("  ✓ Can create directories\n")
    unlink(test_dir, recursive = TRUE)
  } else {
    cat("  ✗ Cannot create directories\n")
  }
}, error = function(e) {
  cat("  ✗ Error creating directories:", e$message, "\n")
})

cat("\n")

# 4. Test basic split_image functionality (if magick is available)
cat("4. Testing split_image function...\n")
if (requireNamespace("magick", quietly = TRUE)) {
  tryCatch({
    # Create a simple test image
    test_img <- magick::image_blank(100, 100, color = "white")
    test_img <- magick::image_annotate(test_img, "TEST", size = 30, color = "black")
    
    test_input <- file.path(tempdir(), "test_image.png")
    test_output <- file.path(tempdir(), "split_output")
    
    magick::image_write(test_img, test_input)
    
    # Test split function
    result <- split_image(
      input_paths = test_input,
      output_dir = test_output,
      split_options = list(direction = "vertical", split_position = 0.5),
      verbose = FALSE
    )
    
    if (result$summary$successful_count == 1) {
      cat("  ✓ split_image works correctly\n")
    } else {
      cat("  ✗ split_image failed\n")
    }
    
    # Clean up
    unlink(test_input)
    unlink(test_output, recursive = TRUE)
    
  }, error = function(e) {
    cat("  ✗ Error testing split_image:", e$message, "\n")
  })
} else {
  cat("  ⚠ Skipping (magick not installed)\n")
}

cat("\n")

# 5. Test basic morph_shapes functionality (if imager is available)
cat("5. Testing morph_shapes function...\n")
if (requireNamespace("imager", quietly = TRUE) && requireNamespace("magick", quietly = TRUE)) {
  tryCatch({
    # Create two simple test images
    test_img1 <- magick::image_blank(50, 50, color = "black")
    test_img1 <- magick::image_draw(test_img1)
    graphics::rect(10, 10, 40, 40, col = "white", border = NA)
    dev.off()
    
    test_img2 <- magick::image_blank(50, 50, color = "black")
    test_img2 <- magick::image_draw(test_img2)
    graphics::points(25, 25, pch = 19, col = "white", cex = 3)
    dev.off()
    
    test_input1 <- file.path(tempdir(), "morph_test1.png")
    test_input2 <- file.path(tempdir(), "morph_test2.png")
    test_output <- file.path(tempdir(), "morph_output")
    
    magick::image_write(test_img1, test_input1)
    magick::image_write(test_img2, test_input2)
    
    # Test morph function
    result <- morph_shapes(
      input_paths = c(test_input1, test_input2),
      output_dir = test_output,
      morphing_options = list(method = "linear", n_steps = 2),
      verbose = FALSE
    )
    
    if (length(result$morphed_images) == 2) {
      cat("  ✓ morph_shapes works correctly\n")
    } else {
      cat("  ✗ morph_shapes failed\n")
    }
    
    # Clean up
    unlink(test_input1)
    unlink(test_input2)
    unlink(test_output, recursive = TRUE)
    
  }, error = function(e) {
    cat("  ✗ Error testing morph_shapes:", e$message, "\n")
  })
} else {
  cat("  ⚠ Skipping (imager and/or magick not installed)\n")
}

cat("\n")

# Final summary
cat("===========================\n")
cat("Testing Complete!\n\n")

if (length(missing_pkgs) == 0) {
  cat("✓ Your installation looks good!\n")
  cat("\nYou can now launch the app with:\n")
  cat("  run_haug_app()\n\n")
  cat("Then navigate to the 'Morph Shapes' tab.\n")
} else {
  cat("⚠ Please install missing packages before using the morph shapes feature.\n")
}

cat("\nFor detailed usage instructions, see: MORPH_SHAPES_GUIDE.md\n")
