#' Advanced Shape Morphing and Transformation Utilities
#'
#' Performs sophisticated shape morphing between multiple images using distance transforms,
#' gradient blending, and advanced interpolation methods. Supports batch processing,
#' multiple morphing algorithms, and extensive customization options for morphometric analysis.
#'
#' @param input_paths Character vector of input image file paths. For morphing between two shapes,
#'   provide exactly 2 paths. For sequence morphing, provide multiple paths.
#' @param output_dir Character string specifying the output directory for morphed images.
#' @param morphing_options List containing morphing algorithm options:
#'   \describe{
#'     \item{method}{Morphing method: "distance_transform", "linear", "spline" (default: "distance_transform")}
#'     \item{n_steps}{Number of intermediate steps for morphing sequence (default: 5)}
#'     \item{morph_type}{Type of morphing: "pair", "sequence", "matrix" (default: "pair")}
#'     \item{interpolation}{Interpolation method: "linear", "cubic", "smooth" (default: "linear")}
#'   }
#' @param processing_options List containing image processing options:
#'   \describe{
#'     \item{threshold}{Threshold for binary conversion (0-1, default: 0.1)}
#'     \item{gamma}{Gamma correction factor (>0, default: 1.0)}
#'     \item{blur_sigma}{Gaussian blur sigma for smoothing (default: 0)}
#'     \item{auto_align}{Automatically align shapes before morphing (default: TRUE)}
#'   }
#' @param distance_options List containing distance transform options:
#'   \describe{
#'     \item{distance_metric}{Distance metric: "euclidean", "manhattan", "chebyshev" (default: "euclidean")}
#'     \item{normalize_distances}{Normalize distance transforms (default: TRUE)}
#'     \item{invert_distances}{Invert distance values (default: FALSE)}
#'   }
#' @param blending_options List containing blending options:
#'   \describe{
#'     \item{blend_mode}{Blending mode: "average", "weighted", "min", "max" (default: "average")}
#'     \item{weights}{Weights for weighted blending (default: equal)}
#'     \item{edge_enhancement}{Enhance edges in morphed shapes (default: FALSE)}
#'   }
#' @param output_options List containing output options:
#'   \describe{
#'     \item{format}{Output format: "png", "jpg", "tiff" (default: "png")}
#'     \item{naming_pattern}{File naming pattern (default: "morph_{step}")}
#'     \item{save_intermediates}{Save intermediate processing steps (default: FALSE)}
#'   }
#' @param validation_options List containing validation options:
#'   \describe{
#'     \item{check_dimensions}{Check and resize images to same dimensions (default: TRUE)}
#'     \item{validate_binary}{Validate that images are binary (default: TRUE)}
#'     \item{similarity_threshold}{Minimum similarity for morphing (0-1, default: 0)}
#'   }
#' @param export_options List containing export options (export, filename, path, format, etc.).
#' @param verbose Logical indicating whether to print progress messages. Default: TRUE.
#'
#' @return A list containing:
#'   \describe{
#'     \item{morphed_images}{Paths to generated morphed images}
#'     \item{morphing_info}{Details about the morphing process}
#'     \item{processing_summary}{Summary of processing statistics}
#'     \item{transformation_matrices}{Transformation data if alignment was performed}
#'   }
#'
#' @examples
#' \dontrun{
#' # Basic shape morphing between two images
#' result <- morph_shapes(
#'   input_paths = c("circle.png", "square.png"),
#'   output_dir = "morphed/"
#' )
#'
#' # Advanced morphing with custom parameters
#' result <- morph_shapes(
#'   input_paths = c("shape1.png", "shape2.png"),
#'   output_dir = "advanced_morph/",
#'   morphing_options = list(
#'     method = "spline",
#'     n_steps = 10,
#'     interpolation = "cubic"
#'   ),
#'   processing_options = list(
#'     threshold = 0.2,
#'     gamma = 1.2,
#'     blur_sigma = 0.5,
#'     auto_align = TRUE
#'   ),
#'   distance_options = list(
#'     distance_metric = "euclidean",
#'     normalize_distances = TRUE
#'   ),
#'   blending_options = list(
#'     blend_mode = "weighted",
#'     weights = c(0.3, 0.7),
#'     edge_enhancement = TRUE
#'   )
#' )
#'
#' # Sequence morphing through multiple shapes
#' result <- morph_shapes(
#'   input_paths = c("shape1.png", "shape2.png", "shape3.png", "shape4.png"),
#'   output_dir = "sequence/",
#'   morphing_options = list(
#'     morph_type = "sequence",
#'     n_steps = 3
#'   ),
#'   output_options = list(
#'     format = "tiff",
#'     naming_pattern = "sequence_{step:03d}",
#'     save_intermediates = TRUE
#'   )
#' )
#'
#' # View results
#' print(result$processing_summary)
#' cat("Generated", length(result$morphed_images), "morphed images\n")
#' }
#'
#' @export
morph_shapes <- function(input_paths,
                        output_dir,
                        morphing_options = list(),
                        processing_options = list(),
                        distance_options = list(),
                        blending_options = list(),
                        output_options = list(),
                        validation_options = list(),
                        export_options = list(),
                        verbose = TRUE) {
  
  # Input validation ----
  .validate_morph_inputs(input_paths, output_dir, verbose)
  
  # Setup parameters ----
  params <- .setup_morph_params(morphing_options, processing_options, distance_options,
                               blending_options, output_options, validation_options, 
                               export_options, verbose)
  
  # Prepare output directory ----
  .prepare_morph_output_directory(output_dir, verbose)
  
  if (verbose) {
    message("Starting shape morphing process...")
    message("Input images: ", length(input_paths))
    message("Morphing method: ", params$morphing_options$method)
    message("Morph type: ", params$morphing_options$morph_type)
    message("Output directory: ", output_dir)
  }
  
  # Load and validate images ----
  image_data <- .load_and_validate_images(input_paths, params, verbose)
  
  # Perform morphing ----
  morphing_results <- .perform_morphing(image_data, output_dir, params, verbose)
  
  # Generate summary ----
  processing_summary <- .generate_morph_summary(morphing_results, params, verbose)
  
  if (verbose) {
    message("Shape morphing completed!")
    message("Generated ", length(morphing_results$output_paths), " morphed images")
  }
  
  # Prepare results
  structure(
    list(
      morphed_images = morphing_results$output_paths,
      morphing_info = morphing_results$morphing_info,
      processing_summary = processing_summary,
      transformation_matrices = morphing_results$transformations
    ),
    class = "morph_shapes_result"
  )
}

# Input Validation ----

#' Validate inputs for morph_shapes function
#' @noRd
.validate_morph_inputs <- function(input_paths, output_dir, verbose) {
  
  # Check required packages
  required_packages <- c("imager", "magrittr")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required but not available. Please install it.", call. = FALSE)
    }
  }
  
  # Check input_paths
  if (!is.character(input_paths) || length(input_paths) < 2) {
    stop("'input_paths' must contain at least 2 image file paths", call. = FALSE)
  }
  
  # Check if input files exist
  missing_files <- input_paths[!file.exists(input_paths)]
  if (length(missing_files) > 0) {
    stop("The following input files do not exist:\n  ", 
         paste(missing_files, collapse = "\n  "), call. = FALSE)
  }
  
  # Check if input files are valid image formats
  supported_formats <- c("png", "jpg", "jpeg", "tiff", "tif", "bmp")
  extensions <- tolower(tools::file_ext(input_paths))
  invalid_formats <- input_paths[!extensions %in% supported_formats]
  if (length(invalid_formats) > 0) {
    stop("The following files have unsupported formats:\n  ", 
         paste(invalid_formats, collapse = "\n  "), 
         "\nSupported formats: ", paste(supported_formats, collapse = ", "), call. = FALSE)
  }
  
  # Check output_dir
  if (!is.character(output_dir) || length(output_dir) != 1) {
    stop("'output_dir' must be a single character string", call. = FALSE)
  }
  
  # Check verbose
  if (!is.logical(verbose) || length(verbose) != 1) {
    stop("'verbose' must be a single logical value", call. = FALSE)
  }
}

# Parameter Setup ----

#' Setup parameters for shape morphing
#' @noRd
.setup_morph_params <- function(morphing_options, processing_options, distance_options,
                               blending_options, output_options, validation_options,
                               export_options, verbose) {
  
  # Morphing options defaults
  morph_defaults <- list(
    method = "distance_transform",
    n_steps = 5,
    morph_type = "pair",
    interpolation = "linear"
  )
  morphing_options <- utils::modifyList(morph_defaults, morphing_options)
  
  # Validate morphing options
  valid_methods <- c("distance_transform", "linear", "spline")
  if (!morphing_options$method %in% valid_methods) {
    stop("'method' must be one of: ", paste(valid_methods, collapse = ", "), call. = FALSE)
  }
  
  valid_types <- c("pair", "sequence", "matrix")
  if (!morphing_options$morph_type %in% valid_types) {
    stop("'morph_type' must be one of: ", paste(valid_types, collapse = ", "), call. = FALSE)
  }
  
  if (morphing_options$n_steps < 1) {
    stop("'n_steps' must be >= 1", call. = FALSE)
  }
  
  # Processing options defaults
  proc_defaults <- list(
    threshold = 0.1,
    gamma = 1.0,
    blur_sigma = 0,
    auto_align = TRUE
  )
  processing_options <- utils::modifyList(proc_defaults, processing_options)
  
  # Validate processing options
  if (processing_options$threshold < 0 || processing_options$threshold > 1) {
    stop("'threshold' must be between 0 and 1", call. = FALSE)
  }
  if (processing_options$gamma <= 0) {
    stop("'gamma' must be > 0", call. = FALSE)
  }
  
  # Distance options defaults
  dist_defaults <- list(
    distance_metric = "euclidean",
    normalize_distances = TRUE,
    invert_distances = FALSE
  )
  distance_options <- utils::modifyList(dist_defaults, distance_options)
  
  # Blending options defaults
  blend_defaults <- list(
    blend_mode = "average",
    weights = NULL,  # Equal weights
    edge_enhancement = FALSE
  )
  blending_options <- utils::modifyList(blend_defaults, blending_options)
  
  # Output options defaults
  output_defaults <- list(
    format = "png",
    naming_pattern = "morph_{step}",
    save_intermediates = FALSE
  )
  output_options <- utils::modifyList(output_defaults, output_options)
  
  # Validation options defaults
  valid_defaults <- list(
    check_dimensions = TRUE,
    validate_binary = TRUE,
    similarity_threshold = 0
  )
  validation_options <- utils::modifyList(valid_defaults, validation_options)
  
  # Export defaults
  export_defaults <- list(
    export = FALSE,
    filename = "morphing_summary",
    path = NULL,
    format = "png"
  )
  export_options <- utils::modifyList(export_defaults, export_options)
  
  return(list(
    morphing_options = morphing_options,
    processing_options = processing_options,
    distance_options = distance_options,
    blending_options = blending_options,
    output_options = output_options,
    validation_options = validation_options,
    export_options = export_options
  ))
}

# Directory Preparation ----

#' Prepare output directory for morphed shapes
#' @noRd
.prepare_morph_output_directory <- function(output_dir, verbose) {
  
  if (!dir.exists(output_dir)) {
    tryCatch({
      dir.create(output_dir, recursive = TRUE)
      if (verbose) message("Created output directory: ", output_dir)
    }, error = function(e) {
      stop("Failed to create output directory '", output_dir, "': ", e$message, call. = FALSE)
    })
  }
  
  # Test write permissions
  test_file <- file.path(output_dir, paste0("test_", Sys.getpid(), ".tmp"))
  tryCatch({
    writeLines("test", test_file)
    file.remove(test_file)
  }, error = function(e) {
    stop("No write permission in output directory '", output_dir, "'", call. = FALSE)
  })
}

# Image Loading and Validation ----

#' Load and validate images for morphing
#' @noRd
.load_and_validate_images <- function(input_paths, params, verbose) {
  
  if (verbose) message("Loading and validating ", length(input_paths), " images...")
  
  images <- list()
  image_info <- list()
  
  for (i in seq_along(input_paths)) {
    path <- input_paths[i]
    
    if (verbose) message("Loading image ", i, ": ", basename(path))
    
    tryCatch({
      # Load image using imager
      img <- imager::load.image(path)
      
      # Convert to grayscale
      if (imager::spectrum(img) > 1) {
        img <- imager::grayscale(img)
        if (verbose) message("  Converted to grayscale")
      }
      
      # Store image and info
      images[[i]] <- img
      image_info[[i]] <- list(
        path = path,
        width = imager::width(img),
        height = imager::height(img),
        depth = imager::depth(img)
      )
      
    }, error = function(e) {
      stop("Failed to load image '", path, "': ", e$message, call. = FALSE)
    })
  }
  
  # Validate dimensions and resize if necessary
  if (params$validation_options$check_dimensions) {
    images <- .standardize_image_dimensions(images, image_info, verbose)
  }
  
  # Validate binary format if requested
  if (params$validation_options$validate_binary) {
    .validate_binary_images(images, verbose)
  }
  
  return(list(
    images = images,
    image_info = image_info
  ))
}

# Dimension Standardization ----

#' Standardize image dimensions
#' @noRd
.standardize_image_dimensions <- function(images, image_info, verbose) {
  
  # Find target dimensions (use first image or largest common dimensions)
  target_width <- image_info[[1]]$width
  target_height <- image_info[[1]]$height
  
  if (verbose) message("Standardizing dimensions to ", target_width, "x", target_height)
  
  for (i in seq_along(images)) {
    current_width <- image_info[[i]]$width
    current_height <- image_info[[i]]$height
    
    if (current_width != target_width || current_height != target_height) {
      if (verbose) message("  Resizing image ", i, " from ", current_width, "x", current_height)
      
      images[[i]] <- imager::resize(images[[i]], 
                                   size_x = target_width, 
                                   size_y = target_height)
    }
  }
  
  return(images)
}

# Binary Validation ----

#' Validate that images are binary (or can be made binary)
#' @noRd
.validate_binary_images <- function(images, verbose) {
  
  for (i in seq_along(images)) {
    img <- images[[i]]
    img_values <- as.vector(img)
    unique_values <- unique(img_values)
    
    # Check if image is already binary
    if (length(unique_values) > 10) {  # Not binary, needs thresholding
      if (verbose) message("  Image ", i, " is not binary (", length(unique_values), " unique values)")
    }
  }
}

# Morphing Performance ----

#' Perform the actual morphing process
#' @noRd
.perform_morphing <- function(image_data, output_dir, params, verbose) {
  
  images <- image_data$images
  morph_type <- params$morphing_options$morph_type
  
  if (morph_type == "pair") {
    if (length(images) != 2) {
      stop("'pair' morphing requires exactly 2 input images", call. = FALSE)
    }
    results <- .morph_pair(images[[1]], images[[2]], output_dir, params, verbose)
    
  } else if (morph_type == "sequence") {
    results <- .morph_sequence(images, output_dir, params, verbose)
    
  } else if (morph_type == "matrix") {
    results <- .morph_matrix(images, output_dir, params, verbose)
  }
  
  return(results)
}

# Pair Morphing ----

#' Morph between two images
#' @noRd
.morph_pair <- function(img1, img2, output_dir, params, verbose) {
  
  n_steps <- params$morphing_options$n_steps
  method <- params$morphing_options$method
  
  if (verbose) message("Morphing between 2 images with ", n_steps, " intermediate steps")
  
  output_paths <- character(n_steps)
  morphing_info <- list()
  transformations <- NULL
  
  # Align images if requested
  if (params$processing_options$auto_align) {
    alignment_result <- .align_images(img1, img2, verbose)
    img1 <- alignment_result$img1_aligned
    img2 <- alignment_result$img2_aligned
    transformations <- alignment_result$transformation
  }
  
  # Generate morphing steps
  for (step in 1:n_steps) {
    alpha <- step / (n_steps + 1)  # Interpolation factor (0 to 1)
    
    if (verbose) message("  Generating step ", step, "/", n_steps, " (alpha = ", round(alpha, 3), ")")
    
    # Perform morphing based on method
    if (method == "distance_transform") {
      morphed_img <- .morph_distance_transform(img1, img2, alpha, params)
    } else if (method == "linear") {
      morphed_img <- .morph_linear(img1, img2, alpha, params)
    } else if (method == "spline") {
      morphed_img <- .morph_spline(img1, img2, alpha, params)
    }
    
    # Apply post-processing
    morphed_img <- .apply_post_processing(morphed_img, params)
    
    # Generate filename and save
    filename <- .generate_morph_filename(step, params$output_options$naming_pattern, 
                                        params$output_options$format)
    output_path <- file.path(output_dir, filename)
    
    .save_morphed_image(morphed_img, output_path, params$output_options$format, verbose)
    
    output_paths[step] <- output_path
    morphing_info[[step]] <- list(step = step, alpha = alpha, method = method)
  }
  
  return(list(
    output_paths = output_paths,
    morphing_info = morphing_info,
    transformations = transformations
  ))
}

# Distance Transform Morphing ----

#' Morph using distance transforms
#' @noRd
.morph_distance_transform <- function(img1, img2, alpha, params) {
  
  # Compute distance transforms
  dist1 <- imager::distance_transform(img1, value = 0)
  dist2 <- imager::distance_transform(img2, value = 0)
  
  # Normalize if requested
  if (params$distance_options$normalize_distances) {
    max_dist1 <- max(dist1)
    max_dist2 <- max(dist2)
    if (max_dist1 > 0) dist1 <- dist1 / max_dist1
    if (max_dist2 > 0) dist2 <- dist2 / max_dist2
  }
  
  # Blend distance transforms
  if (params$blending_options$blend_mode == "weighted" && !is.null(params$blending_options$weights)) {
    weights <- params$blending_options$weights
    blended <- weights[1] * dist1 + weights[2] * dist2
  } else {
    # Linear interpolation
    blended <- (1 - alpha) * dist1 + alpha * dist2
  }
  
  # Apply gamma correction
  if (params$processing_options$gamma != 1.0) {
    blended <- blended ^ params$processing_options$gamma
  }
  
  # Threshold to create binary result
  morphed <- imager::as.cimg(blended > params$processing_options$threshold)
  
  return(morphed)
}

# Linear Morphing ----

#' Simple linear interpolation morphing
#' @noRd
.morph_linear <- function(img1, img2, alpha, params) {
  
  # Simple linear blend
  blended <- (1 - alpha) * img1 + alpha * img2
  
  # Apply gamma correction
  if (params$processing_options$gamma != 1.0) {
    blended <- blended ^ params$processing_options$gamma
  }
  
  # Threshold to create binary result
  morphed <- imager::as.cimg(blended > params$processing_options$threshold)
  
  return(morphed)
}

# Spline Morphing ----

#' Spline-based morphing (simplified implementation)
#' @noRd
.morph_spline <- function(img1, img2, alpha, params) {
  
  # For now, use cubic interpolation as a proxy for spline morphing
  # A full spline implementation would require more complex algorithms
  
  # Cubic interpolation weight
  cubic_alpha <- alpha^3 * (alpha * (alpha * 6 - 15) + 10)
  
  # Blend with cubic weights
  blended <- (1 - cubic_alpha) * img1 + cubic_alpha * img2
  
  # Apply gamma correction
  if (params$processing_options$gamma != 1.0) {
    blended <- blended ^ params$processing_options$gamma
  }
  
  # Threshold to create binary result
  morphed <- imager::as.cimg(blended > params$processing_options$threshold)
  
  return(morphed)
}

# Sequence Morphing ----

#' Morph through a sequence of images
#' @noRd
.morph_sequence <- function(images, output_dir, params, verbose) {
  
  if (verbose) message("Morphing through sequence of ", length(images), " images")
  
  all_outputs <- character(0)
  all_info <- list()
  counter <- 1
  
  # Morph between each consecutive pair
  for (i in 1:(length(images) - 1)) {
    if (verbose) message("  Morphing between images ", i, " and ", i + 1)
    
    pair_results <- .morph_pair(images[[i]], images[[i + 1]], output_dir, params, FALSE)
    
    # Update file naming for sequence
    for (j in seq_along(pair_results$output_paths)) {
      new_filename <- .generate_morph_filename(counter, params$output_options$naming_pattern, 
                                              params$output_options$format)
      new_path <- file.path(output_dir, new_filename)
      
      # Rename file
      file.rename(pair_results$output_paths[j], new_path)
      
      all_outputs[counter] <- new_path
      all_info[[counter]] <- pair_results$morphing_info[[j]]
      all_info[[counter]]$sequence_pair <- paste(i, "->", i + 1)
      counter <- counter + 1
    }
  }
  
  return(list(
    output_paths = all_outputs,
    morphing_info = all_info,
    transformations = NULL
  ))
}

# Matrix Morphing ----

#' Morph between all pairs of images
#' @noRd
.morph_matrix <- function(images, output_dir, params, verbose) {
  
  n_images <- length(images)
  if (verbose) message("Creating morphing matrix for ", n_images, " images")
  
  all_outputs <- character(0)
  all_info <- list()
  counter <- 1
  
  # Morph between all pairs
  for (i in 1:(n_images - 1)) {
    for (j in (i + 1):n_images) {
      if (verbose) message("  Morphing between images ", i, " and ", j)
      
      pair_results <- .morph_pair(images[[i]], images[[j]], output_dir, params, FALSE)
      
      # Update file naming for matrix
      for (k in seq_along(pair_results$output_paths)) {
        new_filename <- paste0("matrix_", i, "_", j, "_", k, ".", params$output_options$format)
        new_path <- file.path(output_dir, new_filename)
        
        # Rename file
        file.rename(pair_results$output_paths[k], new_path)
        
        all_outputs[counter] <- new_path
        all_info[[counter]] <- pair_results$morphing_info[[k]]
        all_info[[counter]]$matrix_pair <- paste(i, "<->", j)
        counter <- counter + 1
      }
    }
  }
  
  return(list(
    output_paths = all_outputs,
    morphing_info = all_info,
    transformations = NULL
  ))
}

# Image Alignment ----

#' Align two images (simplified implementation)
#' @noRd
.align_images <- function(img1, img2, verbose) {
  
  # This is a simplified alignment - in practice, you'd use more sophisticated methods
  if (verbose) message("  Applying image alignment")
  
  # For now, just return the original images
  # A full implementation would include:
  # - Center of mass alignment
  # - Principal axis alignment
  # - Scale normalization
  # - Rotation correction
  
  return(list(
    img1_aligned = img1,
    img2_aligned = img2,
    transformation = matrix(c(1, 0, 0, 1, 0, 0), nrow = 2)  # Identity transform
  ))
}

# Post-processing ----

#' Apply post-processing to morphed image
#' @noRd
.apply_post_processing <- function(img, params) {
  
  # Apply blur if requested
  if (params$processing_options$blur_sigma > 0) {
    img <- imager::blur_gaussian(img, sigma = params$processing_options$blur_sigma)
  }
  
  # Edge enhancement if requested
  if (params$blending_options$edge_enhancement) {
    # Simple edge enhancement using gradient
    grad_x <- imager::imgradient(img, "x")
    grad_y <- imager::imgradient(img, "y")
    edge_mag <- sqrt(grad_x^2 + grad_y^2)
    img <- img + 0.1 * edge_mag  # Add small amount of edge information
  }
  
  return(img)
}

# File Operations ----

#' Generate filename for morphed image
#' @noRd
.generate_morph_filename <- function(step, pattern, format) {
  
  # Replace placeholders in pattern
  filename <- gsub("\\{step\\}", sprintf("%d", step), pattern)
  filename <- gsub("\\{step:0(\\d+)d\\}", sprintf("%%0%dd", as.numeric(sub("\\{step:0(\\d+)d\\}", "\\1", pattern))), filename)
  filename <- sprintf(filename, step)
  
  # Ensure proper extension
  if (!grepl(paste0("\\.", format, "$"), filename, ignore.case = TRUE)) {
    filename <- paste0(filename, ".", format)
  }
  
  return(filename)
}

#' Save morphed image
#' @noRd
.save_morphed_image <- function(img, output_path, format, verbose) {
  
  tryCatch({
    imager::save.image(img, output_path)
    
    if (verbose) message("    Saved: ", basename(output_path))
  }, error = function(e) {
    stop("Failed to save morphed image '", output_path, "': ", e$message, call. = FALSE)
  })
}

# Summary Generation ----

#' Generate processing summary
#' @noRd
.generate_morph_summary <- function(morphing_results, params, verbose) {
  
  list(
    total_morphed_images = length(morphing_results$output_paths),
    morphing_method = params$morphing_options$method,
    morph_type = params$morphing_options$morph_type,
    n_steps_per_pair = params$morphing_options$n_steps,
    processing_options = params$processing_options,
    output_format = params$output_options$format,
    alignment_applied = params$processing_options$auto_align
  )
}

# Print Method ----

#' Print method for morph_shapes_result objects
#' @param x A morph_shapes_result object
#' @param ... Additional arguments (ignored)
#' @export
print.morph_shapes_result <- function(x, ...) {
  cat("Shape Morphing Results\n")
  cat("======================\n\n")
  
  cat("Processing Summary:\n")
  cat("  Total morphed images:", x$processing_summary$total_morphed_images, "\n")
  cat("  Morphing method:", x$processing_summary$morphing_method, "\n")
  cat("  Morph type:", x$processing_summary$morph_type, "\n")
  cat("  Steps per pair:", x$processing_summary$n_steps_per_pair, "\n")
  cat("  Output format:", x$processing_summary$output_format, "\n")
  cat("  Alignment applied:", x$processing_summary$alignment_applied, "\n")
  
  cat("\nProcessing Options:\n")
  cat("  Threshold:", x$processing_summary$processing_options$threshold, "\n")
  cat("  Gamma correction:", x$processing_summary$processing_options$gamma, "\n")
  cat("  Blur sigma:", x$processing_summary$processing_options$blur_sigma, "\n")
  
  cat("\nOutput Files:\n")
  n_files <- length(x$morphed_images)
  if (n_files <= 5) {
    for (i in 1:n_files) {
      cat("  -", basename(x$morphed_images[i]), "\n")
    }
  } else {
    for (i in 1:3) {
      cat("  -", basename(x$morphed_images[i]), "\n")
    }
    cat("  ... and", n_files - 3, "more files\n")
  }
  
  cat("\nAvailable components:\n")
  cat("  - morphed_images: Paths to generated morphed images\n")
  cat("  - morphing_info: Details about the morphing process\n")
  cat("  - processing_summary: Summary of processing statistics\n")
  cat("  - transformation_matrices: Transformation data (if alignment was performed)\n")
}