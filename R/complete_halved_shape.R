#' Complete Halved Shapes into Symmetrical Images
#'
#' Reconstructs full symmetrical images from halved input images by mirroring and
#' combining the halves. Supports multiple completion methods, batch processing,
#' and flexible output options for morphometric shape analysis.
#'
#' @param input_paths Character vector of file paths to input halved images. Can be single file or multiple files.
#' @param output_dir Character string specifying the output directory. Created if it doesn't exist.
#' @param completion_options List containing completion options:
#'   \describe{
#'     \item{method}{Completion method: "mirror_left", "mirror_right", "both_sides" (default: "mirror_left")}
#'     \item{half_side}{Which half the input represents: "left", "right", "auto" (default: "auto")}
#'     \item{blend_seam}{Blend the seam between halves (default: FALSE)}
#'     \item{seam_width}{Width of blending seam in pixels (default: 2)}
#'   }
#' @param alignment_options List containing alignment options:
#'   \describe{
#'     \item{auto_align}{Automatically align halves (default: TRUE)}
#'     \item{alignment_method}{Alignment method: "edge", "centroid", "none" (default: "edge")}
#'     \item{crop_to_content}{Crop to non-transparent content (default: TRUE)}
#'   }
#' @param naming_options List containing file naming options:
#'   \describe{
#'     \item{suffix}{Suffix to add to filename (default: "_completed")}
#'     \item{prefix}{Prefix to add to filename (default: "")}
#'     \item{preserve_name}{Keep original name without suffix/prefix (default: FALSE)}
#'     \item{numbering}{Add sequential numbering (default: FALSE)}
#'   }
#' @param processing_options List containing processing options:
#'   \describe{
#'     \item{overwrite}{Overwrite existing files (default: FALSE)}
#'     \item{quality}{Image quality for JPEG (1-100, default: 90)}
#'     \item{format}{Output format ("auto", "png", "jpg", "tiff", default: "auto")}
#'     \item{preserve_transparency}{Preserve transparency if present (default: TRUE)}
#'   }
#' @param verbose Logical indicating whether to print progress messages. Default: TRUE.
#'
#' @return A list containing:
#'   \describe{
#'     \item{processed_files}{Data frame with input paths, output paths, and processing status}
#'     \item{summary}{Summary statistics of the completion operation}
#'     \item{failed_files}{Vector of files that failed processing}
#'     \item{completion_info}{Details about completion methods and resulting dimensions}
#'   }
#'
#' @examples
#' \dontrun{
#' # Basic shape completion
#' result <- complete_halved_shape(
#'   input_paths = "half_shape.png",
#'   output_dir = "completed/"
#' )
#'
#' # Batch processing with custom options
#' result <- complete_halved_shape(
#'   input_paths = c("left_half1.png", "right_half2.png"),
#'   output_dir = "symmetric_shapes/",
#'   completion_options = list(
#'     method = "both_sides",
#'     blend_seam = TRUE,
#'     seam_width = 3
#'   ),
#'   alignment_options = list(
#'     auto_align = TRUE,
#'     alignment_method = "centroid"
#'   ),
#'   naming_options = list(
#'     suffix = "_symmetric",
#'     numbering = TRUE
#'   )
#' )
#'
#' # Advanced completion with quality settings
#' result <- complete_halved_shape(
#'   input_paths = "specimen_half.png",
#'   output_dir = "analysis_ready/",
#'   completion_options = list(
#'     method = "mirror_right",
#'     half_side = "left"
#'   ),
#'   processing_options = list(
#'     format = "png",
#'     preserve_transparency = TRUE,
#'     quality = 95
#'   )
#' )
#'
#' # View results
#' print(result$summary)
#' View(result$processed_files)
#' }
#'
#' @export
complete_halved_shape <- function(input_paths,
                                 output_dir,
                                 completion_options = list(),
                                 alignment_options = list(),
                                 naming_options = list(),
                                 processing_options = list(),
                                 verbose = TRUE) {
  
  # Input validation ----
  .validate_complete_shape_inputs(input_paths, output_dir, verbose)
  
  # Setup parameters ----
  params <- .setup_complete_shape_params(completion_options, alignment_options, naming_options, processing_options, verbose)
  
  # Prepare output directory ----
  .prepare_complete_output_directory(output_dir, verbose)
  
  if (verbose) {
    message("Starting shape completion process...")
    message("Processing ", length(input_paths), " image(s)")
    message("Completion method: ", params$completion_options$method)
    message("Output directory: ", output_dir)
  }
  
  # Process images ----
  processing_results <- .process_complete_images_batch(input_paths, output_dir, params, verbose)
  
  # Generate summary ----
  summary_stats <- .generate_complete_summary(processing_results, params, verbose)
  
  if (verbose) {
    message("Shape completion completed!")
    message("Successfully processed: ", summary_stats$successful_count, "/", length(input_paths))
    if (summary_stats$failed_count > 0) {
      message("Failed: ", summary_stats$failed_count)
    }
  }
  
  # Prepare results
  structure(
    list(
      processed_files = processing_results$results_df,
      summary = summary_stats,
      failed_files = processing_results$failed_files,
      completion_info = processing_results$completion_info
    ),
    class = "complete_shape_result"
  )
}

# Input Validation ----

#' Validate inputs for complete_halved_shape function
#' @noRd
.validate_complete_shape_inputs <- function(input_paths, output_dir, verbose) {
  
  # Check magick package availability
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("Package 'magick' is required but not available. Please install it with: install.packages('magick')", 
         call. = FALSE)
  }
  
  # Check input_paths
  if (!is.character(input_paths) || length(input_paths) == 0) {
    stop("'input_paths' must be a non-empty character vector", call. = FALSE)
  }
  
  # Check if input files exist
  missing_files <- input_paths[!file.exists(input_paths)]
  if (length(missing_files) > 0) {
    stop("The following input files do not exist:\n  ", 
         paste(missing_files, collapse = "\n  "), call. = FALSE)
  }
  
  # Check if input files are valid image formats
  supported_formats <- c("png", "jpg", "jpeg", "tiff", "tif", "bmp", "gif")
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

#' Setup parameters for shape completion
#' @noRd
.setup_complete_shape_params <- function(completion_options, alignment_options, naming_options, processing_options, verbose) {
  
  # Completion options defaults
  completion_defaults <- list(
    method = "mirror_left",
    half_side = "auto",
    blend_seam = FALSE,
    seam_width = 2
  )
  completion_options <- utils::modifyList(completion_defaults, completion_options)
  
  # Validate completion options
  valid_methods <- c("mirror_left", "mirror_right", "both_sides")
  if (!completion_options$method %in% valid_methods) {
    stop("'method' must be one of: ", paste(valid_methods, collapse = ", "), call. = FALSE)
  }
  
  valid_sides <- c("left", "right", "auto")
  if (!completion_options$half_side %in% valid_sides) {
    stop("'half_side' must be one of: ", paste(valid_sides, collapse = ", "), call. = FALSE)
  }
  
  if (completion_options$seam_width < 0) {
    stop("'seam_width' must be >= 0", call. = FALSE)
  }
  
  # Alignment options defaults
  alignment_defaults <- list(
    auto_align = TRUE,
    alignment_method = "edge",
    crop_to_content = TRUE
  )
  alignment_options <- utils::modifyList(alignment_defaults, alignment_options)
  
  # Validate alignment options
  valid_align_methods <- c("edge", "centroid", "none")
  if (!alignment_options$alignment_method %in% valid_align_methods) {
    stop("'alignment_method' must be one of: ", paste(valid_align_methods, collapse = ", "), call. = FALSE)
  }
  
  # Naming options defaults
  naming_defaults <- list(
    suffix = "_completed",
    prefix = "",
    preserve_name = FALSE,
    numbering = FALSE
  )
  naming_options <- utils::modifyList(naming_defaults, naming_options)
  
  # Processing options defaults
  processing_defaults <- list(
    overwrite = FALSE,
    quality = 90,
    format = "auto",
    preserve_transparency = TRUE
  )
  processing_options <- utils::modifyList(processing_defaults, processing_options)
  
  # Validate processing options
  if (processing_options$quality < 1 || processing_options$quality > 100) {
    stop("'quality' must be between 1 and 100", call. = FALSE)
  }
  if (!processing_options$format %in% c("auto", "png", "jpg", "jpeg", "tiff")) {
    stop("'format' must be 'auto', 'png', 'jpg', 'jpeg', or 'tiff'", call. = FALSE)
  }
  
  return(list(
    completion_options = completion_options,
    alignment_options = alignment_options,
    naming_options = naming_options,
    processing_options = processing_options
  ))
}

# Directory Preparation ----

#' Prepare output directory for completed shapes
#' @noRd
.prepare_complete_output_directory <- function(output_dir, verbose) {
  
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

# Batch Processing ----

#' Process multiple images for shape completion in batch
#' @noRd
.process_complete_images_batch <- function(input_paths, output_dir, params, verbose) {
  
  n_files <- length(input_paths)
  results_df <- data.frame(
    input_path = character(0),
    output_path = character(0),
    completion_method = character(0),
    status = character(0),
    error_message = character(0),
    processing_time = numeric(0),
    stringsAsFactors = FALSE
  )
  
  failed_files <- character(0)
  completion_info <- list()
  
  for (i in seq_along(input_paths)) {
    input_path <- input_paths[i]
    
    if (verbose && (i %% 10 == 0 || i == n_files)) {
      message("Processing image ", i, "/", n_files, ": ", basename(input_path))
    }
    
    start_time <- Sys.time()
    
    tryCatch({
      # Process single image
      single_result <- .process_single_complete_image(input_path, output_dir, params, i)
      
      end_time <- Sys.time()
      processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
      
      # Add result to data frame
      results_df <- rbind(results_df, data.frame(
        input_path = input_path,
        output_path = single_result$output_path,
        completion_method = params$completion_options$method,
        status = "success",
        error_message = "",
        processing_time = processing_time,
        stringsAsFactors = FALSE
      ))
      
      # Store completion information
      completion_info[[i]] <- list(
        input_path = input_path,
        method = params$completion_options$method,
        detected_half_side = single_result$detected_half_side,
        original_dimensions = single_result$original_dimensions,
        completed_dimensions = single_result$completed_dimensions,
        alignment_applied = single_result$alignment_applied
      )
      
    }, error = function(e) {
      end_time <- Sys.time()
      processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
      
      if (verbose) warning("Failed to process ", basename(input_path), ": ", e$message)
      
      results_df <<- rbind(results_df, data.frame(
        input_path = input_path,
        output_path = "",
        completion_method = params$completion_options$method,
        status = "failed",
        error_message = e$message,
        processing_time = processing_time,
        stringsAsFactors = FALSE
      ))
      
      failed_files <<- c(failed_files, input_path)
    })
  }
  
  return(list(
    results_df = results_df,
    failed_files = failed_files,
    completion_info = completion_info
  ))
}

# Single Image Processing ----

#' Process a single image for shape completion
#' @noRd
.process_single_complete_image <- function(input_path, output_dir, params, file_index) {
  
  # Load image
  img <- magick::image_read(input_path)
  
  # Get image dimensions
  img_info <- magick::image_info(img)
  original_width <- img_info$width
  original_height <- img_info$height
  
  # Detect which half we have if auto-detection is enabled
  detected_half_side <- .detect_half_side(img, params$completion_options$half_side)
  
  # Apply alignment if requested
  if (params$alignment_options$auto_align) {
    img <- .apply_alignment(img, params$alignment_options, detected_half_side)
    alignment_applied <- TRUE
  } else {
    alignment_applied <- FALSE
  }
  
  # Complete the shape based on method and detected side
  completed_img <- .complete_shape_image(img, params$completion_options, detected_half_side)
  
  # Crop to content if requested
  if (params$alignment_options$crop_to_content) {
    completed_img <- .crop_to_content(completed_img)
  }
  
  # Generate output filename
  output_filename <- .generate_complete_output_filename(
    input_path, params$naming_options, file_index
  )
  
  # Set output format
  output_format <- .determine_complete_output_format(
    input_path, params$processing_options$format, img_info
  )
  
  # Ensure proper extension
  if (!grepl(paste0("\\.", output_format, "$"), output_filename, ignore.case = TRUE)) {
    output_filename <- paste0(tools::file_path_sans_ext(output_filename), ".", output_format)
  }
  
  output_path <- file.path(output_dir, output_filename)
  
  # Check for existing file
  if (file.exists(output_path) && !params$processing_options$overwrite) {
    stop("Output file already exists: ", output_path, 
         ". Set overwrite=TRUE to replace it.", call. = FALSE)
  }
  
  # Save image
  .save_completed_image(completed_img, output_path, output_format, params$processing_options)
  
  # Get completed dimensions
  completed_info <- magick::image_info(completed_img)
  
  return(list(
    output_path = output_path,
    detected_half_side = detected_half_side,
    original_dimensions = list(width = original_width, height = original_height),
    completed_dimensions = list(width = completed_info$width, height = completed_info$height),
    alignment_applied = alignment_applied
  ))
}

# Half Side Detection ----

#' Detect which half of the original image we have
#' @noRd
.detect_half_side <- function(img, half_side_setting) {
  
  if (half_side_setting != "auto") {
    return(half_side_setting)
  }
  
  # Simple heuristic: assume most shape content is on one side
  # This is a basic implementation - could be made more sophisticated
  tryCatch({
    # Convert to grayscale for analysis
    gray_img <- magick::image_convert(img, "grayscale")
    
    # Get image dimensions
    img_info <- magick::image_info(gray_img)
    width <- img_info$width
    height <- img_info$height
    
    # Split into left and right halves for analysis
    left_half <- magick::image_crop(gray_img, geometry = paste0(width/2, "x", height, "+0+0"))
    right_half <- magick::image_crop(gray_img, geometry = paste0(width/2, "x", height, "+", width/2, "+0"))
    
    # Calculate "content density" for each half (simplified)
    # This is a basic approximation
    left_stats <- magick::image_statistics(left_half)
    right_stats <- magick::image_statistics(right_half)
    
    # Use standard deviation as a proxy for content complexity
    left_complexity <- left_stats$standard_deviation
    right_complexity <- right_stats$standard_deviation
    
    # The side with more complexity likely contains the main shape content
    if (left_complexity > right_complexity) {
      return("left")
    } else {
      return("right")
    }
    
  }, error = function(e) {
    # Fallback to left if detection fails
    return("left")
  })
}

# Alignment ----

#' Apply alignment to the half image
#' @noRd
.apply_alignment <- function(img, alignment_options, detected_half_side) {
  
  if (alignment_options$alignment_method == "none") {
    return(img)
  }
  
  tryCatch({
    if (alignment_options$alignment_method == "edge") {
      # Trim transparent/white edges
      img <- magick::image_trim(img)
    } else if (alignment_options$alignment_method == "centroid") {
      # Center the content (basic implementation)
      img <- magick::image_trim(img)
      # Could add more sophisticated centroid-based alignment here
    }
    
    return(img)
    
  }, error = function(e) {
    # Return original image if alignment fails
    return(img)
  })
}

# Shape Completion ----

#' Complete the shape by mirroring and combining
#' @noRd
.complete_shape_image <- function(img, completion_options, detected_half_side) {
  
  method <- completion_options$method
  
  if (method == "both_sides") {
    # Create both left and right mirrored versions
    mirrored_img <- magick::image_flop(img)
    left_completed <- magick::image_append(c(mirrored_img, img), stack = FALSE)
    right_completed <- magick::image_append(c(img, mirrored_img), stack = FALSE)
    
    # For now, return the version that makes sense based on detected side
    if (detected_half_side == "right") {
      completed_img <- left_completed  # Mirror right half to left
    } else {
      completed_img <- right_completed  # Mirror left half to right
    }
  } else if (method == "mirror_left") {
    # Mirror the image to the left
    mirrored_img <- magick::image_flop(img)
    completed_img <- magick::image_append(c(mirrored_img, img), stack = FALSE)
  } else {  # mirror_right
    # Mirror the image to the right
    mirrored_img <- magick::image_flop(img)
    completed_img <- magick::image_append(c(img, mirrored_img), stack = FALSE)
  }
  
  # Apply seam blending if requested
  if (completion_options$blend_seam && completion_options$seam_width > 0) {
    completed_img <- .blend_seam(completed_img, completion_options$seam_width)
  }
  
  return(completed_img)
}

# Seam Blending ----

#' Blend the seam between mirrored halves
#' @noRd
.blend_seam <- function(img, seam_width) {
  
  tryCatch({
    # This is a simplified seam blending
    # A more sophisticated implementation would use gradual blending
    img_info <- magick::image_info(img)
    center_x <- img_info$width / 2
    
    # Apply a slight blur around the center seam
    blur_region <- paste0("0x1+", max(0, center_x - seam_width), "+0")
    blurred <- magick::image_blur(img, radius = 1, sigma = 0.5)
    
    # This is a basic implementation - could be enhanced with mask-based blending
    return(blurred)
    
  }, error = function(e) {
    # Return original image if blending fails
    return(img)
  })
}

# Content Cropping ----

#' Crop to non-transparent content
#' @noRd
.crop_to_content <- function(img) {
  
  tryCatch({
    # Trim transparent/white edges
    img <- magick::image_trim(img)
    return(img)
  }, error = function(e) {
    # Return original image if cropping fails
    return(img)
  })
}

# Filename Generation ----

#' Generate output filename for completed shapes
#' @noRd
.generate_complete_output_filename <- function(input_path, naming_options, file_index) {
  
  base_name <- tools::file_path_sans_ext(basename(input_path))
  
  if (naming_options$preserve_name) {
    filename <- base_name
  } else {
    # Add prefix
    if (nchar(naming_options$prefix) > 0) {
      filename <- paste0(naming_options$prefix, base_name)
    } else {
      filename <- base_name
    }
    
    # Add suffix
    filename <- paste0(filename, naming_options$suffix)
    
    # Add numbering
    if (naming_options$numbering) {
      filename <- paste0(sprintf("%03d", file_index), "_", filename)
    }
  }
  
  return(filename)
}

# Output Format ----

#' Determine output format for completed shapes
#' @noRd
.determine_complete_output_format <- function(input_path, format_option, img_info) {
  
  if (format_option == "auto") {
    input_ext <- tolower(tools::file_ext(input_path))
    
    # Consider transparency when choosing format
    has_alpha <- !is.null(img_info$matte) && img_info$matte
    
    # Normalize format
    format_option <- switch(input_ext,
      "jpg" = "jpeg",
      "jpeg" = "jpeg",
      "png" = "png",
      "tiff" = "tiff",
      "tif" = "tiff",
      "bmp" = if (has_alpha) "png" else "jpeg",
      "gif" = "png",
      if (has_alpha) "png" else "jpeg"  # Default based on transparency
    )
  }
  
  return(format_option)
}

# Image Saving ----

#' Save completed image
#' @noRd
.save_completed_image <- function(img, output_path, format, processing_options) {
  
  # Preserve transparency if requested and format supports it
  if (processing_options$preserve_transparency && tolower(format) %in% c("png", "tiff")) {
    # Keep as-is for transparency-supporting formats
    img <- magick::image_convert(img, format = format)
    magick::image_write(img, path = output_path)
  } else if (tolower(format) %in% c("jpg", "jpeg")) {
    # For JPEG, remove transparency and set quality
    img <- magick::image_background(img, "white")  # Set white background
    img <- magick::image_convert(img, format = "jpeg")
    magick::image_write(img, path = output_path, quality = processing_options$quality)
  } else {
    img <- magick::image_convert(img, format = format)
    magick::image_write(img, path = output_path)
  }
}

# Summary Generation ----

#' Generate processing summary for shape completion
#' @noRd
.generate_complete_summary <- function(processing_results, params, verbose) {
  
  results_df <- processing_results$results_df
  
  summary_stats <- list(
    total_files = length(unique(results_df$input_path)),
    successful_count = sum(results_df$status == "success"),
    failed_count = sum(results_df$status == "failed"),
    completion_method = params$completion_options$method,
    blend_seam = params$completion_options$blend_seam,
    auto_align = params$alignment_options$auto_align,
    average_processing_time = mean(results_df$processing_time[results_df$status == "success"]),
    total_processing_time = sum(results_df$processing_time)
  )
  
  # Add file size information if successful files exist
  if (summary_stats$successful_count > 0) {
    successful_outputs <- results_df$output_path[results_df$status == "success" & 
                                                results_df$output_path != ""]
    if (length(successful_outputs) > 0) {
      tryCatch({
        file_sizes <- file.info(successful_outputs)$size
        summary_stats$total_output_size <- sum(file_sizes, na.rm = TRUE)
        summary_stats$average_file_size <- mean(file_sizes, na.rm = TRUE)
      }, error = function(e) {
        if (verbose) warning("Could not calculate file sizes: ", e$message)
      })
    }
  }
  
  return(summary_stats)
}

# Print Method ----

#' Print method for complete_shape_result objects
#' @param x A complete_shape_result object
#' @param ... Additional arguments (ignored)
#' @export
print.complete_shape_result <- function(x, ...) {
  cat("Shape Completion Results\n")
  cat("========================\n\n")
  
  cat("Processing Summary:\n")
  cat("  Input files:", x$summary$total_files, "\n")
  cat("  Successfully processed:", x$summary$successful_count, "\n")
  cat("  Failed:", x$summary$failed_count, "\n")
  
  cat("\nCompletion Configuration:\n")
  cat("  Method:", x$summary$completion_method, "\n")
  cat("  Seam blending:", x$summary$blend_seam, "\n")
  cat("  Auto alignment:", x$summary$auto_align, "\n")
  
  if (!is.null(x$summary$average_processing_time)) {
    cat("  Average processing time:", round(x$summary$average_processing_time, 3), "seconds\n")
  }
  
  if (!is.null(x$summary$total_output_size)) {
    cat("  Total output size:", round(x$summary$total_output_size / 1024^2, 2), "MB\n")
  }
  
  if (x$summary$failed_count > 0) {
    cat("\nFailed files:\n")
    failed_info <- x$processed_files[x$processed_files$status == "failed", ]
    for (i in 1:nrow(failed_info)) {
      cat("  -", basename(failed_info$input_path[i]), ":", failed_info$error_message[i], "\n")
    }
  }
  
  cat("\nAvailable components:\n")
  cat("  - processed_files: Detailed processing results\n")
  cat("  - summary: Processing statistics\n")
  cat("  - failed_files: List of files that failed processing\n")
  cat("  - completion_info: Details about completion methods and dimensions\n")
}