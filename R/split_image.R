#' Split and Process Images with Advanced Options
#'
#' Splits images along horizontal or vertical lines with customizable split positions,
#' optional mirroring, and flexible processing options. Supports batch processing
#' of multiple images with comprehensive validation and output management.
#'
#' @param input_paths Character vector of file paths to input images. Can be single file or multiple files.
#' @param output_dir Character string specifying the output directory. Created if it doesn't exist.
#' @param split_options List containing splitting options:
#'   \describe{
#'     \item{direction}{Split direction: "horizontal" or "vertical" (default: "vertical")}
#'     \item{split_position}{Relative position of split (0-1, default: 0.5)}
#'     \item{multiple_splits}{Vector of split positions for multiple splits (optional)}
#'     \item{mirror_parts}{Which parts to mirror: "none", "first", "second", "both" (default: "second")}
#'   }
#' @param naming_options List containing file naming options:
#'   \describe{
#'     \item{suffix_first}{Suffix for first part (default: auto-generated)}
#'     \item{suffix_second}{Suffix for second part (default: auto-generated)}
#'     \item{prefix}{Prefix to add to all output files (default: "")}
#'     \item{numbering}{Add sequential numbering (default: FALSE)}
#'   }
#' @param processing_options List containing processing options:
#'   \describe{
#'     \item{overwrite}{Overwrite existing files (default: FALSE)}
#'     \item{quality}{Image quality for JPEG (1-100, default: 90)}
#'     \item{format}{Output format ("auto", "png", "jpg", "tiff", default: "auto")}
#'     \item{preserve_metadata}{Preserve image metadata (default: TRUE)}
#'   }
#' @param verbose Logical indicating whether to print progress messages. Default: TRUE.
#'
#' @return A list containing:
#'   \describe{
#'     \item{processed_files}{Data frame with input paths, output paths, and processing status}
#'     \item{summary}{Summary statistics of the splitting operation}
#'     \item{failed_files}{Vector of files that failed processing}
#'     \item{split_info}{Details about split positions and resulting dimensions}
#'   }
#'
#' @examples
#' \dontrun{
#' # Basic vertical split
#' result <- split_image(
#'   input_paths = "shape.png",
#'   output_dir = "split_images/"
#' )
#'
#' # Horizontal split with custom position and mirroring
#' result <- split_image(
#'   input_paths = c("img1.jpg", "img2.png"),
#'   output_dir = "processed/",
#'   split_options = list(
#'     direction = "horizontal",
#'     split_position = 0.6,
#'     mirror_parts = "first"
#'   ),
#'   naming_options = list(
#'     suffix_first = "_top_mirrored",
#'     suffix_second = "_bottom"
#'   )
#' )
#'
#' # Multiple splits
#' result <- split_image(
#'   input_paths = "large_image.png",
#'   output_dir = "segments/",
#'   split_options = list(
#'     direction = "vertical",
#'     multiple_splits = c(0.33, 0.67),
#'     mirror_parts = "none"
#'   )
#' )
#'
#' # View results
#' print(result$summary)
#' View(result$processed_files)
#' }
#'
#' @export
split_image <- function(input_paths,
                       output_dir,
                       split_options = list(),
                       naming_options = list(),
                       processing_options = list(),
                       verbose = TRUE) {
  
  # Input validation ----
  .validate_split_image_inputs(input_paths, output_dir, verbose)
  
  # Setup parameters ----
  params <- .setup_split_image_params(split_options, naming_options, processing_options, verbose)
  
  # Prepare output directory ----
  .prepare_split_output_directory(output_dir, verbose)
  
  if (verbose) {
    message("Starting image splitting process...")
    message("Processing ", length(input_paths), " image(s)")
    message("Split direction: ", params$split_options$direction)
    message("Output directory: ", output_dir)
  }
  
  # Process images ----
  processing_results <- .process_split_images_batch(input_paths, output_dir, params, verbose)
  
  # Generate summary ----
  summary_stats <- .generate_split_summary(processing_results, params, verbose)
  
  if (verbose) {
    message("Image splitting completed!")
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
      split_info = processing_results$split_info
    ),
    class = "split_image_result"
  )
}

# Input Validation ----

#' Validate inputs for split_image function
#' @noRd
.validate_split_image_inputs <- function(input_paths, output_dir, verbose) {
  
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

#' Setup parameters for image splitting
#' @noRd
.setup_split_image_params <- function(split_options, naming_options, processing_options, verbose) {
  
  # Split options defaults
  split_defaults <- list(
    direction = "vertical",
    split_position = 0.5,
    multiple_splits = NULL,
    mirror_parts = "second"
  )
  split_options <- utils::modifyList(split_defaults, split_options)
  
  # Validate split options
  if (!split_options$direction %in% c("horizontal", "vertical")) {
    stop("'direction' must be 'horizontal' or 'vertical'", call. = FALSE)
  }
  
  # Handle split positions
  if (is.null(split_options$multiple_splits)) {
    if (split_options$split_position < 0 || split_options$split_position > 1) {
      stop("'split_position' must be between 0 and 1", call. = FALSE)
    }
    split_positions <- split_options$split_position
  } else {
    if (!is.numeric(split_options$multiple_splits) || 
        any(split_options$multiple_splits < 0) || 
        any(split_options$multiple_splits > 1)) {
      stop("'multiple_splits' must be numeric values between 0 and 1", call. = FALSE)
    }
    split_positions <- sort(unique(split_options$multiple_splits))
  }
  split_options$split_positions <- split_positions
  
  if (!split_options$mirror_parts %in% c("none", "first", "second", "both")) {
    stop("'mirror_parts' must be 'none', 'first', 'second', or 'both'", call. = FALSE)
  }
  
  # Naming options defaults
  naming_defaults <- list(
    suffix_first = NULL,  # Auto-generate based on direction
    suffix_second = NULL, # Auto-generate based on direction
    prefix = "",
    numbering = FALSE
  )
  naming_options <- utils::modifyList(naming_defaults, naming_options)
  
  # Auto-generate suffixes if not provided
  if (is.null(naming_options$suffix_first) || is.null(naming_options$suffix_second)) {
    if (split_options$direction == "horizontal") {
      naming_options$suffix_first <- naming_options$suffix_first %||% "_upper"
      naming_options$suffix_second <- naming_options$suffix_second %||% "_lower"
    } else {
      naming_options$suffix_first <- naming_options$suffix_first %||% "_left"
      naming_options$suffix_second <- naming_options$suffix_second %||% "_right"
    }
  }
  
  # Processing options defaults
  processing_defaults <- list(
    overwrite = FALSE,
    quality = 90,
    format = "auto",
    preserve_metadata = TRUE
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
    split_options = split_options,
    naming_options = naming_options,
    processing_options = processing_options
  ))
}

# Utility function for null coalescing
`%||%` <- function(a, b) if (is.null(a)) b else a

# Directory Preparation ----

#' Prepare output directory for split images
#' @noRd
.prepare_split_output_directory <- function(output_dir, verbose) {
  
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

#' Process multiple images for splitting in batch
#' @noRd
.process_split_images_batch <- function(input_paths, output_dir, params, verbose) {
  
  n_files <- length(input_paths)
  results_df <- data.frame(
    input_path = character(0),
    output_path = character(0),
    part_type = character(0),
    status = character(0),
    error_message = character(0),
    processing_time = numeric(0),
    stringsAsFactors = FALSE
  )
  
  failed_files <- character(0)
  split_info <- list()
  
  for (i in seq_along(input_paths)) {
    input_path <- input_paths[i]
    
    if (verbose && (i %% 10 == 0 || i == n_files)) {
      message("Processing image ", i, "/", n_files, ": ", basename(input_path))
    }
    
    start_time <- Sys.time()
    
    tryCatch({
      # Process single image
      single_result <- .process_single_split_image(input_path, output_dir, params, i)
      
      end_time <- Sys.time()
      processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
      
      # Add results to data frame
      for (j in seq_along(single_result$output_paths)) {
        results_df <- rbind(results_df, data.frame(
          input_path = input_path,
          output_path = single_result$output_paths[j],
          part_type = single_result$part_types[j],
          status = "success",
          error_message = "",
          processing_time = processing_time / length(single_result$output_paths),
          stringsAsFactors = FALSE
        ))
      }
      
      # Store split information
      split_info[[i]] <- list(
        input_path = input_path,
        split_positions = params$split_options$split_positions,
        direction = params$split_options$direction,
        original_dimensions = single_result$original_dimensions,
        output_dimensions = single_result$output_dimensions
      )
      
    }, error = function(e) {
      end_time <- Sys.time()
      processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
      
      if (verbose) warning("Failed to process ", basename(input_path), ": ", e$message)
      
      results_df <<- rbind(results_df, data.frame(
        input_path = input_path,
        output_path = "",
        part_type = "",
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
    split_info = split_info
  ))
}

# Single Image Processing ----

#' Process a single image for splitting
#' @noRd
.process_single_split_image <- function(input_path, output_dir, params, file_index) {
  
  # Load image
  img <- magick::image_read(input_path)
  
  # Get image dimensions
  img_info <- magick::image_info(img)
  original_width <- img_info$width
  original_height <- img_info$height
  
  # Split image
  split_results <- .split_image_at_positions(img, params$split_options, original_width, original_height)
  
  # Apply mirroring
  mirrored_results <- .apply_mirroring(split_results$parts, params$split_options$mirror_parts, params$split_options$direction)
  
  # Generate output filenames and save images
  output_paths <- character(length(mirrored_results))
  part_types <- character(length(mirrored_results))
  output_dimensions <- list()
  
  for (j in seq_along(mirrored_results)) {
    # Generate filename
    part_info <- split_results$part_info[[j]]
    output_filename <- .generate_split_output_filename(
      input_path, part_info, params$naming_options, file_index
    )
    
    # Set output format
    output_format <- .determine_split_output_format(
      input_path, params$processing_options$format
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
    .save_split_image(mirrored_results[[j]], output_path, output_format, params$processing_options)
    
    # Store information
    output_paths[j] <- output_path
    part_types[j] <- part_info$type
    
    # Get output dimensions
    output_info <- magick::image_info(mirrored_results[[j]])
    output_dimensions[[j]] <- list(width = output_info$width, height = output_info$height)
  }
  
  return(list(
    output_paths = output_paths,
    part_types = part_types,
    original_dimensions = list(width = original_width, height = original_height),
    output_dimensions = output_dimensions
  ))
}

# Image Splitting ----

#' Split image at specified positions
#' @noRd
.split_image_at_positions <- function(img, split_options, width, height) {
  
  direction <- split_options$direction
  split_positions <- split_options$split_positions
  
  # Add boundaries (0 and 1) to split positions
  all_positions <- sort(unique(c(0, split_positions, 1)))
  
  parts <- list()
  part_info <- list()
  
  for (i in 1:(length(all_positions) - 1)) {
    start_pos <- all_positions[i]
    end_pos <- all_positions[i + 1]
    
    if (direction == "horizontal") {
      # Horizontal split
      start_pixel <- round(start_pos * height)
      end_pixel <- round(end_pos * height)
      crop_height <- end_pixel - start_pixel
      
      crop_geometry <- sprintf("%dx%d+0+%d", width, crop_height, start_pixel)
      part_type <- if (i == 1) "upper" else if (i == length(all_positions) - 1) "lower" else paste0("middle_", i - 1)
      
    } else {
      # Vertical split
      start_pixel <- round(start_pos * width)
      end_pixel <- round(end_pos * width)
      crop_width <- end_pixel - start_pixel
      
      crop_geometry <- sprintf("%dx%d+%d+0", crop_width, height, start_pixel)
      part_type <- if (i == 1) "left" else if (i == length(all_positions) - 1) "right" else paste0("middle_", i - 1)
    }
    
    # Crop the image
    cropped_part <- magick::image_crop(img, geometry = crop_geometry)
    
    parts[[i]] <- cropped_part
    part_info[[i]] <- list(
      type = part_type,
      position_start = start_pos,
      position_end = end_pos,
      index = i
    )
  }
  
  return(list(
    parts = parts,
    part_info = part_info
  ))
}

# Mirroring Application ----

#' Apply mirroring to image parts
#' @noRd
.apply_mirroring <- function(parts, mirror_parts, split_direction = "vertical") {
  
  if (mirror_parts == "none") {
    return(parts)
  }
  
  mirrored_parts <- parts
  
  for (i in seq_along(parts)) {
    should_mirror <- FALSE
    
    if (mirror_parts == "first" && i == 1) {
      should_mirror <- TRUE
    } else if (mirror_parts == "second" && i == 2) {
      should_mirror <- TRUE
    } else if (mirror_parts == "both") {
      should_mirror <- TRUE
    }
    
    if (should_mirror) {
      # For vertical splits, mirror horizontally (flop)
      # For horizontal splits, mirror vertically (flip)
      if (split_direction == "vertical") {
        mirrored_parts[[i]] <- magick::image_flop(parts[[i]])
      } else {
        mirrored_parts[[i]] <- magick::image_flip(parts[[i]])
      }
    }
  }
  
  return(mirrored_parts)
}

# Filename Generation ----

#' Generate output filename for split parts
#' @noRd
.generate_split_output_filename <- function(input_path, part_info, naming_options, file_index) {
  
  base_name <- tools::file_path_sans_ext(basename(input_path))
  
  # Add prefix
  if (nchar(naming_options$prefix) > 0) {
    filename <- paste0(naming_options$prefix, base_name)
  } else {
    filename <- base_name
  }
  
  # Add part suffix
  part_suffix <- switch(part_info$type,
    "upper" = naming_options$suffix_first,
    "lower" = naming_options$suffix_second,
    "left" = naming_options$suffix_first,
    "right" = naming_options$suffix_second,
    paste0("_part_", part_info$index)  # For multiple splits
  )
  
  filename <- paste0(filename, part_suffix)
  
  # Add numbering
  if (naming_options$numbering) {
    filename <- paste0(sprintf("%03d", file_index), "_", filename)
  }
  
  return(filename)
}

# Output Format ----

#' Determine output format for split images
#' @noRd
.determine_split_output_format <- function(input_path, format_option) {
  
  if (format_option == "auto") {
    input_ext <- tolower(tools::file_ext(input_path))
    # Normalize format
    format_option <- switch(input_ext,
      "jpg" = "jpeg",
      "jpeg" = "jpeg",
      "png" = "png",
      "tiff" = "tiff",
      "tif" = "tiff",
      "bmp" = "png",  # Convert BMP to PNG
      "gif" = "png",  # Convert GIF to PNG
      "png"  # Default
    )
  }
  
  return(format_option)
}

# Image Saving ----

#' Save split image
#' @noRd
.save_split_image <- function(img, output_path, format, processing_options) {
  
  # Convert format if needed
  if (tolower(format) %in% c("jpg", "jpeg")) {
    img <- magick::image_convert(img, format = "jpeg")
    # Write image with quality setting for JPEG
    magick::image_write(img, path = output_path, quality = processing_options$quality)
  } else {
    img <- magick::image_convert(img, format = format)
    # Write image
    magick::image_write(img, path = output_path)
  }
}

# Summary Generation ----

#' Generate processing summary for split images
#' @noRd
.generate_split_summary <- function(processing_results, params, verbose) {
  
  results_df <- processing_results$results_df
  
  summary_stats <- list(
    total_files = length(unique(results_df$input_path)),
    successful_count = length(unique(results_df$input_path[results_df$status == "success"])),
    failed_count = length(unique(results_df$input_path[results_df$status == "failed"])),
    total_outputs = nrow(results_df[results_df$status == "success", ]),
    split_direction = params$split_options$direction,
    split_positions = params$split_options$split_positions,
    mirror_settings = params$split_options$mirror_parts,
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

#' Print method for split_image_result objects
#' @param x A split_image_result object
#' @param ... Additional arguments (ignored)
#' @export
print.split_image_result <- function(x, ...) {
  cat("Image Splitting Results\n")
  cat("=======================\n\n")
  
  cat("Processing Summary:\n")
  cat("  Input files:", x$summary$total_files, "\n")
  cat("  Successfully processed:", x$summary$successful_count, "\n")
  cat("  Failed:", x$summary$failed_count, "\n")
  cat("  Total output files:", x$summary$total_outputs, "\n")
  
  cat("\nSplit Configuration:\n")
  cat("  Direction:", x$summary$split_direction, "\n")
  cat("  Split positions:", paste(x$summary$split_positions, collapse = ", "), "\n")
  cat("  Mirroring:", x$summary$mirror_settings, "\n")
  
  if (!is.null(x$summary$average_processing_time)) {
    cat("  Average processing time:", round(x$summary$average_processing_time, 3), "seconds\n")
  }
  
  if (!is.null(x$summary$total_output_size)) {
    cat("  Total output size:", round(x$summary$total_output_size / 1024^2, 2), "MB\n")
  }
  
  if (x$summary$failed_count > 0) {
    cat("\nFailed files:\n")
    failed_info <- x$processed_files[x$processed_files$status == "failed", ]
    unique_failed <- failed_info[!duplicated(failed_info$input_path), ]
    for (i in 1:nrow(unique_failed)) {
      cat("  -", basename(unique_failed$input_path[i]), ":", unique_failed$error_message[i], "\n")
    }
  }
  
  cat("\nAvailable components:\n")
  cat("  - processed_files: Detailed processing results\n")
  cat("  - summary: Processing statistics\n")
  cat("  - failed_files: List of files that failed processing\n")
  cat("  - split_info: Details about split positions and dimensions\n")
}