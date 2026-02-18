#' Cut Images in Half and Retain Right Half
#'
#' Processes images by cutting them vertically in half and retaining only the right half.
#' Supports batch processing of multiple images with flexible naming and output options.
#' Useful for preprocessing symmetrical objects where only one half is needed for analysis.
#'
#' @param input_paths Character vector of file paths to input images. Can be single file or multiple files.
#' @param output_dir Character string specifying the output directory. Created if it doesn't exist.
#' @param crop_options List containing cropping options:
#'   \describe{
#'     \item{side}{Which side to retain: "right" (default), "left", or "both"}
#'     \item{offset_percent}{Percentage offset from center (0-50, default: 0)}
#'     \item{maintain_aspect}{Maintain original aspect ratio (default: TRUE)}
#'   }
#' @param naming_options List containing file naming options:
#'   \describe{
#'     \item{suffix}{Suffix to add to filename (default: "_right_half")}
#'     \item{prefix}{Prefix to add to filename (default: "")}
#'     \item{preserve_name}{Keep original name without suffix/prefix (default: FALSE)}
#'     \item{numbering}{Add sequential numbering (default: FALSE)}
#'   }
#' @param processing_options List containing processing options:
#'   \describe{
#'     \item{overwrite}{Overwrite existing files (default: FALSE)}
#'     \item{quality}{Image quality for JPEG (1-100, default: 90)}
#'     \item{format}{Output format ("auto", "png", "jpg", "tiff", default: "auto")}
#'   }
#' @param verbose Logical indicating whether to print progress messages. Default: TRUE.
#'
#' @return A list containing:
#'   \describe{
#'     \item{processed_files}{Data frame with input paths, output paths, and processing status}
#'     \item{summary}{Summary statistics of the processing operation}
#'     \item{failed_files}{Vector of files that failed processing}
#'   }
#'
#' @examples
#' \dontrun{
#' # Basic usage - cut single image
#' result <- cut_image_right_half(
#'   input_paths = "path/to/image.png",
#'   output_dir = "output/"
#' )
#'
#' # Batch processing with custom options
#' result <- cut_image_right_half(
#'   input_paths = c("img1.png", "img2.jpg", "img3.png"),
#'   output_dir = "processed_images/",
#'   crop_options = list(
#'     side = "left",
#'     offset_percent = 5
#'   ),
#'   naming_options = list(
#'     suffix = "_left_side",
#'     numbering = TRUE
#'   ),
#'   processing_options = list(
#'     overwrite = TRUE,
#'     format = "png"
#'   )
#' )
#'
#' # Process both halves
#' result <- cut_image_right_half(
#'   input_paths = "symmetric_shape.png",
#'   output_dir = "halves/",
#'   crop_options = list(side = "both")
#' )
#'
#' # View results
#' print(result$summary)
#' View(result$processed_files)
#' }
#'
#' @export
cut_image_right_half <- function(input_paths,
                                output_dir,
                                crop_options = list(),
                                naming_options = list(),
                                processing_options = list(),
                                verbose = TRUE) {
  
  # Input validation ----
  .validate_cut_image_inputs(input_paths, output_dir, verbose)
  
  # Setup parameters ----
  params <- .setup_cut_image_params(crop_options, naming_options, processing_options, verbose)
  
  # Prepare output directory ----
  .prepare_output_directory(output_dir, verbose)
  
  if (verbose) {
    message("Starting image cropping process...")
    message("Processing ", length(input_paths), " image(s)")
    message("Output directory: ", output_dir)
  }
  
  # Process images ----
  processing_results <- .process_images_batch(input_paths, output_dir, params, verbose)
  
  # Generate summary ----
  summary_stats <- .generate_cut_summary(processing_results, verbose)
  
  if (verbose) {
    message("Image cropping completed!")
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
      failed_files = processing_results$failed_files
    ),
    class = "cut_image_result"
  )
}

# Input Validation ----

#' Validate inputs for cut_image_right_half function
#' @noRd
.validate_cut_image_inputs <- function(input_paths, output_dir, verbose) {
  
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

#' Setup parameters for image cutting
#' @noRd
.setup_cut_image_params <- function(crop_options, naming_options, processing_options, verbose) {
  
  # Crop options defaults
  crop_defaults <- list(
    side = "right",
    offset_percent = 0,
    maintain_aspect = TRUE
  )
  crop_options <- utils::modifyList(crop_defaults, crop_options)
  
  # Validate crop options
  if (!crop_options$side %in% c("right", "left", "both")) {
    stop("'side' must be 'right', 'left', or 'both'", call. = FALSE)
  }
  if (crop_options$offset_percent < 0 || crop_options$offset_percent > 50) {
    stop("'offset_percent' must be between 0 and 50", call. = FALSE)
  }
  
  # Naming options defaults
  naming_defaults <- list(
    suffix = "_right_half",
    prefix = "",
    preserve_name = FALSE,
    numbering = FALSE
  )
  naming_options <- utils::modifyList(naming_defaults, naming_options)
  
  # Adjust suffix based on side if not explicitly set
  if (identical(naming_options$suffix, naming_defaults$suffix)) {
    naming_options$suffix <- switch(crop_options$side,
      "right" = "_right_half",
      "left" = "_left_half",
      "both" = "_half"
    )
  }
  
  # Processing options defaults
  processing_defaults <- list(
    overwrite = FALSE,
    quality = 90,
    format = "auto"
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
    crop_options = crop_options,
    naming_options = naming_options,
    processing_options = processing_options
  ))
}

# Directory Preparation ----

#' Prepare output directory
#' @noRd
.prepare_output_directory <- function(output_dir, verbose) {
  
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

#' Process multiple images in batch
#' @noRd
.process_images_batch <- function(input_paths, output_dir, params, verbose) {
  
  n_files <- length(input_paths)
  results_df <- data.frame(
    input_path = character(0),
    output_path = character(0),
    status = character(0),
    error_message = character(0),
    processing_time = numeric(0),
    stringsAsFactors = FALSE
  )
  
  failed_files <- character(0)
  
  for (i in seq_along(input_paths)) {
    input_path <- input_paths[i]
    
    if (verbose && (i %% 10 == 0 || i == n_files)) {
      message("Processing image ", i, "/", n_files, ": ", basename(input_path))
    }
    
    start_time <- Sys.time()
    
    tryCatch({
      # Process single image
      single_result <- .process_single_image(input_path, output_dir, params, i)
      
      end_time <- Sys.time()
      processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
      
      # Handle multiple outputs (for "both" sides)
      if (is.list(single_result$output_path)) {
        # Multiple outputs
        for (j in seq_along(single_result$output_path)) {
          results_df <- rbind(results_df, data.frame(
            input_path = input_path,
            output_path = single_result$output_path[[j]],
            status = "success",
            error_message = "",
            processing_time = processing_time / length(single_result$output_path),
            stringsAsFactors = FALSE
          ))
        }
      } else {
        # Single output
        results_df <- rbind(results_df, data.frame(
          input_path = input_path,
          output_path = single_result$output_path,
          status = "success",
          error_message = "",
          processing_time = processing_time,
          stringsAsFactors = FALSE
        ))
      }
      
    }, error = function(e) {
      end_time <- Sys.time()
      processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
      
      if (verbose) warning("Failed to process ", basename(input_path), ": ", e$message)
      
      results_df <<- rbind(results_df, data.frame(
        input_path = input_path,
        output_path = "",
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
    failed_files = failed_files
  ))
}

# Single Image Processing ----

#' Process a single image
#' @noRd
.process_single_image <- function(input_path, output_dir, params, file_index) {
  
  # Load image
  img <- magick::image_read(input_path)
  
  # Get image dimensions
  img_info <- magick::image_info(img)
  original_width <- img_info$width
  original_height <- img_info$height
  
  # Calculate crop geometries
  offset_pixels <- round(original_width * params$crop_options$offset_percent / 100)
  crop_width <- original_width / 2
  
  sides_to_process <- if (params$crop_options$side == "both") {
    c("right", "left")
  } else {
    params$crop_options$side
  }
  
  output_paths <- list()
  
  for (side in sides_to_process) {
    # Calculate crop geometry for this side
    if (side == "right") {
      x_offset <- crop_width - offset_pixels
    } else {  # left
      x_offset <- offset_pixels
    }
    
    crop_geometry <- paste0(crop_width, "x", original_height, "+", x_offset, "+0")
    
    # Crop image
    cropped_img <- magick::image_crop(img, geometry = crop_geometry)
    
    # Generate output filename
    output_filename <- .generate_output_filename(
      input_path, side, params$naming_options, file_index
    )
    
    # Set output format
    output_format <- .determine_output_format(
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
    .save_processed_image(cropped_img, output_path, output_format, params$processing_options)
    
    output_paths[[side]] <- output_path
  }
  
  # Return single path if only one side, list if both
  if (length(output_paths) == 1) {
    return(list(output_path = output_paths[[1]]))
  } else {
    return(list(output_path = output_paths))
  }
}

# Filename Generation ----

#' Generate output filename
#' @noRd
.generate_output_filename <- function(input_path, side, naming_options, file_index) {
  
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
    
    # Add suffix (adjust for side if processing both)
    suffix <- naming_options$suffix
    if (naming_options$suffix == "_half" && side != "right") {
      suffix <- paste0("_", side, "_half")
    }
    filename <- paste0(filename, suffix)
    
    # Add numbering
    if (naming_options$numbering) {
      filename <- paste0(sprintf("%03d", file_index), "_", filename)
    }
  }
  
  return(filename)
}

# Output Format ----

#' Determine output format
#' @noRd
.determine_output_format <- function(input_path, format_option) {
  
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

#' Save processed image
#' @noRd
.save_processed_image <- function(img, output_path, format, processing_options) {
  
  # Set quality for JPEG
  if (tolower(format) %in% c("jpg", "jpeg")) {
    img <- magick::image_format(img, format = "jpeg")
    img <- magick::image_quality(img, quality = processing_options$quality)
  } else {
    img <- magick::image_format(img, format = format)
  }
  
  # Write image
  magick::image_write(img, path = output_path)
}

# Summary Generation ----

#' Generate processing summary
#' @noRd
.generate_cut_summary <- function(processing_results, verbose) {
  
  results_df <- processing_results$results_df
  
  summary_stats <- list(
    total_files = length(unique(results_df$input_path)),
    successful_count = sum(results_df$status == "success"),
    failed_count = sum(results_df$status == "failed"),
    total_outputs = nrow(results_df[results_df$status == "success", ]),
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

#' Print method for cut_image_result objects
#' @param x A cut_image_result object
#' @param ... Additional arguments (ignored)
#' @export
print.cut_image_result <- function(x, ...) {
  cat("Image Cutting Results\n")
  cat("=====================\n\n")
  
  cat("Processing Summary:\n")
  cat("  Input files:", x$summary$total_files, "\n")
  cat("  Successfully processed:", x$summary$successful_count, "\n")
  cat("  Failed:", x$summary$failed_count, "\n")
  cat("  Total output files:", x$summary$total_outputs, "\n")
  
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
}