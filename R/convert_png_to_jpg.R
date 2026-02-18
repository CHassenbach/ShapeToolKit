#' Convert and Process PNG Images to JPG or BMP Format
#'
#' Imports PNG images, resizes them with proportional scaling, adds padding,
#' and converts to JPG or BMP format with comprehensive error handling and
#' batch processing capabilities.
#'
#' @param input_path Character string specifying the path to input PNG file(s).
#'   Can be a single file path or directory path for batch processing.
#' @param output_dir Character string specifying the output directory. 
#'   If NULL, uses the same directory as input. Default: NULL.
#' @param dimensions List specifying target dimensions. Use either:
#'   \describe{
#'     \item{width}{Fixed width in pixels (height will be proportional)}
#'     \item{height}{Fixed height in pixels (width will be proportional)}
#'   }
#' @param padding Numeric value specifying padding in pixels around the image. Default: 10.
#' @param format Character string specifying output format ("jpg" or "bmp"). Default: "jpg".
#' @param quality Numeric value (0-100) specifying JPG quality. Default: 95.
#' @param background Character string specifying background color for padding. Default: "white".
#' @param batch_processing Logical indicating whether to process all PNG files in a directory.
#'   Only used when input_path is a directory. Default: TRUE.
#' @param overwrite Logical indicating whether to overwrite existing files. Default: FALSE.
#' @param verbose Logical indicating whether to print progress messages. Default: TRUE.
#'
#' @return A data frame with conversion results including:
#'   \describe{
#'     \item{input_file}{Original file path}
#'     \item{output_file}{Output file path}
#'     \item{original_size}{Original image dimensions}
#'     \item{processed_size}{Final image dimensions}
#'     \item{success}{Whether conversion was successful}
#'     \item{message}{Status or error message}
#'   }
#'
#' @examples
#' \dontrun{
#' # Single file conversion
#' result <- convert_png_to_image(
#'   input_path = "my_image.png",
#'   dimensions = list(width = 800),
#'   format = "jpg"
#' )
#'
#' # Batch processing with custom settings
#' result <- convert_png_to_image(
#'   input_path = "image_directory/",
#'   output_dir = "processed_images/",
#'   dimensions = list(height = 600),
#'   padding = 20,
#'   format = "bmp",
#'   background = "black"
#' )
#'
#' # High quality JPG conversion
#' result <- convert_png_to_image(
#'   input_path = "high_res.png",
#'   dimensions = list(width = 1200),
#'   quality = 100,
#'   padding = 0
#' )
#' }
#'
#' @export
convert_png_to_image <- function(input_path,
                                output_dir = NULL,
                                dimensions = list(width = 800),
                                padding = 10,
                                format = "jpg",
                                quality = 95,
                                background = "white",
                                batch_processing = TRUE,
                                overwrite = FALSE,
                                verbose = TRUE) {
  
  # Input validation ----
  .cpi_validate_inputs(input_path, output_dir, dimensions, padding, 
                                   format, quality, background, overwrite, verbose)
  
  # Setup parameters ----
  params <- .cpi_setup_params(output_dir, dimensions, padding, format, 
                                          quality, background, overwrite, verbose)
  
  # Get list of files to process ----
  files_to_process <- .cpi_get_files_to_process(input_path, batch_processing, verbose)
  
  if (verbose) {
    message("Starting image conversion...")
    message("Files to process: ", length(files_to_process))
    message("Output format: ", format)
  }
  
  # Process files ----
  results <- .cpi_process_image_files(files_to_process, params)
  
  # Summary ----
  successful <- sum(results$success)
  if (verbose) {
    message("Conversion completed!")
    message("Successful: ", successful, "/", nrow(results))
    if (successful < nrow(results)) {
      message("Failed conversions: ", nrow(results) - successful)
    }
  }
  
  return(results)
}

# Input Validation ----

#' Validate inputs for convert_png_to_image function
#' @noRd
.cpi_validate_inputs <- function(input_path, output_dir, dimensions, padding,
                                             format, quality, background, overwrite, verbose) {
  
  # Check magick package
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("Package 'magick' is required but not installed. Please install with: install.packages('magick')",
         call. = FALSE)
  }
  
  # Check input_path
  if (!is.character(input_path) || length(input_path) != 1 || nchar(input_path) == 0) {
    stop("'input_path' must be a non-empty character string", call. = FALSE)
  }
  
  if (!file.exists(input_path) && !dir.exists(input_path)) {
    stop("Input path does not exist: ", input_path, call. = FALSE)
  }
  
  # Check output_dir
  if (!is.null(output_dir)) {
    if (!is.character(output_dir) || length(output_dir) != 1) {
      stop("'output_dir' must be NULL or a character string", call. = FALSE)
    }
  }
  
  # Check dimensions
  if (!is.list(dimensions) || length(dimensions) == 0) {
    stop("'dimensions' must be a non-empty list", call. = FALSE)
  }
  
  if (!("width" %in% names(dimensions)) && !("height" %in% names(dimensions))) {
    stop("'dimensions' must contain either 'width' or 'height'", call. = FALSE)
  }
  
  if ("width" %in% names(dimensions)) {
    if (!is.numeric(dimensions$width) || dimensions$width <= 0) {
      stop("'width' must be a positive number", call. = FALSE)
    }
  }
  
  if ("height" %in% names(dimensions)) {
    if (!is.numeric(dimensions$height) || dimensions$height <= 0) {
      stop("'height' must be a positive number", call. = FALSE)
    }
  }
  
  # Check other parameters
  if (!is.numeric(padding) || length(padding) != 1 || padding < 0) {
    stop("'padding' must be a non-negative number", call. = FALSE)
  }
  
  valid_formats <- c("jpg", "jpeg", "bmp")
  format <- tolower(format)
  if (!format %in% valid_formats) {
    stop("'format' must be one of: ", paste(valid_formats, collapse = ", "), call. = FALSE)
  }
  
  if (!is.numeric(quality) || length(quality) != 1 || quality < 0 || quality > 100) {
    stop("'quality' must be a number between 0 and 100", call. = FALSE)
  }
  
  if (!is.character(background) || length(background) != 1) {
    stop("'background' must be a character string", call. = FALSE)
  }
  
  if (!is.logical(overwrite) || length(overwrite) != 1) {
    stop("'overwrite' must be a single logical value", call. = FALSE)
  }
  
  if (!is.logical(verbose) || length(verbose) != 1) {
    stop("'verbose' must be a single logical value", call. = FALSE)
  }
}

# Parameter Setup ----

#' Setup parameters for image conversion
#' @noRd
.cpi_setup_params <- function(output_dir, dimensions, padding, format, 
                                          quality, background, overwrite, verbose) {
  
  # Normalize format
  format <- tolower(format)
  if (format == "jpeg") format <- "jpg"
  
  return(list(
    output_dir = output_dir,
    dimensions = dimensions,
    padding = padding,
    format = format,
    quality = quality,
    background = background,
    overwrite = overwrite,
    verbose = verbose
  ))
}

# File Processing ----

#' Get list of files to process
#' @noRd
.cpi_get_files_to_process <- function(input_path, batch_processing, verbose) {
  
  if (file.exists(input_path) && !dir.exists(input_path)) {
    # Single file
    if (!grepl("\\.png$", input_path, ignore.case = TRUE)) {
      stop("Input file must be a PNG file", call. = FALSE)
    }
    return(input_path)
  } else if (dir.exists(input_path) && batch_processing) {
    # Directory - find all PNG files
    png_files <- list.files(
      input_path, 
      pattern = "\\.png$", 
      full.names = TRUE, 
      ignore.case = TRUE,
      recursive = FALSE
    )
    
    if (length(png_files) == 0) {
      stop("No PNG files found in directory: ", input_path, call. = FALSE)
    }
    
    if (verbose) message("Found ", length(png_files), " PNG files in directory")
    return(png_files)
  } else {
    stop("Invalid input path or batch processing disabled for directory", call. = FALSE)
  }
}

#' Process multiple image files
#' @noRd
.cpi_process_image_files <- function(files, params) {
  
  # Initialize results data frame
  results <- data.frame(
    input_file = character(length(files)),
    output_file = character(length(files)),
    original_size = character(length(files)),
    processed_size = character(length(files)),
    success = logical(length(files)),
    message = character(length(files)),
    stringsAsFactors = FALSE
  )
  
  # Process each file
  for (i in seq_along(files)) {
    file_path <- files[i]
    
    if (params$verbose) message("Processing: ", basename(file_path))
    
  result <- .cpi_process_single_image(file_path, params)
    
    # Store results
    results[i, "input_file"] <- file_path
    results[i, "output_file"] <- result$output_path
    results[i, "original_size"] <- result$original_size
    results[i, "processed_size"] <- result$processed_size
    results[i, "success"] <- result$success
    results[i, "message"] <- result$message
  }
  
  return(results)
}

#' Process a single image file
#' @noRd
.cpi_process_single_image <- function(file_path, params) {
  
  tryCatch({
    # Setup output path
  output_path <- .cpi_setup_output_path(file_path, params)
    
    # Check if output exists and overwrite setting
    if (file.exists(output_path) && !params$overwrite) {
      return(list(
        output_path = output_path,
        original_size = "Unknown",
        processed_size = "Unknown", 
        success = FALSE,
        message = "Output file exists and overwrite is FALSE"
      ))
    }
    
    # Load and process image
    img <- magick::image_read(file_path)
    
    # Get original dimensions
    img_info <- magick::image_info(img)
    original_size <- paste0(img_info$width, "x", img_info$height)
    
    # Convert to RGB color space
    img <- magick::image_convert(img, colorspace = "RGB")
    
    # Resize image
  resized_img <- .cpi_resize_image_proportionally(img, params$dimensions)
    
    # Add padding if specified
    if (params$padding > 0) {
  padded_img <- .cpi_add_image_padding(resized_img, params$padding, params$background)
    } else {
      padded_img <- resized_img
    }
    
    # Get final dimensions
    final_info <- magick::image_info(padded_img)
    processed_size <- paste0(final_info$width, "x", final_info$height)
    
    # Save image
  .cpi_save_processed_image(padded_img, output_path, params)
    
    return(list(
      output_path = output_path,
      original_size = original_size,
      processed_size = processed_size,
      success = TRUE,
      message = "Conversion successful"
    ))
    
  }, error = function(e) {
    return(list(
      output_path = NA_character_,
      original_size = "Unknown",
      processed_size = "Unknown",
      success = FALSE,
      message = paste("Error:", e$message)
    ))
  })
}

#' Setup output file path
#' @noRd
.cpi_setup_output_path <- function(file_path, params) {
  
  # Determine output directory
  if (is.null(params$output_dir)) {
    output_dir <- dirname(file_path)
  } else {
    output_dir <- params$output_dir
    # Create directory if it doesn't exist
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }
  }
  
  # Generate output filename
  file_name <- tools::file_path_sans_ext(basename(file_path))
  output_filename <- paste0(file_name, ".", params$format)
  
  return(file.path(output_dir, output_filename))
}

#' Resize image proportionally
#' @noRd
.cpi_resize_image_proportionally <- function(img, dimensions) {
  
  # Get current dimensions
  img_info <- magick::image_info(img)
  original_width <- img_info$width
  original_height <- img_info$height
  
  # Calculate new dimensions
  if (!is.null(dimensions$width)) {
    new_width <- dimensions$width
    new_height <- round((new_width / original_width) * original_height)
  } else if (!is.null(dimensions$height)) {
    new_height <- dimensions$height
    new_width <- round((new_height / original_height) * original_width)
  } else {
    stop("Either width or height must be specified in dimensions", call. = FALSE)
  }
  
  # Resize image
  geometry_string <- paste0(new_width, "x", new_height)
  magick::image_resize(img, geometry_string)
}

#' Add padding around image
#' @noRd
.cpi_add_image_padding <- function(img, padding, background) {
  
  # Get image dimensions
  img_info <- magick::image_info(img)
  current_width <- img_info$width
  current_height <- img_info$height
  
  # Calculate padded dimensions
  padded_width <- current_width + 2 * padding
  padded_height <- current_height + 2 * padding
  
  # Add padding
  geometry_string <- paste0(padded_width, "x", padded_height)
  magick::image_extent(img, geometry = geometry_string, color = background)
}

#' Save processed image
#' @noRd
.cpi_save_processed_image <- function(img, output_path, params) {
  
  if (params$format == "jpg") {
    magick::image_write(
      img, 
      path = output_path, 
      format = "jpg", 
      quality = params$quality
    )
  } else if (params$format == "bmp") {
    # Ensure 24-bit BMP
    img <- magick::image_convert(img, type = "truecolor")
    magick::image_write(img, path = output_path, format = "bmp")
  }
}


# Compatibility wrapper ----------------------------------------------------

#' Convert PNG to JPG (compatibility wrapper)
#'
#' This function maintains backward compatibility with older code that called
#' `convert_png_to_jpg()`. It simply forwards to [convert_png_to_image()],
#' forcing `format = "jpg"` while exposing the same parameters (except `format`).
#'
#' @inheritParams convert_png_to_image
#'
#' @return See [convert_png_to_image()]. Returns a data.frame with per-file results.
#'
#' @examples
#' \dontrun{
#' convert_png_to_jpg(
#'   input_path = "path/to/images",
#'   output_dir = "path/to/output",
#'   dimensions = list(width = 800),
#'   padding = 10,
#'   quality = 95
#' )
#' }
#'
#' @export
convert_png_to_jpg <- function(input_path,
                               output_dir = NULL,
                               dimensions = list(width = 800),
                               padding = 10,
                               quality = 95,
                               background = "white",
                               batch_processing = TRUE,
                               overwrite = FALSE,
                               verbose = TRUE) {
  convert_png_to_image(
    input_path = input_path,
    output_dir = output_dir,
    dimensions = dimensions,
    padding = padding,
    format = "jpg",
    quality = quality,
    background = background,
    batch_processing = batch_processing,
    overwrite = overwrite,
    verbose = verbose
  )
}