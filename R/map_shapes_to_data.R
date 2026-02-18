#' Map Shape Files to Data Frame
#'
#' Maps shape files (JPEG/JPG) from a directory to rows in a data frame based on
#' ID matching. Shapes are imported using adapted Momocs code and added as a new
#' column. Supports batch processing, validation, and comprehensive error handling.
#'
#' @param data A data frame containing IDs for matching with shape file names.
#' @param id_col Character string specifying the column containing IDs to match
#'   with shape file names (without extensions).
#' @param shape_folder Character string specifying the directory containing
#'   JPEG/JPG shape files.
#' @param shape_col Character string specifying the name of the new shape column.
#'   Default: "shape".
#' @param options List containing processing options:
#'   \describe{
#'     \item{recursive}{Search subdirectories recursively (default: FALSE)}
#'     \item{case_sensitive}{Case-sensitive filename matching (default: FALSE)}
#'     \item{fail_on_missing}{Stop if any shapes are missing (default: FALSE)}
#'     \item{validate_shapes}{Validate imported shapes (default: TRUE)}
#'   }
#' @param verbose Logical indicating whether to print progress messages. Default: TRUE.
#'
#' @return A data frame with an additional shape column containing imported shapes
#'   as Out objects or NULL for unmatched rows. Also includes attributes:
#'   \describe{
#'     \item{mapping_summary}{Summary of mapping results}
#'     \item{failed_imports}{List of files that failed to import}
#'   }
#'
#' @examples
#' \dontrun{
#' # Basic shape mapping
#' data_with_shapes <- map_shapes_to_data(
#'   data = my_data,
#'   id_col = "specimen_id",
#'   shape_folder = "shapes/"
#' )
#'
#' # Advanced options
#' data_with_shapes <- map_shapes_to_data(
#'   data = my_data,
#'   id_col = "specimen_id", 
#'   shape_folder = "shapes/",
#'   shape_col = "outline",
#'   options = list(
#'     recursive = TRUE,
#'     case_sensitive = TRUE,
#'     fail_on_missing = TRUE,
#'     validate_shapes = TRUE
#'   )
#' )
#'
#' # Check mapping results
#' summary_attr <- attr(data_with_shapes, "mapping_summary")
#' print(summary_attr)
#' }
#'
#' @export
map_shapes_to_data <- function(data,
                              id_col,
                              shape_folder,
                              shape_col = "shape",
                              options = list(),
                              verbose = TRUE) {
  
  # Input validation ----
  .validate_shape_mapping_inputs(data, id_col, shape_folder, shape_col, verbose)
  
  # Setup parameters ----
  params <- .setup_shape_mapping_params(options, verbose)
  
  if (verbose) {
    message("Starting shape mapping...")
    message("Shape folder: ", shape_folder)
    message("ID column: ", id_col)
    message("Target column: ", shape_col)
  }
  
  # Find shape files ----
  shape_files <- .find_shape_files(shape_folder, params, verbose)
  
  if (length(shape_files) == 0) {
    if (verbose) warning("No JPEG/JPG files found in specified folder")
    data[[shape_col]] <- vector("list", nrow(data))
    return(.add_mapping_attributes(data, list(), list(), 0, 0))
  }
  
  if (verbose) message("Found ", length(shape_files), " shape files")
  
  # Create file name mapping ----
  file_mapping <- .create_file_mapping(shape_files, params, verbose)
  
  # Map shapes to data ----
  mapping_result <- .map_shapes_to_rows(data, id_col, file_mapping, params, verbose)
  
  # Add shape column to data ----
  data[[shape_col]] <- mapping_result$shapes
  
  # Generate summary ----
  if (verbose) {
    message("Shape mapping completed!")
    message("Successful mappings: ", mapping_result$n_successful)
    message("Failed imports: ", length(mapping_result$failed_imports))
    message("Missing shapes: ", mapping_result$n_missing)
  }
  
  # Add attributes and return ----
  result <- .add_mapping_attributes(
    data, 
    mapping_result$mapping_summary, 
    mapping_result$failed_imports,
    mapping_result$n_successful,
    mapping_result$n_missing
  )
  
  return(result)
}

# Input Validation ----

#' Validate inputs for map_shapes_to_data
#' @noRd
.validate_shape_mapping_inputs <- function(data, id_col, shape_folder, shape_col, verbose) {
  
  # Check data
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame", call. = FALSE)
  }
  if (nrow(data) == 0) {
    stop("'data' cannot be empty", call. = FALSE)
  }
  
  # Check id_col
  if (!is.character(id_col) || length(id_col) != 1) {
    stop("'id_col' must be a single character string", call. = FALSE)
  }
  if (!id_col %in% colnames(data)) {
    stop("Column '", id_col, "' does not exist in data", call. = FALSE)
  }
  
  # Check shape_folder
  if (!is.character(shape_folder) || length(shape_folder) != 1) {
    stop("'shape_folder' must be a single character string", call. = FALSE)
  }
  if (!dir.exists(shape_folder)) {
    stop("Shape folder does not exist: ", shape_folder, call. = FALSE)
  }
  
  # Check shape_col
  if (!is.character(shape_col) || length(shape_col) != 1) {
    stop("'shape_col' must be a single character string", call. = FALSE)
  }
  if (shape_col %in% colnames(data)) {
    warning("Column '", shape_col, "' already exists and will be overwritten")
  }
  
  # Check verbose
  if (!is.logical(verbose) || length(verbose) != 1) {
    stop("'verbose' must be a single logical value", call. = FALSE)
  }
}

# Parameter Setup ----

#' Setup parameters for shape mapping
#' @noRd
.setup_shape_mapping_params <- function(options, verbose) {
  
  # Default options
  defaults <- list(
    recursive = FALSE,
    case_sensitive = FALSE,
    fail_on_missing = FALSE,
    validate_shapes = TRUE
  )
  
  params <- utils::modifyList(defaults, options)
  
  # Validate options
  if (!is.logical(params$recursive)) {
    stop("'recursive' option must be logical", call. = FALSE)
  }
  if (!is.logical(params$case_sensitive)) {
    stop("'case_sensitive' option must be logical", call. = FALSE)
  }
  if (!is.logical(params$fail_on_missing)) {
    stop("'fail_on_missing' option must be logical", call. = FALSE)
  }
  if (!is.logical(params$validate_shapes)) {
    stop("'validate_shapes' option must be logical", call. = FALSE)
  }
  
  return(params)
}

# File Discovery ----

#' Find shape files in directory
#' @noRd
.find_shape_files <- function(shape_folder, params, verbose) {
  
  # Define pattern for JPEG files
  pattern <- "\\.(jpeg|jpg)$"
  if (!params$case_sensitive) {
    pattern <- paste0("(?i)", pattern)  # Case-insensitive
  }
  
  # Find files
  tryCatch({
    files <- list.files(
      shape_folder,
      pattern = pattern,
      full.names = TRUE,
      recursive = params$recursive,
      ignore.case = !params$case_sensitive
    )
    
    # Filter out directories (shouldn't happen with list.files, but safety check)
    files <- files[!file.info(files)$isdir]
    
    return(files)
    
  }, error = function(e) {
    stop("Error searching for shape files: ", e$message, call. = FALSE)
  })
}

# File Mapping ----

#' Create mapping from file paths to IDs
#' @noRd
.create_file_mapping <- function(shape_files, params, verbose) {
  
  # Extract base names without extensions
  base_names <- tools::file_path_sans_ext(basename(shape_files))
  
  # Create mapping table
  mapping <- data.frame(
    file_path = shape_files,
    id = base_names,
    stringsAsFactors = FALSE
  )
  
  # Handle case sensitivity
  if (!params$case_sensitive) {
    mapping$id_match <- tolower(mapping$id)
  } else {
    mapping$id_match <- mapping$id
  }
  
  # Check for duplicate IDs
  duplicates <- duplicated(mapping$id_match)
  if (any(duplicates)) {
    duplicate_ids <- unique(mapping$id_match[duplicates])
    if (verbose) {
      warning("Found duplicate shape file names (will use first occurrence): ", 
              paste(duplicate_ids, collapse = ", "))
    }
    # Keep only first occurrence of each ID
    mapping <- mapping[!duplicates, ]
  }
  
  return(mapping)
}

# Shape Mapping ----

#' Map shapes to data rows
#' @noRd
.map_shapes_to_rows <- function(data, id_col, file_mapping, params, verbose) {
  
  # Prepare ID matching
  data_ids <- as.character(data[[id_col]])
  if (!params$case_sensitive) {
    match_ids <- tolower(data_ids)
    mapping_ids <- file_mapping$id_match
  } else {
    match_ids <- data_ids
    mapping_ids <- file_mapping$id_match
  }
  
  # Find matches
  matches <- match(match_ids, mapping_ids)
  
  # Initialize results
  shapes <- vector("list", nrow(data))
  failed_imports <- list()
  n_successful <- 0
  n_missing <- sum(is.na(matches))
  
  # Process matches
  for (i in seq_along(matches)) {
    match_idx <- matches[i]
    
    if (!is.na(match_idx)) {
      # Found matching file
      file_path <- file_mapping$file_path[match_idx]
      
      if (verbose && i %% 50 == 0) {
        message("Processing shape ", i, "/", length(matches))
      }
      
      # Import shape
      shape_result <- .import_single_shape(file_path, params, verbose)
      
      if (shape_result$success) {
        shapes[[i]] <- shape_result$shape
        n_successful <- n_successful + 1
      } else {
        shapes[[i]] <- NULL
        failed_imports[[length(failed_imports) + 1]] <- list(
          row = i,
          id = data_ids[i],
          file_path = file_path,
          error = shape_result$error
        )
      }
    } else {
      # No matching file found
      shapes[[i]] <- NULL
    }
  }
  
  # Check for missing shapes if required
  if (params$fail_on_missing && n_missing > 0) {
    missing_ids <- data_ids[is.na(matches)]
    stop("Missing shape files for IDs: ", paste(head(missing_ids, 5), collapse = ", "),
         if (length(missing_ids) > 5) " ..." else "", call. = FALSE)
  }
  
  # Generate mapping summary
  mapping_summary <- list(
    total_rows = nrow(data),
    files_found = nrow(file_mapping),
    successful_mappings = n_successful,
    failed_imports = length(failed_imports),
    missing_shapes = n_missing,
    mapping_rate = round(n_successful / nrow(data) * 100, 1)
  )
  
  return(list(
    shapes = shapes,
    mapping_summary = mapping_summary,
    failed_imports = failed_imports,
    n_successful = n_successful,
    n_missing = n_missing
  ))
}

# Shape Import ----

#' Import a single shape file
#' @noRd
.import_single_shape <- function(file_path, params, verbose) {
  
  tryCatch({
    # Import using adapted Momocs code
    shape_data <- import_jpg(file_path)
    shape_out <- Out(shape_data)
    
    # Validate if requested
    if (params$validate_shapes) {
      if (!.validate_imported_shape(shape_out)) {
        return(list(
          success = FALSE,
          shape = NULL,
          error = "Shape validation failed"
        ))
      }
    }
    
    return(list(
      success = TRUE,
      shape = shape_out,
      error = NULL
    ))
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      shape = NULL,
      error = e$message
    ))
  })
}

#' Validate imported shape
#' @noRd
.validate_imported_shape <- function(shape_out) {
  
  # Basic validation checks
  if (!inherits(shape_out, "Out")) {
    return(FALSE)
  }
  
  if (is.null(shape_out$coo) || length(shape_out$coo) == 0) {
    return(FALSE)
  }
  
  # Check if coordinates exist and are valid
  coords <- shape_out$coo[[1]]
  if (is.null(coords) || !is.matrix(coords) || ncol(coords) != 2) {
    return(FALSE)
  }
  
  # Check for sufficient points
  if (nrow(coords) < 3) {
    return(FALSE)
  }
  
  # Check for finite coordinates
  if (!all(is.finite(coords))) {
    return(FALSE)
  }
  
  return(TRUE)
}

# Result Attribution ----

#' Add mapping attributes to result
#' @noRd
.add_mapping_attributes <- function(data, mapping_summary, failed_imports, n_successful, n_missing) {
  
  attr(data, "mapping_summary") <- mapping_summary
  attr(data, "failed_imports") <- failed_imports
  attr(data, "n_successful") <- n_successful
  attr(data, "n_missing") <- n_missing
  attr(data, "shape_mapping_result") <- TRUE
  
  class(data) <- c("shape_mapped_data", class(data))
  
  return(data)
}

# Print method for shape mapping results ----

#' Print method for shape_mapped_data objects
#' @param x A data frame with shape mapping results
#' @param ... Additional arguments (ignored)
#' @export
print.shape_mapped_data <- function(x, ...) {
  
  # Print data frame normally first
  class(x) <- class(x)[class(x) != "shape_mapped_data"]
  print(x)
  
  # Add mapping summary
  summary <- attr(x, "mapping_summary")
  if (!is.null(summary)) {
    cat("\nShape Mapping Summary:\n")
    cat("======================\n")
    cat("Total rows:", summary$total_rows, "\n")
    cat("Shape files found:", summary$files_found, "\n")
    cat("Successful mappings:", summary$successful_mappings, "\n")
    cat("Failed imports:", summary$failed_imports, "\n")
    cat("Missing shapes:", summary$missing_shapes, "\n")
    cat("Mapping rate:", summary$mapping_rate, "%\n")
    
    # Show failed imports if any
    failed <- attr(x, "failed_imports")
    if (length(failed) > 0) {
      cat("\nFailed imports (first 3):\n")
      for (i in seq_len(min(3, length(failed)))) {
        cat("  Row", failed[[i]]$row, "(ID:", failed[[i]]$id, "):", failed[[i]]$error, "\n")
      }
      if (length(failed) > 3) {
        cat("  ... and", length(failed) - 3, "more\n")
      }
    }
  }
}