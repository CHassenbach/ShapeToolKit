#' Analyze Shape Images and Perform PCA
#'
#' This function processes a directory of shape images, normalizes the shapes,
#' performs Elliptical Fourier Analysis (EFA) and Principal Component Analysis (PCA),
#' displays a plot of PC contributions, and saves PCA scores to an Excel file.
#'
#' @param shape_dir A character string specifying the directory containing shape files
#'   (images in JPG format). The directory must exist and contain at least one valid JPG file.
#' @param norm Logical. Controls EFA normalization. If TRUE, Fourier descriptors are
#'   normalized to the first harmonic ("align by first harmonic"). If FALSE, they are
#'   normalized to the longest radius. Shapes are always centered, scaled, and slid to
#'   the specified start direction prior to EFA. Default is TRUE.
#' @param output_dir A character string specifying the directory where the Excel file will be saved.
#'   If NULL, the file will be saved in the working directory.
#' @param output_file A character string specifying the name of the Excel file where PCA scores will
#'   be saved. Default is "shape_analysis.xlsx".
#' @param num_pcs Integer. The number of principal components to display in the PC contribution plot.
#'   Must be between 1 and 50. Default is 10.
#' @param start_point A character string specifying the starting point for alignment during shape
#'   normalization. Options are "up", "left", "down", or "right". Default is "left".
#' @param harmonics Integer. Number of harmonics to use for EFA. Default is NULL (automatic).
#' @param verbose Logical. Should progress messages be printed? Default is TRUE.
#'
#' @return A list containing:
#'   \describe{
#'     \item{pca_results}{The PCA object}
#'     \item{efa_results}{The EFA coefficients object}
#'     \item{scores}{Data frame of PCA scores}
#'     \item{summary}{Character string of PCA summary}
#'     \item{output_path}{Path where Excel results were saved}
#'     \item{summary_txt_path}{Path to saved PCA summary .txt file}
#'     \item{pc_plot_jpg_path}{Path to saved PC contributions .jpg}
#'     \item{reconstruction_files}{List of paths to reconstruction CSV files (rotation, center, sdev, metadata)}
#'     \item{metadata}{List of analysis metadata (n_shapes, harmonics, etc.)}
#'   }
#'
#' @details
#' The function uses adapted Momocs code to import, normalize, and analyze shape outlines:
#' - **Import**: Reads JPG/JPEG files and converts to shape outlines
#' - **Pre-processing**: Centers, scales, and slides shapes to a consistent start
#'   direction (up/left/down/right)
#' - **Normalization**: EFA descriptors normalized to first harmonic when `norm = TRUE`,
#'   or to the longest radius when `norm = FALSE`
#' - **Elliptical Fourier Analysis (EFA)**: Generates Fourier descriptors for the shape outlines
#' - **Principal Component Analysis (PCA)**: Decomposes Fourier descriptors into principal components
#'
#' PCA scores for all analyzed shapes are saved to an Excel file. A plot of the contributions of the
#' specified number of principal components is displayed.
#'
#' @section Dependencies:
#' This function requires the following R packages:
#' - \pkg{openxlsx} for writing PCA scores to an Excel file
#'
#' Shape analysis functions are included from adapted Momocs code (see ATTRIBUTION.md)
#'
#' @examples
#' \dontrun{
#' # Basic shape analysis
#' result <- shape_analysis(
#'   shape_dir = "path/to/shape/directory",
#'   output_file = "my_analysis.xlsx"
#' )
#'
#' # Advanced analysis with custom parameters
#' result <- shape_analysis(
#'   shape_dir = "path/to/shape/directory",
#'   norm = TRUE,
#'   output_dir = "path/to/output/directory",
#'   output_file = "shape_analysis_results.xlsx",
#'   num_pcs = 5,
#'   start_point = "down",
#'   harmonics = 20,
#'   verbose = TRUE
#' )
#'
#' # Access results
#' print(result$summary)
#' head(result$scores)
#' }
#'
#' @export
shape_analysis <- function(shape_dir,
                          norm = TRUE,
                          output_dir = NULL,
                          output_file = "shape_analysis.xlsx",
                          num_pcs = 10,
                          start_point = "left",
                          harmonics = NULL,
                          align_orientation = FALSE,
                          verbose = TRUE) {
  
  # Input validation ----
  .validate_shape_analysis_inputs(
    shape_dir, norm, output_dir, output_file, 
    num_pcs, start_point, harmonics, verbose
  )
  
  # Setup output directory ----
  output_dir <- .setup_output_directory(output_dir, verbose)
  output_file_path <- file.path(output_dir, output_file)
  
  # Import and validate shapes ----
  if (verbose) message("Importing shape files from: ", shape_dir)
  
  shapes <- .import_shape_files(shape_dir, verbose)
  
  # Capture original shape metadata BEFORE any preprocessing
  # This is needed for denormalization during reconstruction
  shape_metadata <- NULL
  if (norm) {
    if (verbose) message("Extracting shape metadata from original shapes...")
    shape_metadata <- list()
    for (i in seq_along(shapes$coo)) {
      coords <- shapes$coo[[i]]
      # Calculate centroid from ORIGINAL coordinates
      centroid <- colMeans(coords)
      # Calculate size (mean distance from centroid)
      dists <- sqrt(rowSums((coords - rep(centroid, each = nrow(coords)))^2))
      size <- mean(dists)
      shape_metadata[[i]] <- list(
        centroid = centroid,
        size = size,
        name = names(shapes$coo)[i]
      )
    }
    if (verbose) message("  Captured metadata for ", length(shape_metadata), " shapes")
  }
  
  # Always pre-process: center -> scale -> slide in user-selected direction
  if (verbose) message("Pre-processing shapes: center -> scale -> slide '", start_point, "'")
  normalized_shapes <- .normalize_shapes(shapes, start_point, align_orientation, verbose)
  
  # Perform Elliptical Fourier Analysis ----
  if (verbose) message("Performing Elliptical Fourier Analysis...")
  # 'norm' controls whether descriptors are normalized to first harmonic (TRUE) or longest radius (FALSE)
  # Use start = TRUE to set a consistent phasing across shapes.
  
  efa_results <- .perform_efa(normalized_shapes, norm = norm, harmonics = harmonics, start = TRUE, verbose = verbose)
  
  # Perform PCA ----
  if (verbose) message("Performing Principal Component Analysis...")
  # Match manual script default: center = TRUE, scale. = FALSE (and keep fallback behavior if needed)
  # We'll directly run with scale.=FALSE to be consistent; robust fallback no longer needed.
  pca_results <- tryCatch({
    PCA(efa_results, center = TRUE, scale. = FALSE)
  }, error = function(e) {
    stop("Principal Component Analysis failed: ", conditionMessage(e), call. = FALSE)
  })
  
  # Store efa_results for reconstruction (keeping reference before any cleanup)
  efa_results_stored <- efa_results
  
  # Debug: Check what's in EFA results for reconstruction
  if (verbose) {
    message("EFA results structure for reconstruction:")
    message("  Components: ", paste(names(efa_results), collapse = ", "))
    message("  norm attribute: ", if (!is.null(efa_results$norm)) efa_results$norm else "NULL")
    message("  r1 present: ", !is.null(efa_results$r1))
    message("  r2 present: ", !is.null(efa_results$r2))
    message("  baseline1 present: ", !is.null(efa_results$baseline1))
    message("  baseline2 present: ", !is.null(efa_results$baseline2))
    message("  method attribute: ", if (!is.null(attr(efa_results, "method"))) attr(efa_results, "method") else "NULL")
  }
  
  # Extract and format results ----
  if (verbose) message("Extracting PCA scores...")
  scores <- .extract_pca_scores(pca_results)
  
  # Save results to Excel ----
  if (verbose) message("Saving results to: ", output_file_path)
  .save_results_to_excel(scores, output_file_path)
  
  # Generate summary and plots ----
  if (verbose) message("Generating summary and plots...")
  analysis_summary <- .generate_pca_summary(pca_results)
  
  # Create PC contribution plot
  pc_plot <- .create_pc_contribution_plot(pca_results, num_pcs)

  # Save additional artifacts (TXT summary and JPG plot) ----
  # Build output file paths based on Excel file name
  output_stem <- tools::file_path_sans_ext(basename(output_file_path))
  summary_txt_path <- file.path(output_dir, paste0(output_stem, "_summary.txt"))
  pc_plot_jpg_path <- file.path(output_dir, paste0(output_stem, "_pc_contrib.jpg"))

  # Write summary text
  tryCatch({
    writeLines(analysis_summary, con = summary_txt_path, useBytes = TRUE)
  }, error = function(e) {
    warning("Failed to write summary TXT: ", conditionMessage(e))
  })

  # Save plot as JPEG (regenerate if needed)
  tryCatch({
    grDevices::jpeg(filename = pc_plot_jpg_path, width = 1600, height = 1200, quality = 95)
    if (!is.null(pc_plot)) {
      print(pc_plot)
    } else {
      max_pcs <- min(num_pcs, ncol(pca_results$x))
      print(PCcontrib(pca_results, nax = 1:max_pcs, sd.r = c(-2,-1,0,1,2)))
    }
    grDevices::dev.off()
  }, error = function(e) {
    grDevices::dev.off()
    warning("Failed to save PC contribution plot JPG: ", conditionMessage(e))
  })
  
  # Save reconstruction data as CSV files ----
  if (verbose) message("Saving reconstruction model data (CSV format)...")
  reconstruction_files <- .save_reconstruction_csv(
    pca_results = pca_results,
    efa_results = efa_results_stored,
    norm = norm,
    harmonics = harmonics,
    start_point = start_point,
    output_dir = output_dir,
    output_stem = output_stem,
    verbose = verbose
  )
  
  # Prepare metadata
  metadata <- list(
    n_shapes = length(shapes),
    n_harmonics = ifelse(is.null(harmonics), "automatic", harmonics),
    normalization = norm,
    used_raw_shapes = FALSE,
    start_point = start_point,
    analysis_date = Sys.time()
  )
  
  if (verbose) message("Analysis completed successfully!")
  
  # Return comprehensive results ----
  structure(
    list(
      pca_results = pca_results,
      efa_results = efa_results_stored,
      scores = scores,
      summary = analysis_summary,
      output_path = output_file_path,
      summary_txt_path = summary_txt_path,
      pc_plot_jpg_path = pc_plot_jpg_path,
      reconstruction_files = reconstruction_files,
      metadata = metadata,
      pc_contribution_plot = pc_plot
    ),
    class = "shape_analysis_result"
  )
}

# Helper Functions ----

#' Validate inputs for shape_analysis function
#' @noRd
.validate_shape_analysis_inputs <- function(shape_dir, norm, output_dir, output_file,
                                          num_pcs, start_point, harmonics, verbose) {
  # Check required packages
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required but not installed. Please install it with: install.packages('openxlsx')",
         call. = FALSE)
  }
  
  # Validate shape_dir
  if (!is.character(shape_dir) || length(shape_dir) != 1 || nchar(shape_dir) == 0) {
    stop("'shape_dir' must be a non-empty character string", call. = FALSE)
  }
  if (!dir.exists(shape_dir)) {
    stop("The specified shape directory does not exist: ", shape_dir, call. = FALSE)
  }
  
  # Validate norm
  if (!is.logical(norm) || length(norm) != 1) {
    stop("'norm' must be a single logical value", call. = FALSE)
  }
  
  # Validate output_dir
  if (!is.null(output_dir)) {
    if (!is.character(output_dir) || length(output_dir) != 1 || nchar(output_dir) == 0) {
      stop("'output_dir' must be NULL or a non-empty character string", call. = FALSE)
    }
  }
  
  # Validate output_file
  if (!is.character(output_file) || length(output_file) != 1 || nchar(output_file) == 0) {
    stop("'output_file' must be a non-empty character string", call. = FALSE)
  }
  if (!grepl("\\.(xlsx|xls)$", output_file, ignore.case = TRUE)) {
    stop("'output_file' must have .xlsx or .xls extension", call. = FALSE)
  }
  
  # Validate num_pcs
  if (!is.numeric(num_pcs) || length(num_pcs) != 1 || num_pcs < 1 || num_pcs > 50 || num_pcs != as.integer(num_pcs)) {
    stop("'num_pcs' must be an integer between 1 and 50", call. = FALSE)
  }
  
  # Validate start_point
  valid_start_points <- c("up", "left", "down", "right")
  if (!is.character(start_point) || length(start_point) != 1 || !start_point %in% valid_start_points) {
    stop("'start_point' must be one of: ", paste(valid_start_points, collapse = ", "), call. = FALSE)
  }
  
  # Validate harmonics
  if (!is.null(harmonics)) {
    if (!is.numeric(harmonics) || length(harmonics) != 1 || harmonics < 1 || harmonics > 100 || harmonics != as.integer(harmonics)) {
      stop("'harmonics' must be NULL or an integer between 1 and 100", call. = FALSE)
    }
  }
  
  # Validate verbose
  if (!is.logical(verbose) || length(verbose) != 1) {
    stop("'verbose' must be a single logical value", call. = FALSE)
  }
}

#' Setup output directory
#' @noRd
.setup_output_directory <- function(output_dir, verbose) {
  if (is.null(output_dir)) {
    output_dir <- getwd()
    if (verbose) message("Using current working directory for output: ", output_dir)
  } else {
    if (!dir.exists(output_dir)) {
      stop("The specified output directory does not exist: ", output_dir, call. = FALSE)
    }
  }
  return(output_dir)
}

#' Import shape files from directory
#' @noRd
.import_shape_files <- function(shape_dir, verbose) {
  # Get list of shape files
  shape_files <- list.files(
    shape_dir, 
    pattern = "\\.(jpg|jpeg)$", 
    full.names = TRUE, 
    ignore.case = TRUE
  )
  
  if (length(shape_files) == 0) {
    stop("No JPG/JPEG files found in the specified directory: ", shape_dir, call. = FALSE)
  }
  
  if (verbose) message("Found ", length(shape_files), " shape files")
  
  # Import shapes
  tryCatch({
    shapes <- import_jpg(shape_files) %>% Out()
    if (verbose) message("Successfully imported ", length(shapes), " shapes")
    return(shapes)
  }, error = function(e) {
    stop("Failed to import shape files: ", e$message, call. = FALSE)
  })
}

#' Normalize shapes
#' @noRd
.normalize_shapes <- function(shapes, start_point, align_orientation, verbose) {
  tryCatch({
    normalized_shapes <- shapes %>%
      coo_center() %>%
      coo_scale()
    if (isTRUE(align_orientation)) {
      normalized_shapes <- normalized_shapes %>% coo_aligncalliper()
      if (verbose) message("Orientation aligned using major axis (caliper alignment)")
    }
    normalized_shapes <- normalized_shapes %>% coo_slidedirection(start_point)
    
    if (verbose) message("Shapes normalized with start point: ", start_point)
    return(normalized_shapes)
  }, error = function(e) {
    stop("Failed to normalize shapes: ", e$message, call. = FALSE)
  })
}

#' Perform Elliptical Fourier Analysis
#' @noRd
.perform_efa <- function(normalized_shapes, norm, harmonics, start = NULL, verbose) {
  tryCatch({
    # Build argument list to support optional 'start' parameter
    args <- list(x = normalized_shapes, norm = norm)
    if (!is.null(harmonics)) args$nb.h <- harmonics
    if (!is.null(start)) args$start <- start
    efa_results <- do.call(efourier, args)
    
    if (verbose) message("EFA completed with normalization: ", norm)
    return(efa_results)
  }, error = function(e) {
    stop("Elliptical Fourier Analysis failed: ", e$message, call. = FALSE)
  })
}

#' Perform Principal Component Analysis
#' @noRd
.perform_pca <- function() {
  # No longer used: PCA is invoked directly with scale. = FALSE to match manual pipeline
}

#' Extract PCA scores as data frame
#' @noRd
.extract_pca_scores <- function(pca_results) {
  tryCatch({
    scores <- as.data.frame(pca_results$x)
    scores$ID <- rownames(scores)
    scores <- scores[, c("ID", setdiff(names(scores), "ID"))]
    return(scores)
  }, error = function(e) {
    stop("Failed to extract PCA scores: ", e$message, call. = FALSE)
  })
}

#' Save results to Excel file
#' @noRd
.save_results_to_excel <- function(scores, output_file_path) {
  tryCatch({
    openxlsx::write.xlsx(scores, file = output_file_path, rowNames = FALSE)
  }, error = function(e) {
    stop("Failed to write Excel file to ", output_file_path, ": ", e$message, call. = FALSE)
  })
}

#' Save reconstruction data as CSV files
#' @noRd
.save_reconstruction_csv <- function(pca_results, efa_results, norm, harmonics, 
                                     start_point, output_dir, output_stem, verbose) {
  
  # Calculate derived values
  n_components <- ncol(pca_results$x)
  n_specimens <- nrow(pca_results$x)
  n_harmonics <- if (!is.null(efa_results$coe)) ncol(efa_results$coe) / 4 else NULL
  
  # Calculate variance explained
  variance_explained <- if (!is.null(pca_results$eig)) {
    pca_results$eig / sum(pca_results$eig) * 100
  } else {
    NULL
  }
  
  # Create file paths
  rotation_path <- file.path(output_dir, paste0(output_stem, "_pca_rotation.csv"))
  center_path <- file.path(output_dir, paste0(output_stem, "_pca_center.csv"))
  sdev_path <- file.path(output_dir, paste0(output_stem, "_pca_sdev.csv"))
  metadata_path <- file.path(output_dir, paste0(output_stem, "_reconstruction_metadata.txt"))
  
  # Save PCA rotation matrix (eigenvectors)
  tryCatch({
    rotation_df <- as.data.frame(pca_results$rotation)
    rotation_df <- cbind(coefficient = rownames(pca_results$rotation), rotation_df)
    write.csv(rotation_df, file = rotation_path, row.names = FALSE)
    if (verbose) message("  Saved: ", basename(rotation_path))
  }, error = function(e) {
    warning("Failed to save rotation matrix: ", e$message)
  })
  
  # Save PCA center vector (mean coefficients)
  tryCatch({
    center_df <- data.frame(
      coefficient = names(pca_results$center),
      value = pca_results$center
    )
    write.csv(center_df, file = center_path, row.names = FALSE)
    if (verbose) message("  Saved: ", basename(center_path))
  }, error = function(e) {
    warning("Failed to save center vector: ", e$message)
  })
  
  # Save PCA standard deviations
  tryCatch({
    sdev_df <- data.frame(
      PC = paste0("PC", 1:length(pca_results$sdev)),
      sdev = pca_results$sdev
    )
    if (!is.null(variance_explained)) {
      sdev_df$variance_pct <- variance_explained
    }
    write.csv(sdev_df, file = sdev_path, row.names = FALSE)
    if (verbose) message("  Saved: ", basename(sdev_path))
  }, error = function(e) {
    warning("Failed to save standard deviations: ", e$message)
  })
  
  # Save metadata as human-readable text file
  tryCatch({
    info_lines <- c(
      "Shape Reconstruction Model Metadata",
      "====================================",
      "",
      paste("Generated:", format(Sys.time())),
      "",
      "Analysis Parameters:",
      "--------------------",
      paste("  Normalization:", norm),
      paste("  Harmonics Used:", ifelse(is.null(n_harmonics), "automatic", n_harmonics)),
      paste("  Start Point:", start_point),
      paste("  Number of Specimens:", n_specimens),
      paste("  Number of PCs:", n_components),
      paste("  Total Coefficients:", length(pca_results$center)),
      "",
      "Reconstruction Components:",
      "-------------------------",
      paste("  Eigenvectors (rotation):", nrow(pca_results$rotation), "x", ncol(pca_results$rotation)),
      paste("  Center vector length:", length(pca_results$center)),
      paste("  Standard deviations:", length(pca_results$sdev)),
      "",
      "Variance Explained by PC:",
      "------------------------"
    )
    
    if (!is.null(variance_explained)) {
      n_show <- min(10, length(variance_explained))
      for (i in 1:n_show) {
        info_lines <- c(info_lines, sprintf("  PC%d: %.2f%%", i, variance_explained[i]))
      }
      cumsum_var <- cumsum(variance_explained)
      info_lines <- c(info_lines, "", 
                     sprintf("  Cumulative variance (first 10 PCs): %.2f%%", cumsum_var[min(10, length(cumsum_var))]))
    }
    
    info_lines <- c(info_lines, "",
      "Usage:",
      "------",
      "To load this reconstruction model in R:",
      paste0("  model <- load_reconstruction_csv('", output_dir, "')"),
      "",
      "The model includes 4 CSV files:",
      paste0("  - ", output_stem, "_pca_rotation.csv"),
      paste0("  - ", output_stem, "_pca_center.csv"),
      paste0("  - ", output_stem, "_pca_sdev.csv"),
      paste0("  - ", output_stem, "_reconstruction_metadata.txt (this file)")
    )
    
    writeLines(info_lines, con = metadata_path, useBytes = TRUE)
    if (verbose) message("  Saved: ", basename(metadata_path))
  }, error = function(e) {
    warning("Failed to save reconstruction metadata: ", e$message)
  })
  
  # Return list of created files
  return(list(
    rotation = rotation_path,
    center = center_path,
    sdev = sdev_path,
    metadata = metadata_path
  ))
}

#' Generate PCA summary
#' @noRd
.generate_pca_summary <- function(pca_results) {
  tryCatch({
    pca_summary <- capture.output(summary(pca_results))
    return(paste(pca_summary, collapse = "\n"))
  }, error = function(e) {
    warning("Failed to generate PCA summary: ", e$message)
    return("PCA summary generation failed")
  })
}

#' Create PC contribution plot
#' @noRd
.create_pc_contribution_plot <- function(pca_results, num_pcs) {
  tryCatch({
    # Ensure we don't request more PCs than available
    max_pcs <- min(num_pcs, ncol(pca_results$x))
    PCcontrib(pca_results, nax = 1:max_pcs, sd.r = c(-2, -1, 0, 1, 2))
  }, error = function(e) {
    warning("Failed to create PC contribution plot: ", e$message)
    return(NULL)
  })
}

# Print method for shape_analysis_result ----

#' Print method for shape_analysis_result objects
#' @param x A shape_analysis_result object
#' @param ... Additional arguments (ignored)
#' @export
print.shape_analysis_result <- function(x, ...) {
  cat("Shape Analysis Results\n")
  cat("======================\n\n")
  cat("Analysis metadata:\n")
  cat("  Number of shapes:", x$metadata$n_shapes, "\n")
  cat("  Harmonics:", x$metadata$n_harmonics, "\n")
  cat("  Normalization:", x$metadata$normalization, "\n")
  cat("  Start point:", x$metadata$start_point, "\n")
  cat("  Analysis date:", format(x$metadata$analysis_date), "\n\n")
  
  cat("Output files:\n")
  cat("  PCA scores:", x$output_path, "\n")
  cat("  Summary:", x$summary_txt_path, "\n")
  cat("  PC plot:", x$pc_plot_jpg_path, "\n")
  if (!is.null(x$reconstruction_files)) {
    cat("  Reconstruction files:\n")
    cat("    - Rotation:", basename(x$reconstruction_files$rotation), "\n")
    cat("    - Center:", basename(x$reconstruction_files$center), "\n")
    cat("    - Sdev:", basename(x$reconstruction_files$sdev), "\n")
    cat("    - Metadata:", basename(x$reconstruction_files$metadata), "\n")
  }
  cat("  Reconstruction info:", x$reconstruction_info_path, "\n\n")
  
  cat("PCA Summary:\n")
  cat(x$summary, "\n")
}

# Reconstruction Model Loader ----

#' Load Shape Reconstruction Model from CSV Files
#'
#' Loads a reconstruction model saved by `shape_analysis()` from CSV files
#' that contain PCA eigenvectors, centering values, and standard deviations.
#'
#' @param folder_path Character string specifying the folder containing the CSV files,
#'   OR the path to one of the CSV files (e.g., "*_pca_rotation.csv"). The function
#'   will automatically find all related files based on the common prefix.
#' @param validate Logical indicating whether to validate the loaded data. Default: TRUE.
#' @param verbose Logical indicating whether to print loading messages. Default: TRUE.
#'
#' @return A list containing:
#'   \describe{
#'     \item{rotation}{Matrix of eigenvectors (loadings) from PCA}
#'     \item{center}{Centering vector from PCA}
#'     \item{sdev}{Standard deviations from PCA}
#'     \item{variance_explained}{Vector of variance explained by each PC (if available)}
#'     \item{parameters}{List of analysis parameters (norm, harmonics, start_point, etc.)}
#'   }
#'
#' @details
#' This function loads reconstruction models created by `shape_analysis()` in CSV format.
#' The model is split across 4 files:
#' 
#' - `*_pca_rotation.csv`: Eigenvector matrix
#' - `*_pca_center.csv`: Mean coefficient vector
#' - `*_pca_sdev.csv`: Standard deviations and variance explained
#' - `*_reconstruction_metadata.txt`: Analysis parameters
#'
#' The loaded model can be used with shape reconstruction functions to generate
#' shape outlines from PC scores.
#'
#' @examples
#' \dontrun{
#' # Load a reconstruction model by folder
#' model <- load_reconstruction_csv("path/to/analysis_folder")
#'
#' # Or by pointing to one of the files
#' model <- load_reconstruction_csv("analysis_pca_rotation.csv")
#'
#' # Check model information
#' print(model$parameters)
#' }
#'
#' @export
load_reconstruction_csv <- function(folder_path, validate = TRUE, verbose = TRUE) {
  
  # Input validation ----
  if (!is.character(folder_path) || length(folder_path) != 1 || !nzchar(folder_path)) {
    stop("'folder_path' must be a non-empty character string", call. = FALSE)
  }
  
  if (!is.logical(validate) || length(validate) != 1) {
    stop("'validate' must be a single logical value", call. = FALSE)
  }
  
  if (!is.logical(verbose) || length(verbose) != 1) {
    stop("'verbose' must be a single logical value", call. = FALSE)
  }
  
  # Determine file paths ----
  if (dir.exists(folder_path)) {
    # It's a folder - find the files
    csv_files <- list.files(folder_path, pattern = "_pca_rotation\\.csv$", full.names = TRUE)
    if (length(csv_files) == 0) {
      stop("No reconstruction CSV files found in folder: ", folder_path, call. = FALSE)
    }
    rotation_path <- csv_files[1]
  } else if (file.exists(folder_path)) {
    # It's a file - extract the base name
    rotation_path <- folder_path
  } else {
    stop("Path does not exist: ", folder_path, call. = FALSE)
  }
  
  # Extract base name (remove _pca_rotation.csv suffix)
  base_name <- sub("_pca_rotation\\.csv$", "", rotation_path)
  folder <- dirname(rotation_path)
  
  center_path <- paste0(base_name, "_pca_center.csv")
  sdev_path <- paste0(base_name, "_pca_sdev.csv")
  metadata_path <- paste0(base_name, "_reconstruction_metadata.txt")
  
  # Check all files exist
  missing_files <- c()
  if (!file.exists(rotation_path)) missing_files <- c(missing_files, "rotation")
  if (!file.exists(center_path)) missing_files <- c(missing_files, "center")
  if (!file.exists(sdev_path)) missing_files <- c(missing_files, "sdev")
  
  if (length(missing_files) > 0) {
    stop("Missing required CSV files: ", paste(missing_files, collapse = ", "), call. = FALSE)
  }
  
  # Load data ----
  if (verbose) message("Loading reconstruction model from CSV files...")
  
  # Load rotation matrix
  rotation_df <- tryCatch({
    read.csv(rotation_path, stringsAsFactors = FALSE)
  }, error = function(e) {
    stop("Failed to load rotation matrix: ", e$message, call. = FALSE)
  })
  
  rotation <- as.matrix(rotation_df[, -1])  # Remove first column (coefficient names)
  rownames(rotation) <- rotation_df[, 1]
  
  if (verbose) message("  Loaded rotation matrix: ", nrow(rotation), " x ", ncol(rotation))
  
  # Load center vector
  center_df <- tryCatch({
    read.csv(center_path, stringsAsFactors = FALSE)
  }, error = function(e) {
    stop("Failed to load center vector: ", e$message, call. = FALSE)
  })
  
  center <- center_df$value
  names(center) <- center_df$coefficient
  
  if (verbose) message("  Loaded center vector: ", length(center), " values")
  
  # Load standard deviations
  sdev_df <- tryCatch({
    read.csv(sdev_path, stringsAsFactors = FALSE)
  }, error = function(e) {
    stop("Failed to load standard deviations: ", e$message, call. = FALSE)
  })
  
  sdev <- sdev_df$sdev
  variance_explained <- if ("variance_pct" %in% names(sdev_df)) sdev_df$variance_pct else NULL
  
  if (verbose) message("  Loaded sdev vector: ", length(sdev), " values")
  
  # Parse metadata if available
  parameters <- list()
  if (file.exists(metadata_path)) {
    metadata_lines <- readLines(metadata_path, warn = FALSE)
    
    # Extract parameters from metadata file
    norm_line <- grep("Normalization:", metadata_lines, value = TRUE)
    if (length(norm_line) > 0) {
      parameters$norm <- grepl("TRUE", norm_line, ignore.case = TRUE)
    }
    
    harmonics_line <- grep("Harmonics Used:", metadata_lines, value = TRUE)
    if (length(harmonics_line) > 0) {
      parameters$n_harmonics <- as.numeric(gsub(".*: ", "", harmonics_line))
    }
    
    start_line <- grep("Start Point:", metadata_lines, value = TRUE)
    if (length(start_line) > 0) {
      parameters$start_point <- trimws(gsub(".*: ", "", start_line))
    }
    
    if (verbose) message("  Loaded metadata: norm=", parameters$norm, ", harmonics=", parameters$n_harmonics)
  }
  
  # Add computed parameters
  parameters$n_components <- ncol(rotation)
  parameters$n_coefficients <- length(center)
  
  # Build model object
  model <- list(
    rotation = rotation,
    center = center,
    sdev = sdev,
    variance_explained = variance_explained,
    parameters = parameters
  )
  
  # Validate ----
  if (validate) {
    if (verbose) message("Validating model structure...")
    
    # Check dimensions consistency
    if (nrow(rotation) != length(center)) {
      stop("Dimension mismatch: rotation rows (", nrow(rotation),
           ") != center length (", length(center), ")", call. = FALSE)
    }
    
    if (ncol(rotation) != length(sdev)) {
      warning("Number of PCs in rotation (", ncol(rotation),
              ") != sdev length (", length(sdev), ")")
    }
    
    if (verbose) {
      message("Model validation passed")
      message("  Coefficients: ", length(center))
      message("  PCs: ", ncol(rotation))
      if (!is.null(parameters$n_harmonics)) {
        message("  Harmonics: ", parameters$n_harmonics)
      }
      if (!is.null(parameters$norm)) {
        message("  Normalization: ", parameters$norm)
      }
    }
  }
  
  # Return model
  if (verbose) message("Reconstruction model loaded successfully")
  
  return(model)
}