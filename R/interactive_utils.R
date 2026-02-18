#' Interactive Plotting Utilities
#'
#' Helper functions for interactive morphospace plots with real-time shape reconstruction.

# Helper operator for NULL coalescing
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

#' Extract PCA model components for caching
#'
#' Extracts the essential PCA model components (rotation, center, sdev, method)
#' needed for shape reconstruction from PC scores.
#'
#' @param pca_obj A PCA object (from Momocs PCA function)
#' @return A list containing rotation, center, sdev, and method
#' @export
extract_pca_model <- function(pca_obj) {
  if (!inherits(pca_obj, "PCA")) {
    stop("Input must be a PCA object from Momocs")
  }
  
  model <- list(
    rotation = pca_obj$rotation,
    center = pca_obj$center,
    sdev = pca_obj$sdev,
    method = pca_obj$method
  )
  
  # Validate components
  if (is.null(model$rotation) || is.null(model$center) || is.null(model$sdev)) {
    stop("PCA object is missing essential components (rotation, center, or sdev)")
  }
  
  return(model)
}


#' Load PCA model from CSV files for plotting
#'
#' Auto-detects and loads PCA model CSV files created by shape_analysis().
#' Can accept either a folder path, or a path to any of the PCA CSV files.
#'
#' @param data_file_path Path to folder containing PCA CSV files, or path to any PCA CSV file
#'   (e.g., *_pca_rotation.csv, *_pca_center.csv, or *_pca_sdev.csv)
#' @return A list containing the PCA model components, or NULL if not found
#' @export
load_pca_model_for_plotting <- function(data_file_path) {
  if (is.null(data_file_path) || !nzchar(data_file_path)) {
    warning("No file path provided")
    return(NULL)
  }
  
  # Determine the base path
  if (dir.exists(data_file_path)) {
    # It's a directory - look for rotation file
    csv_files <- list.files(data_file_path, pattern = "_pca_rotation\\.csv$", full.names = TRUE)
    if (length(csv_files) == 0) {
      message("No PCA model files found in directory: ", data_file_path)
      return(NULL)
    }
    rotation_path <- csv_files[1]
    base_path <- sub("_pca_rotation\\.csv$", "", rotation_path)
  } else if (file.exists(data_file_path)) {
    # It's a file - determine which type and extract base name
    if (grepl("_pca_rotation\\.csv$", data_file_path)) {
      base_path <- sub("_pca_rotation\\.csv$", "", data_file_path)
    } else if (grepl("_pca_center\\.csv$", data_file_path)) {
      base_path <- sub("_pca_center\\.csv$", "", data_file_path)
    } else if (grepl("_pca_sdev\\.csv$", data_file_path)) {
      base_path <- sub("_pca_sdev\\.csv$", "", data_file_path)
    } else {
      # Assume it's a data file and try to construct PCA file paths
      data_dir <- dirname(data_file_path)
      base_name <- tools::file_path_sans_ext(basename(data_file_path))
      base_path <- file.path(data_dir, base_name)
    }
  } else {
    warning("File or directory not found: ", data_file_path)
    return(NULL)
  }
  
  # Construct expected file paths from base path
  rotation_path <- paste0(base_path, "_pca_rotation.csv")
  center_path <- paste0(base_path, "_pca_center.csv")
  sdev_path <- paste0(base_path, "_pca_sdev.csv")
  metadata_path <- paste0(base_path, "_reconstruction_metadata.txt")
  
  # Check if files exist
  if (!file.exists(rotation_path) || !file.exists(center_path) || !file.exists(sdev_path)) {
    message("PCA model files not found. Expected files:")
    message("  - ", basename(rotation_path))
    message("  - ", basename(center_path))
    message("  - ", basename(sdev_path))
    message("Base path used: ", base_path)
    return(NULL)
  }
  
  # Try using load_reconstruction_csv if available
  if (exists("load_reconstruction_csv", mode = "function")) {
    tryCatch({
      model <- load_reconstruction_csv(rotation_path, verbose = FALSE)
      message("Successfully loaded PCA model")
      message("  Method: ", model$method %||% "efourier")
      message("  Harmonics: ", model$n_harmonics %||% "unknown")
      message("  PCs available: ", length(model$sdev))
      return(model)
    }, error = function(e) {
      warning("Failed to load PCA model using load_reconstruction_csv: ", e$message)
      # Fall through to manual loading
    })
  }
  
  # Manual loading as fallback
  tryCatch({
    # Load rotation matrix
    rotation_df <- read.csv(rotation_path, stringsAsFactors = FALSE)
    rotation <- as.matrix(rotation_df[, -1, drop = FALSE])
    rownames(rotation) <- rotation_df[, 1]
    
    # Load center vector
    center_df <- read.csv(center_path, stringsAsFactors = FALSE)
    center <- setNames(center_df$value, center_df$coefficient)
    
    # Load standard deviations
    sdev_df <- read.csv(sdev_path, stringsAsFactors = FALSE)
    sdev <- sdev_df$sdev
    
    # Load metadata if available
    method <- "efourier"  # Default
    n_harmonics <- NULL
    norm <- TRUE
    
    if (file.exists(metadata_path)) {
      metadata_lines <- readLines(metadata_path)
      
      # Extract method from metadata
      method_line <- grep("Fourier Method:", metadata_lines, value = TRUE)
      if (length(method_line) > 0) {
        method <- trimws(sub(".*Fourier Method:\\s*", "", method_line[1]))
      }
      
      # Extract harmonics
      harm_line <- grep("Harmonics Used:", metadata_lines, value = TRUE)
      if (length(harm_line) > 0) {
        harm_str <- trimws(sub(".*Harmonics Used:\\s*", "", harm_line[1]))
        if (harm_str != "automatic") {
          n_harmonics <- as.integer(harm_str)
        }
      }
      
      # Extract normalization
      norm_line <- grep("Normalization:", metadata_lines, value = TRUE)
      if (length(norm_line) > 0) {
        norm_str <- trimws(sub(".*Normalization:\\s*", "", norm_line[1]))
        norm <- (norm_str == "TRUE")
      }
    }
    
    # Calculate harmonics from coefficient count if not in metadata
    if (is.null(n_harmonics)) {
      n_coefs <- length(center)
      # efourier has 4 coefficients per harmonic, rfourier/tfourier have 2
      cph <- ifelse(method == "efourier", 4, 2)
      n_harmonics <- n_coefs / cph
    }
    
    model <- list(
      rotation = rotation,
      center = center,
      sdev = sdev,
      method = method,
      n_harmonics = n_harmonics,
      norm = norm
    )
    
    message("Successfully loaded PCA model for interactive plotting")
    message("  Method: ", method)
    message("  Harmonics: ", n_harmonics)
    message("  PCs available: ", length(sdev))
    
    return(model)
    
  }, error = function(e) {
    warning("Failed to load PCA model files: ", e$message)
    return(NULL)
  })
}


#' Convert ggplot2 to interactive plotly with custom hover
#'
#' Converts a ggplot2 object to plotly with custom hover text showing IDs
#' and coordinates. Configures event tracking for morphospace interaction.
#'
#' @param gg_plot A ggplot2 object
#' @param data Original data frame with ID column
#' @param x_col X-axis column name
#' @param y_col Y-axis column name
#' @param id_col Column name containing IDs (default: "ID")
#' @param source_id Source identifier for plotly events (default: "morphospace")
#' @return A plotly object
#' @export
convert_to_interactive_plot <- function(gg_plot, data, x_col, y_col, 
                                       id_col = "ID", source_id = "morphospace") {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' is required for interactive plots")
  }
  
  # Add ID to hover text if available
  if (id_col %in% names(data)) {
    hover_text <- paste0(
      "ID: ", data[[id_col]], "\n",
      x_col, ": ", round(data[[x_col]], 3), "\n",
      y_col, ": ", round(data[[y_col]], 3)
    )
    
    # Add hover text to plot data
    gg_plot$data$hover_text <- hover_text[match(
      paste(gg_plot$data[[x_col]], gg_plot$data[[y_col]]),
      paste(data[[x_col]], data[[y_col]])
    )]
  }
  
  # Convert to plotly
  plotly_obj <- plotly::ggplotly(gg_plot, tooltip = "text", source = source_id)
  
  # Configure layout
  plotly_obj <- plotly::layout(
    plotly_obj,
    hovermode = "closest",
    dragmode = "pan"
  )
  
  return(plotly_obj)
}


#' Reconstruct shape from hover coordinates
#'
#' Internal helper to reconstruct a shape from PC coordinates in real-time
#' during hover events. Wrapper around reconstruction functions.
#'
#' @param pca_model PCA model list (from load_pca_model_for_plotting or extract_pca_model)
#' @param x_value X-axis coordinate value
#' @param y_value Y-axis coordinate value
#' @param x_pc_index PC index for x-axis (e.g., 1 for PC1, 3 for PC3)
#' @param y_pc_index PC index for y-axis (e.g., 2 for PC2, 4 for PC4)
#' @param other_pcs Named vector of other PC values (default: all zeros)
#' @param nb_pts Number of points for shape outline (default: 120)
#' @return Matrix of x,y coordinates for the reconstructed shape
#' @noRd
.reconstruct_shape_from_hover <- function(pca_model, x_value, y_value, 
                                          x_pc_index = 1, y_pc_index = 2, 
                                          other_pcs = NULL, nb_pts = 120) {
  # Construct PC scores vector
  n_pcs <- length(pca_model$sdev)
  pc_scores <- rep(0, n_pcs)
  
  # Set the values for the plotted PCs
  pc_scores[x_pc_index] <- x_value
  pc_scores[y_pc_index] <- y_value
  
  # Add any other specified PCs
  if (!is.null(other_pcs) && length(other_pcs) > 0) {
    pc_indices <- as.integer(gsub("PC", "", names(other_pcs)))
    # Don't override the x and y axes
    pc_indices <- pc_indices[!pc_indices %in% c(x_pc_index, y_pc_index) & pc_indices <= n_pcs]
    if (length(pc_indices) > 0) {
      pc_scores[pc_indices] <- other_pcs[paste0("PC", pc_indices)]
    }
  }
  
  # Reconstruct coefficients from PC scores (absolute values, not SD units)
  contribution <- as.vector(pc_scores %*% t(pca_model$rotation))
  reconstructed_coefs <- pca_model$center + contribution
  
  # Split into Fourier components based on method
  method <- pca_model$method
  n_harmonics <- pca_model$n_harmonics
  
  if (is.null(method)) method <- "efourier"
  if (is.null(n_harmonics)) {
    # Calculate from coefficient count
    cph <- ifelse(method == "efourier", 4, 2)
    n_harmonics <- length(reconstructed_coefs) / cph
  }
  
  # Use coeff_split to organize coefficients
  cph <- ifelse(method == "efourier", 4, 2)
  coef_list <- coeff_split(reconstructed_coefs, cph = cph)
  
  # For efourier, set ao and co to 0 (position is determined by plot coordinates)
  if (method == "efourier") {
    coef_list$ao <- 0
    coef_list$co <- 0
  }
  
  # Call appropriate inverse Fourier function
  coords <- switch(method,
    efourier = efourier_i(coef_list, nb.h = n_harmonics, nb.pts = nb_pts),
    rfourier = if (exists("rfourier_i", mode = "function")) {
      rfourier_i(coef_list, nb.h = n_harmonics, nb.pts = nb_pts)
    } else {
      efourier_i(coef_list, nb.h = n_harmonics, nb.pts = nb_pts)
    },
    tfourier = if (exists("tfourier_i", mode = "function")) {
      tfourier_i(coef_list, nb.h = n_harmonics, nb.pts = nb_pts)
    } else {
      efourier_i(coef_list, nb.h = n_harmonics, nb.pts = nb_pts)
    },
    sfourier = if (exists("sfourier_i", mode = "function")) {
      sfourier_i(coef_list, nb.h = n_harmonics, nb.pts = nb_pts)
    } else {
      efourier_i(coef_list, nb.h = n_harmonics, nb.pts = nb_pts)
    },
    # Default to efourier
    efourier_i(coef_list, nb.h = n_harmonics, nb.pts = nb_pts)
  )
  
  return(coords)
}
