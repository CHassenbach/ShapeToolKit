#' Create Elbow Plot for Optimal Cluster Number Determination
#'
#' Generates an elbow plot to help determine the optimal number of clusters for
#' k-means clustering by plotting the within-cluster sum of squares (WCSS) against
#' the number of clusters. Includes multiple methods for optimal k detection.
#'
#' @param data A data frame containing the points for clustering analysis.
#' @param x_col Character string specifying the x-axis column name.
#' @param y_col Character string specifying the y-axis column name.
#' @param clustering_options List containing clustering options:
#'   \describe{
#'     \item{max_k}{Maximum number of clusters to test (default: 10)}
#'     \item{min_k}{Minimum number of clusters to test (default: 1)}
#'     \item{scale}{Scale coordinates before clustering (default: TRUE)}
#'     \item{nstart}{Number of random starts for k-means (default: 25)}
#'   }
#' @param detection_methods Character vector of methods to detect optimal k:
#'   "elbow" (elbow method), "silhouette" (silhouette analysis), 
#'   "gap" (gap statistic). Default: c("elbow").
#' @param styling List containing plot styling options:
#'   \describe{
#'     \item{title}{Plot title (default: "Elbow Plot for Optimal k")}
#'     \item{point_size}{Size of points (default: 3)}
#'     \item{line_size}{Size of connecting line (default: 1)}
#'     \item{point_color}{Color of points (default: "steelblue")}
#'     \item{line_color}{Color of line (default: "steelblue")}
#'     \item{theme_base_size}{Base font size (default: 12)}
#'   }
#' @param labels List containing axis label options:
#'   \describe{
#'     \item{x_label}{X-axis label (default: "Number of Clusters (k)")}
#'     \item{y_label}{Y-axis label (default: "Within-Cluster Sum of Squares")}
#'   }
#' @param export_options List containing export options (export, filename, path, format).
#' @param verbose Logical indicating whether to print progress messages. Default: TRUE.
#'
#' @return A list containing:
#'   \describe{
#'     \item{plot}{The ggplot2 elbow plot object}
#'     \item{wcss_data}{Data frame with k values and corresponding WCSS}
#'     \item{optimal_k}{Suggested optimal k values by different methods}
#'     \item{cluster_results}{Full clustering results for each k}
#'   }
#'
#' @examples
#' # Basic elbow plot
#' test_data <- data.frame(
#'   x = c(rnorm(50, 0, 1), rnorm(50, 3, 1), rnorm(50, 6, 1)),
#'   y = c(rnorm(50, 0, 1), rnorm(50, 3, 1), rnorm(50, 6, 1))
#' )
#'
#' result <- elbow_plot(
#'   data = test_data,
#'   x_col = "x", y_col = "y"
#' )
#'
#' # Advanced elbow plot with multiple detection methods
#' result <- elbow_plot(
#'   data = test_data,
#'   x_col = "x", y_col = "y",
#'   clustering_options = list(
#'     max_k = 15,
#'     min_k = 2,
#'     scale = TRUE,
#'     nstart = 50
#'   ),
#'   detection_methods = c("elbow", "silhouette"),
#'   styling = list(
#'     title = "K-means Elbow Analysis",
#'     point_size = 4,
#'     point_color = "red"
#'   )
#' )
#'
#' # View results
#' print(result$plot)
#' print(result$optimal_k)
#'
#' @export
elbow_plot <- function(data,
                      x_col,
                      y_col,
                      clustering_options = list(),
                      detection_methods = c("elbow"),
                      styling = list(),
                      labels = list(),
                      export_options = list(),
                      verbose = TRUE) {
  
  # Input validation ----
  .validate_elbow_plot_inputs(data, x_col, y_col, detection_methods, verbose)
  
  # Setup parameters ----
  params <- .setup_elbow_plot_params(clustering_options, styling, labels, export_options, verbose)
  
  # Prepare data for clustering ----
  clean_data <- .prepare_elbow_data(data, x_col, y_col, params$clustering_options$scale, verbose)
  
  if (verbose) {
    message("Computing WCSS for k = ", params$clustering_options$min_k, " to ", params$clustering_options$max_k)
    message("Using ", nrow(clean_data), " data points")
  }
  
  # Compute WCSS for range of k values ----
  wcss_results <- .compute_wcss_range(clean_data, params$clustering_options, verbose)
  
  # Apply optimal k detection methods ----
  optimal_k_results <- .detect_optimal_k(wcss_results, clean_data, detection_methods, params, verbose)
  
  # Create elbow plot ----
  elbow_plot <- .create_elbow_plot(wcss_results$wcss_data, optimal_k_results, params)
  
  # Export if requested ----
  if (params$export_options$export) {
    .export_elbow_plot(elbow_plot, params$export_options, verbose)
  }
  
  # Prepare results ----
  if (verbose) {
    message("Elbow analysis completed!")
    if (length(optimal_k_results) > 0) {
      for (method in names(optimal_k_results)) {
        message("Optimal k (", method, "): ", optimal_k_results[[method]])
      }
    }
  }
  
  structure(
    list(
      plot = elbow_plot,
      wcss_data = wcss_results$wcss_data,
      optimal_k = optimal_k_results,
      cluster_results = wcss_results$cluster_results
    ),
    class = "elbow_plot_result"
  )
}

# Input Validation ----

#' Validate inputs for elbow_plot function
#' @noRd
.validate_elbow_plot_inputs <- function(data, x_col, y_col, detection_methods, verbose) {
  
  # Check data
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame", call. = FALSE)
  }
  if (nrow(data) == 0) {
    stop("'data' cannot be empty", call. = FALSE)
  }
  
  # Check columns
  if (!is.character(x_col) || length(x_col) != 1) {
    stop("'x_col' must be a single character string", call. = FALSE)
  }
  if (!is.character(y_col) || length(y_col) != 1) {
    stop("'y_col' must be a single character string", call. = FALSE)
  }
  
  if (!x_col %in% colnames(data)) {
    stop("Column '", x_col, "' does not exist in data", call. = FALSE)
  }
  if (!y_col %in% colnames(data)) {
    stop("Column '", y_col, "' does not exist in data", call. = FALSE)
  }
  
  # Check data types
  if (!is.numeric(data[[x_col]])) {
    stop("Column '", x_col, "' must be numeric", call. = FALSE)
  }
  if (!is.numeric(data[[y_col]])) {
    stop("Column '", y_col, "' must be numeric", call. = FALSE)
  }
  
  # Check detection methods
  valid_methods <- c("elbow", "silhouette", "gap")
  if (!all(detection_methods %in% valid_methods)) {
    invalid <- detection_methods[!detection_methods %in% valid_methods]
    stop("Invalid detection methods: ", paste(invalid, collapse = ", "), 
         ". Valid methods: ", paste(valid_methods, collapse = ", "), call. = FALSE)
  }
  
  # Check verbose
  if (!is.logical(verbose) || length(verbose) != 1) {
    stop("'verbose' must be a single logical value", call. = FALSE)
  }
}

# Parameter Setup ----

#' Setup parameters for elbow plot
#' @noRd
.setup_elbow_plot_params <- function(clustering_options, styling, labels, export_options, verbose) {
  
  # Clustering options defaults
  clustering_defaults <- list(
    max_k = 10,
    min_k = 1,
    scale = TRUE,
    nstart = 25
  )
  clustering_options <- utils::modifyList(clustering_defaults, clustering_options)
  
  # Validate clustering options
  if (clustering_options$max_k < clustering_options$min_k) {
    stop("'max_k' must be >= 'min_k'", call. = FALSE)
  }
  if (clustering_options$min_k < 1) {
    stop("'min_k' must be >= 1", call. = FALSE)
  }
  if (clustering_options$nstart < 1) {
    stop("'nstart' must be >= 1", call. = FALSE)
  }
  
  # Styling defaults
  styling_defaults <- list(
    title = "Elbow Plot for Optimal k",
    point_size = 3,
    line_size = 1,
    point_color = "steelblue",
    line_color = "steelblue",
    theme_base_size = 12
  )
  styling <- utils::modifyList(styling_defaults, styling)
  
  # Label defaults
  labels_defaults <- list(
    x_label = "Number of Clusters (k)",
    y_label = "Within-Cluster Sum of Squares"
  )
  labels <- utils::modifyList(labels_defaults, labels)
  
  # Export defaults
  export_defaults <- list(
    export = FALSE,
    filename = "elbow_plot",
    path = NULL,
    format = "png",
    width = 10,
    height = 6,
    dpi = 300
  )
  export_options <- utils::modifyList(export_defaults, export_options)
  
  return(list(
    clustering_options = clustering_options,
    styling = styling,
    labels = labels,
    export_options = export_options
  ))
}

# Data Preparation ----

#' Prepare data for elbow analysis
#' @noRd
.prepare_elbow_data <- function(data, x_col, y_col, scale_data, verbose) {
  
  # Extract coordinates and remove missing values
  coordinates <- data[, c(x_col, y_col)]
  clean_coords <- coordinates[complete.cases(coordinates), ]
  
  # Remove infinite values
  finite_rows <- apply(clean_coords, 1, function(x) all(is.finite(x)))
  clean_coords <- clean_coords[finite_rows, ]
  
  rows_removed <- nrow(data) - nrow(clean_coords)
  if (verbose && rows_removed > 0) {
    message("Removed ", rows_removed, " rows with missing or infinite values")
  }
  
  if (nrow(clean_coords) < 2) {
    stop("Insufficient valid data points for clustering analysis", call. = FALSE)
  }
  
  # Scale if requested
  if (scale_data) {
    clean_coords <- scale(clean_coords)
    if (verbose) message("Data scaled for clustering analysis")
  }
  
  return(clean_coords)
}

# WCSS Computation ----

#' Compute WCSS for range of k values
#' @noRd
.compute_wcss_range <- function(coordinates, clustering_options, verbose) {
  
  k_range <- clustering_options$min_k:clustering_options$max_k
  wcss_values <- numeric(length(k_range))
  cluster_results <- list()
  
  for (i in seq_along(k_range)) {
    k <- k_range[i]
    
    if (verbose && (i %% 3 == 0 || i == length(k_range))) {
      message("Computing k = ", k, " (", i, "/", length(k_range), ")")
    }
    
    tryCatch({
      if (k == 1) {
        # For k=1, WCSS is the total sum of squares from the centroid
        centroid <- colMeans(coordinates)
        wcss_values[i] <- sum(apply(coordinates, 1, function(x) sum((x - centroid)^2)))
        cluster_results[[i]] <- list(
          cluster = rep(1, nrow(coordinates)),
          centers = matrix(centroid, nrow = 1),
          tot.withinss = wcss_values[i]
        )
      } else {
        # Regular k-means
        result <- stats::kmeans(coordinates, centers = k, nstart = clustering_options$nstart)
        wcss_values[i] <- result$tot.withinss
        cluster_results[[i]] <- result
      }
    }, error = function(e) {
      if (verbose) warning("Failed to compute k-means for k = ", k, ": ", e$message)
      wcss_values[i] <- NA
      cluster_results[[i]] <- NULL
    })
  }
  
  # Create WCSS data frame
  wcss_data <- data.frame(
    k = k_range,
    wcss = wcss_values,
    stringsAsFactors = FALSE
  )
  
  # Remove any failed computations
  valid_rows <- !is.na(wcss_data$wcss)
  wcss_data <- wcss_data[valid_rows, ]
  cluster_results <- cluster_results[valid_rows]
  
  if (nrow(wcss_data) == 0) {
    stop("Failed to compute WCSS for any k values", call. = FALSE)
  }
  
  return(list(
    wcss_data = wcss_data,
    cluster_results = cluster_results
  ))
}

# Optimal K Detection ----

#' Detect optimal k using various methods
#' @noRd
.detect_optimal_k <- function(wcss_results, coordinates, detection_methods, params, verbose) {
  
  optimal_k <- list()
  wcss_data <- wcss_results$wcss_data
  
  for (method in detection_methods) {
    if (verbose) message("Applying ", method, " method for optimal k detection")
    
    optimal_k[[method]] <- switch(method,
      "elbow" = .detect_elbow_method(wcss_data, verbose),
      "silhouette" = .detect_silhouette_method(coordinates, wcss_results$cluster_results, wcss_data, verbose),
      "gap" = .detect_gap_method(coordinates, wcss_data, params$clustering_options, verbose)
    )
  }
  
  return(optimal_k)
}

#' Detect optimal k using elbow method
#' @noRd
.detect_elbow_method <- function(wcss_data, verbose) {
  
  if (nrow(wcss_data) < 3) {
    if (verbose) warning("Insufficient data points for elbow method")
    return(NA)
  }
  
  tryCatch({
    # Calculate second differences to find elbow
    wcss <- wcss_data$wcss
    k <- wcss_data$k
    
    # Normalize WCSS and k for better elbow detection
    wcss_norm <- (wcss - min(wcss)) / (max(wcss) - min(wcss))
    k_norm <- (k - min(k)) / (max(k) - min(k))
    
    # Calculate distances from line connecting first and last points
    first_point <- c(k_norm[1], wcss_norm[1])
    last_point <- c(k_norm[length(k_norm)], wcss_norm[length(wcss_norm)])
    
    distances <- numeric(length(k))
    for (i in seq_along(k)) {
      point <- c(k_norm[i], wcss_norm[i])
      # Distance from point to line
      distances[i] <- abs((last_point[2] - first_point[2]) * point[1] - 
                         (last_point[1] - first_point[1]) * point[2] + 
                         last_point[1] * first_point[2] - last_point[2] * first_point[1]) /
                      sqrt((last_point[2] - first_point[2])^2 + (last_point[1] - first_point[1])^2)
    }
    
    # Find k with maximum distance (elbow point)
    elbow_idx <- which.max(distances)
    return(k[elbow_idx])
    
  }, error = function(e) {
    if (verbose) warning("Elbow method failed: ", e$message)
    return(NA)
  })
}

#' Detect optimal k using silhouette method
#' @noRd
.detect_silhouette_method <- function(coordinates, cluster_results, wcss_data, verbose) {
  
  if (!requireNamespace("cluster", quietly = TRUE)) {
    if (verbose) warning("Package 'cluster' required for silhouette method but not available")
    return(NA)
  }
  
  tryCatch({
    silhouette_scores <- numeric(nrow(wcss_data))
    
    for (i in seq_along(cluster_results)) {
      k <- wcss_data$k[i]
      result <- cluster_results[[i]]
      
      if (k == 1 || is.null(result)) {
        silhouette_scores[i] <- 0  # Silhouette is 0 for k=1
      } else {
        sil <- cluster::silhouette(result$cluster, dist(coordinates))
        silhouette_scores[i] <- mean(sil[, "sil_width"])
      }
    }
    
    # Find k with maximum average silhouette width
    valid_scores <- !is.na(silhouette_scores) & wcss_data$k > 1
    if (any(valid_scores)) {
      best_idx <- which.max(silhouette_scores[valid_scores])
      return(wcss_data$k[valid_scores][best_idx])
    } else {
      return(NA)
    }
    
  }, error = function(e) {
    if (verbose) warning("Silhouette method failed: ", e$message)
    return(NA)
  })
}

#' Detect optimal k using gap statistic
#' @noRd
.detect_gap_method <- function(coordinates, wcss_data, clustering_options, verbose) {
  
  if (!requireNamespace("cluster", quietly = TRUE)) {
    if (verbose) warning("Package 'cluster' required for gap statistic but not available")
    return(NA)
  }
  
  tryCatch({
    gap_stat <- cluster::clusGap(
      coordinates,
      FUN = stats::kmeans,
      K.max = max(wcss_data$k),
      B = 50,  # Number of bootstrap samples
      nstart = clustering_options$nstart
    )
    
    # Find optimal k using "firstmax" method
    optimal_k <- cluster::maxSE(gap_stat$Tab[, "gap"], gap_stat$Tab[, "SE.sim"])
    return(optimal_k)
    
  }, error = function(e) {
    if (verbose) warning("Gap statistic method failed: ", e$message)
    return(NA)
  })
}

# Plot Creation ----

#' Create the elbow plot
#' @noRd
.create_elbow_plot <- function(wcss_data, optimal_k_results, params) {
  
  # Create base plot
  plot <- ggplot2::ggplot(wcss_data, ggplot2::aes(x = k, y = wcss)) +
    ggplot2::geom_point(
      size = params$styling$point_size,
      color = params$styling$point_color
    ) +
    ggplot2::geom_line(
      size = params$styling$line_size,
      color = params$styling$line_color
    ) +
    ggplot2::labs(
      title = params$styling$title,
      x = params$labels$x_label,
      y = params$labels$y_label
    ) +
    ggplot2::theme_minimal(base_size = params$styling$theme_base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = params$styling$theme_base_size + 4, face = "bold")
    )
  
  # Add vertical lines for optimal k values
  colors <- c("elbow" = "red", "silhouette" = "blue", "gap" = "green")
  
  for (method in names(optimal_k_results)) {
    optimal_k <- optimal_k_results[[method]]
    if (!is.na(optimal_k)) {
      plot <- plot +
        ggplot2::geom_vline(
          xintercept = optimal_k,
          color = colors[method],
          linetype = "dashed",
          alpha = 0.7
        ) +
        ggplot2::annotate(
          "text",
          x = optimal_k + 0.3,
          y = max(wcss_data$wcss) * 0.9,
          label = paste0(method, " (k=", optimal_k, ")"),
          color = colors[method],
          angle = 90,
          vjust = -0.5,
          size = 3
        )
    }
  }
  
  return(plot)
}

# Export ----

#' Export elbow plot
#' @noRd
.export_elbow_plot <- function(plot, export_options, verbose) {
  
  # Setup file path
  if (!is.null(export_options$path)) {
    if (!dir.exists(export_options$path)) {
      stop("Export path does not exist: ", export_options$path, call. = FALSE)
    }
    file_path <- file.path(export_options$path, paste0(export_options$filename, ".", export_options$format))
  } else {
    file_path <- paste0(export_options$filename, ".", export_options$format)
  }
  
  if (verbose) message("Exporting elbow plot to: ", file_path)
  
  tryCatch({
    ggplot2::ggsave(
      filename = file_path,
      plot = plot,
      width = export_options$width,
      height = export_options$height,
      dpi = export_options$dpi,
      device = export_options$format
    )
    
    if (verbose) message("Elbow plot exported successfully")
  }, error = function(e) {
    stop("Failed to export elbow plot: ", e$message, call. = FALSE)
  })
}

# Print method ----

#' Print method for elbow_plot_result objects
#' @param x An elbow_plot_result object
#' @param ... Additional arguments (ignored)
#' @export
print.elbow_plot_result <- function(x, ...) {
  cat("Elbow Plot Analysis Results\n")
  cat("===========================\n\n")
  
  cat("WCSS Analysis:\n")
  cat("  K range:", min(x$wcss_data$k), "to", max(x$wcss_data$k), "\n")
  cat("  Data points:", nrow(x$wcss_data), "\n\n")
  
  cat("Optimal k suggestions:\n")
  if (length(x$optimal_k) > 0) {
    for (method in names(x$optimal_k)) {
      optimal_val <- x$optimal_k[[method]]
      if (is.na(optimal_val)) {
        cat("  ", method, "method: Not available\n")
      } else {
        cat("  ", method, "method: k =", optimal_val, "\n")
      }
    }
  } else {
    cat("  No optimal k detection methods applied\n")
  }
  
  cat("\nAvailable components:\n")
  cat("  - plot: ggplot2 elbow plot\n")
  cat("  - wcss_data: WCSS values for each k\n")
  cat("  - optimal_k: Suggested optimal k values\n")
  cat("  - cluster_results: Full clustering results\n")
}