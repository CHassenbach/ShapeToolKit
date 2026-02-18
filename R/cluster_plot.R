#' Create Cluster Plot with Automatic Clustering
#'
#' Performs clustering on coordinate data and visualizes the resulting clusters
#' with various clustering methods. Supports convex hulls, shapes at cluster centers,
#' and multiple visualization options.
#'
#' @param data A data frame containing the points to cluster.
#' @param x_col Character string specifying the column name for the x-axis.
#' @param y_col Character string specifying the column name for the y-axis.
#' @param shape_col Optional character string specifying the column containing 
#'   shape data for visualization at cluster centers. Default: "shape".
#' @param clustering List containing clustering options:
#'   \describe{
#'     \item{method}{Clustering method: "kmeans", "hierarchical", "distance_threshold"}
#'     \item{k}{Number of clusters (for kmeans/hierarchical)}
#'     \item{scale}{Whether to scale coordinates before clustering (default: TRUE)}
#'     \item{distance_threshold}{Distance threshold (for distance_threshold method)}
#'   }
#' @param visualization List containing visualization options:
#'   \describe{
#'     \item{hulls}{Show convex hulls around clusters (default: TRUE)}
#'     \item{centers}{Show cluster centers (default: FALSE)}
#'     \item{ellipses}{Show confidence ellipses (default: FALSE)}
#'     \item{shapes}{Show shapes at cluster centers (default: FALSE)}
#'   }
#' @param styling List containing styling options:
#'   \describe{
#'     \item{point}{Point styling (size, shape, color, fill)}
#'     \item{hull}{Hull styling (alpha, color, fill)}
#'     \item{text}{Text styling (title_size, label_size, tick_size)}
#'   }
#' @param labels List containing label options (title, x_label, y_label).
#' @param export_options List containing export options (export, filename, path, format).
#' @param verbose Logical indicating whether to print progress messages. Default: TRUE.
#'
#' @return A list containing:
#'   \describe{
#'     \item{plot}{The ggplot2 object}
#'     \item{clusters}{Vector of cluster assignments}
#'     \item{centers}{Data frame of cluster centers}
#'     \item{metadata}{Clustering metadata}
#'   }
#'
#' @examples
#' # Basic clustering
#' test_data <- data.frame(
#'   x = c(rnorm(50, 0, 1), rnorm(50, 3, 1)),
#'   y = c(rnorm(50, 0, 1), rnorm(50, 3, 1))
#' )
#'
#' result <- cluster_plot(
#'   data = test_data,
#'   x_col = "x", y_col = "y",
#'   clustering = list(method = "kmeans", k = 2)
#' )
#'
#' # Advanced clustering with custom styling
#' result <- cluster_plot(
#'   data = test_data,
#'   x_col = "x", y_col = "y",
#'   clustering = list(
#'     method = "hierarchical",
#'     k = 3,
#'     scale = TRUE
#'   ),
#'   visualization = list(
#'     hulls = TRUE,
#'     centers = TRUE,
#'     ellipses = FALSE
#'   ),
#'   styling = list(
#'     point = list(size = 3, shape = 16),
#'     hull = list(alpha = 0.3),
#'     text = list(title_size = 20)
#'   ),
#'   labels = list(
#'     title = "Hierarchical Clustering Results",
#'     x_label = "Principal Component 1",
#'     y_label = "Principal Component 2"
#'   )
#' )
#'
#' @export
cluster_plot <- function(data,
                        x_col,
                        y_col,
                        shape_col = "shape",
                        clustering = list(),
                        visualization = list(),
                        styling = list(),
                        labels = list(),
                        export_options = list(),
                        verbose = TRUE) {
  
  # Input validation ----
  .validate_cluster_plot_inputs(data, x_col, y_col, shape_col, verbose)
  
  # Setup parameters with defaults ----
  params <- .setup_cluster_plot_params(
    clustering, visualization, styling, labels, export_options,
    x_col, y_col, verbose
  )
  
  # Prepare data for clustering ----
  clean_data <- .prepare_clustering_data(data, x_col, y_col, verbose)
  
  # Perform clustering ----
  if (verbose) message("Performing ", params$clustering$method, " clustering...")
  cluster_result <- .perform_clustering(clean_data, x_col, y_col, params$clustering, verbose)
  
  # Add cluster assignments to data ----
  augmented_data <- clean_data
  augmented_data$cluster <- factor(cluster_result$clusters)
  
  # Calculate cluster centers ----
  cluster_centers <- .calculate_cluster_centers(augmented_data, x_col, y_col, verbose)
  
  if (verbose) {
    message("Found ", length(unique(cluster_result$clusters)), " clusters")
    message("Cluster sizes: ", paste(table(cluster_result$clusters), collapse = ", "))
  }
  
  # Create base plot using shape_plot ----
  if (verbose) message("Creating cluster visualization...")
  plot <- .create_cluster_base_plot(augmented_data, x_col, y_col, params)
  
  # Add visualization features ----
  if (params$visualization$centers) {
    plot <- .add_cluster_centers(plot, cluster_centers, params)
  }
  
  if (params$visualization$ellipses) {
    plot <- .add_cluster_ellipses(plot, augmented_data, x_col, y_col, params, verbose)
  }
  
  if (params$visualization$shapes && shape_col %in% colnames(data)) {
    plot <- .add_cluster_shapes(plot, augmented_data, cluster_centers, shape_col, params, verbose)
  }
  
  # Export if requested ----
  if (params$export_options$export) {
    .export_cluster_plot(plot, params$export_options, verbose)
  }
  
  # Prepare results ----
  metadata <- list(
    method = params$clustering$method,
    n_clusters = length(unique(cluster_result$clusters)),
    scaled = params$clustering$scale,
    analysis_date = Sys.time()
  )
  
  if (verbose) message("Cluster analysis completed!")
  
  structure(
    list(
      plot = plot,
      clusters = cluster_result$clusters,
      centers = cluster_centers,
      metadata = metadata
    ),
    class = "cluster_plot_result"
  )
}

# Input Validation ----

#' Validate inputs for cluster_plot function
#' @noRd
.validate_cluster_plot_inputs <- function(data, x_col, y_col, shape_col, verbose) {
  # Check required parameters
  if (missing(data)) stop("'data' is required", call. = FALSE)
  if (missing(x_col)) stop("'x_col' is required", call. = FALSE)
  if (missing(y_col)) stop("'y_col' is required", call. = FALSE)
  
  # Check data
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame", call. = FALSE)
  }
  if (nrow(data) == 0) {
    stop("'data' cannot be empty", call. = FALSE)
  }
  
  # Check column existence
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
  
  # Check verbose
  if (!is.logical(verbose) || length(verbose) != 1) {
    stop("'verbose' must be a single logical value", call. = FALSE)
  }
}

# Parameter Setup ----

#' Setup default parameters for cluster_plot
#' @noRd
.setup_cluster_plot_params <- function(clustering, visualization, styling, labels, 
                                      export_options, x_col, y_col, verbose) {
  
  # Setup clustering defaults
  clustering_defaults <- list(
    method = "kmeans",
    k = 3,
    scale = TRUE,
    distance_threshold = NULL
  )
  clustering <- utils::modifyList(clustering_defaults, clustering)
  
  # Validate clustering parameters
  valid_methods <- c("kmeans", "hierarchical", "distance_threshold")
  if (!clustering$method %in% valid_methods) {
    stop("Clustering method must be one of: ", paste(valid_methods, collapse = ", "), call. = FALSE)
  }
  
  if (clustering$method %in% c("kmeans", "hierarchical")) {
    if (!is.numeric(clustering$k) || clustering$k < 1) {
      stop("'k' must be a positive integer", call. = FALSE)
    }
  }
  
  if (clustering$method == "distance_threshold" && is.null(clustering$distance_threshold)) {
    stop("'distance_threshold' must be specified for distance_threshold method", call. = FALSE)
  }
  
  # Setup visualization defaults
  visualization_defaults <- list(
    hulls = TRUE,
    centers = FALSE,
    ellipses = FALSE,
    shapes = FALSE
  )
  visualization <- utils::modifyList(visualization_defaults, visualization)
  
  # Setup styling defaults
  styling_defaults <- list(
    point = list(
      size = 2,
      shape = 21,
      color = NULL,  # Will be auto-generated
      fill = NULL    # Will be auto-generated
    ),
    hull = list(
      alpha = 0.2,
      color = "black",
      fill = NULL    # Will be auto-generated
    ),
    text = list(
      title_size = 24,
      label_size = 20,
      tick_size = 15
    )
  )
  styling <- .merge_nested_lists(styling_defaults, styling)
  
  # Setup label defaults
  labels_defaults <- list(
    title = NULL,
    x_label = x_col,
    y_label = y_col
  )
  labels <- utils::modifyList(labels_defaults, labels)
  
  # Setup export defaults
  export_defaults <- list(
    export = FALSE,
    filename = "cluster_plot_output",
    path = NULL,
    format = "tiff",
    width = 10,
    height = 8,
    dpi = 300
  )
  export_options <- utils::modifyList(export_defaults, export_options)
  
  return(list(
    clustering = clustering,
    visualization = visualization,
    styling = styling,
    labels = labels,
    export_options = export_options
  ))
}

#' Merge nested lists recursively (helper function)
#' @noRd
.merge_nested_lists <- function(default, user) {
  for (name in names(user)) {
    if (name %in% names(default) && is.list(default[[name]]) && is.list(user[[name]])) {
      default[[name]] <- utils::modifyList(default[[name]], user[[name]])
    } else {
      default[[name]] <- user[[name]]
    }
  }
  return(default)
}

# Data Preparation ----

#' Prepare data for clustering
#' @noRd
.prepare_clustering_data <- function(data, x_col, y_col, verbose) {
  # Remove rows with missing values
  clean_data <- data %>%
    dplyr::filter(
      !is.na(.data[[x_col]]) & is.finite(.data[[x_col]]) &
      !is.na(.data[[y_col]]) & is.finite(.data[[y_col]])
    )
  
  rows_removed <- nrow(data) - nrow(clean_data)
  if (verbose && rows_removed > 0) {
    message("Removed ", rows_removed, " rows with missing or infinite values")
  }
  
  if (nrow(clean_data) < 3) {
    stop("Insufficient data for clustering (need >= 3 points)", call. = FALSE)
  }
  
  return(clean_data)
}

# Clustering ----

#' Perform clustering based on specified method
#' @noRd
.perform_clustering <- function(data, x_col, y_col, clustering_params, verbose) {
  
  # Extract coordinates
  coordinates <- data[, c(x_col, y_col)]
  
  # Scale if requested
  if (clustering_params$scale) {
    coordinates <- scale(coordinates)
    if (verbose) message("Coordinates scaled for clustering")
  }
  
  # Perform clustering based on method
  clusters <- switch(clustering_params$method,
    "kmeans" = .perform_kmeans_clustering(coordinates, clustering_params$k, verbose),
    "hierarchical" = .perform_hierarchical_clustering(coordinates, clustering_params$k, verbose),
    "distance_threshold" = .perform_distance_threshold_clustering(coordinates, clustering_params$distance_threshold, verbose)
  )
  
  return(list(
    clusters = clusters,
    method = clustering_params$method
  ))
}

#' Perform k-means clustering
#' @noRd
.perform_kmeans_clustering <- function(coordinates, k, verbose) {
  tryCatch({
    clustering <- stats::kmeans(coordinates, centers = k, nstart = 20)
    if (verbose) message("K-means clustering completed with ", k, " clusters")
    return(clustering$cluster)
  }, error = function(e) {
    stop("K-means clustering failed: ", e$message, call. = FALSE)
  })
}

#' Perform hierarchical clustering
#' @noRd
.perform_hierarchical_clustering <- function(coordinates, k, verbose) {
  tryCatch({
    distance_matrix <- stats::dist(coordinates)
    hclust_obj <- stats::hclust(distance_matrix)
    clusters <- stats::cutree(hclust_obj, k = k)
    if (verbose) message("Hierarchical clustering completed with ", k, " clusters")
    return(clusters)
  }, error = function(e) {
    stop("Hierarchical clustering failed: ", e$message, call. = FALSE)
  })
}

#' Perform distance threshold clustering
#' @noRd
.perform_distance_threshold_clustering <- function(coordinates, threshold, verbose) {
  tryCatch({
    distance_matrix <- stats::dist(coordinates)
    hclust_obj <- stats::hclust(distance_matrix)
    clusters <- stats::cutree(hclust_obj, h = threshold)
    n_clusters <- length(unique(clusters))
    if (verbose) message("Distance threshold clustering completed with ", n_clusters, " clusters")
    return(clusters)
  }, error = function(e) {
    stop("Distance threshold clustering failed: ", e$message, call. = FALSE)
  })
}

# Cluster Centers ----

#' Calculate cluster centers
#' @noRd
.calculate_cluster_centers <- function(data, x_col, y_col, verbose) {
  tryCatch({
    centers <- data %>%
      dplyr::filter(cluster != "0") %>%  # Exclude noise points if any
      dplyr::group_by(cluster) %>%
      dplyr::summarize(
        center_x = mean(.data[[x_col]], na.rm = TRUE),
        center_y = mean(.data[[y_col]], na.rm = TRUE),
        n_points = dplyr::n(),
        .groups = "drop"
      )
    
    if (verbose) message("Calculated centers for ", nrow(centers), " clusters")
    return(centers)
  }, error = function(e) {
    stop("Failed to calculate cluster centers: ", e$message, call. = FALSE)
  })
}

# Plot Creation ----

#' Create base cluster plot using shape_plot
#' @noRd
.create_cluster_base_plot <- function(data, x_col, y_col, params) {
  
  # Generate colors for clusters
  n_clusters <- length(unique(data$cluster))
  cluster_colors <- scales::hue_pal()(n_clusters)
  
  # Setup styling for shape_plot
  point_colors <- if (is.null(params$styling$point$color)) cluster_colors else params$styling$point$color
  point_fills <- if (is.null(params$styling$point$fill)) cluster_colors else params$styling$point$fill
  hull_fills <- if (is.null(params$styling$hull$fill)) cluster_colors else params$styling$hull$fill
  
  # Create plot using shape_plot
  tryCatch({
    shape_plot(
      data = data,
      x_col = x_col,
      y_col = y_col,
      group_col = "cluster",
      styling = list(
        point = list(
          color = point_colors,
          fill = point_fills,
          shape = params$styling$point$shape,
          size = params$styling$point$size
        ),
        text = params$styling$text
      ),
      features = list(
        hulls = list(
          show = params$visualization$hulls,
          fill = hull_fills,
          color = params$styling$hull$color,
          alpha = params$styling$hull$alpha
        )
      ),
      labels = params$labels,
      verbose = FALSE  # Avoid double messaging
    )
  }, error = function(e) {
    stop("Failed to create base cluster plot: ", e$message, call. = FALSE)
  })
}

# Visualization Features ----

#' Add cluster centers to plot
#' @noRd
.add_cluster_centers <- function(plot, cluster_centers, params) {
  plot +
    ggplot2::geom_point(
      data = cluster_centers,
      ggplot2::aes(x = center_x, y = center_y),
      size = params$styling$point$size * 2,
      shape = 4,  # Cross shape
      color = "black",
      stroke = 2
    )
}

#' Add confidence ellipses to plot
#' @noRd
.add_cluster_ellipses <- function(plot, data, x_col, y_col, params, verbose) {
  
  if (!requireNamespace("ellipse", quietly = TRUE)) {
    if (verbose) warning("Package 'ellipse' required for confidence ellipses but not available")
    return(plot)
  }
  
  # Generate ellipse data for each cluster
  ellipse_data <- data %>%
    dplyr::group_by(cluster) %>%
    dplyr::do({
      cluster_data <- .
      if (nrow(cluster_data) > 2) {
        tryCatch({
          center <- c(mean(cluster_data[[x_col]]), mean(cluster_data[[y_col]]))
          cov_matrix <- stats::cov(cluster_data[, c(x_col, y_col)])
          ellipse_points <- ellipse::ellipse(cov_matrix, centre = center, level = 0.95)
          data.frame(
            x = ellipse_points[, 1],
            y = ellipse_points[, 2],
            cluster = cluster_data$cluster[1]
          )
        }, error = function(e) {
          if (verbose) warning("Failed to create ellipse for cluster ", cluster_data$cluster[1])
          data.frame(x = numeric(0), y = numeric(0), cluster = character(0))
        })
      } else {
        data.frame(x = numeric(0), y = numeric(0), cluster = character(0))
      }
    }) %>%
    dplyr::ungroup()
  
  if (nrow(ellipse_data) > 0) {
    n_clusters <- length(unique(ellipse_data$cluster))
    ellipse_colors <- if (is.null(params$styling$hull$fill)) {
      scales::hue_pal()(n_clusters)
    } else {
      params$styling$hull$fill
    }
    
    plot <- plot +
      ggplot2::geom_polygon(
        data = ellipse_data,
        ggplot2::aes(x = x, y = y, group = cluster, fill = cluster),
        alpha = params$styling$hull$alpha,
        color = params$styling$hull$color
      ) +
      ggplot2::scale_fill_manual(values = ellipse_colors)
  }
  
  return(plot)
}

#' Add shapes at cluster centers
#' @noRd
.add_cluster_shapes <- function(plot, data, cluster_centers, shape_col, params, verbose) {
  
  if (verbose) message("Adding shapes at cluster centers...")
  
  # This is a placeholder for shape functionality
  # The actual implementation would depend on your shape data structure
  if (verbose) message("Shape overlay at cluster centers needs implementation based on shape data format")
  
  return(plot)
}

# Export ----

#' Export cluster plot
#' @noRd
.export_cluster_plot <- function(plot, export_options, verbose) {
  
  # Setup file path
  if (!is.null(export_options$path)) {
    if (!dir.exists(export_options$path)) {
      stop("Export path does not exist: ", export_options$path, call. = FALSE)
    }
    file_path <- file.path(export_options$path, paste0(export_options$filename, ".", export_options$format))
  } else {
    file_path <- paste0(export_options$filename, ".", export_options$format)
  }
  
  if (verbose) message("Exporting cluster plot to: ", file_path)
  
  tryCatch({
    ggplot2::ggsave(
      filename = file_path,
      plot = plot,
      width = export_options$width,
      height = export_options$height,
      dpi = export_options$dpi,
      device = export_options$format
    )
    
    if (verbose) message("Cluster plot exported successfully")
  }, error = function(e) {
    stop("Failed to export cluster plot: ", e$message, call. = FALSE)
  })
}

# Print method ----

#' Print method for cluster_plot_result objects
#' @param x A cluster_plot_result object
#' @param ... Additional arguments (ignored)
#' @export
print.cluster_plot_result <- function(x, ...) {
  cat("Cluster Plot Results\n")
  cat("====================\n\n")
  cat("Clustering metadata:\n")
  cat("  Method:", x$metadata$method, "\n")
  cat("  Number of clusters:", x$metadata$n_clusters, "\n")
  cat("  Coordinates scaled:", x$metadata$scaled, "\n")
  cat("  Analysis date:", format(x$metadata$analysis_date), "\n\n")
  
  cat("Cluster sizes:\n")
  cluster_sizes <- table(x$clusters)
  for (i in seq_along(cluster_sizes)) {
    cat("  Cluster", names(cluster_sizes)[i], ":", cluster_sizes[i], "points\n")
  }
  
  cat("\nAvailable components:\n")
  cat("  - plot: ggplot2 object\n")
  cat("  - clusters: Vector of cluster assignments\n")
  cat("  - centers: Data frame of cluster centers\n")
  cat("  - metadata: Analysis metadata\n")
}