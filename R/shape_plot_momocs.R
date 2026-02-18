#' Advanced Momocs-Specific Shape Visualization
#'
#' Creates sophisticated PCA plots with shape reconstructions displayed at convex hull
#' points or custom positions. Specialized for Momocs geometric morphometric analysis
#' with extensive customization options for publication-ready figures.
#'
#' @param pca_result The PCA result object from PCA().
#' @param coo_object The Coo object containing shape coordinate data.
#' @param pca_options List containing PCA plotting options:
#'   \describe{
#'     \item{pc_x}{Principal component for x-axis (default: 1)}
#'     \item{pc_y}{Principal component for y-axis (default: 2)}
#'     \item{variance_explained}{Show variance explained in axis labels (default: TRUE)}
#'   }
#' @param shape_options List containing shape visualization options:
#'   \describe{
#'     \item{positions}{Shape positions: "hull", "extremes", "grid", "custom" (default: "hull")}
#'     \item{shape_size}{Scaling factor for shapes (default: 0.01)}
#'     \item{n_shapes}{Number of shapes to display for "grid" mode (default: 9)}
#'     \item{custom_positions}{Custom positions data frame for "custom" mode}
#'   }
#' @param shape_styling List containing shape styling options:
#'   \describe{
#'     \item{fill_color}{Fill color for shapes (default: "black")}
#'     \item{border_color}{Border color for shapes (default: "black")}
#'     \item{border_width}{Width of shape borders (default: 0.5)}
#'     \item{transparency}{Shape transparency (0-1, default: 0.8)}
#'   }
#' @param positioning List containing shape positioning options:
#'   \describe{
#'     \item{shift_distance}{Distance to shift shapes from points (default: 0)}
#'     \item{auto_shift}{Automatically calculate shift distance (default: TRUE)}
#'     \item{horizontal_offset}{Global horizontal offset (default: 0)}
#'     \item{vertical_offset}{Global vertical offset (default: 0)}
#'   }
#' @param point_options List containing scatter point options:
#'   \describe{
#'     \item{show_points}{Show PCA scatter points (default: TRUE)}
#'     \item{point_color}{Color of points (default: "gray50")}
#'     \item{point_size}{Size of points (default: 2)}
#'     \item{point_shape}{Shape of points (default: 21)}
#'   }
#' @param axis_options List containing axis customization options:
#'   \describe{
#'     \item{show_arrows}{Show arrowed axes (default: TRUE)}
#'     \item{show_grid}{Show background grid (default: FALSE)}
#'     \item{tick_positions}{Tick mark positions: "auto", "custom", "none" (default: "auto")}
#'     \item{custom_ticks}{Custom tick positions list}
#'   }
#' @param styling List containing plot styling options:
#'   \describe{
#'     \item{title}{Plot title (default: NULL)}
#'     \item{x_label}{Custom x-axis label (default: auto-generated)}
#'     \item{y_label}{Custom y-axis label (default: auto-generated)}
#'     \item{title_size}{Font size for title (default: 16)}
#'     \item{label_size}{Font size for axis labels (default: 14)}
#'     \item{tick_size}{Font size for tick labels (default: 12)}
#'   }
#' @param layout_options List containing layout options:
#'   \describe{
#'     \item{aspect_ratio}{Aspect ratio: "fixed", "free" (default: "fixed")}
#'     \item{margins}{Plot margins adjustment (default: 0.1)}
#'     \item{legend_position}{Legend position if applicable (default: "none")}
#'   }
#' @param export_options List containing export options (export, filename, path, format, etc.).
#' @param verbose Logical indicating whether to print progress messages. Default: TRUE.
#'
#' @return A list containing:
#'   \describe{
#'     \item{plot}{The ggplot2 object}
#'     \item{shape_info}{Information about displayed shapes and their positions}
#'     \item{pca_summary}{Summary of PCA results used}
#'     \item{plot_data}{Data used for plotting}
#'   }
#'
#' @examples
#' \dontrun{
#' # Load Momocs and prepare data
#' library(Momocs)
#' 
#' # Basic usage with bot dataset
#' bot_shapes <- bot %>% coo_center() %>% coo_scale() %>% coo_align()
#' bot_fourier <- bot_shapes %>% efourier(nb.h = 10)
#' bot_pca <- PCA(bot_fourier)
#'
#' # Basic shape plot at hull points
#' result <- shape_plot_momocs(
#'   pca_result = bot_pca,
#'   coo_object = bot_shapes
#' )
#'
#' # Advanced plot with custom shape positioning
#' result <- shape_plot_momocs(
#'   pca_result = bot_pca,
#'   coo_object = bot_shapes,
#'   pca_options = list(
#'     pc_x = 1, pc_y = 2,
#'     variance_explained = TRUE
#'   ),
#'   shape_options = list(
#'     positions = "extremes",
#'     shape_size = 0.015
#'   ),
#'   shape_styling = list(
#'     fill_color = "steelblue",
#'     border_color = "darkblue",
#'     transparency = 0.7
#'   ),
#'   positioning = list(
#'     auto_shift = TRUE,
#'     shift_distance = 0.2
#'   ),
#'   styling = list(
#'     title = "Morphometric Shape Analysis",
#'     title_size = 18
#'   )
#' )
#'
#' # Grid-based shape display
#' result <- shape_plot_momocs(
#'   pca_result = bot_pca,
#'   coo_object = bot_shapes,
#'   shape_options = list(
#'     positions = "grid",
#'     n_shapes = 16,
#'     shape_size = 0.008
#'   ),
#'   export_options = list(
#'     export = TRUE,
#'     filename = "momocs_analysis",
#'     format = "pdf"
#'   )
#' )
#'
#' # View results
#' print(result$plot)
#' summary(result$shape_info)
#' }
#'
#' @export
shape_plot_momocs <- function(pca_result,
                             coo_object,
                             pca_options = list(),
                             shape_options = list(),
                             shape_styling = list(),
                             positioning = list(),
                             point_options = list(),
                             axis_options = list(),
                             styling = list(),
                             layout_options = list(),
                             export_options = list(),
                             verbose = TRUE) {
  
  # Input validation ----
  .validate_momocs_inputs(pca_result, coo_object, verbose)
  
  # Setup parameters ----
  params <- .setup_momocs_params(pca_options, shape_options, shape_styling, positioning, 
                                point_options, axis_options, styling, layout_options, 
                                export_options, verbose)
  
  # Extract and prepare PCA data ----
  pca_data <- .extract_momocs_pca_data(pca_result, params$pca_options, verbose)
  
  # Determine shape positions ----
  shape_positions <- .determine_shape_positions(pca_data, coo_object, params, verbose)
  
  if (verbose) {
    message("Creating Momocs shape plot...")
    message("PCA components: PC", params$pca_options$pc_x, " vs PC", params$pca_options$pc_y)
    message("Shape positions: ", params$shape_options$positions)
    message("Number of shapes to display: ", length(shape_positions$indices))
  }
  
  # Create base plot ----
  base_plot <- .create_momocs_base_plot(pca_data, params, verbose)
  
  # Add shapes to plot ----
  final_plot <- .add_momocs_shapes_to_plot(base_plot, coo_object, shape_positions, pca_data, params, verbose)
  
  # Export if requested ----
  if (params$export_options$export) {
    .export_momocs_plot(final_plot, params$export_options, verbose)
  }
  
  # Generate summary information ----
  plot_summary <- .generate_momocs_summary(pca_result, pca_data, shape_positions, params)
  
  if (verbose) {
    message("Momocs plot creation completed!")
  }
  
  # Prepare results
  structure(
    list(
      plot = final_plot,
      shape_info = shape_positions,
      pca_summary = plot_summary$pca_info,
      plot_data = pca_data
    ),
    class = "momocs_plot_result"
  )
}

# Input Validation ----

#' Validate inputs for shape_plot_momocs function
#' @noRd
.validate_momocs_inputs <- function(pca_result, coo_object, verbose) {
  
  # Check required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not available", call. = FALSE)
  }
  
  # Check PCA result
  if (!inherits(pca_result, "PCA")) {
    stop("'pca_result' must be a PCA object from PCA()", call. = FALSE)
  }
  
  # Check Coo object
  if (!inherits(coo_object, "Coo")) {
    stop("'coo_object' must be a Coo object from Momocs", call. = FALSE)
  }
  
  # Check that PCA and Coo objects are compatible
  if (nrow(pca_result$x) != length(coo_object$coo)) {
    stop("Number of specimens in PCA result does not match Coo object", call. = FALSE)
  }
  
  # Check that PCA has sufficient components
  if (ncol(pca_result$x) < 2) {
    stop("PCA result must have at least 2 components", call. = FALSE)
  }
  
  # Check verbose
  if (!is.logical(verbose) || length(verbose) != 1) {
    stop("'verbose' must be a single logical value", call. = FALSE)
  }
}

# Parameter Setup ----

#' Setup parameters for Momocs plotting
#' @noRd
.setup_momocs_params <- function(pca_options, shape_options, shape_styling, positioning,
                                point_options, axis_options, styling, layout_options,
                                export_options, verbose) {
  
  # PCA options defaults
  pca_defaults <- list(
    pc_x = 1,
    pc_y = 2,
    variance_explained = TRUE
  )
  pca_options <- utils::modifyList(pca_defaults, pca_options)
  
  # Shape options defaults
  shape_defaults <- list(
    positions = "hull",
    shape_size = 0.01,
    n_shapes = 9,
    custom_positions = NULL
  )
  shape_options <- utils::modifyList(shape_defaults, shape_options)
  
  # Validate shape options
  valid_positions <- c("hull", "extremes", "grid", "custom")
  if (!shape_options$positions %in% valid_positions) {
    stop("'positions' must be one of: ", paste(valid_positions, collapse = ", "), call. = FALSE)
  }
  
  if (shape_options$positions == "custom" && is.null(shape_options$custom_positions)) {
    stop("'custom_positions' must be provided when positions = 'custom'", call. = FALSE)
  }
  
  # Shape styling defaults
  styling_defaults <- list(
    fill_color = "black",
    border_color = "black",
    border_width = 0.5,
    transparency = 0.8
  )
  shape_styling <- utils::modifyList(styling_defaults, shape_styling)
  
  # Validate transparency
  if (shape_styling$transparency < 0 || shape_styling$transparency > 1) {
    stop("'transparency' must be between 0 and 1", call. = FALSE)
  }
  
  # Positioning defaults
  pos_defaults <- list(
    shift_distance = 0,
    auto_shift = TRUE,
    horizontal_offset = 0,
    vertical_offset = 0
  )
  positioning <- utils::modifyList(pos_defaults, positioning)
  
  # Point options defaults
  point_defaults <- list(
    show_points = TRUE,
    point_color = "gray50",
    point_size = 2,
    point_shape = 21
  )
  point_options <- utils::modifyList(point_defaults, point_options)
  
  # Axis options defaults
  axis_defaults <- list(
    show_arrows = TRUE,
    show_grid = FALSE,
    tick_positions = "auto",
    custom_ticks = NULL
  )
  axis_options <- utils::modifyList(axis_defaults, axis_options)
  
  # Styling defaults
  style_defaults <- list(
    title = NULL,
    x_label = NULL,  # Auto-generated
    y_label = NULL,  # Auto-generated
    title_size = 16,
    label_size = 14,
    tick_size = 12
  )
  styling <- utils::modifyList(style_defaults, styling)
  
  # Layout defaults
  layout_defaults <- list(
    aspect_ratio = "fixed",
    margins = 0.1,
    legend_position = "none"
  )
  layout_options <- utils::modifyList(layout_defaults, layout_options)
  
  # Export defaults
  export_defaults <- list(
    export = FALSE,
    filename = "momocs_plot",
    path = NULL,
    format = "png",
    width = 10,
    height = 8,
    dpi = 300
  )
  export_options <- utils::modifyList(export_defaults, export_options)
  
  return(list(
    pca_options = pca_options,
    shape_options = shape_options,
    shape_styling = shape_styling,
    positioning = positioning,
    point_options = point_options,
    axis_options = axis_options,
    styling = styling,
    layout_options = layout_options,
    export_options = export_options
  ))
}

# PCA Data Extraction ----

#' Extract PCA data for plotting
#' @noRd
.extract_momocs_pca_data <- function(pca_result, pca_options, verbose) {
  
  # Extract PCA scores
  pca_scores <- pca_result$x
  
  # Check if requested components exist
  max_pc <- ncol(pca_scores)
  if (pca_options$pc_x > max_pc || pca_options$pc_y > max_pc) {
    stop("Requested PC components exceed available components (", max_pc, ")", call. = FALSE)
  }
  
  # Create data frame
  pca_data <- data.frame(
    PCx = pca_scores[, pca_options$pc_x],
    PCy = pca_scores[, pca_options$pc_y],
    specimen_id = 1:nrow(pca_scores)
  )
  
  # Add variance explained information
  if (pca_options$variance_explained && !is.null(pca_result$eig)) {
    variance_explained <- pca_result$eig / sum(pca_result$eig) * 100
    pca_data$variance_x <- variance_explained[pca_options$pc_x]
    pca_data$variance_y <- variance_explained[pca_options$pc_y]
  }
  
  if (verbose) {
    message("Extracted PCA data for ", nrow(pca_data), " specimens")
    if (pca_options$variance_explained && !is.null(pca_result$eig)) {
      variance_explained <- pca_result$eig / sum(pca_result$eig) * 100
      message("PC", pca_options$pc_x, " variance: ", round(variance_explained[pca_options$pc_x], 2), "%")
      message("PC", pca_options$pc_y, " variance: ", round(variance_explained[pca_options$pc_y], 2), "%")
    }
  }
  
  return(pca_data)
}

# Shape Position Determination ----

#' Determine which shapes to display and where
#' @noRd
.determine_shape_positions <- function(pca_data, coo_object, params, verbose) {
  
  position_type <- params$shape_options$positions
  
  if (position_type == "hull") {
    positions <- .get_hull_positions(pca_data, verbose)
  } else if (position_type == "extremes") {
    positions <- .get_extreme_positions(pca_data, verbose)
  } else if (position_type == "grid") {
    positions <- .get_grid_positions(pca_data, params$shape_options$n_shapes, verbose)
  } else if (position_type == "custom") {
    positions <- .get_custom_positions(pca_data, params$shape_options$custom_positions, verbose)
  }
  
  # Calculate positioning adjustments
  if (params$positioning$auto_shift) {
    positions$shifts <- .calculate_auto_shifts(pca_data, positions, params$positioning)
  } else {
    positions$shifts <- .calculate_manual_shifts(positions, params$positioning)
  }
  
  return(positions)
}

# Hull Positions ----

#' Get convex hull positions
#' @noRd
.get_hull_positions <- function(pca_data, verbose) {
  
  tryCatch({
    hull_indices <- grDevices::chull(pca_data$PCx, pca_data$PCy)
    
    if (verbose) message("Found ", length(hull_indices), " hull points")
    
    return(list(
      indices = hull_indices,
      x_pos = pca_data$PCx[hull_indices],
      y_pos = pca_data$PCy[hull_indices],
      type = "hull"
    ))
    
  }, error = function(e) {
    stop("Failed to compute convex hull: ", e$message, call. = FALSE)
  })
}

# Extreme Positions ----

#' Get extreme positions (min/max for each PC)
#' @noRd
.get_extreme_positions <- function(pca_data, verbose) {
  
  # Find extreme specimens
  extremes <- c(
    which.min(pca_data$PCx), which.max(pca_data$PCx),
    which.min(pca_data$PCy), which.max(pca_data$PCy)
  )
  
  # Remove duplicates
  extremes <- unique(extremes)
  
  if (verbose) message("Found ", length(extremes), " extreme positions")
  
  return(list(
    indices = extremes,
    x_pos = pca_data$PCx[extremes],
    y_pos = pca_data$PCy[extremes],
    type = "extremes"
  ))
}

# Grid Positions ----

#' Get grid-based positions
#' @noRd
.get_grid_positions <- function(pca_data, n_shapes, verbose) {
  
  # Create regular grid across PCA space
  x_range <- range(pca_data$PCx)
  y_range <- range(pca_data$PCy)
  
  # Determine grid dimensions
  grid_dim <- ceiling(sqrt(n_shapes))
  
  # Create grid coordinates
  x_grid <- seq(x_range[1], x_range[2], length.out = grid_dim)
  y_grid <- seq(y_range[1], y_range[2], length.out = grid_dim)
  
  grid_coords <- expand.grid(x = x_grid, y = y_grid)
  
  # Find nearest specimens to grid points
  grid_indices <- numeric(min(nrow(grid_coords), n_shapes))
  
  for (i in seq_along(grid_indices)) {
    distances <- sqrt((pca_data$PCx - grid_coords$x[i])^2 + 
                     (pca_data$PCy - grid_coords$y[i])^2)
    grid_indices[i] <- which.min(distances)
  }
  
  # Remove duplicates
  grid_indices <- unique(grid_indices)
  
  if (verbose) message("Selected ", length(grid_indices), " specimens for grid display")
  
  return(list(
    indices = grid_indices,
    x_pos = pca_data$PCx[grid_indices],
    y_pos = pca_data$PCy[grid_indices],
    type = "grid"
  ))
}

# Custom Positions ----

#' Get custom positions
#' @noRd
.get_custom_positions <- function(pca_data, custom_positions, verbose) {
  
  # Validate custom positions
  if (!is.data.frame(custom_positions) || 
      !all(c("specimen_id") %in% colnames(custom_positions))) {
    stop("custom_positions must be a data frame with 'specimen_id' column", call. = FALSE)
  }
  
  # Validate specimen IDs
  valid_ids <- custom_positions$specimen_id %in% pca_data$specimen_id
  if (!all(valid_ids)) {
    warning("Some custom specimen IDs not found in PCA data")
    custom_positions <- custom_positions[valid_ids, ]
  }
  
  if (verbose) message("Using ", nrow(custom_positions), " custom positions")
  
  return(list(
    indices = custom_positions$specimen_id,
    x_pos = pca_data$PCx[custom_positions$specimen_id],
    y_pos = pca_data$PCy[custom_positions$specimen_id],
    type = "custom"
  ))
}

# Shift Calculations ----

#' Calculate automatic shifts from center
#' @noRd
.calculate_auto_shifts <- function(pca_data, positions, positioning_params) {
  
  # Calculate center of PCA space
  center_x <- mean(pca_data$PCx)
  center_y <- mean(pca_data$PCy)
  
  # Calculate direction vectors from center
  direction_x <- positions$x_pos - center_x
  direction_y <- positions$y_pos - center_y
  
  # Normalize direction vectors
  magnitudes <- sqrt(direction_x^2 + direction_y^2)
  magnitudes[magnitudes == 0] <- 1  # Avoid division by zero
  
  direction_x <- direction_x / magnitudes
  direction_y <- direction_y / magnitudes
  
  # Calculate shift distance based on data range
  data_range <- max(range(pca_data$PCx), range(pca_data$PCy))
  auto_shift <- positioning_params$shift_distance * diff(data_range) * 0.1
  
  # Apply shifts
  shift_x <- direction_x * auto_shift + positioning_params$horizontal_offset
  shift_y <- direction_y * auto_shift + positioning_params$vertical_offset
  
  return(list(x = shift_x, y = shift_y))
}

#' Calculate manual shifts
#' @noRd
.calculate_manual_shifts <- function(positions, positioning_params) {
  
  n_positions <- length(positions$indices)
  
  return(list(
    x = rep(positioning_params$horizontal_offset, n_positions),
    y = rep(positioning_params$vertical_offset, n_positions)
  ))
}

# Base Plot Creation ----

#' Create the base plot
#' @noRd
.create_momocs_base_plot <- function(pca_data, params, verbose) {
  
  # Create base ggplot
  plot <- ggplot2::ggplot(pca_data, ggplot2::aes(x = PCx, y = PCy))
  
  # Add points if requested
  if (params$point_options$show_points) {
    plot <- plot +
      ggplot2::geom_point(
        size = params$point_options$point_size,
        color = params$point_options$point_color,
        shape = params$point_options$point_shape
      )
  }
  
  # Apply theme
  plot <- plot + .apply_momocs_theme(params, pca_data)
  
  # Add axes if requested
  if (params$axis_options$show_arrows) {
    plot <- .add_arrowed_axes(plot, pca_data, params)
  }
  
  # Add labels
  plot <- .add_momocs_labels(plot, params, pca_data)
  
  return(plot)
}

# Theme Application ----

#' Apply Momocs-specific theme
#' @noRd
.apply_momocs_theme <- function(params, pca_data) {
  
  theme <- ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = if (params$axis_options$show_grid) ggplot2::element_line(color = "gray90") else ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = params$styling$title_size, hjust = 0.5),
      legend.position = params$layout_options$legend_position
    )
  
  # Set aspect ratio
  if (params$layout_options$aspect_ratio == "fixed") {
    theme <- theme + ggplot2::coord_fixed()
  }
  
  return(theme)
}

# Arrowed Axes ----

#' Add arrowed axes to plot
#' @noRd
.add_arrowed_axes <- function(plot, pca_data, params) {
  
  # Calculate axis ranges with margins
  x_range <- range(pca_data$PCx)
  y_range <- range(pca_data$PCy)
  
  margin <- params$layout_options$margins
  x_expand <- margin * diff(x_range)
  y_expand <- margin * diff(y_range)
  
  # Add x-axis arrow
  plot <- plot +
    ggplot2::geom_segment(
      ggplot2::aes(x = x_range[1] - x_expand, xend = x_range[2] + x_expand,
                  y = 0, yend = 0),
      arrow = grid::arrow(length = grid::unit(0.3, "cm")),
      color = "black"
    )
  
  # Add y-axis arrow
  plot <- plot +
    ggplot2::geom_segment(
      ggplot2::aes(x = 0, xend = 0,
                  y = y_range[1] - y_expand, yend = y_range[2] + y_expand),
      arrow = grid::arrow(length = grid::unit(0.3, "cm")),
      color = "black"
    )
  
  # Add tick marks if requested
  if (params$axis_options$tick_positions == "auto") {
    plot <- .add_auto_ticks(plot, x_range, y_range, params)
  }
  
  return(plot)
}

# Automatic Tick Marks ----

#' Add automatic tick marks
#' @noRd
.add_auto_ticks <- function(plot, x_range, y_range, params) {
  
  # Generate tick positions
  x_ticks <- pretty(x_range)
  y_ticks <- pretty(y_range)
  
  # Remove zero and extreme ticks
  x_ticks <- x_ticks[x_ticks != 0 & x_ticks > x_range[1] & x_ticks < x_range[2]]
  y_ticks <- y_ticks[y_ticks != 0 & y_ticks > y_range[1] & y_ticks < y_range[2]]
  
  # Calculate tick length
  tick_length <- 0.01 * max(diff(x_range), diff(y_range))
  
  # Add x-axis ticks
  if (length(x_ticks) > 0) {
    plot <- plot +
      ggplot2::geom_segment(
        data = data.frame(x = x_ticks),
        ggplot2::aes(x = x, xend = x, y = -tick_length, yend = tick_length),
        color = "black"
      ) +
      ggplot2::geom_text(
        data = data.frame(x = x_ticks, y = 0),
        ggplot2::aes(x = x, y = y, label = round(x, 2)),
        vjust = 2, size = params$styling$tick_size / 3
      )
  }
  
  # Add y-axis ticks
  if (length(y_ticks) > 0) {
    plot <- plot +
      ggplot2::geom_segment(
        data = data.frame(y = y_ticks),
        ggplot2::aes(y = y, yend = y, x = -tick_length, xend = tick_length),
        color = "black"
      ) +
      ggplot2::geom_text(
        data = data.frame(x = 0, y = y_ticks),
        ggplot2::aes(x = x, y = y, label = round(y, 2)),
        hjust = 2, size = params$styling$tick_size / 3
      )
  }
  
  return(plot)
}

# Labels ----

#' Add labels to the plot
#' @noRd
.add_momocs_labels <- function(plot, params, pca_data) {
  
  # Generate axis labels
  x_label <- params$styling$x_label
  y_label <- params$styling$y_label
  
  if (is.null(x_label)) {
    x_label <- paste0("PC", params$pca_options$pc_x)
    if (params$pca_options$variance_explained && "variance_x" %in% names(pca_data)) {
      x_label <- paste0(x_label, " (", round(pca_data$variance_x[1], 1), "%)")
    }
  }
  
  if (is.null(y_label)) {
    y_label <- paste0("PC", params$pca_options$pc_y)
    if (params$pca_options$variance_explained && "variance_y" %in% names(pca_data)) {
      y_label <- paste0(y_label, " (", round(pca_data$variance_y[1], 1), "%)")
    }
  }
  
  # Add title
  if (!is.null(params$styling$title)) {
    plot <- plot + ggplot2::labs(title = params$styling$title)
  }
  
  # Add axis labels as annotations
  x_range <- range(pca_data$PCx)
  y_range <- range(pca_data$PCy)
  
  plot <- plot +
    ggplot2::annotate(
      "text",
      x = max(x_range) * 0.9,
      y = min(y_range) * 0.1,
      label = x_label,
      size = params$styling$label_size / 3
    ) +
    ggplot2::annotate(
      "text",
      x = min(x_range) * 0.1,
      y = max(y_range) * 0.9,
      label = y_label,
      size = params$styling$label_size / 3,
      angle = 90
    )
  
  return(plot)
}

# Shape Addition ----

#' Add shapes to the plot
#' @noRd
.add_momocs_shapes_to_plot <- function(plot, coo_object, shape_positions, pca_data, params, verbose) {
  
  n_shapes <- length(shape_positions$indices)
  
  if (verbose) message("Adding ", n_shapes, " shapes to plot")
  
  for (i in seq_along(shape_positions$indices)) {
    shape_index <- shape_positions$indices[i]
    
    # Extract shape coordinates
    shape_coords <- coo_object$coo[[shape_index]]
    
    if (is.null(shape_coords) || nrow(shape_coords) == 0) {
      if (verbose) warning("Empty shape data for specimen ", shape_index)
      next
    }
    
    # Scale shape
    scaled_coords <- shape_coords * params$shape_options$shape_size
    
    # Position shape
    final_x <- scaled_coords[, 1] + shape_positions$x_pos[i] + shape_positions$shifts$x[i]
    final_y <- scaled_coords[, 2] + shape_positions$y_pos[i] + shape_positions$shifts$y[i]
    
    # Create shape data frame
    shape_df <- data.frame(x = final_x, y = final_y)
    
    # Add shape to plot
    plot <- plot +
      ggplot2::geom_polygon(
        data = shape_df,
        ggplot2::aes(x = x, y = y),
        fill = params$shape_styling$fill_color,
        color = params$shape_styling$border_color,
        size = params$shape_styling$border_width,
        alpha = params$shape_styling$transparency
      )
  }
  
  return(plot)
}

# Export ----

#' Export Momocs plot
#' @noRd
.export_momocs_plot <- function(plot, export_options, verbose) {
  
  # Setup file path
  if (!is.null(export_options$path)) {
    if (!dir.exists(export_options$path)) {
      stop("Export path does not exist: ", export_options$path, call. = FALSE)
    }
    file_path <- file.path(export_options$path, paste0(export_options$filename, ".", export_options$format))
  } else {
    file_path <- paste0(export_options$filename, ".", export_options$format)
  }
  
  if (verbose) message("Exporting plot to: ", file_path)
  
  tryCatch({
    ggplot2::ggsave(
      filename = file_path,
      plot = plot,
      width = export_options$width,
      height = export_options$height,
      dpi = export_options$dpi,
      device = export_options$format
    )
    
    if (verbose) message("Plot exported successfully")
  }, error = function(e) {
    stop("Failed to export plot: ", e$message, call. = FALSE)
  })
}

# Summary Generation ----

#' Generate summary information
#' @noRd
.generate_momocs_summary <- function(pca_result, pca_data, shape_positions, params) {
  
  # PCA information
  pca_info <- list(
    n_specimens = nrow(pca_data),
    pc_x = params$pca_options$pc_x,
    pc_y = params$pca_options$pc_y,
    total_components = ncol(pca_result$x)
  )
  
  # Add variance information if available
  if (!is.null(pca_result$eig)) {
    variance_explained <- pca_result$eig / sum(pca_result$eig) * 100
    pca_info$variance_x <- variance_explained[params$pca_options$pc_x]
    pca_info$variance_y <- variance_explained[params$pca_options$pc_y]
    pca_info$cumulative_variance <- sum(variance_explained[1:max(params$pca_options$pc_x, params$pca_options$pc_y)])
  }
  
  return(list(pca_info = pca_info))
}

# Print Method ----

#' Print method for momocs_plot_result objects
#' @param x A momocs_plot_result object
#' @param ... Additional arguments (ignored)
#' @export
print.momocs_plot_result <- function(x, ...) {
  cat("Momocs Shape Plot Results\n")
  cat("=========================\n\n")
  
  cat("PCA Summary:\n")
  cat("  Components plotted: PC", x$pca_summary$pc_x, " vs PC", x$pca_summary$pc_y, "\n")
  cat("  Total specimens:", x$pca_summary$n_specimens, "\n")
  cat("  Total components:", x$pca_summary$total_components, "\n")
  
  if (!is.null(x$pca_summary$variance_x)) {
    cat("  Variance explained:\n")
    cat("    PC", x$pca_summary$pc_x, ":", round(x$pca_summary$variance_x, 2), "%\n")
    cat("    PC", x$pca_summary$pc_y, ":", round(x$pca_summary$variance_y, 2), "%\n")
    cat("    Cumulative:", round(x$pca_summary$cumulative_variance, 2), "%\n")
  }
  
  cat("\nShape Display:\n")
  cat("  Position method:", x$shape_info$type, "\n")
  cat("  Number of shapes:", length(x$shape_info$indices), "\n")
  cat("  Shape specimens:", paste(head(x$shape_info$indices, 6), collapse = ", "))
  if (length(x$shape_info$indices) > 6) {
    cat(", ...")
  }
  cat("\n")
  
  cat("\nAvailable components:\n")
  cat("  - plot: ggplot2 object\n")
  cat("  - shape_info: Details about displayed shapes\n")
  cat("  - pca_summary: PCA analysis summary\n")
  cat("  - plot_data: Data used for plotting\n")
}