#' Create Advanced Morphometric Analysis Panels
#'
#' Generates customizable multi-plot panels for morphometric analysis including hull plots,
#' contour plots, boxplots, and combined visualizations. Supports flexible layouts,
#' styling options, and export capabilities for publication-ready figures.
#'
#' @param data A data frame containing the morphometric data to be plotted.
#' @param x_col Character string specifying the x-axis column name.
#' @param y_col Character string specifying the y-axis column name.
#' @param group_col Character string specifying the grouping column name.
#' @param panel_options List containing panel configuration options:
#'   \describe{
#'     \item{type}{Panel type: "hulls", "contours", "boxplots", "mixed" (default: "hulls")}
#'     \item{layout}{Panel layout: "individual", "combined", "both" (default: "both")}
#'     \item{ncol}{Number of columns in panel layout (default: 2)}
#'     \item{include_combined}{Include combined plot with all groups (default: TRUE)}
#'   }
#' @param group_options List containing group selection and styling options:
#'   \describe{
#'     \item{group_vals}{Vector of specific groups to include (default: all)}
#'     \item{colors}{Vector of colors for groups (default: auto-generated)}
#'     \item{color_palette}{Color palette name for automatic generation (default: "Set1")}
#'   }
#' @param plot_features List containing plot feature options:
#'   \describe{
#'     \item{hull_alpha}{Transparency for convex hulls (0-1, default: 0.3)}
#'     \item{contour_linewidth}{Width of contour lines (default: 1)}
#'     \item{point_size}{Size of data points (default: 2)}
#'     \item{point_shape}{Shape of data points (default: 21)}
#'     \item{point_fill}{Fill color for points (default: NULL)}
#'   }
#' @param styling List containing plot styling options:
#'   \describe{
#'     \item{plot_style}{Style theme: "Haug", "minimal", "classic" (default: "Haug")}
#'     \item{title_size}{Font size for plot titles (default: 14)}
#'     \item{label_size}{Font size for axis labels (default: 12)}
#'     \item{tick_size}{Font size for axis tick labels (default: 10)}
#'     \item{axis_linewidth}{Width of axis lines (default: 0.5)}
#'   }
#' @param labels List containing label customization options:
#'   \describe{
#'     \item{panel_title}{Overall panel title (default: auto-generated)}
#'     \item{individual_titles}{Custom titles for individual plots (default: auto-generated)}
#'     \item{x_label}{Custom x-axis label (default: column name)}
#'     \item{y_label}{Custom y-axis label (default: column name)}
#'   }
#' @param layout_options List containing layout customization options:
#'   \describe{
#'     \item{plot_spacing}{Spacing between plots (default: 0.02)}
#'     \item{legend_position}{Legend position: "right", "bottom", "none" (default: "right")}
#'     \item{guides_collect}{Collect guides across plots (default: TRUE)}
#'   }
#' @param export_options List containing export options (export, filename, path, format, etc.).
#' @param verbose Logical indicating whether to print progress messages. Default: TRUE.
#'
#' @return A list containing:
#'   \describe{
#'     \item{panel}{The combined patchwork panel plot}
#'     \item{individual_plots}{List of individual ggplot objects}
#'     \item{panel_info}{Information about panel configuration}
#'     \item{group_summary}{Summary of groups and their properties}
#'   }
#'
#' @examples
#' # Basic panel with hulls
#' result <- Haug_panel(
#'   data = morpho_data,
#'   x_col = "PC1", y_col = "PC2",
#'   group_col = "Species"
#' )
#'
#' # Advanced mixed panel
#' result <- Haug_panel(
#'   data = morpho_data,
#'   x_col = "PC1", y_col = "PC2",
#'   group_col = "Species",
#'   panel_options = list(
#'     type = "mixed",
#'     layout = "both",
#'     ncol = 3
#'   ),
#'   group_options = list(
#'     group_vals = c("Species_A", "Species_B", "Species_C"),
#'     color_palette = "viridis"
#'   ),
#'   plot_features = list(
#'     hull_alpha = 0.2,
#'     contour_linewidth = 1.5,
#'     point_size = 3
#'   ),
#'   styling = list(
#'     plot_style = "minimal",
#'     title_size = 16
#'   )
#' )
#'
#' # Contour analysis panel
#' result <- Haug_panel(
#'   data = shape_data,
#'   x_col = "EFA1", y_col = "EFA2",
#'   group_col = "Population",
#'   panel_options = list(
#'     type = "contours",
#'     layout = "individual"
#'   ),
#'   export_options = list(
#'     export = TRUE,
#'     filename = "contour_analysis",
#'     format = "pdf"
#'   )
#' )
#'
#' # View results
#' print(result$panel)
#' summary(result$panel_info)
#'
#' @export
Haug_panel <- function(data,
                      x_col,
                      y_col,
                      group_col,
                      panel_options = list(),
                      group_options = list(),
                      plot_features = list(),
                      styling = list(),
                      labels = list(),
                      layout_options = list(),
                      export_options = list(),
                      verbose = TRUE) {
  
  # Input validation ----
  .validate_haug_panel_inputs(data, x_col, y_col, group_col, verbose)
  
  # Setup parameters ----
  params <- .setup_haug_panel_params(panel_options, group_options, plot_features, styling, labels, layout_options, export_options, verbose)
  
  # Prepare data and groups ----
  prepared_data <- .prepare_haug_panel_data(data, x_col, y_col, group_col, params, verbose)
  
  if (verbose) {
    message("Creating morphometric analysis panel...")
    message("Panel type: ", params$panel_options$type)
    message("Layout: ", params$panel_options$layout)
    message("Groups: ", length(prepared_data$group_vals))
  }
  
  # Generate individual plots ----
  individual_plots <- .generate_haug_panel_plots(prepared_data, x_col, y_col, group_col, params, verbose)
  
  # Create panel layout ----
  panel_plot <- .create_haug_panel_layout(individual_plots, params, verbose)
  
  # Export if requested ----
  if (params$export_options$export) {
    .export_haug_panel(panel_plot, params$export_options, verbose)
  }
  
  # Generate summary information ----
  panel_info <- .generate_haug_panel_info(prepared_data, params, individual_plots)
  
  if (verbose) {
    message("Panel creation completed!")
    message("Generated ", length(individual_plots), " individual plots")
  }
  
  # Prepare results
  structure(
    list(
      panel = panel_plot,
      individual_plots = individual_plots,
      panel_info = panel_info,
      group_summary = prepared_data$group_summary
    ),
    class = "haug_panel_result"
  )
}

# Input Validation ----

#' Validate inputs for Haug_panel function
#' @noRd
.validate_haug_panel_inputs <- function(data, x_col, y_col, group_col, verbose) {
  
  # Check required packages
  required_packages <- c("ggplot2", "dplyr", "patchwork")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required but not available. Please install it.", call. = FALSE)
    }
  }
  
  # Check data
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame", call. = FALSE)
  }
  if (nrow(data) == 0) {
    stop("'data' cannot be empty", call. = FALSE)
  }
  
  # Check columns
  required_cols <- c(x_col, y_col, group_col)
  missing_cols <- required_cols[!required_cols %in% colnames(data)]
  if (length(missing_cols) > 0) {
    stop("The following required columns are missing from data: ", 
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  
  # Check data types
  if (!is.numeric(data[[x_col]])) {
    stop("Column '", x_col, "' must be numeric", call. = FALSE)
  }
  if (!is.numeric(data[[y_col]])) {
    stop("Column '", y_col, "' must be numeric", call. = FALSE)
  }
  
  # Check for sufficient data
  if (nrow(data) < 3) {
    stop("At least 3 data points are required for meaningful visualization", call. = FALSE)
  }
  
  # Check verbose
  if (!is.logical(verbose) || length(verbose) != 1) {
    stop("'verbose' must be a single logical value", call. = FALSE)
  }
}

# Parameter Setup ----

#' Setup parameters for Haug panel creation
#' @noRd
.setup_haug_panel_params <- function(panel_options, group_options, plot_features, styling, labels, layout_options, export_options, verbose) {
  
  # Panel options defaults
  panel_defaults <- list(
    type = "hulls",
    layout = "both",
    ncol = 2,
    include_combined = TRUE
  )
  panel_options <- utils::modifyList(panel_defaults, panel_options)
  
  # Validate panel options
  valid_types <- c("hulls", "contours", "boxplots", "mixed")
  if (!panel_options$type %in% valid_types) {
    stop("'type' must be one of: ", paste(valid_types, collapse = ", "), call. = FALSE)
  }
  
  valid_layouts <- c("individual", "combined", "both")
  if (!panel_options$layout %in% valid_layouts) {
    stop("'layout' must be one of: ", paste(valid_layouts, collapse = ", "), call. = FALSE)
  }
  
  if (panel_options$ncol < 1) {
    stop("'ncol' must be >= 1", call. = FALSE)
  }
  
  # Group options defaults
  group_defaults <- list(
    group_vals = NULL,  # Will be set from data
    colors = NULL,      # Will be auto-generated
    color_palette = "Set1"
  )
  group_options <- utils::modifyList(group_defaults, group_options)
  
  # Plot features defaults
  features_defaults <- list(
    hull_alpha = 0.3,
    contour_linewidth = 1,
    point_size = 2,
    point_shape = 21,
    point_fill = NULL
  )
  plot_features <- utils::modifyList(features_defaults, plot_features)
  
  # Validate features
  if (plot_features$hull_alpha < 0 || plot_features$hull_alpha > 1) {
    stop("'hull_alpha' must be between 0 and 1", call. = FALSE)
  }
  
  # Styling defaults
  styling_defaults <- list(
    plot_style = "Haug",
    title_size = 14,
    label_size = 12,
    tick_size = 10,
    axis_linewidth = 0.5
  )
  styling <- utils::modifyList(styling_defaults, styling)
  
  # Validate styling
  valid_styles <- c("Haug", "minimal", "classic")
  if (!styling$plot_style %in% valid_styles) {
    stop("'plot_style' must be one of: ", paste(valid_styles, collapse = ", "), call. = FALSE)
  }
  
  # Labels defaults
  labels_defaults <- list(
    panel_title = NULL,      # Auto-generated
    individual_titles = NULL, # Auto-generated
    x_label = NULL,          # Use column name
    y_label = NULL           # Use column name
  )
  labels <- utils::modifyList(labels_defaults, labels)
  
  # Layout options defaults
  layout_defaults <- list(
    plot_spacing = 0.02,
    legend_position = "right",
    guides_collect = TRUE
  )
  layout_options <- utils::modifyList(layout_defaults, layout_options)
  
  # Export defaults
  export_defaults <- list(
    export = FALSE,
    filename = "haug_panel",
    path = NULL,
    format = "png",
    width = 12,
    height = 8,
    dpi = 300
  )
  export_options <- utils::modifyList(export_defaults, export_options)
  
  return(list(
    panel_options = panel_options,
    group_options = group_options,
    plot_features = plot_features,
    styling = styling,
    labels = labels,
    layout_options = layout_options,
    export_options = export_options
  ))
}

# Data Preparation ----

#' Prepare data for Haug panel creation
#' @noRd
.prepare_haug_panel_data <- function(data, x_col, y_col, group_col, params, verbose) {
  
  # Remove missing values
  complete_data <- data[complete.cases(data[, c(x_col, y_col, group_col)]), ]
  
  rows_removed <- nrow(data) - nrow(complete_data)
  if (verbose && rows_removed > 0) {
    message("Removed ", rows_removed, " rows with missing values")
  }
  
  if (nrow(complete_data) < 3) {
    stop("Insufficient complete data for visualization", call. = FALSE)
  }
  
  # Determine groups
  all_groups <- unique(complete_data[[group_col]])
  if (is.null(params$group_options$group_vals)) {
    group_vals <- all_groups
  } else {
    # Validate specified groups exist
    invalid_groups <- params$group_options$group_vals[!params$group_options$group_vals %in% all_groups]
    if (length(invalid_groups) > 0) {
      stop("The following groups do not exist in data: ", paste(invalid_groups, collapse = ", "), call. = FALSE)
    }
    group_vals <- params$group_options$group_vals
  }
  
  # Filter data to selected groups
  filtered_data <- complete_data[complete_data[[group_col]] %in% group_vals, ]
  
  # Generate colors if not provided
  colors <- params$group_options$colors
  if (is.null(colors)) {
    colors <- .generate_group_colors(group_vals, params$group_options$color_palette)
  } else if (length(colors) != length(group_vals)) {
    warning("Number of colors does not match number of groups. Auto-generating colors.")
    colors <- .generate_group_colors(group_vals, params$group_options$color_palette)
  }
  
  # Generate group summary
  group_summary <- .generate_group_summary(filtered_data, x_col, y_col, group_col, group_vals)
  
  return(list(
    data = filtered_data,
    group_vals = group_vals,
    colors = colors,
    group_summary = group_summary
  ))
}

# Color Generation ----

#' Generate colors for groups
#' @noRd
.generate_group_colors <- function(group_vals, color_palette) {
  
  n_groups <- length(group_vals)
  
  tryCatch({
    # Try different color palette options
    if (color_palette == "viridis") {
      colors <- viridisLite::viridis(n_groups)
    } else if (color_palette == "Set1") {
      colors <- RColorBrewer::brewer.pal(min(n_groups, 9), "Set1")[1:n_groups]
    } else if (color_palette == "Dark2") {
      colors <- RColorBrewer::brewer.pal(min(n_groups, 8), "Dark2")[1:n_groups]
    } else {
      # Fallback to ggplot2 default colors
      colors <- scales::hue_pal()(n_groups)
    }
  }, error = function(e) {
    # Final fallback
    colors <- scales::hue_pal()(n_groups)
  })
  
  names(colors) <- group_vals
  return(colors)
}

# Group Summary ----

#' Generate summary statistics for groups
#' @noRd
.generate_group_summary <- function(data, x_col, y_col, group_col, group_vals) {
  
  summary_list <- list()
  
  for (group in group_vals) {
    group_data <- data[data[[group_col]] == group, ]
    
    if (nrow(group_data) > 0) {
      summary_list[[group]] <- list(
        n = nrow(group_data),
        x_mean = mean(group_data[[x_col]], na.rm = TRUE),
        x_sd = sd(group_data[[x_col]], na.rm = TRUE),
        y_mean = mean(group_data[[y_col]], na.rm = TRUE),
        y_sd = sd(group_data[[y_col]], na.rm = TRUE),
        x_range = range(group_data[[x_col]], na.rm = TRUE),
        y_range = range(group_data[[y_col]], na.rm = TRUE)
      )
    }
  }
  
  return(summary_list)
}

# Plot Generation ----

#' Generate individual plots for the panel
#' @noRd
.generate_haug_panel_plots <- function(prepared_data, x_col, y_col, group_col, params, verbose) {
  
  plot_list <- list()
  data <- prepared_data$data
  group_vals <- prepared_data$group_vals
  colors <- prepared_data$colors
  
  panel_type <- params$panel_options$type
  layout_type <- params$panel_options$layout
  
  if (panel_type == "hulls") {
    plot_list <- .generate_hull_plots(data, x_col, y_col, group_col, group_vals, colors, params, verbose)
  } else if (panel_type == "contours") {
    plot_list <- .generate_contour_plots(data, x_col, y_col, group_col, group_vals, colors, params, verbose)
  } else if (panel_type == "boxplots") {
    plot_list <- .generate_boxplots(data, x_col, y_col, group_col, group_vals, colors, params, verbose)
  } else if (panel_type == "mixed") {
    plot_list <- .generate_mixed_plots(data, x_col, y_col, group_col, group_vals, colors, params, verbose)
  }
  
  return(plot_list)
}

# Hull Plots ----

#' Generate hull plots
#' @noRd
.generate_hull_plots <- function(data, x_col, y_col, group_col, group_vals, colors, params, verbose) {
  
  plot_list <- list()
  layout_type <- params$panel_options$layout
  
  if (layout_type %in% c("individual", "both")) {
    # Individual hull plots for each group
    for (i in seq_along(group_vals)) {
      group_val <- group_vals[i]
      
      if (verbose) message("Creating hull plot for group: ", group_val)
      
      plot_list[[paste0("hull_", group_val)]] <- .create_single_hull_plot(
        data, x_col, y_col, group_col, group_val, colors[group_val], params
      )
    }
  }
  
  if (layout_type %in% c("combined", "both") && params$panel_options$include_combined) {
    # Combined hull plot with all groups
    if (verbose) message("Creating combined hull plot")
    
    plot_list[["combined_hulls"]] <- .create_combined_hull_plot(
      data, x_col, y_col, group_col, group_vals, colors, params
    )
  }
  
  return(plot_list)
}

# Single Hull Plot ----

#' Create a single hull plot for one group
#' @noRd
.create_single_hull_plot <- function(data, x_col, y_col, group_col, group_val, color, params) {
  
  # Filter data to the specific group
  group_data <- data[data[[group_col]] == group_val, ]
  
  if (nrow(group_data) < 3) {
    warning("Insufficient data for hull plot for group: ", group_val)
    return(ggplot2::ggplot() + ggplot2::ggtitle(paste("Insufficient data:", group_val)))
  }
  
  # Create hull
  hull_data <- .compute_convex_hull(group_data, x_col, y_col)
  
  # Create plot
  plot <- ggplot2::ggplot(group_data, ggplot2::aes_string(x = x_col, y = y_col)) +
    ggplot2::geom_polygon(data = hull_data, ggplot2::aes_string(x = x_col, y = y_col),
                         fill = color, alpha = params$plot_features$hull_alpha,
                         color = color, size = 0.5) +
    ggplot2::geom_point(size = params$plot_features$point_size,
                       shape = params$plot_features$point_shape,
                       fill = params$plot_features$point_fill %||% color,
                       color = "black") +
    ggplot2::labs(title = paste("Hull for", group_val),
                 x = params$labels$x_label %||% x_col,
                 y = params$labels$y_label %||% y_col) +
    .apply_haug_theme(params$styling)
  
  return(plot)
}

# Combined Hull Plot ----

#' Create combined hull plot with all groups
#' @noRd
.create_combined_hull_plot <- function(data, x_col, y_col, group_col, group_vals, colors, params) {
  
  plot <- ggplot2::ggplot(data, ggplot2::aes_string(x = x_col, y = y_col, color = group_col, fill = group_col))
  
  # Add hulls for each group
  for (group_val in group_vals) {
    group_data <- data[data[[group_col]] == group_val, ]
    
    if (nrow(group_data) >= 3) {
      hull_data <- .compute_convex_hull(group_data, x_col, y_col)
      hull_data[[group_col]] <- group_val
      
      plot <- plot +
        ggplot2::geom_polygon(data = hull_data, 
                             ggplot2::aes_string(x = x_col, y = y_col, fill = group_col),
                             alpha = params$plot_features$hull_alpha,
                             color = colors[group_val], size = 0.5)
    }
  }
  
  # Add points
  plot <- plot +
    ggplot2::geom_point(size = params$plot_features$point_size,
                       shape = params$plot_features$point_shape) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::labs(title = "Combined Hulls for All Groups",
                 x = params$labels$x_label %||% x_col,
                 y = params$labels$y_label %||% y_col) +
    .apply_haug_theme(params$styling)
  
  return(plot)
}

# Contour Plots ----

#' Generate contour plots
#' @noRd
.generate_contour_plots <- function(data, x_col, y_col, group_col, group_vals, colors, params, verbose) {
  
  plot_list <- list()
  layout_type <- params$panel_options$layout
  
  if (layout_type %in% c("individual", "both")) {
    # Individual contour plots for each group
    for (i in seq_along(group_vals)) {
      group_val <- group_vals[i]
      
      if (verbose) message("Creating contour plot for group: ", group_val)
      
      plot_list[[paste0("contour_", group_val)]] <- .create_single_contour_plot(
        data, x_col, y_col, group_col, group_val, colors[group_val], params
      )
    }
  }
  
  if (layout_type %in% c("combined", "both") && params$panel_options$include_combined) {
    # Combined contour plot
    if (verbose) message("Creating combined contour plot")
    
    plot_list[["combined_contours"]] <- .create_combined_contour_plot(
      data, x_col, y_col, group_col, group_vals, colors, params
    )
  }
  
  return(plot_list)
}

# Single Contour Plot ----

#' Create a single contour plot for one group
#' @noRd
.create_single_contour_plot <- function(data, x_col, y_col, group_col, group_val, color, params) {
  
  # Filter data to the specific group
  group_data <- data[data[[group_col]] == group_val, ]
  
  if (nrow(group_data) < 5) {
    warning("Insufficient data for contour plot for group: ", group_val)
    return(ggplot2::ggplot() + ggplot2::ggtitle(paste("Insufficient data:", group_val)))
  }
  
  # Create plot with density contours
  plot <- ggplot2::ggplot(group_data, ggplot2::aes_string(x = x_col, y = y_col)) +
    ggplot2::stat_density_2d(color = color, size = params$plot_features$contour_linewidth) +
    ggplot2::geom_point(size = params$plot_features$point_size,
                       shape = params$plot_features$point_shape,
                       fill = params$plot_features$point_fill %||% color,
                       color = "black") +
    ggplot2::labs(title = paste("Contours for", group_val),
                 x = params$labels$x_label %||% x_col,
                 y = params$labels$y_label %||% y_col) +
    .apply_haug_theme(params$styling)
  
  return(plot)
}

# Combined Contour Plot ----

#' Create combined contour plot with all groups
#' @noRd
.create_combined_contour_plot <- function(data, x_col, y_col, group_col, group_vals, colors, params) {
  
  plot <- ggplot2::ggplot(data, ggplot2::aes_string(x = x_col, y = y_col, color = group_col)) +
    ggplot2::stat_density_2d(size = params$plot_features$contour_linewidth) +
    ggplot2::geom_point(size = params$plot_features$point_size,
                       shape = params$plot_features$point_shape,
                       ggplot2::aes_string(fill = group_col)) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::labs(title = "Combined Contours for All Groups",
                 x = params$labels$x_label %||% x_col,
                 y = params$labels$y_label %||% y_col) +
    .apply_haug_theme(params$styling)
  
  return(plot)
}

# Boxplots ----

#' Generate boxplots
#' @noRd
.generate_boxplots <- function(data, x_col, y_col, group_col, group_vals, colors, params, verbose) {
  
  if (verbose) message("Creating boxplots")
  
  # Prepare data in long format
  long_data <- .prepare_boxplot_data(data, x_col, y_col, group_col)
  
  # Create boxplot
  plot <- ggplot2::ggplot(long_data, ggplot2::aes_string(x = "Variable", y = "Value", fill = group_col)) +
    ggplot2::geom_boxplot() +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::labs(title = "Distribution Comparison",
                 x = "Variables",
                 y = "Values") +
    .apply_haug_theme(params$styling)
  
  return(list("boxplot" = plot))
}

# Mixed Plots ----

#' Generate mixed plots (combination of different types)
#' @noRd
.generate_mixed_plots <- function(data, x_col, y_col, group_col, group_vals, colors, params, verbose) {
  
  plot_list <- list()
  
  # Generate a few hull plots
  hull_plots <- .generate_hull_plots(data, x_col, y_col, group_col, group_vals[1:min(2, length(group_vals))], colors, params, FALSE)
  
  # Generate a contour plot
  contour_plots <- .generate_contour_plots(data, x_col, y_col, group_col, group_vals, colors, params, FALSE)
  
  # Generate a boxplot
  boxplots <- .generate_boxplots(data, x_col, y_col, group_col, group_vals, colors, params, FALSE)
  
  # Combine all plots
  plot_list <- c(hull_plots, contour_plots[1], boxplots)
  
  return(plot_list)
}

# Utility Functions ----

#' Compute convex hull for a group of points
#' @noRd
.compute_convex_hull <- function(data, x_col, y_col) {
  
  if (nrow(data) < 3) {
    return(data.frame())
  }
  
  tryCatch({
    points <- data[, c(x_col, y_col)]
    hull_indices <- grDevices::chull(points)
    hull_data <- points[hull_indices, ]
    return(hull_data)
  }, error = function(e) {
    warning("Failed to compute convex hull: ", e$message)
    return(data.frame())
  })
}

#' Prepare data for boxplots
#' @noRd
.prepare_boxplot_data <- function(data, x_col, y_col, group_col) {
  
  # Convert to long format
  long_data <- data.frame(
    Group = rep(data[[group_col]], 2),
    Variable = rep(c(x_col, y_col), each = nrow(data)),
    Value = c(data[[x_col]], data[[y_col]])
  )
  
  names(long_data)[1] <- group_col
  return(long_data)
}

#' Apply Haug theme to plots
#' @noRd
.apply_haug_theme <- function(styling) {
  
  if (styling$plot_style == "Haug") {
    theme <- ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = styling$title_size, face = "bold"),
        axis.title = ggplot2::element_text(size = styling$label_size),
        axis.text = ggplot2::element_text(size = styling$tick_size),
        axis.line = ggplot2::element_line(size = styling$axis_linewidth),
        panel.grid.minor = ggplot2::element_blank()
      )
  } else if (styling$plot_style == "minimal") {
    theme <- ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = styling$title_size),
        axis.title = ggplot2::element_text(size = styling$label_size),
        axis.text = ggplot2::element_text(size = styling$tick_size)
      )
  } else {  # classic
    theme <- ggplot2::theme_classic() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = styling$title_size),
        axis.title = ggplot2::element_text(size = styling$label_size),
        axis.text = ggplot2::element_text(size = styling$tick_size)
      )
  }
  
  return(theme)
}

# Panel Layout ----

#' Create the final panel layout
#' @noRd
.create_haug_panel_layout <- function(individual_plots, params, verbose) {
  
  if (length(individual_plots) == 0) {
    stop("No plots generated for panel", call. = FALSE)
  }
  
  if (verbose) message("Arranging ", length(individual_plots), " plots in panel")
  
  # Create panel using patchwork
  panel <- patchwork::wrap_plots(individual_plots, ncol = params$panel_options$ncol)
  
  # Apply layout options
  if (params$layout_options$guides_collect) {
    panel <- panel + patchwork::plot_layout(guides = "collect")
  }
  
  # Add overall title if specified
  if (!is.null(params$labels$panel_title)) {
    panel <- panel + patchwork::plot_annotation(title = params$labels$panel_title)
  }
  
  return(panel)
}

# Export ----

#' Export Haug panel
#' @noRd
.export_haug_panel <- function(panel, export_options, verbose) {
  
  # Setup file path
  if (!is.null(export_options$path)) {
    if (!dir.exists(export_options$path)) {
      stop("Export path does not exist: ", export_options$path, call. = FALSE)
    }
    file_path <- file.path(export_options$path, paste0(export_options$filename, ".", export_options$format))
  } else {
    file_path <- paste0(export_options$filename, ".", export_options$format)
  }
  
  if (verbose) message("Exporting panel to: ", file_path)
  
  tryCatch({
    ggplot2::ggsave(
      filename = file_path,
      plot = panel,
      width = export_options$width,
      height = export_options$height,
      dpi = export_options$dpi,
      device = export_options$format
    )
    
    if (verbose) message("Panel exported successfully")
  }, error = function(e) {
    stop("Failed to export panel: ", e$message, call. = FALSE)
  })
}

# Panel Information ----

#' Generate panel information summary
#' @noRd
.generate_haug_panel_info <- function(prepared_data, params, individual_plots) {
  
  list(
    panel_type = params$panel_options$type,
    layout_type = params$panel_options$layout,
    n_groups = length(prepared_data$group_vals),
    group_names = prepared_data$group_vals,
    n_plots = length(individual_plots),
    plot_names = names(individual_plots),
    total_observations = nrow(prepared_data$data),
    styling_theme = params$styling$plot_style
  )
}

# Print Method ----

#' Print method for haug_panel_result objects
#' @param x A haug_panel_result object
#' @param ... Additional arguments (ignored)
#' @export
print.haug_panel_result <- function(x, ...) {
  cat("Haug Panel Analysis Results\n")
  cat("===========================\n\n")
  
  cat("Panel Configuration:\n")
  cat("  Type:", x$panel_info$panel_type, "\n")
  cat("  Layout:", x$panel_info$layout_type, "\n")
  cat("  Number of plots:", x$panel_info$n_plots, "\n")
  cat("  Number of groups:", x$panel_info$n_groups, "\n")
  
  cat("\nGroups analyzed:\n")
  for (i in 1:length(x$panel_info$group_names)) {
    group_name <- x$panel_info$group_names[i]
    n_obs <- x$group_summary[[group_name]]$n
    cat("  -", group_name, "(n =", n_obs, ")\n")
  }
  
  cat("\nTotal observations:", x$panel_info$total_observations, "\n")
  cat("Styling theme:", x$panel_info$styling_theme, "\n")
  
  cat("\nAvailable components:\n")
  cat("  - panel: Combined patchwork panel plot\n")
  cat("  - individual_plots: List of individual ggplot objects\n")
  cat("  - panel_info: Panel configuration details\n")
  cat("  - group_summary: Statistical summary for each group\n")
}