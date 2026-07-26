#' Generate a Customizable Scatter Plot with Hulls and Contours
#'
#' Creates a highly customizable scatter plot with optional convex hulls, 
#' contours, and shape overlays. Supports grouping, multiple styling options,
#' and various export formats.
#'
#' @param data A data frame containing the data to be plotted.
#' @param x_col Character string specifying the x-axis column name.
#' @param y_col Character string specifying the y-axis column name.
#' @param group_col Optional character string specifying the grouping column name.
#' @param group_vals Optional vector specifying which group values to display.
#'   If NULL, all unique values in group_col are used.
#' @param styling List containing styling options. See Details for available options.
#' @param features List containing feature options (hulls, contours, shapes). 
#'   See Details for available options.
#' @param labels List containing label options (title, axis labels).
#' @param export_options List containing export options. See Details for available options.
#' @param interactive Logical indicating whether to enable interactive plotly mode. Default: FALSE.
#'   When TRUE, returns a plotly object with hover events. Requires plotly package.
#' @param pca_model Optional PCA model list for interactive reconstruction. Only used when interactive = TRUE.
#'   If provided, enables real-time shape reconstruction on hover. Can be created with extract_pca_model().
#' @param verbose Logical indicating whether to print progress messages. Default: TRUE.
#'
#' @details
#' The `styling` parameter accepts a list with the following options:
#' \describe{
#'   \item{plot_style}{Style theme: "Haug", "inverted_Haug", "publication" (default: "Haug")}
#'   \item{point}{List with point styling (color, fill, shape, size)}
#'   \item{text}{List with text styling (title_size, label_size, tick_size, legend_size)}
#'   \item{axis}{List with axis styling (linewidth, tick_length, tick_margin, central_axes, aspect)}
#'
#' Aspect options:
#' - "auto" (default): no fixed aspect unless shapes are drawn, then 1:1
#' - "free": never fix aspect (let ggplot scale axes independently)
#' - "1:1" or "2:1": lock aspect using coord_fixed (interpreted as width:height)
#' }
#'
#' The `features` parameter accepts a list with the following options:
#' \describe{
#'   \item{hulls}{List with hull options (show, groups, fill, color, alpha, linetype)}
#'   \item{contours}{List with contour options (show, groups, colors, linewidth)}
#'   \item{shapes}{List with shape options (show, groups, size, shift, adjustments)}
#' }
#'
#' The `export_options` parameter accepts a list with the following options:
#' \describe{
#'   \item{export}{Logical, whether to export the plot (default: FALSE)}
#'   \item{filename}{Base filename without extension}
#'   \item{path}{Optional output directory (if NULL, uses working directory)}
#'   \item{format}{Output format: "rds", "svg", "tiff", or "png"}
#'   \item{width}{Width in inches (optional for SVG, required for TIFF/PNG, ignored for RDS)}
#'   \item{height}{Height in inches (optional for SVG, required for TIFF/PNG, ignored for RDS)}
#'   \item{dpi}{Resolution in dots per inch for raster formats only (default: 300)}
#' }
#' Notes on export formats:
#' - RDS: Saves the ggplot object for later editing in R
#' - SVG: Vector format with base dimensions (scalable without quality loss)
#' - TIFF/PNG: Raster formats requiring width, height, and DPI specifications
#'
#' When \code{interactive = TRUE}, the plot is converted to plotly for interactive exploration.
#' If \code{pca_model} is also provided, hovering over the morphospace will show:
#' - For data points: ID and existing shape (if available in data)
#' - For empty space: Reconstructed hypothetical shape at those PC coordinates
#'
#' @return A ggplot2 object (default) or plotly object (when interactive = TRUE).
#'
#' @examples
#' # Basic scatter plot
#' test_data <- data.frame(
#'   PC1 = rnorm(100), PC2 = rnorm(100),
#'   species = sample(c("A", "B", "C"), 100, replace = TRUE)
#' )
#'
#' # Simple plot
#' p1 <- shape_plot(test_data, x_col = "PC1", y_col = "PC2")
#'
#' # Grouped plot with hulls
#' p2 <- shape_plot(
#'   data = test_data,
#'   x_col = "PC1", y_col = "PC2", group_col = "species",
#'   features = list(hulls = list(show = TRUE))
#' )
#'
#' # Advanced customization
#' p3 <- shape_plot(
#'   data = test_data,
#'   x_col = "PC1", y_col = "PC2", group_col = "species",
#'   styling = list(
#'     plot_style = "publication",
#'     point = list(size = 3, shape = 16),
#'     text = list(title_size = 20)
#'   ),
#'   features = list(
#'     hulls = list(show = TRUE, alpha = 0.3),
#'     contours = list(show = TRUE)
#'   ),
#'   labels = list(
#'     title = "PCA Analysis",
#'     x_label = "First Principal Component",
#'     y_label = "Second Principal Component"
#'   )
#' )
#'
#' @param z_col Optional character string naming the column for the Z axis in 3D mode. Default NULL.
#' @export
shape_plot <- function(data,
                      x_col,
                      y_col,
                      z_col = NULL,
                      group_col = NULL,
                      group_vals = NULL,
                      styling = list(),
                      features = list(),
                      labels = list(),
                      export_options = list(),
                      interactive = FALSE,
                      pca_model = NULL,
                      gradient = NULL,
                      verbose = TRUE) {
  
  # Input validation ----
  .validate_shape_plot_inputs(data, x_col, y_col, group_col, group_vals, verbose)
  
  # Setup parameters with defaults ----
  params <- .setup_shape_plot_params(
    data, x_col, y_col, group_col, group_vals, 
    styling, features, labels, export_options, verbose
  )
  
  # 3D mode: short-circuit to plotly scatter3d ----
  if (!is.null(z_col)) {
    if (!z_col %in% colnames(data)) {
      stop("Column '", z_col, "' does not exist in data", call. = FALSE)
    }
    return(.build_3d_plot(data, x_col, y_col, z_col, group_col, params, verbose,
                          interactive = interactive, pca_model = pca_model,
                          gradient = gradient))
  }

  # Clean and prepare data ----
  clean_data <- .prepare_plot_data(data, x_col, y_col, group_col, params$features$shapes$show, 
                                  params$features$shapes$shape_col, verbose)
  
  if (verbose) {
    message("Creating shape plot with ", nrow(clean_data), " data points")
    if (!is.null(group_col)) {
      message("Groups: ", paste(params$group_vals, collapse = ", "))
    }
  }
  
  # Create base plot ----
  plot <- .create_base_plot(clean_data, x_col, y_col, params)
  
  # Add points ----
  plot <- .add_points_to_plot(plot, clean_data, x_col, y_col, group_col, params, gradient = gradient)
  
  # Add features ----
  if (params$features$hulls$show) {
    plot <- .add_hulls_to_plot(plot, clean_data, x_col, y_col, group_col, params, verbose)
  }
  
  if (params$features$contours$show) {
    plot <- .add_contours_to_plot(plot, clean_data, x_col, y_col, group_col, params, verbose)
  }
  
  if (params$features$shapes$show) {
    plot <- .add_shapes_to_plot(plot, clean_data, x_col, y_col, group_col, params, verbose)
  }
  
  # Apply styling and theming ----
  plot <- .apply_plot_styling(plot, params)
  # Apply aspect (auto/free or fixed) to avoid distortion when needed
  asp_choice <- params$styling$axis$aspect
  # Interpret aspect option
  apply_fixed <- FALSE
  fixed_ratio <- 1
  if (is.null(asp_choice) || identical(asp_choice, "") || identical(asp_choice, "free")) {
    apply_fixed <- FALSE
  } else if (identical(asp_choice, "auto")) {
    # Only lock when shapes are drawn to preserve geometry
    if (isTRUE(params$features$shapes$show)) {
      apply_fixed <- TRUE
      fixed_ratio <- 1
    }
  } else if (is.numeric(asp_choice) && is.finite(asp_choice) && asp_choice > 0) {
    apply_fixed <- TRUE
    fixed_ratio <- as.numeric(asp_choice)
  } else if (is.character(asp_choice)) {
    # Accept tokens like "1:1", "2:1" (width:height)
    if (grepl(":", asp_choice, fixed = TRUE)) {
      parts <- strsplit(asp_choice, ":", fixed = TRUE)[[1]]
      if (length(parts) == 2) {
        w <- suppressWarnings(as.numeric(parts[1]))
        h <- suppressWarnings(as.numeric(parts[2]))
        if (is.finite(w) && is.finite(h) && w > 0) {
          apply_fixed <- TRUE
          fixed_ratio <- h / w # coord_fixed ratio is y/x
        }
      }
    } else if (nzchar(asp_choice)) {
      # Any other non-empty string: fallback to fixed 1:1
      apply_fixed <- TRUE
      fixed_ratio <- 1
    }
  }
  if (apply_fixed) {
    # Avoid clipping custom annotations near the panel edges
    plot <- plot + ggplot2::coord_fixed(ratio = fixed_ratio, expand = TRUE, clip = "off")
  } else {
    # Keep free coordinates but ensure annotations outside the panel aren't cropped
    plot <- plot + ggplot2::coord_cartesian(clip = "off")
  }
  # Centralized axes overlay (legacy style)
  if (isTRUE(params$styling$axis$central_axes)) {
    plot <- .apply_central_axes(plot, clean_data, x_col, y_col, params)
  }
  
  # Export if requested ----
  if (params$export_options$export) {
    .export_plot(plot, params$export_options, verbose)
  }
  
  # Convert to interactive plotly if requested ----
  if (interactive) {
    if (!requireNamespace("plotly", quietly = TRUE)) {
      warning("Package 'plotly' is required for interactive mode. Returning static ggplot2 object.")
      return(plot)
    }
    
    if (verbose) {
      message("Converting to interactive plotly plot...")
    }
    
    # Create custom hover text with IDs
    hover_data <- clean_data
    id_col <- if ("ID" %in% names(hover_data)) "ID" else NULL
    
    if (!is.null(id_col)) {
      hover_text <- paste0(
        "ID: ", hover_data[[id_col]], "\n",
        x_col, ": ", round(hover_data[[x_col]], 3), "\n",
        y_col, ": ", round(hover_data[[y_col]], 3)
      )
      
      # Store hover text in plot data for plotly conversion
      plot$data$text <- hover_text
    }
    
    # Convert to plotly with event source for Shiny reactivity
    plotly_plot <- plotly::ggplotly(plot, tooltip = "text", source = "morphospace")
    
    # Add invisible background grid to capture hover events across entire plot area
    # This allows reconstruction to work everywhere, not just near data points
    if (!is.null(pca_model)) {
      # Get plot range from data
      x_range <- range(clean_data[[x_col]], na.rm = TRUE)
      y_range <- range(clean_data[[y_col]], na.rm = TRUE)
      
      # Expand range slightly
      x_padding <- diff(x_range) * 0.1
      y_padding <- diff(y_range) * 0.1
      x_range <- x_range + c(-x_padding, x_padding)
      y_range <- y_range + c(-y_padding, y_padding)
      
      # Create a grid of invisible points covering the plot area
      grid_density <- 50  # Points per axis
      x_grid <- seq(x_range[1], x_range[2], length.out = grid_density)
      y_grid <- seq(y_range[1], y_range[2], length.out = grid_density)
      grid_points <- expand.grid(x = x_grid, y = y_grid)
      
      # Add invisible trace at the beginning (will be rendered first, below everything)
      invisible_trace <- list(
        x = grid_points$x,
        y = grid_points$y,
        type = "scatter",
        mode = "markers",
        marker = list(
          size = 8,
          opacity = 0,  # Completely invisible
          color = "rgba(0,0,0,0)"
        ),
        hoverinfo = "x+y",
        hovertemplate = paste0(x_col, ": %{x:.3f}<br>", y_col, ": %{y:.3f}<extra></extra>"),
        showlegend = FALSE,
        name = "morphospace"
      )
      
      # Selection highlight: initially empty, updated via plotlyProxy on click (trace index 1)
      selection_trace_2d <- list(
        x = list(NA_real_), y = list(NA_real_),
        type = "scatter", mode = "markers",
        marker = list(
          size = 16, symbol = "circle-open", color = "#e63946",
          line = list(width = 3, color = "#e63946")
        ),
        hoverinfo = "skip", showlegend = FALSE, name = "selection"
      )
      # Insert: invisible grid at plotly index 0, selection highlight at plotly index 1
      plotly_plot$x$data <- c(list(invisible_trace), list(selection_trace_2d), plotly_plot$x$data)
    }
    
    # Reorder traces to ensure points are on top of hulls/polygons
    # This is crucial for click events to work correctly on data points
    if (length(plotly_plot$x$data) > 1) {
      # Skip the first TWO fixed traces when interactive (invisible grid + selection highlight)
      start_idx <- if (!is.null(pca_model)) 3 else 1
      traces_to_check <- seq(start_idx, length(plotly_plot$x$data))
      
      # Identify point traces vs polygon/fill traces
      point_indices <- which(sapply(traces_to_check, function(idx) {
        trace <- plotly_plot$x$data[[idx]]
        # Points have mode="markers" or type="scatter" with markers
        (!is.null(trace$mode) && grepl("markers", trace$mode)) ||
        (!is.null(trace$type) && trace$type == "scatter" && !is.null(trace$marker))
      }))
      if (length(point_indices) > 0) point_indices <- traces_to_check[point_indices]
      
      polygon_indices <- which(sapply(traces_to_check, function(idx) {
        trace <- plotly_plot$x$data[[idx]]
        # Polygons typically have fill or are paths without markers
        (!is.null(trace$fill) && trace$fill != "none") ||
        (!is.null(trace$mode) && trace$mode == "lines" && is.null(trace$marker))
      }))
      if (length(polygon_indices) > 0) polygon_indices <- traces_to_check[polygon_indices]
      
      # Reorder: invisible grid (if exists), polygons, other traces, then points on top
      if (length(point_indices) > 0 && length(polygon_indices) > 0) {
        base_traces <- if (!is.null(pca_model)) list(plotly_plot$x$data[[1]]) else list()
        other_indices <- setdiff(traces_to_check, c(point_indices, polygon_indices))
        new_order <- c(
          if (!is.null(pca_model)) c(1L, 2L) else NULL,  # Fixed: grid + selection stay first
          polygon_indices, 
          other_indices, 
          point_indices
        )
        plotly_plot$x$data <- plotly_plot$x$data[new_order]
      }
    }
    
    # Configure for better interaction and styling
    plotly_plot <- plotly::layout(
      plotly_plot,
      hovermode = "closest",
      clickmode = "event",
      dragmode = "pan",
      # Minimal axis styling - no labels, no ticks, no grid
      xaxis = list(
        title = "",
        showgrid = FALSE,
        zeroline = TRUE,
        zerolinecolor = "rgba(0,0,0,0.5)",
        showline = FALSE,
        mirror = FALSE,
        ticks = "",
        showticklabels = FALSE
      ),
      yaxis = list(
        title = "",
        showgrid = FALSE,
        zeroline = TRUE,
        zerolinecolor = "rgba(0,0,0,0.5)",
        showline = FALSE,
        mirror = FALSE,
        ticks = "",
        showticklabels = FALSE
      ),
      # Set plot background
      plot_bgcolor = "white",
      paper_bgcolor = "white"
    )
    
    # Attach PCA model if provided (for Shiny use)
    if (!is.null(pca_model)) {
      attr(plotly_plot, "pca_model") <- pca_model
      if (verbose) {
        message("PCA model attached for interactive reconstruction")
      }
    }
    
    return(plotly_plot)
  }
  
  return(plot)
}

# Input Validation ----

#' Validate inputs for shape_plot function
#' @noRd
.validate_shape_plot_inputs <- function(data, x_col, y_col, group_col, group_vals, verbose) {
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
  
  # Check group_col
  if (!is.null(group_col)) {
    if (!is.character(group_col) || length(group_col) != 1) {
      stop("'group_col' must be a single character string", call. = FALSE)
    }
    if (!group_col %in% colnames(data)) {
      stop("Column '", group_col, "' does not exist in data", call. = FALSE)
    }
  }
  
  # Check group_vals
  if (!is.null(group_vals) && !is.null(group_col)) {
    if (!all(group_vals %in% unique(data[[group_col]]))) {
      missing_vals <- group_vals[!group_vals %in% unique(data[[group_col]])]
      stop("The following group values do not exist: ", paste(missing_vals, collapse = ", "), 
           call. = FALSE)
    }
  }
  
  # Check verbose
  if (!is.logical(verbose) || length(verbose) != 1) {
    stop("'verbose' must be a single logical value", call. = FALSE)
  }
}

# Parameter Setup ----

#' Setup default parameters for shape_plot
#' @noRd
.setup_shape_plot_params <- function(data, x_col, y_col, group_col, group_vals,
                                    styling, features, labels, export_options, verbose) {
  
  # Setup group values
  if (!is.null(group_col) && is.null(group_vals)) {
    group_vals <- unique(data[[group_col]])
  }
  
  # Setup colors
  n_groups <- if (!is.null(group_vals)) length(group_vals) else 1
  default_colors <- if (n_groups > 1) scales::hue_pal()(n_groups) else c("#1f77b4")
  
  # Setup styling defaults
  styling_defaults <- list(
    plot_style = "Haug",
    point = list(
      color = default_colors,
      fill = default_colors,
      shape = 21,
      size = 2
    ),
    text = list(
      title_size = 24,
      label_size = 20,
      tick_size = 15,
      legend_size = 13
    ),
    show_legend = TRUE,
    legend_offset_h = 0,
    legend_offset_v = 0.5,
    plot_margin_top = 1.5,
    axis = list(
      linewidth = 1,
      tick_length = 0.005,
      tick_margin = 0.05,
      central_axes = TRUE,
      aspect = "auto"  # default: free aspect unless shapes are shown
    )
  )
  styling <- .merge_nested_lists(styling_defaults, styling)
  # Backward compatibility: map legacy fixed_aspect flag to aspect choice if supplied
  if (!is.null(styling$axis$fixed_aspect)) {
    if (isTRUE(styling$axis$fixed_aspect)) {
      styling$axis$aspect <- "1:1"
    } else if (isFALSE(styling$axis$fixed_aspect) && is.null(styling$axis$aspect)) {
      styling$axis$aspect <- "free"
    }
  }
  # Ensure point aesthetics are not NULL after merge (UI may send NULLs)
  if (is.null(styling$point$color) || length(styling$point$color) == 0) {
    styling$point$color <- default_colors
  }
  if (is.null(styling$point$fill) || length(styling$point$fill) == 0) {
    styling$point$fill <- default_colors
  }
  if (is.null(styling$point$shape) || length(styling$point$shape) == 0) {
    styling$point$shape <- 21
  }
  
  # Setup feature defaults
  features_defaults <- list(
    hulls = list(
      show = FALSE,
      groups = group_vals,
      fill = default_colors,
      color = "black",
      alpha = 0.1,
      linetype = "solid",
      linewidth = 0.5
    ),
    contours = list(
      show = FALSE,
      groups = group_vals,
      colors = "black",
      linewidth = 0.5
    ),
    shapes = list(
      show = FALSE,
      groups = group_vals,
      shape_col = "shape",
      only_hull = TRUE,
      combined_hull = FALSE,
      size = 0.01,
      shift = 0.1,
      x_adjust = 0,
      y_adjust = 0
    )
  )
  features <- .merge_nested_lists(features_defaults, features)
  # Ensure non-NULL defaults after merge (avoid replication errors)
  if (is.null(features$hulls$fill) || length(features$hulls$fill) == 0) {
    features$hulls$fill <- default_colors
  }
  if (is.null(features$hulls$color) || length(features$hulls$color) == 0) {
    features$hulls$color <- "black"
  }
  if (is.null(features$contours$colors) || length(features$contours$colors) == 0) {
    features$contours$colors <- "black"
  }
  if (is.null(features$hulls$linewidth) || length(features$hulls$linewidth) == 0) {
    features$hulls$linewidth <- 0.5
  }
  
  # Setup label defaults
  labels_defaults <- list(
    title = NULL,
    x_label = x_col,
    y_label = y_col,
    x_adjust = c(0, 0),  # c(x, y) adjustments
    y_adjust = c(0, 0),  # c(x, y) adjustments
    x_size = 5,
    y_size = 5,
    rotate_y = FALSE,
    show_borders = TRUE
  )
  labels <- utils::modifyList(labels_defaults, labels)
  
  # Setup export defaults
  export_defaults <- list(
    export = FALSE,
    filename = "shape_plot_output",
    path = NULL,
    format = "tiff",  # "tiff" | "jpg" | "png" etc.
    width = NULL,      # in inches; if NULL and height provided, computed from aspect
    height = NULL,     # in inches; if NULL and width provided, computed from aspect
    dpi = 300          # dots per inch
  )
  export_options <- utils::modifyList(export_defaults, export_options)
  
  return(list(
    group_vals = group_vals,
    styling = styling,
    features = features,
    labels = labels,
    export_options = export_options
  ))
}

#' Merge nested lists recursively
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

#' Resolve a vector (possibly named) to match a set of groups
#' @noRd
.resolve_group_vector <- function(vec, groups, fallback_fn = NULL) {
  groups_chr <- as.character(groups)
  # Determine expected output type
  detect_type <- function(x) {
    t <- typeof(x)
    if (length(x) == 0) return(t)
    if (is.list(x)) return("list")
    t
  }
  out_type <- if (!is.null(vec) && length(vec) > 0) detect_type(vec) else {
    if (!is.null(fallback_fn)) detect_type(fallback_fn(1)) else "character"
  }
  template <- switch(out_type,
    "integer" = integer(1),
    "double" = numeric(1),
    "logical" = logical(1),
    character(1)
  )
  na_value <- switch(out_type,
    "integer" = NA_integer_,
    "double" = NA_real_,
    "logical" = NA,
    NA_character_
  )

  # If vec is NULL/empty, return fallback replicated
  if (is.null(vec) || length(vec) == 0) {
    if (is.null(fallback_fn)) {
      default_val <- switch(out_type,
        "integer" = 1L,
        "double" = 1,
        "logical" = TRUE,
        "character" = "#1f77b4",
        "#1f77b4"
      )
      return(rep_len(default_val, length(groups_chr)))
    }
    return(fallback_fn(length(groups_chr)))
  }

  # If named, align by names
  nm <- names(vec)
  if (!is.null(nm) && any(nzchar(nm))) {
    out <- vapply(groups_chr, function(g) {
      if (g %in% nm) vec[[g]] else na_value
    }, template)
    # Fill missing with rep_len of first (or fallback)
    if (any(is.na(out))) {
      repl <- if (!is.null(fallback_fn)) fallback_fn(sum(is.na(out))) else rep_len(vec[[1]], sum(is.na(out)))
      out[is.na(out)] <- repl
    }
    return(out)
  }
  # Otherwise, just recycle
  return(rep_len(vec, length(groups_chr)))
}

# Data Preparation ----

#' Prepare and clean plot data
#' @noRd
.prepare_plot_data <- function(data, x_col, y_col, group_col, show_shapes, shape_col, verbose) {
  
  # Define required columns
  required_cols <- c(x_col, y_col)
  if (!is.null(group_col)) required_cols <- c(required_cols, group_col)
  if (show_shapes && shape_col %in% colnames(data)) required_cols <- c(required_cols, shape_col)
  
  # Filter out rows with missing or infinite values
  clean_data <- data %>%
    dplyr::filter(
      !is.na(.data[[x_col]]) & is.finite(.data[[x_col]]) &
      !is.na(.data[[y_col]]) & is.finite(.data[[y_col]]) &
      if (!is.null(group_col)) !is.na(.data[[group_col]]) else TRUE &
      if (show_shapes && shape_col %in% colnames(data)) !is.na(.data[[shape_col]]) else TRUE
    )
  
  rows_removed <- nrow(data) - nrow(clean_data)
  if (verbose && rows_removed > 0) {
    message("Removed ", rows_removed, " rows with missing or infinite values")
  }
  
  if (nrow(clean_data) == 0) {
    stop("No valid data points remaining after filtering", call. = FALSE)
  }
  
  return(clean_data)
}

# Plot Creation ----

#' Create base ggplot object
#' @noRd
.create_base_plot <- function(data, x_col, y_col, params) {
  
  # Get style colors
  style_colors <- .get_style_colors(params$styling$plot_style)
  
  # Create base plot
  plot <- ggplot2::ggplot(data, ggplot2::aes_string(x = x_col, y = y_col))
  
  # Add background rectangle for publication style
  if (params$styling$plot_style == "publication") {
    x_range <- range(data[[x_col]], na.rm = TRUE)
    y_range <- range(data[[y_col]], na.rm = TRUE)
    
    plot <- plot +
      ggplot2::geom_rect(
        ggplot2::aes(xmin = x_range[1], xmax = x_range[2], 
                    ymin = y_range[1], ymax = y_range[2]),
        fill = style_colors$plot_background, color = NA, inherit.aes = FALSE
      )
  }
  
  return(plot)
}

#' Get style-specific colors
#' @noRd
.get_style_colors <- function(plot_style) {
  switch(plot_style,
    "inverted_Haug" = list(
      background = "black",
      plot_background = "black", 
      text = "white",
      axis = "white",
      text_field_fill = "black",
      text_field_color = "white"
    ),
    "publication" = list(
      background = "white",
      plot_background = "#f1f1f1",
      text = "black", 
      axis = "black",
      text_field_fill = "white",
      text_field_color = "black"
    ),
    "Haug" = list(
      background = "white",
      plot_background = "white",
      text = "black",
      axis = "black", 
      text_field_fill = "white",
      text_field_color = "black"
    )
  )
}

# Point Addition ----

#' Add points to the plot
#' @noRd
.add_points_to_plot <- function(plot, data, x_col, y_col, group_col, params, gradient = NULL) {

  # Gradient coloring overrides group coloring when a gradient column is provided
  if (!is.null(gradient) && !is.null(gradient$col) && gradient$col %in% names(data)) {
    gcol_sym <- rlang::sym(gradient$col)
    legend_title <- gradient$legend_title %||% gradient$col

    # Map both color and fill to the gradient so shape 21 (filled circle) shows
    # the gradient on the point body, not just the border.
    plot <- plot +
      ggplot2::geom_point(
        data = data,
        ggplot2::aes(x = !!rlang::sym(x_col), y = !!rlang::sym(y_col),
                     color = !!gcol_sym, fill = !!gcol_sym),
        shape = params$styling$point$shape[1],
        size  = params$styling$point$size[1]
      )

    if (!is.null(gradient$mid)) {
      # Three-color gradient with auto median midpoint
      mid_val <- stats::median(data[[gradient$col]], na.rm = TRUE)
      plot <- plot +
        ggplot2::scale_color_gradient2(
          low      = gradient$low,
          mid      = gradient$mid,
          high     = gradient$high,
          midpoint = mid_val,
          name     = legend_title,
          na.value = "grey50",
          guide    = "none"
        ) +
        ggplot2::scale_fill_gradient2(
          low      = gradient$low,
          mid      = gradient$mid,
          high     = gradient$high,
          midpoint = mid_val,
          name     = legend_title,
          na.value = "grey50"
        )
    } else {
      plot <- plot +
        ggplot2::scale_color_gradient(
          low      = gradient$low,
          high     = gradient$high,
          name     = legend_title,
          na.value = "grey50",
          guide    = "none"
        ) +
        ggplot2::scale_fill_gradient(
          low      = gradient$low,
          high     = gradient$high,
          name     = legend_title,
          na.value = "grey50"
        )
    }

    return(plot)
  }

  if (!is.null(group_col) && !is.null(params$group_vals)) {
    # Add grouped points
    point_colors <- .resolve_group_vector(
      params$styling$point$color,
      params$group_vals,
      function(n) { if (requireNamespace("scales", quietly = TRUE)) scales::hue_pal()(n) else rep("#1f77b4", n) }
    )
    point_fills <- .resolve_group_vector(
      params$styling$point$fill,
      params$group_vals,
      function(n) { if (requireNamespace("scales", quietly = TRUE)) scales::hue_pal()(n) else rep("#1f77b4", n) }
    )
    point_shapes <- .resolve_group_vector(
      params$styling$point$shape,
      params$group_vals,
      function(n) rep(21, n)
    )
    point_sizes <- rep_len(params$styling$point$size, length(params$group_vals))
    
    for (i in seq_along(params$group_vals)) {
      group_val <- params$group_vals[i]
      group_data <- data %>% dplyr::filter(!!rlang::sym(group_col) == group_val)
      
      if (nrow(group_data) > 0) {
        plot <- plot +
          ggplot2::geom_point(
            data = group_data,
            ggplot2::aes_string(x = x_col, y = y_col),
            color = point_colors[i],
            fill = point_fills[i], 
            shape = point_shapes[i],
            size = point_sizes[i]
          )
      }
    }

    # Add invisible dummy layer to drive a proper ggplot2 legend
    legend_data <- data %>%
      dplyr::filter(!!rlang::sym(group_col) %in% params$group_vals) %>%
      dplyr::mutate(.legend_group = factor(!!rlang::sym(group_col), levels = params$group_vals))
    names(point_colors) <- as.character(params$group_vals)
    names(point_fills)  <- as.character(params$group_vals)
    names(point_shapes) <- as.character(params$group_vals)
    # Note: fill is intentionally NOT mapped here so scale_fill remains free
    # for other overlay layers (e.g. gap heatmap uses scale_fill_gradient2).
    # The correct fill colour is injected via override.aes in the legend guide.
    plot <- plot +
      ggplot2::geom_point(
        data = legend_data,
        ggplot2::aes(x = !!rlang::sym(x_col), y = !!rlang::sym(y_col),
                     color = .legend_group, shape = .legend_group),
        size = 0, alpha = 0, show.legend = TRUE
      ) +
      ggplot2::scale_color_manual(name = group_col, values = point_colors) +
      ggplot2::scale_shape_manual(name = group_col, values = point_shapes) +
      ggplot2::guides(
        color = ggplot2::guide_legend(override.aes = list(
          color = point_colors, fill = point_fills,
          shape = point_shapes, size = 3, alpha = 1
        )),
        shape = "none"
      )

  } else {
    # Add ungrouped points
    plot <- plot +
      ggplot2::geom_point(
        ggplot2::aes_string(x = x_col, y = y_col),
        color = params$styling$point$color[1],
        fill = params$styling$point$fill[1],
        shape = params$styling$point$shape[1],
        size = params$styling$point$size[1]
      )
  }

  return(plot)
}

#' Add convex hulls to the plot
#' @noRd
.add_hulls_to_plot <- function(plot, data, x_col, y_col, group_col, params, verbose) {
  
  if (is.null(group_col)) {
    # Single hull for all data
    if (nrow(data) >= 3) {
      hull_indices <- grDevices::chull(data[[x_col]], data[[y_col]])
      hull_data <- data[hull_indices, ]
      
      plot <- plot +
        ggplot2::geom_polygon(
          data = hull_data,
          ggplot2::aes_string(x = x_col, y = y_col),
          fill = params$features$hulls$fill[1],
          color = params$features$hulls$color[1],
          alpha = params$features$hulls$alpha[1],
          linetype = params$features$hulls$linetype[1],
          size = params$features$hulls$linewidth[1]
        )
    } else if (verbose) {
      warning("Insufficient points for hull calculation (need >= 3)")
    }
  } else {
    # Group-specific hulls
    hull_groups <- params$features$hulls$groups
    if (is.null(hull_groups)) hull_groups <- params$group_vals

    # Resolve per-group aesthetics (supports named vectors)
    hull_fills <- .resolve_group_vector(
      params$features$hulls$fill,
      hull_groups,
      function(n) { if (requireNamespace("scales", quietly = TRUE)) scales::hue_pal()(n) else rep("#1f77b4", n) }
    )
    hull_colors <- .resolve_group_vector(
      params$features$hulls$color,
      hull_groups,
      function(n) rep("black", n)
    )
    hull_alphas <- .resolve_group_vector(
      params$features$hulls$alpha,
      hull_groups,
      function(n) rep(0.1, n)
    )
    
    for (i in seq_along(hull_groups)) {
      group_val <- hull_groups[i]
      group_data <- data %>% dplyr::filter(!!rlang::sym(group_col) == group_val)
      
      if (verbose) {
        message("Processing hull for group: ", group_val, " (", nrow(group_data), " points)")
      }
      
      if (nrow(group_data) >= 3) {
        tryCatch({
          hull_indices <- grDevices::chull(group_data[[x_col]], group_data[[y_col]])
          hull_data <- group_data[hull_indices, ]
          
          plot <- plot +
            ggplot2::geom_polygon(
              data = hull_data,
              ggplot2::aes_string(x = x_col, y = y_col),
              fill = hull_fills[i],
              color = hull_colors[i],
              alpha = hull_alphas[i],
              linetype = .resolve_group_vector(params$features$hulls$linetype, hull_groups, function(n) rep("solid", n))[i],
              size = .resolve_group_vector(params$features$hulls$linewidth, hull_groups, function(n) rep(0.5, n))[i]
            )
        }, error = function(e) {
          if (verbose) warning("Failed to create hull for group ", group_val, ": ", e$message)
        })
      } else if (verbose) {
        warning("Group ", group_val, " has < 3 points, skipping hull")
      }
    }
  }
  
  return(plot)
}

# Contour Addition ----

#' Add contours to the plot
#' @noRd
.add_contours_to_plot <- function(plot, data, x_col, y_col, group_col, params, verbose) {
  
  if (!requireNamespace("MASS", quietly = TRUE)) {
    if (verbose) warning("Package 'MASS' required for contours but not available")
    return(plot)
  }
  
  contour_groups <- params$features$contours$groups
  if (is.null(group_col) || is.null(contour_groups)) {
    # Single contour for all data
    tryCatch({
      kde_result <- MASS::kde2d(data[[x_col]], data[[y_col]], n = 50)
      contour_data <- .kde_to_dataframe(kde_result)
      
      plot <- plot +
        ggplot2::geom_contour(
          data = contour_data,
          ggplot2::aes(x = x, y = y, z = z),
          color = params$features$contours$colors[1],
          size = params$features$contours$linewidth
        )
    }, error = function(e) {
      if (verbose) warning("Failed to create contours: ", e$message)
    })
  } else {
    # Group-specific contours
    contour_colors <- .resolve_group_vector(
      params$features$contours$colors,
      contour_groups,
      function(n) rep("black", n)
    )
    
    for (i in seq_along(contour_groups)) {
      group_val <- contour_groups[i]
      group_data <- data %>% dplyr::filter(!!rlang::sym(group_col) == group_val)
      
      if (nrow(group_data) >= 10) {  # Need sufficient points for KDE
        tryCatch({
          kde_result <- MASS::kde2d(group_data[[x_col]], group_data[[y_col]], n = 50)
          contour_data <- .kde_to_dataframe(kde_result)
          
          plot <- plot +
            ggplot2::geom_contour(
              data = contour_data,
              ggplot2::aes(x = x, y = y, z = z),
              color = contour_colors[i],
              size = params$features$contours$linewidth
            )
        }, error = function(e) {
          if (verbose) warning("Failed to create contours for group ", group_val, ": ", e$message)
        })
      } else if (verbose) {
        warning("Group ", group_val, " has insufficient points for contours (need >= 10)")
      }
    }
  }
  
  return(plot)
}

#' Convert KDE result to data frame
#' @noRd
.kde_to_dataframe <- function(kde_result) {
  expand.grid(x = kde_result$x, y = kde_result$y) %>%
    dplyr::mutate(z = as.vector(kde_result$z))
}

# Shape Addition ----

#' Add shapes to the plot
#' @noRd
.add_shapes_to_plot <- function(plot, data, x_col, y_col, group_col, params, verbose) {

  shape_col <- params$features$shapes$shape_col
  if (!shape_col %in% colnames(data)) {
    if (verbose) warning("Shape column '", shape_col, "' not found in data")
    return(plot)
  }

  # Filter rows to groups (if provided)
  draw_data <- data
  if (!is.null(group_col) && !is.null(params$features$shapes$groups)) {
    draw_groups <- params$features$shapes$groups
    draw_data <- draw_data %>% dplyr::filter(!!rlang::sym(group_col) %in% draw_groups)
  }

  # If only_hull is requested, keep only the rows that form convex hull
  if (isTRUE(params$features$shapes$only_hull)) {
    if (isTRUE(params$features$shapes$combined_hull) && !is.null(group_col)) {
      # Combined hull: one hull across all selected groups together.
      # This also determines the reference centroid for shift direction later.
      if (nrow(draw_data) >= 3) {
        idx <- tryCatch(grDevices::chull(draw_data[[x_col]], draw_data[[y_col]]), error = function(...) integer())
        draw_data <- if (length(idx)) draw_data[idx, , drop = FALSE] else draw_data[0, , drop = FALSE]
      } else {
        draw_data <- draw_data[0, , drop = FALSE]
      }
    } else if (!is.null(group_col)) {
      draw_data <- draw_data %>% dplyr::group_by(!!rlang::sym(group_col)) %>% dplyr::group_map(~{
        df <- .x
        if (nrow(df) >= 3) {
          idx <- tryCatch(grDevices::chull(df[[x_col]], df[[y_col]]), error = function(...) integer())
          if (length(idx)) df[idx, , drop = FALSE] else df[0, , drop = FALSE]
        } else df[0, , drop = FALSE]
      }) %>% dplyr::bind_rows()
    } else {
      if (nrow(draw_data) >= 3) {
        idx <- tryCatch(grDevices::chull(draw_data[[x_col]], draw_data[[y_col]]), error = function(...) integer())
        draw_data <- if (length(idx)) draw_data[idx, , drop = FALSE] else draw_data[0, , drop = FALSE]
      } else {
        draw_data <- draw_data[0, , drop = FALSE]
      }
    }
  }

  # Keep only rows that have a shape object
  if (!is.list(draw_data[[shape_col]])) {
    if (verbose) warning("Column '", shape_col, "' is not a list of shapes; skipping overlay")
    return(plot)
  }

  # Extract parameters for positioning and scaling (defaults assured in setup)
  s_size <- as.numeric(params$features$shapes$size)
  s_shift <- as.numeric(params$features$shapes$shift)
  s_xadj <- as.numeric(params$features$shapes$x_adjust)
  s_yadj <- as.numeric(params$features$shapes$y_adjust)

  # Pre-compute centroids for shift direction.
  # When combined_hull is TRUE, all shapes use the global centroid of the combined
  # group so they radiate outward uniformly from the shared cloud centre.
  # Otherwise each shape radiates away from its own group centroid.
  global_cx <- mean(draw_data[[x_col]], na.rm = TRUE)
  global_cy <- mean(draw_data[[y_col]], na.rm = TRUE)
  use_combined_centroid <- isTRUE(params$features$shapes$combined_hull) && !is.null(group_col)
  if (!use_combined_centroid && !is.null(group_col) && group_col %in% colnames(draw_data)) {
    group_centroids <- draw_data %>%
      dplyr::group_by(!!rlang::sym(group_col)) %>%
      dplyr::summarise(
        cx = mean(.data[[x_col]], na.rm = TRUE),
        cy = mean(.data[[y_col]], na.rm = TRUE),
        .groups = "drop"
      )
    centroid_lookup <- stats::setNames(
      lapply(seq_len(nrow(group_centroids)), function(k) {
        c(group_centroids$cx[k], group_centroids$cy[k])
      }),
      group_centroids[[group_col]]
    )
  } else {
    centroid_lookup <- NULL  # will fall back to global_cx / global_cy
  }

  # Determine plot aspect ratio (y/x) from styling; default 1:1. For "w:h", ratio = h/w
  get_ratio <- function(asp) {
    if (is.null(asp)) return(1)
    if (is.numeric(asp) && is.finite(asp) && asp > 0) return(as.numeric(asp))
    if (is.character(asp) && grepl(":", asp, fixed = TRUE)) {
      parts <- strsplit(asp, ":", fixed = TRUE)[[1]]
      if (length(parts) == 2) {
        w <- suppressWarnings(as.numeric(parts[1]))
        h <- suppressWarnings(as.numeric(parts[2]))
        if (is.finite(w) && is.finite(h) && w > 0) return(h / w)
      }
    }
    1
  }
  coord_ratio <- get_ratio(params$styling$axis$aspect)

  # Compute global scale in data units (use overall range)
  xr <- range(data[[x_col]], na.rm = TRUE)
  yr <- range(data[[y_col]], na.rm = TRUE)
  scale_fac <- s_size * max(diff(xr), diff(yr))
  if (!is.finite(scale_fac) || scale_fac <= 0) scale_fac <- s_size

  # Helper to safely extract coords from a Momocs Out object
  get_coords <- function(shape_obj) {
    # Expect an 'Out' with $coo[[1]] as a matrix of x,y
    if (is.null(shape_obj)) return(NULL)
    # Try typical slot
    coords <- try({ shape_obj$coo[[1]] }, silent = TRUE)
    if (inherits(coords, "try-error") || is.null(coords)) {
      # Try generic extraction
      coords <- try({ if (!is.null(shape_obj$coo)) shape_obj$coo[[1]] else NULL }, silent = TRUE)
    }
    if (is.null(coords) || !is.matrix(coords) || ncol(coords) != 2) return(NULL)
    coords
  }

  # Build a combined data frame of transformed polygon coordinates for all rows
  shape_rows <- which(vapply(draw_data[[shape_col]], function(x) !is.null(x), logical(1)))
  if (length(shape_rows) == 0) {
    if (verbose) message("No shapes to draw (all NULL)")
    return(plot)
  }

  poly_list <- list()
  idx <- 1L
  for (i in shape_rows) {
    shp <- draw_data[[shape_col]][[i]]
    coords <- get_coords(shp)
    if (is.null(coords)) next

    # Center and scale coordinates
    coords <- coords[is.finite(coords[,1]) & is.finite(coords[,2]), , drop = FALSE]
    if (nrow(coords) < 3) next
    center <- colMeans(coords, na.rm = TRUE)
    rel <- sweep(coords, 2, center, FUN = "-")
    rel <- rel * scale_fac
    # Compensate y dimension so shapes remain visually 1:1 under non-square plot aspect
    if (is.finite(coord_ratio) && coord_ratio > 0 && coord_ratio != 1) {
      rel[,2] <- rel[,2] / coord_ratio
    }

    # Determine offset position for this row
    px <- as.numeric(draw_data[[x_col]][i])
    py <- as.numeric(draw_data[[y_col]][i])
    if (!is.finite(px) || !is.finite(py)) next

    # Shift away from the point along direction from its group centroid (or global
    # centroid when no grouping is used); fallback to upward if the point is exactly
    # at the centroid.
    if (!is.null(centroid_lookup) && !is.null(group_col) && group_col %in% colnames(draw_data)) {
      grp_val <- as.character(draw_data[[group_col]][i])
      cent    <- centroid_lookup[[grp_val]]
      ref_x   <- if (!is.null(cent)) cent[[1]] else global_cx
      ref_y   <- if (!is.null(cent)) cent[[2]] else global_cy
    } else {
      ref_x <- global_cx
      ref_y <- global_cy
    }
    vx <- px - ref_x; vy <- py - ref_y
    vlen <- sqrt(vx^2 + vy^2)
    if (!is.finite(vlen) || vlen == 0) { vx <- 0; vy <- 1; vlen <- 1 }
    ux <- vx / vlen; uy <- vy / vlen
    offx <- px + s_shift * ux + s_xadj
    offy <- py + s_shift * uy + s_yadj

  final <- cbind(rel[,1] + offx, rel[,2] + offy)
    poly_list[[idx]] <- data.frame(
      sx = final[,1], sy = final[,2],
      .shape_id = idx,
      stringsAsFactors = FALSE
    )
    idx <- idx + 1L
  }

  if (length(poly_list) == 0) return(plot)
  shape_df <- dplyr::bind_rows(poly_list)

  # Basic styling for shapes (future: expose in UI)
  shape_border_color <- "black"
  shape_linewidth <- 0.3
  plot <- plot +
    ggplot2::geom_polygon(
      data = shape_df,
      ggplot2::aes(x = sx, y = sy, group = .shape_id),
      inherit.aes = FALSE,
      fill = "black",
      color = shape_border_color,
      linewidth = shape_linewidth
    )

  return(plot)
}

# Styling Application ----

#' Apply styling to the plot
#' @noRd
.apply_plot_styling <- function(plot, params) {
  
  style_colors <- .get_style_colors(params$styling$plot_style)
  
  plot <- plot +
    ggplot2::labs(
      title = params$labels$title,
      x = params$labels$x_label,
      y = params$labels$y_label
    ) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = style_colors$background, color = NA),
      panel.background = ggplot2::element_rect(fill = style_colors$plot_background, color = NA),
      plot.title = ggplot2::element_text(
        size = params$styling$text$title_size, 
        color = style_colors$text
      ),
      axis.title = ggplot2::element_text(
        size = params$styling$text$label_size,
        color = style_colors$text
      ),
      axis.text = ggplot2::element_text(
        size = params$styling$text$tick_size,
        color = style_colors$text
      ),
      axis.line = ggplot2::element_line(
        color = style_colors$axis,
        size = params$styling$axis$linewidth
      ),
      axis.ticks = ggplot2::element_line(
        color = style_colors$axis,
        size = params$styling$axis$linewidth
      ),
      axis.ticks.length = ggplot2::unit(params$styling$axis$tick_length, "npc"),
      legend.position = if (isTRUE(params$styling$show_legend)) "right" else "none",
      legend.text = ggplot2::element_text(size = params$styling$text$legend_size, color = style_colors$text),
      legend.title = ggplot2::element_text(size = params$styling$text$legend_size, color = style_colors$text),
      legend.key = ggplot2::element_rect(fill = style_colors$plot_background, color = NA),
      legend.background = ggplot2::element_rect(fill = style_colors$background, color = NA),
      legend.box.spacing = ggplot2::unit(params$styling$legend_offset_h, "lines"),
      legend.justification = c(0, params$styling$legend_offset_v),
      plot.margin = ggplot2::margin(
        t = params$styling$plot_margin_top,
        r = 0.5, b = 0.5, l = 0.5, unit = "lines"
      )
    )
  
  return(plot)
}

#' Add centralized axes with arrows, custom ticks, and label fields (legacy style)
#' @noRd
.apply_central_axes <- function(plot, data, x_col, y_col, params) {
  # Ensure the panel includes zero on both axes without clamping data to >= 0
  plot <- plot + ggplot2::expand_limits(x = 0, y = 0)

  # Ranges and padding (force 0 to be part of the range used for axis drawing)
  x_range <- range(c(0, data[[x_col]]), na.rm = TRUE)
  y_range <- range(c(0, data[[y_col]]), na.rm = TRUE)
  x_expand <- 0.05 * (x_range[2] - x_range[1])
  y_expand <- 0.05 * (y_range[2] - y_range[1])

  # Ticks (exclude 0 and outermost)
  x_ticks <- pretty(x_range)
  y_ticks <- pretty(y_range)
  x_ticks <- x_ticks[x_ticks != 0 & x_ticks > x_range[1] & x_ticks < x_range[2]]
  y_ticks <- y_ticks[y_ticks != 0 & y_ticks > y_range[1] & y_ticks < y_range[2]]

  # Styling
  style_colors <- .get_style_colors(params$styling$plot_style)
  axis_col <- style_colors$axis
  text_col <- style_colors$text
  lw <- params$styling$axis$linewidth
  tick_len_prop <- params$styling$axis$tick_length
  tick_margin <- params$styling$axis$tick_margin

  # Convert tick length to data units
  x_tick_len <- tick_len_prop * diff(y_range)
  y_tick_len <- tick_len_prop * diff(x_range)

  # Reset/minimal theme with hidden default axes/labels
  plot <- plot +
    ggplot2::theme_minimal(base_family = "sans") +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(
        t = params$styling$plot_margin_top,
        r = tick_margin, b = tick_margin, l = tick_margin, unit = "lines"
      ),
      legend.position = if (isTRUE(params$styling$show_legend)) "right" else "none",
      legend.text = ggplot2::element_text(size = params$styling$text$legend_size, color = text_col),
      legend.title = ggplot2::element_text(size = params$styling$text$legend_size, color = text_col),
      legend.key = ggplot2::element_rect(fill = style_colors$plot_background, color = NA),
      legend.background = ggplot2::element_rect(fill = style_colors$background, color = NA),
      legend.box.spacing = ggplot2::unit(params$styling$legend_offset_h, "lines"),
      legend.justification = c(0, params$styling$legend_offset_v)
    )

  # Arrowed axes through origin
  plot <- plot +
    ggplot2::geom_segment(
      ggplot2::aes(x = x_range[1] - x_expand, xend = x_range[2] + x_expand, y = 0, yend = 0),
      arrow = grid::arrow(length = grid::unit(0.3, "cm")), color = axis_col, size = lw
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(y = y_range[1] - y_expand, yend = y_range[2] + y_expand, x = 0, xend = 0),
      arrow = grid::arrow(length = grid::unit(0.3, "cm")), color = axis_col, size = lw
    )

  # Tick marks
  if (length(x_ticks)) {
    plot <- plot +
      ggplot2::geom_segment(
        data = data.frame(x = x_ticks),
        ggplot2::aes(x = x, xend = x, y = -x_tick_len, yend = x_tick_len),
        color = axis_col, size = lw
      )
  }
  if (length(y_ticks)) {
    plot <- plot +
      ggplot2::geom_segment(
        data = data.frame(y = y_ticks),
        ggplot2::aes(y = y, yend = y, x = -y_tick_len, xend = y_tick_len),
        color = axis_col, size = lw
      )
  }

  # Tick labels
  tick_text_size <- params$styling$text$tick_size / 3
  if (length(x_ticks)) {
    plot <- plot +
      ggplot2::geom_text(
        data = data.frame(x = x_ticks, y = 0),
        ggplot2::aes(x = x, y = y, label = x),
        vjust = 1.5 + tick_len_prop * 50,
        size = tick_text_size, color = text_col
      )
  }
  if (length(y_ticks)) {
    plot <- plot +
      ggplot2::geom_text(
        data = data.frame(x = 0, y = y_ticks),
        ggplot2::aes(x = x, y = y, label = y),
        hjust = 1.5 + tick_len_prop * 50,
        size = tick_text_size, color = text_col
      )
  }

  # Title and custom axis labels with optional borders
  plot <- plot + ggplot2::labs(title = params$labels$title, y = NULL)

  x_adj <- params$labels$x_adjust; if (length(x_adj) != 2) x_adj <- c(0,0)
  y_adj <- params$labels$y_adjust; if (length(y_adj) != 2) y_adj <- c(0,0)

  x_lab_x <- max(x_range) + x_expand + as.numeric(x_adj[1])
  x_lab_y <- -0.05 * diff(y_range) + as.numeric(x_adj[2])
  y_lab_x <- as.numeric(y_adj[1])
  y_lab_y <- max(y_range) + 0.12 * max(y_range) + y_expand + as.numeric(y_adj[2])

  if (isTRUE(params$labels$show_borders)) {
    plot <- plot +
      ggplot2::annotate(
        "label",
        x = x_lab_x, y = x_lab_y,
        label = params$labels$x_label, size = params$labels$x_size,
        label.padding = grid::unit(0.3, "lines"),
        color = style_colors$text_field_color, fill = style_colors$text_field_fill
      ) +
      ggplot2::annotate(
        "label",
        x = y_lab_x, y = y_lab_y,
        label = params$labels$y_label, size = params$labels$y_size,
        label.padding = grid::unit(0.3, "lines"),
        color = style_colors$text_field_color, fill = style_colors$text_field_fill,
        angle = ifelse(isTRUE(params$labels$rotate_y), 90, 0)
      )
  } else {
    plot <- plot +
      ggplot2::annotate(
        "text",
        x = x_lab_x, y = x_lab_y,
        label = params$labels$x_label, size = params$labels$x_size, color = text_col
      ) +
      ggplot2::annotate(
        "text",
        x = y_lab_x, y = y_lab_y,
        label = params$labels$y_label, size = params$labels$y_size, color = text_col,
        angle = ifelse(isTRUE(params$labels$rotate_y), 90, 0)
      )
  }

  return(plot)
}

# Export Functions ----

#' Export plot to file (RDS, SVG, TIFF, or PNG)
#' @noRd
.export_plot <- function(plot, export_options, verbose) {
  
  # Validate format
  fmt <- tolower(export_options$format)
  if (!fmt %in% c("rds", "svg", "tiff", "png")) {
    stop("Unsupported export format: '", export_options$format, "'. ",
         "Supported formats: rds, svg, tiff, png", call. = FALSE)
  }
  
  # Setup file path
  if (!is.null(export_options$path)) {
    if (!dir.exists(export_options$path)) {
      stop("Export path does not exist: ", export_options$path, call. = FALSE)
    }
    file_path <- file.path(export_options$path, 
                          paste0(export_options$filename, ".", fmt))
  } else {
    file_path <- paste0(export_options$filename, ".", fmt)
  }
  
  if (verbose) message("Exporting plot to: ", file_path)
  
  tryCatch({
    if (fmt == "rds") {
      # Save ggplot object as RDS for later editing in R
      saveRDS(plot, file = file_path)
      if (verbose) message("ggplot object saved as RDS")
      
    } else if (fmt == "svg") {
      # Export as SVG (vector format with base dimensions)
      width <- export_options$width
      height <- export_options$height
      
      # Use defaults if not provided
      if (is.null(width)) width <- 8
      if (is.null(height)) height <- 8
      
      ggplot2::ggsave(
        filename = file_path,
        plot = plot,
        width = width,
        height = height,
        device = "svg"
      )
      if (verbose) {
        message(sprintf("Plot exported as SVG (%g x %g inches base size)", 
                       width, height))
      }
      
    } else if (fmt %in% c("tiff", "png")) {
      # Export as raster with specified dimensions
      width <- export_options$width
      height <- export_options$height
      dpi <- if (!is.null(export_options$dpi)) export_options$dpi else 300
      
      # Validate dimensions are provided for raster formats
      if (is.null(width) || is.null(height)) {
        stop("Width and height must be specified for ", 
             toupper(fmt), " export", call. = FALSE)
      }
      
      # Export using ggsave
      ggplot2::ggsave(
        filename = file_path,
        plot = plot,
        width = width,
        height = height,
        dpi = dpi,
        device = fmt
      )
      if (verbose) {
        message(sprintf("Plot exported as %s (%g x %g inches, %d DPI)", 
                       toupper(fmt), width, height, dpi))
      }
    }
    
    if (verbose) message("Export completed successfully")
    
  }, error = function(e) {
    stop("Failed to export plot: ", e$message, call. = FALSE)
  })
}

# 3D Plotting ----

#' Build invisible 3D bounding-box grid for interactive hover reconstruction
#'
#' Creates a scatter3d trace of points on a regular 3D grid covering the full
#' extent of the three PC axes (plus 10 % padding on each side), analogous to
#' the 2D invisible background grid used in 2D interactive mode.  Markers are
#' fully transparent but still fire plotly hover events, so the user can hover
#' anywhere in the morphospace volume to obtain real-time shape reconstructions.
#' The trace must be inserted as the FIRST trace so that curveNumber == 0 in
#' hover events uniquely identifies it.
#'
#' @param data      Clean data frame with finite PC score columns
#' @param x_col,y_col,z_col  Column name strings for the three axes
#' @param grid_density  Number of grid points per axis (default 50; 50^3 = 125000)
#' @return A list suitable for use as a plotly trace argument
#' @noRd
.build_3d_invisible_grid <- function(data, x_col, y_col, z_col,
                                      grid_density = 50) {
  pad <- function(rng) {
    p <- diff(rng) * 0.1
    rng + c(-p, p)
  }

  x_range <- pad(range(data[[x_col]], na.rm = TRUE))
  y_range <- pad(range(data[[y_col]], na.rm = TRUE))
  z_range <- pad(range(data[[z_col]], na.rm = TRUE))

  grid <- expand.grid(
    x = seq(x_range[1], x_range[2], length.out = grid_density),
    y = seq(y_range[1], y_range[2], length.out = grid_density),
    z = seq(z_range[1], z_range[2], length.out = grid_density)
  )

  list(
    type       = "scatter3d",
    mode       = "markers",
    x          = grid$x,
    y          = grid$y,
    z          = grid$z,
    marker     = list(
      size    = 6,
      opacity = 0,
      color   = "rgba(0,0,0,0)"
    ),
    hovertemplate = paste0(
      x_col, ": %{x:.4f}<br>",
      y_col, ": %{y:.4f}<br>",
      z_col, ": %{z:.4f}<extra></extra>"
    ),
    showlegend = FALSE,
    name       = "morphospace_3d"
  )
}

#' Build a 3D scatter plot using plotly scatter3d
#' @noRd
.build_3d_plot <- function(data, x_col, y_col, z_col, group_col, params, verbose,
                           interactive = FALSE, pca_model = NULL, gradient = NULL) {

  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' is required for 3D mode.", call. = FALSE)
  }

  # Filter to finite values on all 3 axes
  clean_data <- data[
    is.finite(data[[x_col]]) &
    is.finite(data[[y_col]]) &
    is.finite(data[[z_col]]),
    , drop = FALSE
  ]
  if (!is.null(group_col) && group_col %in% colnames(clean_data)) {
    clean_data <- clean_data[!is.na(clean_data[[group_col]]), , drop = FALSE]
  }

  if (nrow(clean_data) == 0) {
    stop("No valid data points after filtering NA/Inf in x/y/z columns.", call. = FALSE)
  }

  id_col <- if ("ID" %in% names(clean_data)) "ID" else NULL

  # Assemble source identifier: distinct for interactive 3D to avoid colliding
  # with the 2D "morphospace" hover observer.
  plot_source <- if (interactive) "morphospace_3d" else NULL

  p <- if (!is.null(plot_source)) {
    plotly::plot_ly(source = plot_source)
  } else {
    plotly::plot_ly()
  }

  # In interactive mode, prepend an invisible 3D grid as trace 0 so that its
  # curveNumber is always 0 in hover events (data traces follow at 1, 2, ...).
  # The grid covers the full bounding box of the morphospace with 10% padding,
  # mirroring the 2D invisible background grid approach.
  if (interactive && !is.null(pca_model)) {
    grid_trace <- .build_3d_invisible_grid(clean_data, x_col, y_col, z_col)
    p <- do.call(plotly::add_trace, c(list(p = p), grid_trace))
    # Selection highlight at plotly index 1 (initially NA, updated via plotlyProxy on click)
    p <- plotly::add_trace(
      p,
      type      = "scatter3d",
      mode      = "markers",
      x         = NA_real_,
      y         = NA_real_,
      z         = NA_real_,
      marker    = list(
        size   = 10,
        color  = "#e63946",
        line   = list(width = 2, color = "#880000")
      ),
      hoverinfo  = "skip",
      showlegend = FALSE,
      name       = "selection"
    )
  }

  if (!is.null(gradient) && !is.null(gradient$col) && gradient$col %in% names(clean_data)) {
    # Gradient coloring: single trace with marker colors driven by numeric column
    legend_title <- gradient$legend_title %||% gradient$col
    grad_vals    <- clean_data[[gradient$col]]
    val_min      <- min(grad_vals, na.rm = TRUE)
    val_max      <- max(grad_vals, na.rm = TRUE)

    # Build plotly colorscale list
    if (!is.null(gradient$mid)) {
      mid_norm <- if (val_max > val_min) {
        (stats::median(grad_vals, na.rm = TRUE) - val_min) / (val_max - val_min)
      } else 0.5
      cs_list <- list(
        list(0,        gradient$low),
        list(mid_norm, gradient$mid),
        list(1,        gradient$high)
      )
    } else {
      cs_list <- list(
        list(0, gradient$low),
        list(1, gradient$high)
      )
    }

    hover_text <- if (!is.null(id_col)) {
      paste0("ID: ", clean_data[[id_col]], "<br>",
             x_col, ": ", round(clean_data[[x_col]], 3), "<br>",
             y_col, ": ", round(clean_data[[y_col]], 3), "<br>",
             z_col, ": ", round(clean_data[[z_col]], 3), "<br>",
             legend_title, ": ", round(grad_vals, 3))
    } else {
      paste0(x_col, ": ", round(clean_data[[x_col]], 3), "<br>",
             y_col, ": ", round(clean_data[[y_col]], 3), "<br>",
             z_col, ": ", round(clean_data[[z_col]], 3), "<br>",
             legend_title, ": ", round(grad_vals, 3))
    }

    p <- plotly::add_trace(
      p,
      type       = "scatter3d",
      mode       = "markers",
      x          = clean_data[[x_col]],
      y          = clean_data[[y_col]],
      z          = clean_data[[z_col]],
      name       = legend_title,
      marker     = list(
        size        = max(2, params$styling$point$size) * 2,
        color       = grad_vals,
        colorscale  = cs_list,
        cmin        = val_min,
        cmax        = val_max,
        colorbar    = list(title = list(text = legend_title)),
        showscale   = TRUE,
        opacity     = 0.85
      ),
      text       = hover_text,
      hoverinfo  = "text",
      showlegend = FALSE
    )

  } else if (!is.null(group_col) && group_col %in% colnames(clean_data) && !is.null(params$group_vals)) {
    # Per-group scatter3d traces
    point_colors <- .resolve_group_vector(
      params$styling$point$color,
      params$group_vals,
      function(n) if (requireNamespace("scales", quietly = TRUE)) scales::hue_pal()(n) else rep("#1f77b4", n)
    )

    for (i in seq_along(params$group_vals)) {
      gv <- params$group_vals[i]
      gdata <- clean_data[clean_data[[group_col]] == gv, , drop = FALSE]
      if (nrow(gdata) == 0) next

      hover_text <- if (!is.null(id_col)) {
        paste0("ID: ", gdata[[id_col]], "<br>",
               x_col, ": ", round(gdata[[x_col]], 3), "<br>",
               y_col, ": ", round(gdata[[y_col]], 3), "<br>",
               z_col, ": ", round(gdata[[z_col]], 3))
      } else {
        paste0(x_col, ": ", round(gdata[[x_col]], 3), "<br>",
               y_col, ": ", round(gdata[[y_col]], 3), "<br>",
               z_col, ": ", round(gdata[[z_col]], 3))
      }

      p <- plotly::add_trace(
        p,
        type      = "scatter3d",
        mode      = "markers",
        x         = gdata[[x_col]],
        y         = gdata[[y_col]],
        z         = gdata[[z_col]],
        name      = as.character(gv),
        marker    = list(
          size    = max(2, params$styling$point$size) * 2,
          color   = point_colors[i],
          opacity = 0.85
        ),
        text      = hover_text,
        hoverinfo = "text"
      )
    }
  } else {
    # Single scatter3d trace (no grouping)
    hover_text <- if (!is.null(id_col)) {
      paste0("ID: ", clean_data[[id_col]], "<br>",
             x_col, ": ", round(clean_data[[x_col]], 3), "<br>",
             y_col, ": ", round(clean_data[[y_col]], 3), "<br>",
             z_col, ": ", round(clean_data[[z_col]], 3))
    } else {
      paste0(x_col, ": ", round(clean_data[[x_col]], 3), "<br>",
             y_col, ": ", round(clean_data[[y_col]], 3), "<br>",
             z_col, ": ", round(clean_data[[z_col]], 3))
    }

    p <- plotly::add_trace(
      p,
      type      = "scatter3d",
      mode      = "markers",
      x         = clean_data[[x_col]],
      y         = clean_data[[y_col]],
      z         = clean_data[[z_col]],
      name      = "data",
      marker    = list(
        size    = max(2, params$styling$point$size) * 2,
        color   = params$styling$point$color[1],
        opacity = 0.85
      ),
      text      = hover_text,
      hoverinfo = "text"
    )
  }

  # Add 3D convex hull mesh if requested
  hulls_3d <- params$features$hulls_3d
  if (!is.null(hulls_3d) && isTRUE(hulls_3d$show)) {
    p <- .add_3d_hulls_to_plot(p, clean_data, x_col, y_col, z_col, group_col, params, verbose)
  }

  # Layout: axis titles and background
  p <- plotly::layout(
    p,
    scene = list(
      xaxis = list(title = x_col),
      yaxis = list(title = y_col),
      zaxis = list(title = z_col)
    ),
    paper_bgcolor = "white",
    legend = list(title = list(text = if (!is.null(group_col)) group_col else ""))
  )

  return(p)
}

#' Add per-group 3D convex hull mesh traces (mesh3d) to a plotly figure
#' @noRd
.add_3d_hulls_to_plot <- function(p, data, x_col, y_col, z_col, group_col, params, verbose) {

  if (!requireNamespace("geometry", quietly = TRUE)) {
    if (verbose) warning("Package 'geometry' is required for 3D convex hulls but is not installed.")
    return(p)
  }

  hulls_3d    <- params$features$hulls_3d
  opacity     <- if (!is.null(hulls_3d$opacity)) hulls_3d$opacity else 0.3
  fill        <- if (!is.null(hulls_3d$fill))    isTRUE(hulls_3d$fill) else TRUE
  wireframe   <- isTRUE(hulls_3d$wireframe)
  hull_type   <- if (!is.null(hulls_3d$hull_type)) hulls_3d$hull_type else "convex"
  alpha_value <- if (!is.null(hulls_3d$alpha_value)) hulls_3d$alpha_value else 1.0

  # Resolve per-group colors: use explicit hull colors if provided, else fall back to point palette
  hull_groups <- if (!is.null(group_col) &&
                      group_col %in% colnames(data) &&
                      !is.null(params$group_vals)) {
    # Filter to only the groups selected for 3D hull (mirrors 2D hull$groups logic)
    selected <- hulls_3d$groups
    if (!is.null(selected) && length(selected) > 0) {
      intersect(params$group_vals, as.character(selected))
    } else {
      params$group_vals
    }
  } else {
    NULL
  }

  group_colors <- if (!is.null(hull_groups)) {
    .resolve_group_vector(
      if (!is.null(hulls_3d$colors) && length(hulls_3d$colors) > 0) hulls_3d$colors
      else params$styling$point$color,
      hull_groups,
      function(n) if (requireNamespace("scales", quietly = TRUE)) scales::hue_pal()(n) else rep("#1f77b4", n)
    )
  } else {
    color_src <- if (!is.null(hulls_3d$colors) && length(hulls_3d$colors) > 0) hulls_3d$colors
                 else params$styling$point$color
    color_src[1]
  }

  # Inner helper: build and add mesh3d for one subset of points
  add_hull_for_subset <- function(pts, color, group_name) {
    if (nrow(pts) < 4) {
      if (verbose) warning("Group '", group_name, "' has < 4 points; skipping 3D hull.")
      return(invisible(NULL))
    }

    if (identical(hull_type, "alpha")) {
      # ---- Alpha hull via alphashape3d -----------------------------------
      if (!requireNamespace("alphashape3d", quietly = TRUE)) {
        if (verbose) warning(
          "Package 'alphashape3d' is required for 3D alpha hulls but is not installed. ",
          "Falling back to convex hull for group '", group_name, "'.")
        hull_type <<- "convex"   # fall back for remaining groups too
      } else {
        ash <- tryCatch(
          alphashape3d::ashape3d(pts, alpha = alpha_value),
          error = function(e) {
            if (verbose) warning(
              "3D alpha hull failed for group '", group_name, "': ", e$message)
            NULL
          }
        )
        if (is.null(ash)) return(invisible(NULL))

        # triang column 8: 2 = regular (boundary), 3 = singular (boundary)
        trimat      <- ash$triang
        on_surface  <- trimat[, 8L] %in% c(2L, 3L)
        surf_tri    <- trimat[on_surface, 1:3, drop = FALSE]

        if (nrow(surf_tri) == 0L) {
          if (verbose) warning(
            "Alpha hull for group '", group_name, "' has no surface triangles. ",
            "Try a larger alpha radius.")
          return(invisible(NULL))
        }

        pts_hull <- ash$x[, 1:3, drop = FALSE]   # ash$x cols 1-3 = xyz
        i_idx    <- surf_tri[, 1L] - 1L
        j_idx    <- surf_tri[, 2L] - 1L
        k_idx    <- surf_tri[, 3L] - 1L

        trace_args <- list(
          p           = p,
          type        = "mesh3d",
          x           = pts_hull[, 1L],
          y           = pts_hull[, 2L],
          z           = pts_hull[, 3L],
          i           = i_idx,
          j           = j_idx,
          k           = k_idx,
          opacity     = if (fill) opacity else 0.01,
          facecolor   = rep(color, nrow(surf_tri)),
          flatshading = TRUE,
          showscale   = FALSE,
          name        = paste0(group_name, " alpha hull"),
          showlegend  = TRUE,
          hoverinfo   = "skip"
        )
        if (wireframe) {
          trace_args$contour <- list(
            x = list(show = TRUE, color = color, width = 2),
            y = list(show = TRUE, color = color, width = 2),
            z = list(show = TRUE, color = color, width = 2)
          )
        }
        p <<- do.call(plotly::add_trace, trace_args)
        return(invisible(NULL))
      }
    }

    # ---- Convex hull (default) -----------------------------------------
    hull_faces <- tryCatch(
      geometry::convhulln(pts),
      error = function(e) {
        if (verbose) {
          warning("3D convex hull failed for group '", group_name, "': ", e$message)
        }
        NULL
      }
    )

    if (is.null(hull_faces)) return(invisible(NULL))

    # convhulln returns 1-indexed vertex indices; plotly mesh3d needs 0-indexed
    i_idx <- hull_faces[, 1L] - 1L
    j_idx <- hull_faces[, 2L] - 1L
    k_idx <- hull_faces[, 3L] - 1L

    trace_args <- list(
      p           = p,
      type        = "mesh3d",
      x           = pts[, 1],
      y           = pts[, 2],
      z           = pts[, 3],
      i           = i_idx,
      j           = j_idx,
      k           = k_idx,
      opacity     = if (fill) opacity else 0.01,
      # facecolor sets a uniform face color (raw plotly attribute, not remapped by plotly R)
      facecolor   = rep(color, nrow(hull_faces)),
      flatshading = TRUE,
      showscale   = FALSE,
      name        = paste0(group_name, " hull"),
      showlegend  = TRUE,
      hoverinfo   = "skip"
    )

    if (wireframe) {
      trace_args$contour <- list(
        x = list(show = TRUE, color = color, width = 2),
        y = list(show = TRUE, color = color, width = 2),
        z = list(show = TRUE, color = color, width = 2)
      )
    }

    p <<- do.call(plotly::add_trace, trace_args)
  }

  if (is.null(hull_groups)) {
    # Single hull for all data (no grouping)
    pts_df <- data[, c(x_col, y_col, z_col), drop = FALSE]
    pts_mat <- as.matrix(pts_df)
    pts_mat <- pts_mat[apply(pts_mat, 1, function(r) all(is.finite(r))), , drop = FALSE]
    add_hull_for_subset(pts_mat, group_colors[1], "all")
  } else {
    for (i in seq_along(hull_groups)) {
      gv    <- hull_groups[i]
      gdata <- data[data[[group_col]] == gv, c(x_col, y_col, z_col), drop = FALSE]
      pts_mat <- as.matrix(gdata)
      pts_mat <- pts_mat[apply(pts_mat, 1, function(r) all(is.finite(r))), , drop = FALSE]
      add_hull_for_subset(pts_mat, group_colors[i], as.character(gv))
    }
  }

  return(p)
}