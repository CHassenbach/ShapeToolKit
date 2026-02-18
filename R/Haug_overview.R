#' Generate an Overview of Hull, Contour, and Box Plots
#'
#' Creates a comprehensive multi-panel overview of scatter plots, contour plots, 
#' and boxplots for specified column pairs in the data. Hull plots and contour 
#' plots are generated for each pair of columns, with optional grouping and 
#' statistical visualization.
#'
#' @param data A data frame containing the data to be plotted.
#' @param cols A character vector specifying pairs of column names to plot. 
#'   Must contain an even number of column names (2, 4, 6, etc.).
#' @param group_col Optional character string specifying the grouping column name 
#'   for color and style customization.
#' @param group_vals Optional vector specifying which group values to display. 
#'   If NULL, all unique values in `group_col` are used.
#' @param colors Optional vector specifying colors for each group. If NULL, 
#'   colors are automatically generated.
#' @param point_style List containing point styling options:
#'   \describe{
#'     \item{color}{Point color (default: group colors)}
#'     \item{fill}{Point fill color (default: group colors)}  
#'     \item{shape}{Point shape (default: 21)}
#'     \item{size}{Point size (default: 2)}
#'   }
#' @param text_style List containing text styling options:
#'   \describe{
#'     \item{title_size}{Title font size (default: 24)}
#'     \item{label_size}{Axis label font size (default: 20)}
#'     \item{tick_size}{Tick label font size (default: 15)}
#'   }
#' @param plot_style Character string specifying plot style. Options: "Haug", 
#'   "inverted_Haug", "publication". Default: "Haug".
#' @param hull_options List containing hull plot options:
#'   \describe{
#'     \item{alpha}{Transparency for hulls (default: 0.3)}
#'     \item{show_all}{Show individual hull plots (default: FALSE)}
#'   }
#' @param contour_options List containing contour plot options:
#'   \describe{
#'     \item{linewidth}{Contour line width (default: 1)}
#'     \item{show_all}{Show individual contour plots (default: FALSE)}
#'   }
#' @param export_options List containing export options:
#'   \describe{
#'     \item{pdf}{Export as PDF (default: FALSE)}
#'     \item{filename}{PDF filename (default: "overview_plots.pdf")}
#'     \item{width}{Plot width in inches (default: 10)}
#'     \item{height}{Plot height in inches (default: 10)}
#'     \item{output_dir}{Output directory (default: current directory)}
#'   }
#' @param show_table Logical indicating whether to display hull specimen tables. 
#'   Default: FALSE.
#' @param verbose Logical indicating whether to print progress messages. 
#'   Default: TRUE.
#'
#' @return A list containing:
#'   \describe{
#'     \item{plots}{List of hull and contour plots for each column pair}
#'     \item{boxplot}{Combined boxplot comparing groups across all columns}
#'     \item{tables}{Hull specimen tables (if show_table = TRUE)}
#'     \item{metadata}{Analysis metadata including settings used}
#'   }
#'
#' @examples
#' # Basic usage
#' test_data <- data.frame(
#'   PC1 = rnorm(100), PC2 = rnorm(100),
#'   PC3 = rnorm(100), PC4 = rnorm(100),
#'   species = sample(c("A", "B", "C"), 100, replace = TRUE)
#' )
#' 
#' result <- Haug_overview(
#'   data = test_data, 
#'   cols = c("PC1", "PC2", "PC3", "PC4"),
#'   group_col = "species"
#' )
#'
#' # Advanced usage with custom styling
#' result <- Haug_overview(
#'   data = test_data,
#'   cols = c("PC1", "PC2", "PC3", "PC4"), 
#'   group_col = "species",
#'   colors = c("red", "blue", "green"),
#'   point_style = list(size = 3, shape = 16),
#'   hull_options = list(alpha = 0.2, show_all = TRUE),
#'   export_options = list(pdf = TRUE, filename = "my_overview.pdf")
#' )
#'
#' @export
Haug_overview <- function(data,
                         cols = c("PC1", "PC2"),
                         group_col = NULL,
                         group_vals = NULL,
                         colors = NULL,
                         point_style = list(),
                         text_style = list(),
                         plot_style = "Haug",
                         hull_options = list(),
                         contour_options = list(),
                         export_options = list(),
                         show_table = FALSE,
                         verbose = TRUE) {
  
  # Validate inputs ----
  .validate_haug_overview_inputs(data, cols, group_col, group_vals, colors, plot_style, verbose)
  
  # Setup default parameters ----
  params <- .setup_haug_overview_params(
    data, cols, group_col, group_vals, colors,
    point_style, text_style, hull_options, contour_options, export_options
  )
  # Persist top-level fields that helpers rely on
  params$cols <- cols
  params$group_col <- group_col
  params$plot_style <- plot_style
  
  if (verbose) {
    message("Starting Haug overview analysis...")
    message("- Column pairs: ", length(cols)/2)
    message("- Groups: ", ifelse(is.null(group_col), "None", length(params$group_vals)))
  }
  
  # Generate plots for each column pair ----
  all_plots <- .generate_all_column_pair_plots(data, params, verbose)
  
  # Create combined boxplot ----
  if (verbose) message("Creating combined boxplot...")
  boxplot <- .create_combined_boxplot(data, cols, group_col, params)
  
  # Generate tables if requested ----
  tables <- if (show_table) {
    if (verbose) message("Generating hull specimen tables...")
    .generate_hull_tables(data, cols, group_col, params$group_vals)
  } else {
    NULL
  }
  
  # Export to PDF if requested ----
  if (params$export_options$pdf) {
    if (verbose) message("Exporting to PDF...")
    .export_overview_to_pdf(all_plots, boxplot, tables, params)
  }
  
  # Prepare results ----
  metadata <- list(
    n_column_pairs = length(cols) / 2,
    n_groups = length(params$group_vals),
    group_column = group_col,
    plot_style = plot_style,
    analysis_date = Sys.time()
  )
  
  if (verbose) message("Overview analysis completed!")
  
  structure(
    list(
      plots = all_plots,
      boxplot = boxplot,
      tables = tables,
      metadata = metadata
    ),
    class = "haug_overview_result"
  )
}

# Input Validation ----

#' Validate inputs for Haug_overview function
#' @noRd
.validate_haug_overview_inputs <- function(data, cols, group_col, group_vals, colors, plot_style, verbose) {
  # Check data
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame", call. = FALSE)
  }
  if (nrow(data) == 0) {
    stop("'data' cannot be empty", call. = FALSE)
  }
  
  # Check cols
  if (!is.character(cols) || length(cols) == 0) {
    stop("'cols' must be a non-empty character vector", call. = FALSE)
  }
  if (length(cols) %% 2 != 0) {
    stop("'cols' must contain an even number of column names (pairs)", call. = FALSE)
  }
  if (length(cols) > 30) {
    stop("'cols' cannot contain more than 30 columns (15 pairs)", call. = FALSE)
  }
  
  # Check that all columns exist
  missing_cols <- cols[!cols %in% colnames(data)]
  if (length(missing_cols) > 0) {
    stop("The following columns do not exist in data: ", paste(missing_cols, collapse = ", "), call. = FALSE)
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
      stop("The following group values do not exist: ", paste(missing_vals, collapse = ", "), call. = FALSE)
    }
  }
  
  # Check plot_style
  valid_styles <- c("Haug", "inverted_Haug", "publication")
  if (!plot_style %in% valid_styles) {
    stop("'plot_style' must be one of: ", paste(valid_styles, collapse = ", "), call. = FALSE)
  }
  
  # Check verbose
  if (!is.logical(verbose) || length(verbose) != 1) {
    stop("'verbose' must be a single logical value", call. = FALSE)
  }
}

# Parameter Setup ----

#' Setup default parameters for Haug_overview
#' @noRd
.setup_haug_overview_params <- function(data, cols, group_col, group_vals, colors,
                                       point_style, text_style, hull_options, 
                                       contour_options, export_options) {
  
  # Setup group values
  if (!is.null(group_col) && is.null(group_vals)) {
    group_vals <- unique(data[[group_col]])
  }
  
  # Setup colors
  if (is.null(colors) && !is.null(group_col)) {
    n_groups <- length(group_vals)
    colors <- scales::hue_pal()(n_groups)
    # Name colors by group for reliable mapping
    if (!is.null(group_vals) && length(group_vals) == length(colors)) {
      names(colors) <- as.character(group_vals)
    }
  } else if (is.null(colors)) {
    colors <- c("#1f77b4")  # Default blue
  } else {
    # If user provided colors and group_vals present, try to set names when lengths match
    if (!is.null(group_vals) && length(colors) == length(group_vals) && is.null(names(colors))) {
      names(colors) <- as.character(group_vals)
    }
  }
  
  # Setup point style defaults
  point_defaults <- list(
    color = colors,
    fill = colors,
    shape = 21,
    size = 2
  )
  point_style <- utils::modifyList(point_defaults, point_style)
  # Ensure point colors are named for group mapping
  if (!is.null(group_vals)) {
    if (!is.null(point_style$color) && length(point_style$color) == length(group_vals) && is.null(names(point_style$color))) {
      names(point_style$color) <- as.character(group_vals)
    }
    if (!is.null(point_style$fill) && length(point_style$fill) == length(group_vals) && is.null(names(point_style$fill))) {
      names(point_style$fill) <- as.character(group_vals)
    }
  }
  
  # Setup text style defaults
  text_defaults <- list(
    title_size = 24,
    label_size = 20,
    tick_size = 15
  )
  text_style <- utils::modifyList(text_defaults, text_style)
  
  # Setup hull options defaults
  hull_defaults <- list(
    alpha = 0.3,
    show_all = FALSE
  )
  hull_options <- utils::modifyList(hull_defaults, hull_options)
  
  # Setup contour options defaults
  contour_defaults <- list(
    linewidth = 1,
    show_all = FALSE
  )
  contour_options <- utils::modifyList(contour_defaults, contour_options)
  
  # Setup export options defaults
  export_defaults <- list(
    pdf = FALSE,
    filename = "overview_plots.pdf",
    width = 10,
    height = 10,
    output_dir = NULL
  )
  export_options <- utils::modifyList(export_defaults, export_options)
  
  return(list(
    group_vals = group_vals,
    colors = colors,
    point_style = point_style,
    text_style = text_style,
    hull_options = hull_options,
    contour_options = contour_options,
    export_options = export_options
  ))
}

# Plot Generation ----

#' Generate plots for all column pairs
#' @noRd
.generate_all_column_pair_plots <- function(data, params, verbose) {
  cols <- params$cols
  all_plots <- list()
  
  for (i in seq(1, length(cols), by = 2)) {
    col1 <- cols[i]
    col2 <- cols[i + 1]
    
    if (verbose) message("Processing column pair: ", col1, " vs ", col2)
    
    plots <- .generate_plots_for_column_pair(data, col1, col2, params)
    all_plots[[paste(col1, col2, sep = "_vs_")]] <- plots
  }
  
  return(all_plots)
}

#' Generate plots for a single column pair
#' @noRd
.generate_plots_for_column_pair <- function(data, col1, col2, params) {
  # Calculate dynamic adjustments based on data range
  x_range <- range(data[[col1]], na.rm = TRUE)
  y_range <- range(data[[col2]], na.rm = TRUE)
  
  # Calculate label adjustments
  x_label_adjust_y <- -0.02 * (y_range[2] - y_range[1])
  y_label_adjust_x <- -0.02 * (x_range[2] - x_range[1])
  tick_length <- 0.00001 * min(diff(x_range), diff(y_range))
  
  # Create main hull plot
  hull_plot_title <- paste("Hulls for", col1, "vs", col2)
  hull_plot <- .create_hull_plot(data, col1, col2, hull_plot_title, params, 
                                x_label_adjust_y, tick_length)
  
  # Create individual hull plots if requested
  individual_hull_plots <- if (params$hull_options$show_all) {
    .create_individual_hull_plots(data, col1, col2, params, x_label_adjust_y, tick_length)
  } else {
    NULL
  }
  
  # Create contour plots if requested  
  contour_plots <- if (params$contour_options$show_all) {
    .create_contour_plots(data, col1, col2, params, x_label_adjust_y, y_label_adjust_x, tick_length)
  } else {
    NULL
  }
  
  return(list(
    hull_plot = hull_plot,
    individual_hull_plots = individual_hull_plots,
    contour_plots = contour_plots
  ))
}

#' Create main hull plot
#' @noRd
.create_hull_plot <- function(data, col1, col2, title, params, x_label_adjust_y, tick_length) {
  tryCatch({
    styling <- list(
      plot_style = params$plot_style,
      point = list(
        color = params$point_style$color,
        fill = params$point_style$fill,
        shape = params$point_style$shape,
        size = params$point_style$size
      ),
      text = list(
        title_size = params$text_style$title_size,
        label_size = params$text_style$label_size,
        tick_size = params$text_style$tick_size
      ),
      axis = list(
        linewidth = 1,
        tick_length = tick_length,
        tick_margin = 0.05,
        central_axes = TRUE
      )
    )
    # Ensure hull fill colors are a named vector keyed by group values
    hull_fill <- params$colors
    if (!is.null(params$group_vals) && !is.null(hull_fill) && is.null(names(hull_fill)) && length(hull_fill) == length(params$group_vals)) {
      names(hull_fill) <- as.character(params$group_vals)
    }
    features <- list(
      hulls = list(show = TRUE, groups = params$group_vals, fill = hull_fill, alpha = params$hull_options$alpha),
      contours = list(show = FALSE),
      shapes = list(show = FALSE)
    )
    labels <- list(
      title = title,
      x_label = col1,
      y_label = col2,
      x_adjust = c(0, x_label_adjust_y),
      y_adjust = c(0, 0)
    )
    shape_plot(
      data = data,
      x_col = col1,
      y_col = col2,
      group_col = params$group_col,
      group_vals = params$group_vals,
      styling = styling,
      features = features,
      labels = labels,
      export_options = list(export = FALSE),
      verbose = TRUE
    )
  }, error = function(e) {
    warning("Failed to create hull plot: ", e$message)
    return(NULL)
  })
}

#' Create individual hull plots for each group
#' @noRd
.create_individual_hull_plots <- function(data, col1, col2, params, x_label_adjust_y, tick_length) {
  if (is.null(params$group_vals)) return(NULL)
  
  lapply(params$group_vals, function(group) {
    plot_title <- paste("Hull for Group", group, "(", col1, "vs", col2, ")")
    tryCatch({
      styling <- list(
        plot_style = params$plot_style,
        point = list(
          color = params$point_style$color,
          fill = params$point_style$fill,
          shape = params$point_style$shape,
          size = params$point_style$size
        ),
        text = list(
          title_size = params$text_style$title_size,
          label_size = params$text_style$label_size,
          tick_size = params$text_style$tick_size
        ),
        axis = list(
          linewidth = 1,
          tick_length = tick_length,
          tick_margin = 0.05,
          central_axes = TRUE
        )
      )
      # Ensure per-group hull fill resolves to the group's own color
      hull_fill <- params$colors
      if (!is.null(params$group_vals) && !is.null(hull_fill) && is.null(names(hull_fill)) && length(hull_fill) == length(params$group_vals)) {
        names(hull_fill) <- as.character(params$group_vals)
      }
      features <- list(
        hulls = list(show = TRUE, groups = group, fill = hull_fill, alpha = params$hull_options$alpha),
        contours = list(show = FALSE),
        shapes = list(show = FALSE)
      )
      labels <- list(
        title = plot_title,
        x_label = col1,
        y_label = col2,
        x_adjust = c(0, x_label_adjust_y),
        y_adjust = c(0, 0)
      )
      shape_plot(
        data = data,
        x_col = col1,
        y_col = col2,
        group_col = params$group_col,
        group_vals = group,
        styling = styling,
        features = features,
        labels = labels,
        export_options = list(export = FALSE),
        verbose = TRUE
      )
    }, error = function(e) {
      warning("Failed to create individual hull plot for group ", group, ": ", e$message)
      return(NULL)
    })
  })
}

#' Create contour plots for each group
#' @noRd
.create_contour_plots <- function(data, col1, col2, params, x_label_adjust_y, y_label_adjust_x, tick_length) {
  if (is.null(params$group_vals)) return(NULL)
  
  # Calculate sample counts (base R to avoid NSE deps)
  if (!is.null(params$group_col) && params$group_col %in% names(data)) {
    df_counts <- data[data[[params$group_col]] %in% params$group_vals, , drop = FALSE]
    tab <- as.data.frame(table(df_counts[[params$group_col]]), stringsAsFactors = FALSE)
    names(tab) <- c("group", "count")
    sample_counts <- tab
  } else {
    sample_counts <- data.frame(group = NA, count = nrow(data))
  }
  
  lapply(params$group_vals, function(group) {
    count <- sample_counts$count[sample_counts$group == group]
    count <- if(length(count) == 0) 0 else count
    
    plot_title <- paste("Contours for Group", group, "(", col1, "vs", col2, ", n =", count, ")")
    
    tryCatch({
      styling <- list(
        plot_style = params$plot_style,
        point = list(
          color = params$point_style$color,
          fill = params$point_style$fill,
          shape = params$point_style$shape,
          size = params$point_style$size
        ),
        text = list(
          title_size = params$text_style$title_size,
          label_size = params$text_style$label_size,
          tick_size = params$text_style$tick_size
        ),
        axis = list(
          linewidth = 1,
          tick_length = tick_length,
          tick_margin = 0.05,
          central_axes = TRUE
        )
      )
      # Ensure contour colors are named by group for proper mapping
      contour_cols <- params$colors
      if (!is.null(params$group_vals) && !is.null(contour_cols) && is.null(names(contour_cols)) && length(contour_cols) == length(params$group_vals)) {
        names(contour_cols) <- as.character(params$group_vals)
      }
      features <- list(
        hulls = list(show = FALSE),
        contours = list(show = TRUE, groups = group, colors = contour_cols, linewidth = params$contour_options$linewidth),
        shapes = list(show = FALSE)
      )
      labels <- list(
        title = plot_title,
        x_label = col1,
        y_label = col2,
        x_adjust = c(0, x_label_adjust_y),
        y_adjust = c(y_label_adjust_x, 0)
      )
      shape_plot(
        data = data,
        x_col = col1,
        y_col = col2,
        group_col = params$group_col,
        group_vals = params$group_vals,
        styling = styling,
        features = features,
        labels = labels,
        export_options = list(export = FALSE),
        verbose = TRUE
      )
    }, error = function(e) {
      warning("Failed to create contour plot for group ", group, ": ", e$message)
      return(NULL)
    })
  })
}

# Boxplot Creation ----

#' Create combined boxplot
#' @noRd
.create_combined_boxplot <- function(data, cols, group_col, params) {
  if (is.null(group_col)) {
    # Simple boxplot without grouping
    data_long <- data %>%
      dplyr::select(dplyr::all_of(cols)) %>%
      tidyr::pivot_longer(cols = dplyr::all_of(cols), names_to = "column", values_to = "value")
    
    ggplot2::ggplot(data_long, ggplot2::aes(x = column, y = value)) +
      ggplot2::geom_boxplot(fill = params$colors[1]) +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Distribution Across Columns",
        x = "Columns", 
        y = "Values"
      ) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = params$text_style$title_size),
        axis.title = ggplot2::element_text(size = params$text_style$label_size),
        axis.text = ggplot2::element_text(size = params$text_style$tick_size)
      )
  } else {
    # Grouped boxplot
    data_long <- data %>%
      dplyr::select(dplyr::all_of(c(group_col, cols))) %>%
      tidyr::pivot_longer(cols = dplyr::all_of(cols), names_to = "column", values_to = "value")
    
    ggplot2::ggplot(data_long, ggplot2::aes_string(x = "column", y = "value", fill = group_col)) +
      ggplot2::geom_boxplot() +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Comparison of Groups Across Columns",
        x = "Columns", 
        y = "Values",
        fill = group_col
      ) +
      ggplot2::scale_fill_manual(values = params$colors) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = params$text_style$title_size),
        axis.title = ggplot2::element_text(size = params$text_style$label_size),
        axis.text = ggplot2::element_text(size = params$text_style$tick_size)
      )
  }
}

# Table Generation ----

#' Generate hull specimen tables
#' @noRd
.generate_hull_tables <- function(data, cols, group_col, group_vals) {
  tables <- list()
  
  for (i in seq(1, length(cols), by = 2)) {
    col1 <- cols[i]
    col2 <- cols[i + 1]
    
    tryCatch({
      table <- get_hull_specimen_table(
        data = data,
        x_cols = col1,
        y_cols = col2,
        group_col = group_col,
        group_vals = group_vals
      )
      tables[[paste(col1, col2, sep = "_vs_")]] <- table
    }, error = function(e) {
      warning("Failed to create table for ", col1, " vs ", col2, ": ", e$message)
    })
  }
  
  return(tables)
}

# PDF Export ----

#' Export overview to PDF
#' @noRd
.export_overview_to_pdf <- function(all_plots, boxplot, tables, params) {
  # Setup output path
  if (!is.null(params$export_options$output_dir)) {
    if (!dir.exists(params$export_options$output_dir)) {
      stop("Output directory does not exist: ", params$export_options$output_dir, call. = FALSE)
    }
    pdf_path <- file.path(params$export_options$output_dir, params$export_options$filename)
  } else {
    pdf_path <- params$export_options$filename
  }
  
  tryCatch({
    pdf(
      file = pdf_path, 
      width = params$export_options$width, 
      height = params$export_options$height
    )
    
    # Print hull plots
    hull_plots <- lapply(all_plots, function(plot_set) plot_set$hull_plot)
    hull_plots <- hull_plots[!sapply(hull_plots, is.null)]  # Remove NULL plots
    
    if (length(hull_plots) > 0) {
      hull_pages <- split(hull_plots, ceiling(seq_along(hull_plots) / 2))
      for (page in hull_pages) {
        if (requireNamespace("patchwork", quietly = TRUE)) {
          print(patchwork::wrap_plots(page, ncol = 1))
        } else {
          # Fallback if patchwork not available
          for (plot in page) {
            if (!is.null(plot)) print(plot)
          }
        }
      }
    }
    
    # Print individual hull plots if requested
    if (params$hull_options$show_all) {
      for (plot_set in all_plots) {
        if (!is.null(plot_set$individual_hull_plots)) {
          individual_plots <- plot_set$individual_hull_plots
          individual_plots <- individual_plots[!sapply(individual_plots, is.null)]
          
          if (length(individual_plots) > 0) {
            individual_pages <- split(individual_plots, ceiling(seq_along(individual_plots) / 2))
            for (page in individual_pages) {
              if (requireNamespace("patchwork", quietly = TRUE)) {
                print(patchwork::wrap_plots(page, ncol = 1))
              } else {
                for (plot in page) {
                  if (!is.null(plot)) print(plot)
                }
              }
            }
          }
        }
      }
    }
    
    # Print contour plots if requested
    if (params$contour_options$show_all) {
      for (plot_set in all_plots) {
        if (!is.null(plot_set$contour_plots)) {
          contour_plots <- plot_set$contour_plots
          contour_plots <- contour_plots[!sapply(contour_plots, is.null)]
          
          if (length(contour_plots) > 0) {
            contour_pages <- split(contour_plots, ceiling(seq_along(contour_plots) / 2))
            for (page in contour_pages) {
              if (requireNamespace("patchwork", quietly = TRUE)) {
                print(patchwork::wrap_plots(page, ncol = 1))
              } else {
                for (plot in page) {
                  if (!is.null(plot)) print(plot)
                }
              }
            }
          }
        }
      }
    }
    
    # Print boxplot
    if (!is.null(boxplot)) {
      print(boxplot)
    }
    
    # Print tables if available
    if (!is.null(tables) && length(tables) > 0) {
        for (table_name in names(tables)) {
          tbl_obj <- tables[[table_name]]
          if (is.null(tbl_obj)) next
          # If this looks like a hull_specimen_table_result, print its pages
          if (inherits(tbl_obj, "hull_specimen_table_result") && !is.null(tbl_obj$tables)) {
            for (pg in tbl_obj$tables) {
              if (!is.null(pg)) print(pg)
            }
          } else {
            # Fallback: try to print the object as-is
            print(tbl_obj)
          }
        }
    }
    
    dev.off()
    message("Overview exported to: ", pdf_path)
    
  }, error = function(e) {
    if (dev.cur() > 1) dev.off()  # Close PDF device if open
    stop("Failed to export PDF: ", e$message, call. = FALSE)
  })
}

# Print method ----

#' Print method for haug_overview_result objects
#' @param x A haug_overview_result object
#' @param ... Additional arguments (ignored)
#' @export
print.haug_overview_result <- function(x, ...) {
  cat("Haug Overview Results\n")
  cat("=====================\n\n")
  cat("Analysis metadata:\n")
  cat("  Column pairs:", x$metadata$n_column_pairs, "\n")
  cat("  Groups:", x$metadata$n_groups, "\n")
  cat("  Group column:", ifelse(is.null(x$metadata$group_column), "None", x$metadata$group_column), "\n")
  cat("  Plot style:", x$metadata$plot_style, "\n")
  cat("  Analysis date:", format(x$metadata$analysis_date), "\n\n")
  
  cat("Available components:\n")
  cat("  - plots: List of", length(x$plots), "column pair plot sets\n")
  cat("  - boxplot: Combined boxplot\n")
  if (!is.null(x$tables)) {
    cat("  - tables: Hull specimen tables for", length(x$tables), "column pairs\n")
  }
}