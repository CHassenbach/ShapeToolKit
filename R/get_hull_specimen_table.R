#' Generate Hull Specimen Tables for Multiple Column Pairs
#'
#' Creates formatted tables showing specimens that form convex hulls for specified
#' groups and column pairs. Tables are automatically paginated and include clear
#' section headers for each group-column combination.
#'
#' @param data A data frame containing the analysis data.
#' @param x_cols Character vector of x-axis column names.
#' @param y_cols Character vector of y-axis column names. Must have same length as x_cols.
#' @param group_col Character string specifying the grouping column name.
#' @param group_vals Optional vector of specific group values to include. 
#'   If NULL, all unique groups are used.
#' @param table_options List containing table formatting options:
#'   \describe{
#'     \item{max_rows_per_page}{Maximum rows per page (default: 50)}
#'     \item{base_font_size}{Base font size (default: 10)}
#'     \item{min_font_size}{Minimum font size for large tables (default: 8)}
#'     \item{padding}{Cell padding in mm (default: c(1.2, 1.2))}
#'   }
#' @param output_options List containing output options:
#'   \describe{
#'     \item{include_all_columns}{Include all data columns (default: FALSE)}
#'     \item{id_column}{First column to include (default: auto-detect)}
#'     \item{show_empty_hulls}{Show entries for groups with no valid points (default: TRUE)}
#'   }
#' @param verbose Logical indicating whether to print progress messages. Default: TRUE.
#'
#' @return A list containing:
#'   \describe{
#'     \item{tables}{List of ggplot table objects (one per page)}
#'     \item{summary}{Data frame summarizing hull information}
#'     \item{metadata}{Analysis metadata}
#'   }
#'
#' @examples
#' # Basic hull table
#' test_data <- data.frame(
#'   ID = 1:20,
#'   PC1 = rnorm(20), PC2 = rnorm(20),
#'   PC3 = rnorm(20), PC4 = rnorm(20),
#'   species = sample(c("A", "B", "C"), 20, replace = TRUE)
#' )
#'
#' result <- get_hull_specimen_table(
#'   data = test_data,
#'   x_cols = c("PC1", "PC3"),
#'   y_cols = c("PC2", "PC4"),
#'   group_col = "species"
#' )
#'
#' # Custom formatting
#' result <- get_hull_specimen_table(
#'   data = test_data,
#'   x_cols = "PC1", y_cols = "PC2", 
#'   group_col = "species",
#'   table_options = list(
#'     max_rows_per_page = 30,
#'     base_font_size = 12
#'   ),
#'   output_options = list(
#'     include_all_columns = TRUE,
#'     show_empty_hulls = FALSE
#'   )
#' )
#'
#' @export
get_hull_specimen_table <- function(data,
                                   x_cols,
                                   y_cols,
                                   group_col,
                                   group_vals = NULL,
                                   table_options = list(),
                                   output_options = list(),
                                   verbose = TRUE) {
  
  # Input validation ----
  .validate_hull_table_inputs(data, x_cols, y_cols, group_col, group_vals, verbose)
  
  # Setup parameters ----
  params <- .setup_hull_table_params(table_options, output_options, verbose)
  
  # Setup group values ----
  if (is.null(group_vals)) {
    group_vals <- unique(data[[group_col]])
    group_vals <- group_vals[!is.na(group_vals)]
  }
  
  if (verbose) {
    message("Generating hull specimen tables...")
    message("Column pairs: ", length(x_cols))
    message("Groups: ", length(group_vals))
  }
  
  # Generate combined table data ----
  combined_data <- .generate_hull_table_data(data, x_cols, y_cols, group_col, group_vals, params, verbose)
  
  # Create summary statistics ----
  summary_stats <- .generate_hull_summary(combined_data, verbose)
  
  # Format and paginate tables ----
  tables <- .create_paginated_tables(combined_data, params, verbose)
  
  # Prepare metadata ----
  metadata <- list(
    n_column_pairs = length(x_cols),
    n_groups = length(group_vals),
    n_pages = length(tables),
    total_rows = nrow(combined_data),
    analysis_date = Sys.time()
  )
  
  if (verbose) message("Hull specimen tables completed!")
  
  structure(
    list(
      tables = tables,
      summary = summary_stats,
      metadata = metadata
    ),
    class = "hull_specimen_table_result"
  )
}

# Input Validation ----

#' Validate inputs for get_hull_specimen_table
#' @noRd
.validate_hull_table_inputs <- function(data, x_cols, y_cols, group_col, group_vals, verbose) {
  # Check data
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame", call. = FALSE)
  }
  if (nrow(data) == 0) {
    stop("'data' cannot be empty", call. = FALSE)
  }
  
  # Check columns
  if (!is.character(x_cols) || length(x_cols) == 0) {
    stop("'x_cols' must be a non-empty character vector", call. = FALSE)
  }
  if (!is.character(y_cols) || length(y_cols) == 0) {
    stop("'y_cols' must be a non-empty character vector", call. = FALSE)
  }
  if (length(x_cols) != length(y_cols)) {
    stop("'x_cols' and 'y_cols' must have the same length", call. = FALSE)
  }
  
  # Check column existence
  missing_cols <- c(x_cols, y_cols, group_col)[!c(x_cols, y_cols, group_col) %in% colnames(data)]
  if (length(missing_cols) > 0) {
    stop("The following columns do not exist in data: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  
  # Check group_col
  if (!is.character(group_col) || length(group_col) != 1) {
    stop("'group_col' must be a single character string", call. = FALSE)
  }
  
  # Check group_vals
  if (!is.null(group_vals)) {
    if (!all(group_vals %in% unique(data[[group_col]]))) {
      missing_vals <- group_vals[!group_vals %in% unique(data[[group_col]])]
      stop("The following group values do not exist: ", paste(missing_vals, collapse = ", "), call. = FALSE)
    }
  }
  
  # Check verbose
  if (!is.logical(verbose) || length(verbose) != 1) {
    stop("'verbose' must be a single logical value", call. = FALSE)
  }
}

# Parameter Setup ----

#' Setup parameters for hull table generation
#' @noRd
.setup_hull_table_params <- function(table_options, output_options, verbose) {
  
  # Table options defaults
  table_defaults <- list(
    max_rows_per_page = 50,
    base_font_size = 10,
    min_font_size = 8,
    padding = c(1.2, 1.2)
  )
  table_options <- utils::modifyList(table_defaults, table_options)
  
  # Output options defaults
  output_defaults <- list(
    include_all_columns = FALSE,
    id_column = NULL,
    show_empty_hulls = TRUE
  )
  output_options <- utils::modifyList(output_defaults, output_options)
  
  # Validate numeric parameters
  if (table_options$max_rows_per_page < 1) {
    stop("'max_rows_per_page' must be positive", call. = FALSE)
  }
  if (table_options$base_font_size < 1) {
    stop("'base_font_size' must be positive", call. = FALSE)
  }
  
  return(list(
    table_options = table_options,
    output_options = output_options
  ))
}

# Data Generation ----

#' Generate combined hull table data
#' @noRd
.generate_hull_table_data <- function(data, x_cols, y_cols, group_col, group_vals, params, verbose) {
  
  combined_data <- data.frame()
  
  # Determine columns to include
  if (params$output_options$include_all_columns) {
    cols_to_include <- colnames(data)
  } else {
    # Auto-detect ID column if not specified
    id_column <- if (is.null(params$output_options$id_column)) {
      colnames(data)[1]
    } else {
      params$output_options$id_column
    }
    cols_to_include <- unique(c(id_column, group_col, x_cols, y_cols))
  }
  
  # Process each column pair
  for (i in seq_along(x_cols)) {
    x_col <- x_cols[i]
    y_col <- y_cols[i]
    
    if (verbose) message("Processing column pair ", i, ": ", x_col, " vs ", y_col)
    
    # Process each group
    for (group_val in group_vals) {
      
      # Create title row
      title_text <- paste("Specimens for hull of group:", group_val, "for", x_col, "vs", y_col)
      title_row <- .create_title_row(title_text, cols_to_include)
      
      # Get hull data for this group and column pair
      hull_data <- .get_hull_data_for_group(data, group_val, x_col, y_col, group_col, 
                                           cols_to_include, params$output_options$show_empty_hulls)
      
      # Combine title and hull data
      combined_data <- rbind(combined_data, title_row, hull_data)
    }
  }
  
  return(combined_data)
}

#' Create title row for table
#' @noRd
.create_title_row <- function(title_text, cols_to_include) {
  title_row <- data.frame(matrix("", nrow = 1, ncol = length(cols_to_include)),
                         stringsAsFactors = FALSE)
  colnames(title_row) <- cols_to_include
  title_row[1, 1] <- title_text
  return(title_row)
}

#' Get hull data for a specific group and column pair
#' @noRd
.get_hull_data_for_group <- function(data, group_val, x_col, y_col, group_col, cols_to_include, show_empty_hulls) {
  
  # Filter data for the group
  group_data <- data[data[[group_col]] == group_val, , drop = FALSE]
  
  # Remove non-finite values
  valid_rows <- is.finite(group_data[[x_col]]) & is.finite(group_data[[y_col]])
  group_data <- group_data[valid_rows, , drop = FALSE]
  
  if (nrow(group_data) == 0) {
    # Handle empty data
    if (show_empty_hulls) {
      no_data_row <- data.frame(matrix("", nrow = 1, ncol = length(cols_to_include)),
                               stringsAsFactors = FALSE)
      colnames(no_data_row) <- cols_to_include
      no_data_row[1, 1] <- paste("No valid points for group:", group_val)
      return(no_data_row)
    } else {
      return(data.frame())
    }
  }
  
  if (nrow(group_data) >= 3) {
    # Calculate convex hull
    tryCatch({
      hull_indices <- grDevices::chull(group_data[[x_col]], group_data[[y_col]])
      hull_data <- group_data[hull_indices, cols_to_include, drop = FALSE]
    }, error = function(e) {
      # If hull calculation fails, return all points
      hull_data <- group_data[, cols_to_include, drop = FALSE]
    })
  } else {
    # Fewer than 3 points - include all
    hull_data <- group_data[, cols_to_include, drop = FALSE]
  }
  
  return(hull_data)
}

# Summary Generation ----

#' Generate summary statistics for hull tables
#' @noRd
.generate_hull_summary <- function(combined_data, verbose) {
  
  # Extract title rows to analyze
  title_rows <- combined_data[grepl("^Specimens for hull", combined_data[, 1]), , drop = FALSE]
  
  if (nrow(title_rows) == 0) {
    return(data.frame(
      metric = character(0),
      value = character(0)
    ))
  }
  
  # Parse group and column information from titles
  summary_info <- data.frame(
    total_sections = nrow(title_rows),
    total_rows = nrow(combined_data),
    data_rows = nrow(combined_data) - nrow(title_rows),
    stringsAsFactors = FALSE
  )
  
  # Convert to long format for display
  summary_stats <- data.frame(
    metric = c("Total sections", "Total rows", "Data rows", "Title rows"),
    value = c(summary_info$total_sections, summary_info$total_rows, 
              summary_info$data_rows, nrow(title_rows)),
    stringsAsFactors = FALSE
  )
  
  return(summary_stats)
}

# Table Creation ----

#' Create paginated tables
#' @noRd
.create_paginated_tables <- function(combined_data, params, verbose) {
  
  if (!requireNamespace("ggpubr", quietly = TRUE)) {
    if (verbose) warning("Package 'ggpubr' required for table formatting but not available")
    return(list(combined_data))  # Return raw data if ggpubr not available
  }
  
  # Calculate font size based on data size
  font_size <- .calculate_font_size(nrow(combined_data), params$table_options)
  
  # Create table theme
  table_theme <- ggpubr::ttheme(
    base_size = font_size,
    padding = grid::unit(params$table_options$padding, "mm")
  )
  
  # Split into pages if necessary
  max_rows <- params$table_options$max_rows_per_page
  if (nrow(combined_data) > max_rows) {
    pages <- split(combined_data, ceiling(seq_len(nrow(combined_data)) / max_rows))
    if (verbose) message("Data split into ", length(pages), " pages")
  } else {
    pages <- list(combined_data)
  }
  
  # Create tables
  tables <- lapply(seq_along(pages), function(i) {
    tryCatch({
      ggpubr::ggtexttable(
        pages[[i]],
        rows = NULL,
        theme = table_theme
      )
    }, error = function(e) {
      if (verbose) warning("Failed to create table for page ", i, ": ", e$message)
      return(pages[[i]])  # Return raw data on error
    })
  })
  
  return(tables)
}

#' Calculate appropriate font size for table
#' @noRd
.calculate_font_size <- function(n_rows, table_options) {
  if (n_rows <= table_options$max_rows_per_page) {
    return(table_options$base_font_size)
  } else {
    # Scale down font size for large tables
    scaled_size <- max(
      table_options$min_font_size,
      round(500 / n_rows)
    )
    return(scaled_size)
  }
}

# Print method ----

#' Print method for hull_specimen_table_result objects
#' @param x A hull_specimen_table_result object
#' @param ... Additional arguments (ignored)
#' @export
print.hull_specimen_table_result <- function(x, ...) {
  cat("Hull Specimen Table Results\n")
  cat("===========================\n\n")
  cat("Analysis metadata:\n")
  cat("  Column pairs:", x$metadata$n_column_pairs, "\n")
  cat("  Groups:", x$metadata$n_groups, "\n") 
  cat("  Pages:", x$metadata$n_pages, "\n")
  cat("  Total rows:", x$metadata$total_rows, "\n")
  cat("  Analysis date:", format(x$metadata$analysis_date), "\n\n")
  
  if (nrow(x$summary) > 0) {
    cat("Summary statistics:\n")
    for (i in seq_len(nrow(x$summary))) {
      cat("  ", x$summary$metric[i], ":", x$summary$value[i], "\n")
    }
  }
  
  cat("\nAvailable components:\n")
  cat("  - tables: List of", length(x$tables), "table objects\n")
  cat("  - summary: Summary statistics\n")
  cat("  - metadata: Analysis metadata\n")
}