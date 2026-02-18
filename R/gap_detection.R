# Suppress R CMD check notes for ggplot2 NSE
utils::globalVariables(c("x", "y", "certainty", "group"))

#' Detect Morphospace Gaps with Uncertainty Quantification
#'
#' @param pca_scores Data frame or matrix with PC scores
#' @param uncertainty Proportion of axis range for uncertainty radius
#' @param grid_resolution Number of grid cells along each axis
#' @param monte_carlo_iterations Number of Monte Carlo replicates
#' @param monte_carlo_iterations_bootstrap Optional number of Monte Carlo
#'   replicates to use within each bootstrap replicate. If NULL (default), uses
#'   the value of \\code{monte_carlo_iterations}.
#' @param bootstrap_iterations Number of bootstrap resamples
#' @param bootstrap_sample_size Optional subsample size for bootstrap iterations.
#'   If NULL (default), uses full dataset. Values <= 1 are treated as fractions
#'   (e.g., 0.5 = 50% of data). Values > 1 are treated as absolute counts.
#'   Useful for comparing datasets with different sample sizes by normalizing
#'   to the smallest dataset size.
#' @param bootstrap_progress_every Print a progress message every N bootstrap
#'   iterations when \\code{verbose = TRUE}. Also used for \\code{progress_callback}
#'   updates (e.g., in Shiny). Set to a larger number to reduce console output.
#' @param group_column Optional column name in \\code{pca_scores} used to filter
#'   specimens into groups for analysis. If NULL (default), uses all rows.
#' @param groups Optional vector of group values to include (matched against
#'   \\code{pca_scores[[group_column]]}). If NULL (default), includes all non-NA
#'   values in the chosen group column.
#' @param domain_reference Which data define the analysis domain (grid extent and
#'   hull/bounding box) when group filtering is used. Use \\code{"subset"} to
#'   compute the domain from the filtered data (default), or \\code{"all"} to
#'   compute the domain from the full unfiltered dataset while still computing
#'   occupancy/gaps from the filtered subset.
#' @param estimation_method Estimation strategy. \\code{"bootstrap_mc"} (default)
#'   integrates sampling and measurement uncertainty by running Monte Carlo
#'   perturbations within each bootstrap replicate and averaging gap probability
#'   across bootstraps. \\code{"two_stage"} uses the legacy two-step approach
#'   (Monte Carlo on full data, then bootstrap-based stability) for backward
#'   comparability.
#' @param certainty_thresholds Gap certainty thresholds for polygon extraction
#' @param pc_pairs Optional matrix specifying PC pairs to analyze
#' @param max_pcs Maximum PC to include in automatic pair generation
#' @param domain_mode Analysis domain. Use "hull" to constrain analysis to the
#'   (alpha/convex) hull of the observed points, or "full" to analyze the full
#'   rectangular morphospace defined by the (buffered) PC axis ranges.
#' @param hull_type Type of hull for domain constraint
#' @param alpha_value Alpha parameter for alphahull
#' @param hull_buffer Proportional buffer to add around hull
#' @param use_parallel Use parallel processing
#' @param n_cores Number of cores for parallel processing
#' @param uncertainty_type Type of uncertainty model
#' @param occupancy_method Method to determine cell occupancy
#' @param occupancy_radius Radius for occupancy detection
#' @param progress_callback Optional function for progress updates
#' @param verbose Print progress messages
#'
#' @return List of class morphospace_gaps with results
#' @export
#'
#' @details
#' The default method (\\code{estimation_method = "bootstrap_mc"}) integrates
#' measurement and sampling uncertainty in a single procedure:
#' the dataset is resampled (with replacement) B times and for each resample a
#' Monte Carlo perturbation procedure is run. The resulting per-cell gap
#' probability matrices are averaged across bootstrap replicates.
#'
#' The legacy method (\\code{estimation_method = "two_stage"}) retains the older
#' two-step computation (full-data Monte Carlo + bootstrap-based stability) and
#' combines them as \\code{gap_certainty = gap_probability * gap_stability}.
#'
#' @examples
#' \dontrun{
#' # Assuming pca_result from PCA() contains $x with PC scores
#' gaps <- detect_morphospace_gaps(
#'   pca_scores = pca_result$x,
#'   uncertainty = 0.05,
#'   grid_resolution = 150,
#'   monte_carlo_iterations = 100,
#'   bootstrap_iterations = 200,
#'   max_pcs = 3
#' )
#'
#' # View summary
#' print(gaps$summary_table)
#'
#' # Access results for PC1-PC2
#' pc1_pc2_gaps <- gaps$results$`PC1-PC2`
#' plot(pc1_pc2_gaps$gap_certainty)
#' }
#'
#' @export
detect_morphospace_gaps <- function(pca_scores,
                                    uncertainty = 0.05,
                                    grid_resolution = 150,
                                    monte_carlo_iterations = 100,
                                    monte_carlo_iterations_bootstrap = NULL,
                                    bootstrap_iterations = 200,
                                    bootstrap_sample_size = NULL,
                                    bootstrap_progress_every = 100,
                                    group_column = NULL,
                                    groups = NULL,
                                    domain_reference = c("subset", "all"),
                                    estimation_method = c("bootstrap_mc", "two_stage"),
                                    certainty_thresholds = c(0.80, 0.90, 0.95),
                                    pc_pairs = NULL,
                                    max_pcs = 4,
                                    domain_mode = c("hull", "full"),
                                    hull_type = "alpha",
                                    alpha_value = NULL,
                                    hull_buffer = 0.05,
                                    use_parallel = FALSE,
                                    n_cores = NULL,
                                    uncertainty_type = "gaussian",
                                    occupancy_method = "radius",
                                    occupancy_radius = 1.5,
                                    progress_callback = NULL,
                                    verbose = TRUE) {

  domain_mode <- match.arg(domain_mode)
  domain_reference <- match.arg(domain_reference)
  estimation_method <- match.arg(estimation_method)
  
  # Validate inputs
  if (!is.data.frame(pca_scores) && !is.matrix(pca_scores)) {
    stop("pca_scores must be a data frame or matrix")
  }
  
  # Convert to data frame if matrix
  if (is.matrix(pca_scores)) {
    pca_scores <- as.data.frame(pca_scores)
  }

  # Keep a copy of the full dataset for optional domain definition
  pca_scores_all <- pca_scores
  
  # Check for PC columns
  pc_cols <- grep("^PC[0-9]+$", colnames(pca_scores), value = TRUE)
  if (length(pc_cols) == 0) {
    stop("No columns matching pattern 'PC1', 'PC2', etc. found in pca_scores")
  }

  # Optional grouping filter (analysis points)
  if (!is.null(group_column) && nzchar(group_column)) {
    if (!group_column %in% colnames(pca_scores)) {
      stop(sprintf("group_column '%s' not found in pca_scores", group_column))
    }

    group_values <- pca_scores[[group_column]]

    if (!is.null(groups) && length(groups) > 0) {
      keep <- !is.na(group_values) & (group_values %in% groups)
    } else {
      # If a group column is selected but no groups specified, keep all non-NA
      keep <- !is.na(group_values)
    }

    pca_scores <- pca_scores[keep, , drop = FALSE]

    if (nrow(pca_scores) < 3) {
      stop("Group filtering resulted in fewer than 3 specimens; cannot run gap analysis")
    }

    if (verbose) {
      n_groups <- if (!is.null(groups) && length(groups) > 0) length(unique(groups)) else length(unique(group_values[!is.na(group_values)]))
      cat(sprintf("Filtering by group_column '%s': %d rows retained (%d group level(s))\n",
                  group_column, nrow(pca_scores), n_groups))
    }
  }
  
  # Extract numeric PC indices
  pc_indices <- as.integer(sub("PC", "", pc_cols))
  available_pcs <- sort(pc_indices)
  
  if (verbose) {
    cat(sprintf("Found %d PC columns: %s\n", 
                length(available_pcs), 
                paste(pc_cols, collapse = ", ")))
  }
  
  # Generate PC pairs if not provided
  if (is.null(pc_pairs)) {
    max_pcs <- min(max_pcs, max(available_pcs))
    valid_pcs <- available_pcs[available_pcs <= max_pcs]
    
    if (length(valid_pcs) < 2) {
      stop("Need at least 2 PCs for gap analysis")
    }
    
    # Generate all pairwise combinations
    pc_pairs <- t(combn(valid_pcs, 2))
    
    if (verbose) {
      cat(sprintf("Analyzing %d PC pairs (PC1 to PC%d)\n", 
                  nrow(pc_pairs), max_pcs))
    }
    
    if (!is.null(progress_callback)) {
      progress_callback(sprintf("Analyzing %d PC pairs...", nrow(pc_pairs)), 0)
    }
  } else {
    # Validate provided pairs
    if (!is.matrix(pc_pairs) || ncol(pc_pairs) != 2) {
      stop("pc_pairs must be a matrix with 2 columns")
    }
  }
  
  # Check required packages
  if (domain_mode == "hull" && hull_type == "alpha") {
    if (!requireNamespace("alphahull", quietly = TRUE)) {
      warning("Package 'alphahull' not available. Falling back to convex hull.")
      hull_type <- "convex"
    }
  }
  
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required. Install with: install.packages('sf')")
  }
  
  # Setup parallel processing if requested
  if (use_parallel) {
    if (!requireNamespace("parallel", quietly = TRUE)) {
      warning("Package 'parallel' not available. Using sequential processing.")
      use_parallel <- FALSE
    } else {
      if (is.null(n_cores)) {
        n_cores <- max(1, parallel::detectCores() - 1)
      }
      if (verbose) {
        cat(sprintf("Using parallel processing with %d cores\n", n_cores))
      }
    }
  }
  
  # Validate bootstrap_sample_size
  n_total_samples <- nrow(pca_scores)
  if (!is.null(bootstrap_sample_size)) {
    if (bootstrap_sample_size <= 0) {
      stop("bootstrap_sample_size must be positive")
    }
    # Support both fraction (0-1) and absolute count (>1)
    if (bootstrap_sample_size <= 1) {
      # Fraction mode
      actual_sample_size <- floor(n_total_samples * bootstrap_sample_size)
      if (actual_sample_size < 2) {
        stop(sprintf("bootstrap_sample_size fraction %.3f results in < 2 samples", bootstrap_sample_size))
      }
    } else {
      # Absolute count mode
      actual_sample_size <- floor(bootstrap_sample_size)
      if (actual_sample_size > n_total_samples) {
        warning(sprintf("bootstrap_sample_size (%d) exceeds dataset size (%d). Using full dataset.",
                       actual_sample_size, n_total_samples))
        actual_sample_size <- n_total_samples
      }
      if (actual_sample_size < 2) {
        stop("bootstrap_sample_size must be at least 2")
      }
    }
    if (verbose) {
      cat(sprintf("Bootstrap will resample %d specimens (%.1f%% of %d total)\n",
                  actual_sample_size, 100 * actual_sample_size / n_total_samples, n_total_samples))
    }
  } else {
    actual_sample_size <- n_total_samples
  }
  
  # Store parameters
  parameters <- list(
    uncertainty = uncertainty,
    grid_resolution = grid_resolution,
    monte_carlo_iterations = monte_carlo_iterations,
    monte_carlo_iterations_bootstrap = monte_carlo_iterations_bootstrap,
    bootstrap_iterations = bootstrap_iterations,
    bootstrap_sample_size = bootstrap_sample_size,
    bootstrap_progress_every = bootstrap_progress_every,
    bootstrap_actual_size = actual_sample_size,
    group_column = group_column,
    groups = groups,
    domain_reference = domain_reference,
    estimation_method = estimation_method,
    certainty_thresholds = certainty_thresholds,
    domain_mode = domain_mode,
    hull_type = hull_type,
    alpha_value = alpha_value,
    hull_buffer = hull_buffer,
    uncertainty_type = uncertainty_type,
    occupancy_method = occupancy_method,
    occupancy_radius = occupancy_radius,
    timestamp = Sys.time()
  )
  
  # Initialize results storage
  results <- list()
  summary_rows <- list()
  
  # Analyze each PC pair
  for (i in seq_len(nrow(pc_pairs))) {
    pc_x <- pc_pairs[i, 1]
    pc_y <- pc_pairs[i, 2]
    pair_name <- sprintf("PC%d-PC%d", pc_x, pc_y)
    
    # Construct column names
    col_x <- sprintf("PC%d", pc_x)
    col_y <- sprintf("PC%d", pc_y)
    
    if (verbose) {
      cat(sprintf("\n=== Analyzing %s (%d/%d) ===\n", 
                  pair_name, i, nrow(pc_pairs)))
    }
      
    if (!is.null(progress_callback)) {
      progress_callback(
        sprintf("PC%d-PC%d (%d/%d): Gap estimation", 
                pc_x, pc_y, i, nrow(pc_pairs)),
        0.8 / nrow(pc_pairs)
      )
    }
    
    if (!col_x %in% colnames(pca_scores) || !col_y %in% colnames(pca_scores)) {
      warning(sprintf("Columns %s or %s not found. Skipping %s", 
                     col_x, col_y, pair_name))
      next
    }
    
    points <- pca_scores[, c(col_x, col_y)]
    colnames(points) <- c("x", "y")
    points <- na.omit(points)

    domain_points <- if (identical(domain_reference, "all")) {
      dp <- pca_scores_all[, c(col_x, col_y)]
      colnames(dp) <- c("x", "y")
      na.omit(dp)
    } else {
      points
    }
    
    if (nrow(points) < 3) {
      warning(sprintf("Insufficient points for %s. Skipping.", pair_name))
      next
    }
    
    # Analyze this PC pair
    pair_result <- .analyze_pc_pair(
      points = points,
      domain_points = domain_points,
      uncertainty = uncertainty,
      grid_resolution = grid_resolution,
      monte_carlo_iterations = monte_carlo_iterations,
      monte_carlo_iterations_bootstrap = monte_carlo_iterations_bootstrap,
      bootstrap_iterations = bootstrap_iterations,
      bootstrap_sample_size = bootstrap_sample_size,
      bootstrap_progress_every = bootstrap_progress_every,
      certainty_thresholds = certainty_thresholds,
      domain_mode = domain_mode,
      hull_type = hull_type,
      alpha_value = alpha_value,
      hull_buffer = hull_buffer,
      uncertainty_type = uncertainty_type,
      occupancy_method = occupancy_method,
      occupancy_radius = occupancy_radius,
      use_parallel = use_parallel,
      n_cores = n_cores,
      verbose = verbose,
      estimation_method = estimation_method,
      progress_callback = progress_callback
    )
    
    # Store results
    results[[pair_name]] <- pair_result
    
    # Extract summary metrics
    if (!is.null(pair_result$gap_metrics) && nrow(pair_result$gap_metrics) > 0) {
      summary_rows[[pair_name]] <- cbind(
        pc_pair = pair_name,
        pc_x = pc_x,
        pc_y = pc_y,
        pair_result$gap_metrics
      )
    }
  }
  
  # Combine summary table
  if (length(summary_rows) > 0) {
    summary_table <- do.call(rbind, summary_rows)
    rownames(summary_table) <- NULL
  } else {
    summary_table <- data.frame()
  }
  
  # Prepare output
  output <- list(
    pc_pairs = pc_pairs,
    results = results,
    summary_table = summary_table,
    parameters = parameters
  )
  
  class(output) <- c("morphospace_gaps", "list")
  
  if (verbose) {
    cat("\n=== Gap Detection Complete ===\n")
    cat(sprintf("Analyzed %d PC pairs\n", length(results)))
    cat(sprintf("Detected %d gap regions\n", nrow(summary_table)))
  }
  
  if (!is.null(progress_callback)) {
    progress_callback("Analysis complete!", 0.05)
  }
  
  return(output)
}


#' Analyze Single PC Pair for Gaps
#'
#' Internal function to perform gap analysis on one PC pair
#'
#' @keywords internal
.analyze_pc_pair <- function(points,
                             domain_points = NULL,
                             uncertainty,
                             grid_resolution,
                             monte_carlo_iterations,
                             monte_carlo_iterations_bootstrap = NULL,
                             bootstrap_iterations,
                             bootstrap_sample_size = NULL,
                             bootstrap_progress_every = 100,
                             certainty_thresholds,
                             domain_mode,
                             hull_type,
                             alpha_value,
                             hull_buffer,
                             uncertainty_type,
                             occupancy_method,
                             occupancy_radius,
                             use_parallel,
                             n_cores,
                             verbose,
                             estimation_method = c("bootstrap_mc", "two_stage"),
                             progress_callback = NULL) {

  estimation_method <- match.arg(estimation_method)
  
  n_points <- nrow(points)
  domain_pts <- if (is.null(domain_points)) points else domain_points
  n_domain_points <- nrow(domain_pts)

  # Calculate axis ranges and uncertainty radii based on the analysis domain
  x_range <- range(domain_pts$x)
  y_range <- range(domain_pts$y)
  
  x_span <- diff(x_range)
  y_span <- diff(y_range)
  
  u_x <- uncertainty * x_span
  u_y <- uncertainty * y_span
  
  if (verbose) {
    cat(sprintf("  Points: %d (domain: %d) | X range: [%.3f, %.3f] | Y range: [%.3f, %.3f]\n",
                n_points, n_domain_points, x_range[1], x_range[2], y_range[1], y_range[2]))
    cat(sprintf("  Uncertainty: ±%.3f (X), ±%.3f (Y)\n", u_x, u_y))
  }
  
  # Create regular grid
  grid_obj <- .create_analysis_grid(
    x_range = x_range,
    y_range = y_range,
    resolution = grid_resolution,
    buffer = hull_buffer
  )
  
  grid_x <- grid_obj$grid_x
  grid_y <- grid_obj$grid_y
  cell_size <- grid_obj$cell_size

  # Define analysis domain and mask
  domain_mode <- match.arg(domain_mode, c("hull", "full"))

  if (domain_mode == "hull") {
    # Create analysis domain hull
    domain_hull <- .create_domain_hull(
      points = domain_pts,
      hull_type = hull_type,
      alpha_value = alpha_value,
      buffer = hull_buffer,
      verbose = verbose
    )

    # Filter grid cells within domain
    grid_centers <- expand.grid(x = grid_x, y = grid_y)
    grid_sf <- sf::st_as_sf(grid_centers, coords = c("x", "y"), crs = sf::st_crs(domain_hull))
    in_domain <- as.vector(sf::st_within(grid_sf, domain_hull, sparse = FALSE))
  } else {
    # Full morphospace: use the grid's rectangular extent as the domain
    xlim <- range(grid_x)
    ylim <- range(grid_y)
    bbox_coords <- matrix(
      c(
        xlim[1], ylim[1],
        xlim[2], ylim[1],
        xlim[2], ylim[2],
        xlim[1], ylim[2],
        xlim[1], ylim[1]
      ),
      ncol = 2,
      byrow = TRUE
    )
    bbox_poly <- sf::st_polygon(list(bbox_coords))
    domain_hull <- sf::st_sf(geometry = sf::st_sfc(bbox_poly), crs = NA)
    in_domain <- rep(TRUE, length(grid_x) * length(grid_y))
  }
  
  if (verbose) {
    cat(sprintf("  Grid: %d × %d = %d cells (%d within domain)\n",
                length(grid_x), length(grid_y), 
                length(grid_x) * length(grid_y), sum(in_domain)))
  }
  
  if (identical(estimation_method, "two_stage")) {
    # Legacy two-stage method
    if (verbose) {
      cat(sprintf("  Stage 1: Monte Carlo simulation (%d iterations)\n",
                  monte_carlo_iterations))
    }

    gap_probability <- .compute_gap_probability(
      points = points,
      grid_x = grid_x,
      grid_y = grid_y,
      u_x = u_x,
      u_y = u_y,
      in_domain = in_domain,
      monte_carlo_iterations = monte_carlo_iterations,
      uncertainty_type = uncertainty_type,
      occupancy_method = occupancy_method,
      occupancy_radius = occupancy_radius,
      cell_size = cell_size,
      use_parallel = use_parallel,
      n_cores = n_cores
    )

    if (verbose) {
      cat(sprintf("  Stage 2: Bootstrap resampling (%d iterations)\n",
                  bootstrap_iterations))
    }

    gap_stability <- .compute_gap_stability(
      points = points,
      grid_x = grid_x,
      grid_y = grid_y,
      u_x = u_x,
      u_y = u_y,
      in_domain = in_domain,
      bootstrap_iterations = bootstrap_iterations,
      bootstrap_sample_size = bootstrap_sample_size,
      monte_carlo_iterations = monte_carlo_iterations,
      monte_carlo_iterations_bootstrap = monte_carlo_iterations_bootstrap,
      bootstrap_progress_every = bootstrap_progress_every,
      uncertainty_type = uncertainty_type,
      occupancy_method = occupancy_method,
      occupancy_radius = occupancy_radius,
      cell_size = cell_size,
      use_parallel = use_parallel,
      n_cores = n_cores
    )

    gap_certainty <- gap_probability * gap_stability

  } else {
    # Unified bootstrap + Monte Carlo method (sampling + measurement uncertainty)
    if (verbose) {
      cat(sprintf("  Unified Bootstrap + Monte Carlo (%d bootstraps)\n", bootstrap_iterations))
    }

    mc_iter_boot <- if (is.null(monte_carlo_iterations_bootstrap)) {
      # Unified method default: use the full MC iterations specified by the user
      as.integer(monte_carlo_iterations)
    } else {
      as.integer(monte_carlo_iterations_bootstrap)
    }
    if (!is.finite(mc_iter_boot) || mc_iter_boot < 1) {
      stop("monte_carlo_iterations_bootstrap must be a positive integer")
    }

    if (verbose) {
      cat(sprintf("    Per-bootstrap MC iterations: %d\n", mc_iter_boot))
    }

    gap_probability <- .compute_gap_probability_bootstrap_mc(
      points = points,
      grid_x = grid_x,
      grid_y = grid_y,
      u_x = u_x,
      u_y = u_y,
      in_domain = in_domain,
      bootstrap_iterations = bootstrap_iterations,
      bootstrap_sample_size = bootstrap_sample_size,
      monte_carlo_iterations = mc_iter_boot,
      bootstrap_progress_every = bootstrap_progress_every,
      uncertainty_type = uncertainty_type,
      occupancy_method = occupancy_method,
      occupancy_radius = occupancy_radius,
      cell_size = cell_size,
      use_parallel = use_parallel,
      n_cores = n_cores,
      verbose = verbose,
      progress_callback = progress_callback
    )

    # Stability is not used in the unified estimator
    gap_stability <- NULL
    gap_certainty <- gap_probability
  }
  
  # Extract gap polygons at specified thresholds
  if (verbose) {
    cat("  Extracting gap polygons\n")
  }
  
  gap_results <- .extract_gap_polygons(
    gap_certainty = gap_certainty,
    grid_x = grid_x,
    grid_y = grid_y,
    in_domain = in_domain,
    certainty_thresholds = certainty_thresholds,
    domain_hull = domain_hull,
    points = points
  )
  
  # Compile results
  result <- list(
    grid_x = grid_x,
    grid_y = grid_y,
    gap_probability = gap_probability,
    gap_stability = gap_stability,
    gap_certainty = gap_certainty,
    gap_polygons = gap_results$polygons,
    gap_metrics = gap_results$metrics,
    domain_hull = domain_hull,
    cell_size = cell_size
  )
  
  return(result)
}


#' Create Domain Hull
#'
#' @keywords internal
.create_domain_hull <- function(points, hull_type, alpha_value, buffer, verbose) {
  
  if (hull_type == "alpha" && requireNamespace("alphahull", quietly = TRUE)) {
    
    # Determine alpha value if not provided
    if (is.null(alpha_value)) {
      # Auto-determine alpha: use a more inclusive approach
      # Calculate pairwise distances
      dists <- as.matrix(dist(points))
      diag(dists) <- NA
      
      # Strategy: Use maximum nearest neighbor distance (most inclusive)
      # This ensures even outlier points are included in the hull
      max_nn_dist <- max(apply(dists, 1, min, na.rm = TRUE))
      
      # Use 2x the max nearest neighbor distance for a generous hull
      # This captures the full distribution including sparse regions
      alpha_value <- 2 * max_nn_dist
      
      if (verbose) {
        cat(sprintf("  Alpha hull: auto alpha = %.4f (2x max nearest neighbor)\n", alpha_value))
      }
    }
    
    # Create alpha hull
    tryCatch({
      ahull <- alphahull::ashape(points, alpha = alpha_value)
      
      # Convert to sf polygon
      # Extract edges from alpha hull
      edges <- ahull$edges
      
      if (nrow(edges) == 0) {
        if (verbose) {
          cat("  Alpha hull failed (no edges). Using convex hull.\n")
        }
        hull_type <- "convex"
      } else {
        # Build polygon from edges (simplified approach)
        hull_coords <- unique(rbind(
          ahull$x[edges[, "ind1"], ],
          ahull$x[edges[, "ind2"], ]
        ))
        
        # Order points for polygon (use convex hull of alpha hull points)
        hull_idx <- grDevices::chull(hull_coords)
        hull_coords <- hull_coords[c(hull_idx, hull_idx[1]), ]
        
        hull_poly <- sf::st_polygon(list(hull_coords))
        hull_sf <- sf::st_sfc(hull_poly)
        hull_sf <- sf::st_sf(geometry = hull_sf, crs = NA)
      }
    }, error = function(e) {
      if (verbose) {
        cat(sprintf("  Alpha hull error: %s. Using convex hull.\n", e$message))
      }
      hull_type <<- "convex"
    })
  }
  
  # Fall back to convex hull
  if (hull_type == "convex" || !exists("hull_sf")) {
    hull_idx <- grDevices::chull(points$x, points$y)
    hull_coords <- as.matrix(points[c(hull_idx, hull_idx[1]), ])
    
    hull_poly <- sf::st_polygon(list(hull_coords))
    hull_sf <- sf::st_sfc(hull_poly)
    hull_sf <- sf::st_sf(geometry = hull_sf, crs = NA)
    
    if (verbose) {
      cat("  Using convex hull\n")
    }
  }
  
  # Apply buffer if requested
  if (buffer > 0) {
    # Calculate appropriate buffer distance
    x_span <- diff(range(points$x))
    y_span <- diff(range(points$y))
    buffer_dist <- buffer * mean(c(x_span, y_span))
    
    hull_sf <- sf::st_buffer(hull_sf, dist = buffer_dist)
    
    if (verbose) {
      cat(sprintf("  Applied %.1f%% buffer (%.3f units)\n", 
                  buffer * 100, buffer_dist))
    }
  }
  
  return(hull_sf)
}


#' Create Analysis Grid
#'
#' @keywords internal
.create_analysis_grid <- function(x_range, y_range, resolution, buffer) {
  
  x_span <- diff(x_range)
  y_span <- diff(y_range)
  
  # Expand range slightly for buffer
  x_range_buffered <- x_range + c(-1, 1) * buffer * x_span
  y_range_buffered <- y_range + c(-1, 1) * buffer * y_span
  
  # Create grid
  grid_x <- seq(x_range_buffered[1], x_range_buffered[2], length.out = resolution)
  grid_y <- seq(y_range_buffered[1], y_range_buffered[2], length.out = resolution)
  
  # Calculate cell size (for occupancy radius)
  cell_size <- mean(c(diff(grid_x)[1], diff(grid_y)[1]))
  
  list(
    grid_x = grid_x,
    grid_y = grid_y,
    cell_size = cell_size
  )
}


#' Compute Gap Probability via Monte Carlo
#'
#' @keywords internal
.compute_gap_probability <- function(points,
                                     grid_x,
                                     grid_y,
                                     u_x,
                                     u_y,
                                     in_domain,
                                     monte_carlo_iterations,
                                     uncertainty_type,
                                     occupancy_method,
                                     occupancy_radius,
                                     cell_size,
                                     use_parallel,
                                     n_cores) {
  
  n_grid_x <- length(grid_x)
  n_grid_y <- length(grid_y)
  
  # Initialize occupancy counter
  occupancy_count <- matrix(0, nrow = n_grid_x, ncol = n_grid_y)
  
  # Determine occupancy radius in actual units
  radius <- occupancy_radius * cell_size
  
  # Monte Carlo iterations
  if (use_parallel && requireNamespace("parallel", quietly = TRUE)) {
    # Parallel execution
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    
    # Export necessary objects
    parallel::clusterExport(cl, 
                           varlist = c("points", "u_x", "u_y", 
                                      "uncertainty_type", "grid_x", "grid_y",
                                      "occupancy_method", "radius"),
                           envir = environment())
    
    occupancy_matrices <- parallel::parLapply(cl, 1:monte_carlo_iterations, function(iter) {
      .monte_carlo_iteration(points, u_x, u_y, uncertainty_type,
                            grid_x, grid_y, occupancy_method, radius)
    })
    
    # Sum occupancy matrices
    for (m in occupancy_matrices) {
      occupancy_count <- occupancy_count + m
    }
    
  } else {
    # Sequential execution
    for (iter in 1:monte_carlo_iterations) {
      occupancy_matrix <- .monte_carlo_iteration(
        points, u_x, u_y, uncertainty_type,
        grid_x, grid_y, occupancy_method, radius
      )
      occupancy_count <- occupancy_count + occupancy_matrix
    }
  }
  
  # Calculate occupancy probability
  occupancy_prob <- occupancy_count / monte_carlo_iterations
  
  # Gap probability = 1 - occupancy probability
  gap_prob <- 1 - occupancy_prob
  
  # Mask cells outside domain
  gap_prob_matrix <- matrix(gap_prob, nrow = n_grid_x, ncol = n_grid_y)
  in_domain_matrix <- matrix(in_domain, nrow = n_grid_x, ncol = n_grid_y)
  gap_prob_matrix[!in_domain_matrix] <- NA
  
  return(gap_prob_matrix)
}


#' Single Monte Carlo Iteration
#'
#' @keywords internal
.monte_carlo_iteration <- function(points, u_x, u_y, uncertainty_type,
                                   grid_x, grid_y, occupancy_method, radius) {
  
  n_points <- nrow(points)
  n_grid_x <- length(grid_x)
  n_grid_y <- length(grid_y)
  
  # Perturb points according to uncertainty model
  if (uncertainty_type == "gaussian") {
    # Gaussian: σ chosen so ±u ≈ 95% interval (u = 1.96 * σ)
    sigma_x <- u_x / 1.96
    sigma_y <- u_y / 1.96
    
    perturbed_x <- points$x + rnorm(n_points, mean = 0, sd = sigma_x)
    perturbed_y <- points$y + rnorm(n_points, mean = 0, sd = sigma_y)
    
  } else if (uncertainty_type == "uniform") {
    # Uniform within ±u
    perturbed_x <- points$x + runif(n_points, min = -u_x, max = u_x)
    perturbed_y <- points$y + runif(n_points, min = -u_y, max = u_y)
    
  } else {
    stop("Unknown uncertainty_type. Use 'gaussian' or 'uniform'.")
  }
  
  perturbed_points <- data.frame(x = perturbed_x, y = perturbed_y)
  
  # Determine occupancy for each grid cell
  occupancy <- matrix(0, nrow = n_grid_x, ncol = n_grid_y)
  
  if (occupancy_method == "radius") {
    # For each grid cell, check if any point is within radius
    grid_centers <- expand.grid(x = grid_x, y = grid_y)
    
    for (j in seq_len(nrow(perturbed_points))) {
      # Calculate distances from this point to all grid centers
      dx <- grid_centers$x - perturbed_points$x[j]
      dy <- grid_centers$y - perturbed_points$y[j]
      dists <- sqrt(dx^2 + dy^2)
      
      # Mark cells within radius as occupied
      occupied_cells <- which(dists <= radius)
      if (length(occupied_cells) > 0) {
        # Convert linear index to matrix indices
        row_idx <- ((occupied_cells - 1) %% n_grid_x) + 1
        col_idx <- ((occupied_cells - 1) %/% n_grid_x) + 1
        
        for (k in seq_along(occupied_cells)) {
          occupancy[row_idx[k], col_idx[k]] <- 1
        }
      }
    }
    
  } else if (occupancy_method == "kde") {
    # Kernel density estimation approach
    if (requireNamespace("MASS", quietly = TRUE)) {
      kde <- MASS::kde2d(perturbed_points$x, perturbed_points$y,
                        n = c(n_grid_x, n_grid_y),
                        lims = c(range(grid_x), range(grid_y)))
      
      # Threshold: cells with density > 0 are occupied
      occupancy <- ifelse(kde$z > 0, 1, 0)
    } else {
      # Fall back to radius method
      warning("MASS package not available. Using radius method instead of KDE.")
      return(.monte_carlo_iteration(points, u_x, u_y, uncertainty_type,
                                    grid_x, grid_y, "radius", radius))
    }
  }
  
  return(occupancy)
}


#' Compute Gap Stability via Bootstrap
#'
#' @keywords internal
.compute_gap_stability <- function(points,
                                   grid_x,
                                   grid_y,
                                   u_x,
                                   u_y,
                                   in_domain,
                                   bootstrap_iterations,
                                   bootstrap_sample_size = NULL,
                                   monte_carlo_iterations,
                                   monte_carlo_iterations_bootstrap = NULL,
                                   bootstrap_progress_every = 100,
                                   uncertainty_type,
                                   occupancy_method,
                                   occupancy_radius,
                                   cell_size,
                                   use_parallel,
                                   n_cores) {
  
  n_grid_x <- length(grid_x)
  n_grid_y <- length(grid_y)
  n_points <- nrow(points)
  
  # Determine bootstrap sample size
  if (is.null(bootstrap_sample_size)) {
    boot_size <- n_points
  } else if (bootstrap_sample_size <= 1) {
    # Fraction mode
    boot_size <- floor(n_points * bootstrap_sample_size)
  } else {
    # Absolute count mode
    boot_size <- min(floor(bootstrap_sample_size), n_points)
  }
  
  # Initialize gap classification counter
  gap_count <- matrix(0, nrow = n_grid_x, ncol = n_grid_y)
  
  # Choose gap probability threshold for binary classification
  gap_threshold <- 0.5  # Cells with gap_prob > 0.5 are classified as gaps
  
  # Precompute bootstrap indices to keep RNG behavior stable
  boot_indices_list <- lapply(seq_len(bootstrap_iterations), function(i) {
    sample.int(n_points, size = boot_size, replace = TRUE)
  })

  mc_iter_boot <- if (is.null(monte_carlo_iterations_bootstrap)) {
    max(50, monte_carlo_iterations %/% 2)
  } else {
    as.integer(monte_carlo_iterations_bootstrap)
  }

  if (!is.finite(mc_iter_boot) || mc_iter_boot < 1) {
    stop("monte_carlo_iterations_bootstrap must be a positive integer")
  }

  progress_every <- as.integer(bootstrap_progress_every)
  if (!is.finite(progress_every) || progress_every < 1) {
    progress_every <- bootstrap_iterations
  }

  batch_starts <- seq(1L, bootstrap_iterations, by = progress_every)

  for (start_idx in batch_starts) {
    end_idx <- min(bootstrap_iterations, start_idx + progress_every - 1L)
    batch <- start_idx:end_idx

    for (b in batch) {
      boot_points <- points[boot_indices_list[[b]], , drop = FALSE]

      gap_prob_boot <- .compute_gap_probability(
        points = boot_points,
        grid_x = grid_x,
        grid_y = grid_y,
        u_x = u_x,
        u_y = u_y,
        in_domain = in_domain,
        monte_carlo_iterations = mc_iter_boot,
        uncertainty_type = uncertainty_type,
        occupancy_method = occupancy_method,
        occupancy_radius = occupancy_radius,
        cell_size = cell_size,
        use_parallel = FALSE,
        n_cores = 1
      )

      is_gap <- gap_prob_boot > gap_threshold
      is_gap[is.na(is_gap)] <- FALSE
      gap_count <- gap_count + is_gap
    }
  }
  
  # Calculate stability: proportion of bootstrap iterations where classified as gap
  gap_stability <- gap_count / bootstrap_iterations
  
  # Mask cells outside domain
  in_domain_matrix <- matrix(in_domain, nrow = n_grid_x, ncol = n_grid_y)
  gap_stability[!in_domain_matrix] <- NA
  
  return(gap_stability)
}


#' Compute Integrated Gap Probability via Bootstrap + Monte Carlo
#'
#' Unified estimator integrating sampling and measurement uncertainty:
#' for each bootstrap replicate, runs Monte Carlo perturbations and computes
#' a gap probability matrix; the final gap probability is the mean across
#' bootstrap replicates.
#'
#' @keywords internal
.compute_gap_probability_bootstrap_mc <- function(points,
                                                  grid_x,
                                                  grid_y,
                                                  u_x,
                                                  u_y,
                                                  in_domain,
                                                  bootstrap_iterations,
                                                  bootstrap_sample_size = NULL,
                                                  monte_carlo_iterations,
                                                  bootstrap_progress_every = 100,
                                                  uncertainty_type,
                                                  occupancy_method,
                                                  occupancy_radius,
                                                  cell_size,
                                                  use_parallel,
                                                  n_cores,
                                                  verbose = FALSE,
                                                  progress_callback = NULL) {

  n_grid_x <- length(grid_x)
  n_grid_y <- length(grid_y)
  n_points <- nrow(points)

  # Determine bootstrap sample size
  if (is.null(bootstrap_sample_size)) {
    boot_size <- n_points
  } else if (bootstrap_sample_size <= 1) {
    boot_size <- floor(n_points * bootstrap_sample_size)
  } else {
    boot_size <- min(floor(bootstrap_sample_size), n_points)
  }

  if (boot_size < 2) {
    stop("bootstrap_sample_size results in < 2 samples")
  }

  sum_gap_prob <- matrix(0, nrow = n_grid_x, ncol = n_grid_y)

  in_domain_matrix <- matrix(in_domain, nrow = n_grid_x, ncol = n_grid_y)

  # Precompute bootstrap indices for reproducibility and to avoid RNG issues in parallel
  boot_indices_list <- lapply(seq_len(bootstrap_iterations), function(i) {
    sample.int(n_points, size = boot_size, replace = TRUE)
  })

  progress_every <- as.integer(bootstrap_progress_every)
  if (!is.finite(progress_every) || progress_every < 1) {
    progress_every <- bootstrap_iterations
  }

  batch_starts <- seq(1L, bootstrap_iterations, by = progress_every)

  report_progress <- function(done) {
    if (isTRUE(verbose)) {
      cat(sprintf("    Bootstrap progress: %d/%d\n", done, bootstrap_iterations))
    }
    if (!is.null(progress_callback) && is.function(progress_callback)) {
      progress_callback(sprintf("Bootstrap %d/%d", done, bootstrap_iterations), 0)
    }
  }

  report_batch_start <- function(start_idx, end_idx) {
    if (isTRUE(verbose)) {
      cat(sprintf("    Running bootstraps %d-%d/%d\n", start_idx, end_idx, bootstrap_iterations))
      flush.console()
    }
    if (!is.null(progress_callback) && is.function(progress_callback)) {
      progress_callback(sprintf("Running bootstraps %d-%d/%d", start_idx, end_idx, bootstrap_iterations), 0)
    }
  }

  if (isTRUE(use_parallel) && requireNamespace("parallel", quietly = TRUE)) {
    if (is.null(n_cores) || !is.finite(n_cores) || n_cores < 1) {
      n_cores <- max(1, parallel::detectCores() - 1)
    }

    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)

    # Export required functions and objects
    parallel::clusterExport(
      cl,
      varlist = c(
        "points", "grid_x", "grid_y", "u_x", "u_y", "in_domain",
        "monte_carlo_iterations", "uncertainty_type", "occupancy_method",
        "occupancy_radius", "cell_size", "boot_indices_list",
        ".compute_gap_probability", ".monte_carlo_iteration"
      ),
      envir = environment()
    )

    for (start_idx in batch_starts) {
      end_idx <- min(bootstrap_iterations, start_idx + progress_every - 1L)
      batch <- start_idx:end_idx

      report_batch_start(start_idx, end_idx)

      gap_prob_list <- parallel::parLapply(cl, batch, function(b) {
        boot_points <- points[boot_indices_list[[b]], , drop = FALSE]

        .compute_gap_probability(
          points = boot_points,
          grid_x = grid_x,
          grid_y = grid_y,
          u_x = u_x,
          u_y = u_y,
          in_domain = in_domain,
          monte_carlo_iterations = monte_carlo_iterations,
          uncertainty_type = uncertainty_type,
          occupancy_method = occupancy_method,
          occupancy_radius = occupancy_radius,
          cell_size = cell_size,
          use_parallel = FALSE,
          n_cores = 1
        )
      })

      for (gap_prob_boot in gap_prob_list) {
        gap_prob_boot[!in_domain_matrix] <- 0
        gap_prob_boot[is.na(gap_prob_boot)] <- 0
        sum_gap_prob <- sum_gap_prob + gap_prob_boot
      }

      report_progress(end_idx)
    }
  } else {
    for (start_idx in batch_starts) {
      end_idx <- min(bootstrap_iterations, start_idx + progress_every - 1L)
      batch <- start_idx:end_idx

      report_batch_start(start_idx, end_idx)

      for (b in batch) {
        boot_points <- points[boot_indices_list[[b]], , drop = FALSE]

        gap_prob_boot <- .compute_gap_probability(
          points = boot_points,
          grid_x = grid_x,
          grid_y = grid_y,
          u_x = u_x,
          u_y = u_y,
          in_domain = in_domain,
          monte_carlo_iterations = monte_carlo_iterations,
          uncertainty_type = uncertainty_type,
          occupancy_method = occupancy_method,
          occupancy_radius = occupancy_radius,
          cell_size = cell_size,
          use_parallel = FALSE,
          n_cores = 1
        )

        gap_prob_boot[!in_domain_matrix] <- 0
        gap_prob_boot[is.na(gap_prob_boot)] <- 0
        sum_gap_prob <- sum_gap_prob + gap_prob_boot
      }

      report_progress(end_idx)
    }
  }

  gap_prob_mean <- sum_gap_prob / bootstrap_iterations
  gap_prob_mean[!in_domain_matrix] <- NA

  return(gap_prob_mean)
}


#' Extract Gap Polygons from Certainty Matrix
#'
#' @keywords internal
.extract_gap_polygons <- function(gap_certainty,
                                  grid_x,
                                  grid_y,
                                  in_domain,
                                  certainty_thresholds,
                                  domain_hull,
                                  points) {
  
  n_grid_x <- length(grid_x)
  n_grid_y <- length(grid_y)
  
  all_polygons <- list()
  all_metrics <- list()
  
  for (threshold in certainty_thresholds) {
    
    # Create binary mask for this threshold
    is_gap <- gap_certainty >= threshold
    is_gap[is.na(is_gap)] <- FALSE
    
    if (sum(is_gap) == 0) {
      next  # No gaps at this threshold
    }
    
    # Convert to raster-like structure for contouring
    # Create data frame of gap cells
    gap_cells <- which(is_gap, arr.ind = TRUE)
    
    if (nrow(gap_cells) == 0) {
      next
    }
    
    # Get coordinates of gap cells
    gap_coords <- data.frame(
      x = grid_x[gap_cells[, 1]],
      y = grid_y[gap_cells[, 2]],
      certainty = gap_certainty[gap_cells]
    )
    
    # Create point geometry
    gap_sf <- sf::st_as_sf(gap_coords, coords = c("x", "y"), crs = NA)
    
    # Buffer points to create polygons, then union
    cell_size <- mean(c(diff(grid_x)[1], diff(grid_y)[1]))
    buffer_dist <- cell_size * 0.5
    
    gap_buffered <- sf::st_buffer(gap_sf, dist = buffer_dist)
    gap_union <- sf::st_union(gap_buffered)
    
    # Cast to multipolygon then extract individual polygons
    gap_multi <- sf::st_cast(gap_union, "MULTIPOLYGON")
    gap_polys <- sf::st_cast(gap_multi, "POLYGON")
    
    # Convert to sf data frame
    gap_polys_sf <- sf::st_sf(geometry = gap_polys, crs = NA)
    
    # Intersect with domain hull
    gap_polys_sf <- sf::st_intersection(gap_polys_sf, domain_hull)
    
    if (nrow(gap_polys_sf) == 0) {
      next
    }
    
    # Smooth the polygons to remove grid artifacts
    # Use a tolerance based on grid cell size
    smooth_tolerance <- cell_size * 0.3
    gap_polys_sf <- sf::st_simplify(gap_polys_sf, dTolerance = smooth_tolerance, preserveTopology = TRUE)
    
    # Apply additional smoothing using a small buffer trick (erosion-dilation)
    # This helps smooth out the stepped edges
    smooth_buffer <- cell_size * 0.1
    gap_polys_sf <- sf::st_buffer(gap_polys_sf, dist = -smooth_buffer)  # Erode
    gap_polys_sf <- sf::st_buffer(gap_polys_sf, dist = smooth_buffer)   # Dilate back
    
    # Remove any empty geometries from the smoothing
    gap_polys_sf <- gap_polys_sf[!sf::st_is_empty(gap_polys_sf), ]
    
    if (nrow(gap_polys_sf) == 0) {
      next
    }
    
    # Calculate metrics for each gap polygon
    threshold_value <- as.numeric(unname(threshold))
    metrics <- data.frame(
      gap_id = seq_len(nrow(gap_polys_sf)),
      threshold = rep(threshold_value, nrow(gap_polys_sf)),
      area = as.numeric(sf::st_area(gap_polys_sf)),
      stringsAsFactors = FALSE
    )
    
    # Calculate mean and max certainty
    metrics$mean_certainty <- NA
    metrics$max_certainty <- NA
    
    for (i in seq_len(nrow(gap_polys_sf))) {
      # Find grid cells within this polygon
      poly <- gap_polys_sf[i, ]
      grid_centers <- expand.grid(x = grid_x, y = grid_y)
      grid_centers_sf <- sf::st_as_sf(grid_centers, coords = c("x", "y"), crs = NA)
      
      in_poly <- as.vector(sf::st_within(grid_centers_sf, poly, sparse = FALSE))
      
      if (sum(in_poly) > 0) {
        certainty_values <- gap_certainty[matrix(in_poly, nrow = n_grid_x, ncol = n_grid_y)]
        certainty_values <- certainty_values[!is.na(certainty_values)]
        
        if (length(certainty_values) > 0) {
          metrics$mean_certainty[i] <- mean(certainty_values)
          metrics$max_certainty[i] <- max(certainty_values)
        }
      }
    }
    
    # Calculate centroid
    centroids <- sf::st_centroid(gap_polys_sf)
    centroid_coords <- sf::st_coordinates(centroids)
    metrics$centroid_x <- centroid_coords[, 1]
    metrics$centroid_y <- centroid_coords[, 2]
    
    # Calculate gap depth (distance to nearest occupied point)
    metrics$gap_depth <- NA
    
    for (i in seq_len(nrow(gap_polys_sf))) {
      centroid <- c(metrics$centroid_x[i], metrics$centroid_y[i])
      dists <- sqrt((points$x - centroid[1])^2 + (points$y - centroid[2])^2)
      metrics$gap_depth[i] <- min(dists)
    }
    
    # Add threshold-specific results
    gap_polys_sf$threshold <- threshold
    all_polygons[[as.character(threshold)]] <- gap_polys_sf
    all_metrics[[as.character(threshold)]] <- metrics
  }
  
  # Combine results
  if (length(all_polygons) > 0) {
    polygons <- do.call(rbind, all_polygons)
    metrics <- do.call(rbind, all_metrics)
    rownames(metrics) <- NULL
  } else {
    polygons <- sf::st_sf(geometry = sf::st_sfc(), crs = NA)
    metrics <- data.frame()
  }
  
  list(
    polygons = polygons,
    metrics = metrics
  )
}


#' Print Method for morphospace_gaps
#'
#' @param x Object of class morphospace_gaps
#' @param ... Additional arguments (ignored)
#'
#' @export
print.morphospace_gaps <- function(x, ...) {
  cat("Morphospace Gap Detection Results\n")
  cat("==================================\n\n")
  
  cat(sprintf("PC Pairs Analyzed: %d\n", nrow(x$pc_pairs)))
  cat(sprintf("Total Gaps Detected: %d\n", nrow(x$summary_table)))
  
  cat("\nParameters:\n")
  cat(sprintf("  Uncertainty: %.1f%%\n", x$parameters$uncertainty * 100))
  cat(sprintf("  Grid Resolution: %d\n", x$parameters$grid_resolution))
  cat(sprintf("  Monte Carlo Iterations: %d\n", x$parameters$monte_carlo_iterations))
  cat(sprintf("  Bootstrap Iterations: %d\n", x$parameters$bootstrap_iterations))
  cat(sprintf("  Hull Type: %s\n", x$parameters$hull_type))
  
  if (nrow(x$summary_table) > 0) {
    cat("\nTop Gaps by Area:\n")
    top_gaps <- head(x$summary_table[order(-x$summary_table$area), ], 5)
    print(top_gaps[, c("pc_pair", "threshold", "area", "mean_certainty", "gap_depth")])
  }
  
  invisible(x)
}
