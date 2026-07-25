# Suppress R CMD check notes for NSE
utils::globalVariables(c("bin_id", "region_id", "cell_count", "hypervolume",
                          "mean_gap_prob", "occupancy_probability",
                          "gap_probability", "occupied"))

# ============================================================
# N-Dimensional Morphospace Gap Detection
# ============================================================

#' Detect Morphospace Gaps in N Dimensions
#'
#' Generalises the 2D/3D Monte Carlo + bootstrap gap-detection approach to an
#' arbitrary number of PC axes.  Instead of a heatmap, results are returned as
#' a data frame (one row per grid cell) with occupancy and connected-region
#' labels.
#'
#' @details
#' **Algorithm overview:**
#'
#' A hypercubic grid of \code{bins_per_axis^n_dims} cells is laid over the
#' observed PC-score range.  For each of the
#' \code{bootstrap_iterations * monte_carlo_iterations} replicate pairs:
#' \enumerate{
#'   \item Resample specimens with replacement (bootstrap).
#'   \item Jitter every specimen independently per axis by drawing from the
#'         chosen distribution with bandwidth
#'         \code{uncertainty * axis_range} per axis.
#'   \item Map each jittered point to its grid cell and mark that cell and all
#'         cells within \code{occupancy_radius} cell-widths (L2 norm in
#'         cell-index space) as occupied.
#' }
#' The fraction of replicates in which a cell was marked is its
#' \emph{occupancy probability}.  Cells with
#' \code{occupancy_probability >= occupancy_threshold} are classified as
#' occupied; the remainder are empty.  Connected empty regions are then labeled
#' with a BFS flood-fill.
#'
#' **Cell scaling:** total cells grow as \code{bins_per_axis^n_dims}.  Use
#' conservative bin counts for high dimensions (the default of 12 gives
#' ~249 K cells in 5D).
#'
#' **Shared settings:** the \code{uncertainty}, \code{monte_carlo_iterations},
#' \code{bootstrap_iterations}, \code{bootstrap_sample_size}, and
#' \code{uncertainty_type} parameters match those of
#' \code{\link{detect_morphospace_gaps}} so that results are directly
#' comparable.
#'
#' @param pca_scores Data frame or matrix with PC score columns named PC1, PC2,
#'   etc.
#' @param pc_axes Integer vector of PC axis indices to include.  NULL (default)
#'   uses all PCs up to \code{max_pcs}.
#' @param max_pcs Maximum PC index when \code{pc_axes = NULL}.  Default 5.
#' @param bins_per_axis Integer bins per axis.  Total cells =
#'   \code{bins_per_axis^n_dims}; keep conservative for high dimensions.
#'   Default 12.
#' @param uncertainty Proportion of each axis's observed range used as the MC
#'   jitter bandwidth (independently per axis, since PC axes have different
#'   scales).  Default 0.10.
#' @param monte_carlo_iterations Number of MC perturbation replicates per
#'   bootstrap replicate.  Default 100.
#' @param bootstrap_iterations Number of bootstrap resampling replicates.
#'   Default 200.
#' @param bootstrap_sample_size Optional subsample size per bootstrap replicate.
#'   NULL uses the full dataset.  Values <= 1 are fractions; values > 1 are
#'   absolute counts.  Matches the convention in
#'   \code{\link{detect_morphospace_gaps}}.
#' @param occupancy_radius Occupancy smoothing radius in cell-width units
#'   (L2 norm in integer cell-index space).  Default 1.5, matching the existing
#'   2D implementation.  Pre-computed neighbor offsets:
#'   \code{\{-floor(r),...,floor(r)\}^n_dims} filtered to L2 <= radius.
#' @param uncertainty_type Perturbation distribution: \code{"gaussian"}
#'   (default; sigma = bandwidth / 1.96) or \code{"uniform"} (draw from
#'   \code{[-bandwidth, +bandwidth]}).
#' @param domain_mode \code{"specimen_range"} (default) grids each axis from
#'   its observed min to max.  \code{"full_morphospace"} uses the same bounding
#'   box but explicitly includes all corner cells even if no specimen ever
#'   reached them.  Numerically equivalent for a uniform grid; the difference
#'   is conceptual and affects region labeling of peripheral cells.
#' @param occupancy_threshold Minimum occupancy probability for a cell to be
#'   classified as occupied (default 0.05).
#' @param connectivity Integer connectivity for connected-component labeling.
#'   1 = face-adjacency only (2*n_dims neighbors, default); 2 = face + edge
#'   (3^n_dims - 1 neighbors).
#' @param bootstrap_progress_every Report progress every N bootstrap iterations
#'   when \code{verbose = TRUE} or \code{progress_callback} is supplied.
#'   Default 20.
#' @param progress_callback Optional \code{function(message, increment)} for
#'   Shiny \code{withProgress} updates.
#' @param verbose Print progress messages to console.  Default TRUE.
#'
#' @return A list of class \code{morphospace_gaps_ndim} with elements:
#' \describe{
#'   \item{cell_df}{Data frame, one row per domain cell.  Columns:
#'     \code{bin_id} (integer linear index),
#'     \code{PC<k>_center} (bin-centre coordinate for each axis),
#'     \code{occupancy_probability},
#'     \code{gap_probability},
#'     \code{occupied} (logical),
#'     \code{region_id} (integer; -1 for occupied cells, otherwise the
#'       connected-empty-region label).}
#'   \item{region_summary}{Data frame with one row per connected empty region,
#'     sorted descending by \code{cell_count}: columns \code{region_id},
#'     \code{cell_count}, \code{hypervolume}, \code{mean_gap_prob}, and one
#'     centroid column per PC axis.}
#'   \item{grid_breaks}{Named list of break vectors, one per PC axis.}
#'   \item{grid_centers}{Named list of bin-centre vectors, one per PC axis.}
#'   \item{dims}{Integer vector of bin counts per axis (\code{rep(bins_per_axis, n_dims)}).}
#'   \item{parameters}{List of all analysis parameters.}
#' }
#'
#' @seealso \code{\link{detect_morphospace_gaps}} for the 2D implementation.
#'
#' @examples
#' \donttest{
#' set.seed(42)
#' n <- 80
#' scores <- data.frame(
#'   PC1 = c(rnorm(n/2, -2), rnorm(n/2, 2)),
#'   PC2 = rnorm(n),
#'   PC3 = rnorm(n),
#'   PC4 = rnorm(n),
#'   PC5 = rnorm(n)
#' )
#' result <- detect_morphospace_gaps_ndim(
#'   scores,
#'   bins_per_axis         = 8L,
#'   monte_carlo_iterations = 20L,
#'   bootstrap_iterations   = 20L,
#'   verbose                = FALSE
#' )
#' print(result)
#' }
#'
#' @export
detect_morphospace_gaps_ndim <- function(
    pca_scores,
    pc_axes                = NULL,
    max_pcs                = 5L,
    bins_per_axis          = 12L,
    uncertainty            = 0.10,
    monte_carlo_iterations = 100L,
    bootstrap_iterations   = 200L,
    bootstrap_sample_size  = NULL,
    occupancy_radius       = 1.5,
    uncertainty_type       = c("gaussian", "uniform"),
    domain_mode            = c("specimen_range", "full_morphospace"),
    occupancy_threshold    = 0.05,
    connectivity           = 1L,
    bootstrap_progress_every = 20L,
    progress_callback      = NULL,
    verbose                = TRUE) {

  uncertainty_type <- match.arg(uncertainty_type)
  domain_mode      <- match.arg(domain_mode)

  # ---- Input validation -----------------------------------------------
  if (!is.data.frame(pca_scores) && !is.matrix(pca_scores)) {
    stop("pca_scores must be a data frame or matrix")
  }
  if (is.matrix(pca_scores)) pca_scores <- as.data.frame(pca_scores)

  pc_cols <- grep("^PC[0-9]+$", colnames(pca_scores), value = TRUE)
  if (length(pc_cols) == 0L) {
    stop("No columns matching pattern 'PC1', 'PC2', ... found in pca_scores")
  }
  available_pc_idx <- sort(as.integer(sub("PC", "", pc_cols)))

  if (is.null(pc_axes)) {
    max_pcs <- min(as.integer(max_pcs), max(available_pc_idx))
    pc_axes  <- available_pc_idx[available_pc_idx <= max_pcs]
  } else {
    pc_axes <- sort(as.integer(pc_axes))
    missing <- setdiff(pc_axes, available_pc_idx)
    if (length(missing) > 0L) {
      stop(sprintf("Requested PC axes not found in pca_scores: %s",
                   paste(paste0("PC", missing), collapse = ", ")))
    }
  }

  n_dims <- length(pc_axes)
  if (n_dims < 2L) stop("Need at least 2 PC axes for gap analysis")

  bins_per_axis          <- as.integer(bins_per_axis)
  monte_carlo_iterations <- as.integer(monte_carlo_iterations)
  bootstrap_iterations   <- as.integer(bootstrap_iterations)
  connectivity           <- as.integer(connectivity)
  bootstrap_progress_every <- as.integer(bootstrap_progress_every)

  if (bins_per_axis < 2L)         stop("bins_per_axis must be >= 2")
  if (monte_carlo_iterations < 1L) stop("monte_carlo_iterations must be >= 1")
  if (bootstrap_iterations < 1L)   stop("bootstrap_iterations must be >= 1")
  if (occupancy_radius <= 0)       stop("occupancy_radius must be > 0")
  if (occupancy_threshold <= 0 || occupancy_threshold > 1) {
    stop("occupancy_threshold must be in (0, 1]")
  }
  if (!is.finite(bootstrap_progress_every) || bootstrap_progress_every < 1L) {
    bootstrap_progress_every <- bootstrap_iterations
  }

  # ---- Extract PC columns and remove NA rows --------------------------
  col_names  <- paste0("PC", pc_axes)
  points_mat <- as.matrix(pca_scores[, col_names, drop = FALSE])
  complete   <- complete.cases(points_mat)
  points_mat <- points_mat[complete, , drop = FALSE]
  colnames(points_mat) <- col_names

  n_specimens <- nrow(points_mat)
  if (n_specimens < 3L) stop("Need at least 3 complete specimens after NA removal")

  # ---- Bootstrap sample size ------------------------------------------
  if (!is.null(bootstrap_sample_size) && !is.na(bootstrap_sample_size)) {
    if (bootstrap_sample_size <= 0) stop("bootstrap_sample_size must be positive")
    boot_size <- if (bootstrap_sample_size <= 1) {
      max(2L, as.integer(floor(n_specimens * bootstrap_sample_size)))
    } else {
      min(as.integer(floor(bootstrap_sample_size)), n_specimens)
    }
  } else {
    boot_size <- n_specimens
  }

  # ---- Build per-axis grid --------------------------------------------
  axis_ranges <- apply(points_mat, 2L, range, na.rm = TRUE)  # 2 x n_dims
  axis_spans  <- axis_ranges[2L, ] - axis_ranges[1L, ]
  # Per-axis jitter bandwidth (matches the 2D implementation's scaling)
  uncertainty_bw <- uncertainty * axis_spans

  grid_breaks  <- vector("list", n_dims)
  grid_centers <- vector("list", n_dims)

  for (d in seq_len(n_dims)) {
    grid_breaks[[d]]  <- seq(axis_ranges[1L, d], axis_ranges[2L, d],
                              length.out = bins_per_axis + 1L)
    grid_centers[[d]] <- (grid_breaks[[d]][-1L] +
                            grid_breaks[[d]][-(bins_per_axis + 1L)]) / 2
  }
  names(grid_breaks)  <- col_names
  names(grid_centers) <- col_names

  # Per-axis cell sizes (width of each bin in data units)
  cell_sizes <- vapply(seq_len(n_dims),
    function(d) diff(grid_breaks[[d]])[1L],
    numeric(1L))
  cell_volume <- prod(cell_sizes)

  # ---- Precompute neighbor offsets ------------------------------------
  # Offsets are in cell-index units (integer).  Filtering: L2 norm <= radius.
  offsets_mat <- .ndim_build_neighbor_offsets(n_dims, occupancy_radius)
  n_offsets   <- nrow(offsets_mat)

  # ---- Grid strides for linear indexing -------------------------------
  dims        <- rep(bins_per_axis, n_dims)
  strides     <- c(1L, cumprod(as.integer(dims[-n_dims])))
  total_cells <- prod(dims)

  total_replicates <- bootstrap_iterations * monte_carlo_iterations

  if (verbose) {
    cat(sprintf(
      "[N-dim gaps] %d specimens | %d dims (%s)\n",
      n_specimens, n_dims, paste(col_names, collapse = ", ")))
    cat(sprintf(
      "  Grid: %d bins/axis | %d total cells | radius=%.1f (%d offsets)\n",
      bins_per_axis, total_cells, occupancy_radius, n_offsets))
    cat(sprintf(
      "  Bootstraps: %d | MC/boot: %d | total replicates: %d\n",
      bootstrap_iterations, monte_carlo_iterations, total_replicates))
    cat(sprintf("  Boot sample size: %d | Uncertainty: %.1f%%\n",
                boot_size, uncertainty * 100))
    flush.console()
  }

  # ---- Bootstrap + MC accumulation ------------------------------------
  occ_sum <- numeric(total_cells)

  .report_progress <- function(b) {
    if (isTRUE(verbose)) {
      cat(sprintf("  Bootstrap %d/%d\n", b, bootstrap_iterations))
      flush.console()
    }
    if (is.function(progress_callback)) {
      progress_callback(
        sprintf("N-dim bootstrap %d/%d", b, bootstrap_iterations), 0)
    }
  }

  batch_starts <- seq(1L, bootstrap_iterations, by = bootstrap_progress_every)

  for (start_b in batch_starts) {
    end_b <- min(bootstrap_iterations, start_b + bootstrap_progress_every - 1L)

    for (b in start_b:end_b) {
      boot_idx <- sample.int(n_specimens, size = boot_size, replace = TRUE)
      boot_pts <- points_mat[boot_idx, , drop = FALSE]

      for (m in seq_len(monte_carlo_iterations)) {
        occ_rep <- .ndim_single_replicate_occupancy(
          points_mat       = boot_pts,
          u_vec            = uncertainty_bw,
          uncertainty_type = uncertainty_type,
          grid_breaks      = grid_breaks,
          bins_per_axis    = bins_per_axis,
          n_dims           = n_dims,
          strides          = strides,
          total_cells      = total_cells,
          offsets_mat      = offsets_mat
        )
        occ_sum <- occ_sum + occ_rep
      }
    }

    .report_progress(end_b)
  }

  occupancy_prob <- occ_sum / total_replicates
  gap_prob       <- 1 - occupancy_prob
  occupied_vec   <- occupancy_prob >= occupancy_threshold

  # ---- Connected-component labeling of empty cells --------------------
  if (verbose) {
    cat(sprintf("  Empty cells: %d / %d (%.1f%%); running connected-component labeling...\n",
                sum(!occupied_vec), total_cells,
                100 * sum(!occupied_vec) / total_cells))
    flush.console()
  }

  empty_array  <- array(!occupied_vec, dim = dims)
  labels_array <- .ndim_connected_components(empty_array,
                                              connectivity = connectivity)
  labels_vec   <- as.vector(labels_array)

  # ---- Build output data frame ----------------------------------------
  center_grid      <- do.call(expand.grid, grid_centers)  # total_cells rows
  names(center_grid) <- paste0(col_names, "_center")

  region_id_vec <- ifelse(occupied_vec, -1L, labels_vec)

  cell_df <- data.frame(
    bin_id                = seq_len(total_cells),
    center_grid,
    occupancy_probability = occupancy_prob,
    gap_probability       = gap_prob,
    occupied              = occupied_vec,
    region_id             = as.integer(region_id_vec),
    stringsAsFactors      = FALSE
  )
  rownames(cell_df) <- NULL

  # ---- Region summary -------------------------------------------------
  empty_df <- cell_df[!cell_df$occupied & cell_df$region_id > 0L, , drop = FALSE]
  center_cols <- paste0(col_names, "_center")

  if (nrow(empty_df) > 0L) {
    region_ids <- sort(unique(empty_df$region_id))

    region_rows <- lapply(region_ids, function(rid) {
      sub <- empty_df[empty_df$region_id == rid, , drop = FALSE]
      centroid <- colMeans(sub[, center_cols, drop = FALSE])
      as.data.frame(c(
        list(region_id     = rid,
             cell_count    = nrow(sub),
             hypervolume   = nrow(sub) * cell_volume,
             mean_gap_prob = mean(sub$gap_probability)),
        as.list(centroid)
      ))
    })

    region_summary           <- do.call(rbind, region_rows)
    rownames(region_summary) <- NULL
    region_summary           <- region_summary[
      order(-region_summary$cell_count), , drop = FALSE]
  } else {
    region_summary <- data.frame(
      region_id     = integer(0),
      cell_count    = integer(0),
      hypervolume   = numeric(0),
      mean_gap_prob = numeric(0)
    )
  }

  # ---- Assemble output ------------------------------------------------
  parameters <- list(
    pc_axes                = pc_axes,
    n_dims                 = n_dims,
    bins_per_axis          = bins_per_axis,
    total_cells            = total_cells,
    n_specimens            = n_specimens,
    uncertainty            = uncertainty,
    monte_carlo_iterations = monte_carlo_iterations,
    bootstrap_iterations   = bootstrap_iterations,
    bootstrap_sample_size  = bootstrap_sample_size,
    bootstrap_actual_size  = boot_size,
    occupancy_radius       = occupancy_radius,
    n_offsets              = n_offsets,
    uncertainty_type       = uncertainty_type,
    domain_mode            = domain_mode,
    occupancy_threshold    = occupancy_threshold,
    connectivity           = connectivity,
    timestamp              = Sys.time()
  )

  result <- list(
    cell_df        = cell_df,
    region_summary = region_summary,
    grid_breaks    = grid_breaks,
    grid_centers   = grid_centers,
    dims           = dims,
    parameters     = parameters
  )
  class(result) <- c("morphospace_gaps_ndim", "list")

  if (verbose) {
    n_occ     <- sum(cell_df$occupied)
    n_emp     <- nrow(cell_df) - n_occ
    n_regions <- nrow(region_summary)
    cat(sprintf("\n=== N-Dim Gap Detection Complete ===\n"))
    cat(sprintf("  Occupied: %d (%.1f%%) | Empty: %d (%.1f%%) | Regions: %d\n",
                n_occ, 100 * n_occ / nrow(cell_df),
                n_emp, 100 * n_emp / nrow(cell_df),
                n_regions))
  }

  return(result)
}


# ============================================================
# Internal helpers
# ============================================================

#' Build N-Dimensional Neighbor Offsets
#'
#' Returns an integer matrix whose rows are all offset tuples in
#' \code{\{-floor(radius), ..., floor(radius)\}^n_dims} with L2 norm
#' <= \code{radius}.  The zero-offset (center cell) is included.
#'
#' For \code{radius = 1.5} and \code{n_dims = 5} this returns 51 rows:
#' the 1 center + 10 face neighbors (one non-zero coord) + 40 edge neighbors
#' (exactly two non-zero coords, each ±1, 2+2 = 4 per pair of axes,
#' C(5,2) = 10 pairs).
#'
#' @param n_dims Integer number of dimensions.
#' @param radius Numeric radius in cell-index units.
#' @return Integer matrix with n_dims columns.
#'
#' @keywords internal
.ndim_build_neighbor_offsets <- function(n_dims, radius) {
  r_int       <- as.integer(floor(radius))
  single_axis <- seq.int(-r_int, r_int)
  all_off     <- as.matrix(do.call(expand.grid, rep(list(single_axis), n_dims)))
  sq_norm     <- rowSums(all_off^2)
  storage.mode(all_off) <- "integer"
  all_off[sq_norm <= radius^2, , drop = FALSE]
}


#' Single N-Dim MC Replicate Occupancy Vector
#'
#' Jitters \code{points_mat}, maps each jittered point to its bin-index tuple,
#' expands by \code{offsets_mat}, and returns a binary integer vector of length
#' \code{total_cells} indicating which cells were reached in this replicate.
#'
#' @keywords internal
.ndim_single_replicate_occupancy <- function(points_mat,
                                              u_vec,
                                              uncertainty_type,
                                              grid_breaks,
                                              bins_per_axis,
                                              n_dims,
                                              strides,
                                              total_cells,
                                              offsets_mat) {
  n_pts <- nrow(points_mat)

  # -- Jitter -----------------------------------------------------------
  if (uncertainty_type == "gaussian") {
    # sigma s.t. +/-u covers ~95% of the distribution (u = 1.96 * sigma)
    noise <- matrix(rnorm(n_pts * n_dims), nrow = n_pts, ncol = n_dims)
    noise <- sweep(noise, 2L, u_vec / 1.96, `*`)
  } else {
    noise <- matrix(runif(n_pts * n_dims, -1, 1), nrow = n_pts, ncol = n_dims)
    noise <- sweep(noise, 2L, u_vec, `*`)
  }
  jittered <- points_mat + noise

  # -- Map to bin indices (n_pts x n_dims integer matrix) ---------------
  bin_idx <- matrix(0L, nrow = n_pts, ncol = n_dims)
  for (d in seq_len(n_dims)) {
    bin_idx[, d] <- pmax(1L, pmin(bins_per_axis,
                          findInterval(jittered[, d], grid_breaks[[d]])))
  }

  # -- Expand by neighbor offsets, collect valid linear indices ---------
  n_offsets   <- nrow(offsets_mat)
  # Pre-allocate at maximum possible size (may be partially unused)
  all_linear  <- integer(n_pts * n_offsets)
  write_pos   <- 1L

  for (k in seq_len(n_offsets)) {
    off     <- offsets_mat[k, ]
    nbr_idx <- sweep(bin_idx, 2L, off, `+`)  # n_pts x n_dims

    # Bounds check: all coordinates within [1, bins_per_axis]
    in_bounds <- (rowSums(nbr_idx >= 1L) == n_dims) &
                 (rowSums(nbr_idx <= bins_per_axis) == n_dims)

    nbr_valid <- nbr_idx[in_bounds, , drop = FALSE]
    if (nrow(nbr_valid) > 0L) {
      # Compute linear index: sum((idx - 1) * strides) + 1
      lin      <- as.integer(((nbr_valid - 1L) %*% strides) + 1L)
      n_valid  <- length(lin)
      all_linear[write_pos:(write_pos + n_valid - 1L)] <- lin
      write_pos <- write_pos + n_valid
    }
  }

  if (write_pos > 1L) {
    all_linear <- all_linear[seq_len(write_pos - 1L)]
  } else {
    return(integer(total_cells))
  }

  # Binary occupancy: any point touching a cell counts as occupied
  tab <- tabulate(all_linear, nbins = total_cells)
  as.integer(tab > 0L)
}


#' N-Dimensional Connected-Component Labeling via BFS
#'
#' Labels connected regions of empty cells in an n-dimensional logical array.
#' Uses a padded-array approach to avoid boundary wrap-around in linear-index
#' arithmetic.
#'
#' @param empty_array Logical n-dim array; TRUE = empty (gap) cell.
#' @param connectivity Integer. 1 = face-adjacency only (2*n_dims neighbors,
#'   default). 2 = all 3^n_dims - 1 neighbors (faces + edges + vertices).
#' @return Integer array of the same dimensions as \code{empty_array}.
#'   Occupied cells have label 0 (-1 in the final cell_df); empty cells have a
#'   positive integer label.
#'
#' @keywords internal
.ndim_connected_components <- function(empty_array, connectivity = 1L) {

  dims    <- dim(empty_array)
  n_dims  <- length(dims)

  # -- Pad with FALSE on every side (avoids wrap-around in BFS) ---------
  padded_dims  <- dims + 2L
  padded       <- array(FALSE, dim = padded_dims)

  # interior_idx: list of index ranges for the unpadded region
  interior_idx <- lapply(dims, function(d) seq.int(2L, d + 1L))

  # Assign the unpadded data into the padded interior
  padded <- do.call("[<-", c(list(padded), interior_idx,
                              list(value = as.vector(empty_array))))

  n_padded     <- prod(padded_dims)
  is_empty_p   <- as.vector(padded)          # logical
  labels_p     <- integer(n_padded)          # 0 = occupied or unvisited
  strides_p    <- c(1L, cumprod(as.integer(padded_dims[-n_dims])))

  # -- Adjacency offsets in padded linear space -------------------------
  if (connectivity == 1L) {
    # Face-adjacency: ±1 in exactly one axis
    adj_off <- as.vector(outer(strides_p, c(-1L, 1L)))
  } else {
    # All neighbors with L-inf norm <= 1 (faces + edges + vertices)
    all_off <- as.matrix(do.call(expand.grid, rep(list(-1L:1L), n_dims)))
    center_row <- rowSums(all_off != 0L) == 0L
    all_off    <- all_off[!center_row, , drop = FALSE]
    adj_off    <- as.integer(all_off %*% strides_p)
  }
  adj_off <- unique(adj_off)

  # -- BFS flood-fill ---------------------------------------------------
  visited        <- !is_empty_p          # occupied => pre-visited
  label_id       <- 0L
  unvisited_empty <- which(is_empty_p)   # starts as all empty padded cells

  while (length(unvisited_empty) > 0L) {
    seed <- unvisited_empty[1L]
    if (visited[seed]) {
      unvisited_empty <- unvisited_empty[-1L]
      next
    }

    label_id          <- label_id + 1L
    frontier          <- seed
    visited[seed]     <- TRUE
    labels_p[seed]    <- label_id

    while (length(frontier) > 0L) {
      # All candidate neighbors of the current frontier
      cand <- unique(as.vector(outer(frontier, adj_off, `+`)))
      # Bounds guard (padded array never wraps, but outer can exceed n_padded
      # at extreme corners of the padding layer)
      cand <- cand[cand >= 1L & cand <= n_padded]
      # Keep only unvisited empty cells
      cand <- cand[!visited[cand] & is_empty_p[cand]]

      visited[cand]  <- TRUE
      labels_p[cand] <- label_id
      frontier        <- cand
    }

    # Update the unvisited set (filter out cells now labeled)
    unvisited_empty <- unvisited_empty[!visited[unvisited_empty]]
  }

  # -- Extract the unpadded interior label array ------------------------
  labels_arr_p <- array(labels_p, dim = padded_dims)
  labels_inner <- do.call("[", c(list(labels_arr_p), interior_idx))
  array(labels_inner, dim = dims)
}


# ============================================================
# S3 methods
# ============================================================

#' Print Method for morphospace_gaps_ndim
#'
#' @param x Object of class \code{morphospace_gaps_ndim}.
#' @param ... Additional arguments (ignored).
#'
#' @export
print.morphospace_gaps_ndim <- function(x, ...) {
  p <- x$parameters

  cat("N-Dimensional Morphospace Gap Detection\n")
  cat("========================================\n\n")
  cat(sprintf("Dimensions : %d  (%s)\n",
              p$n_dims, paste(paste0("PC", p$pc_axes), collapse = ", ")))
  cat(sprintf("Bins/axis  : %d  |  Total cells: %d\n",
              p$bins_per_axis, p$total_cells))
  cat(sprintf("Domain mode: %s\n", p$domain_mode))
  cat(sprintf("Uncertainty: %.1f%%  |  Model: %s\n",
              p$uncertainty * 100, p$uncertainty_type))
  cat(sprintf("Bootstrap  : %d  |  MC/boot: %d  (%.0f K replicates)\n",
              p$bootstrap_iterations, p$monte_carlo_iterations,
              p$bootstrap_iterations * p$monte_carlo_iterations / 1000))
  cat(sprintf("Occ. radius: %.1f cell-widths  |  %d neighbor offsets\n",
              p$occupancy_radius, p$n_offsets))
  cat(sprintf("Occ. thresh: %.3f  |  CC connectivity: %d\n\n",
              p$occupancy_threshold, p$connectivity))

  n_cells    <- nrow(x$cell_df)
  n_occupied <- sum(x$cell_df$occupied)
  n_empty    <- n_cells - n_occupied
  n_regions  <- nrow(x$region_summary)

  cat(sprintf("Cells      : %d total | %d occupied (%.1f%%) | %d empty (%.1f%%)\n",
              n_cells,
              n_occupied, 100 * n_occupied / n_cells,
              n_empty,    100 * n_empty    / n_cells))
  cat(sprintf("Gap regions: %d\n\n", n_regions))

  if (n_regions > 0L) {
    cat("Top gap regions by size:\n")
    show_cols <- intersect(c("region_id", "cell_count", "hypervolume",
                             "mean_gap_prob"), colnames(x$region_summary))
    print(head(x$region_summary[, show_cols, drop = FALSE], 5L),
          row.names = FALSE)
  }
  invisible(x)
}
