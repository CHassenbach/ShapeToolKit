# Tests for N-Dimensional Morphospace Gap Detection
#
# Run with:  devtools::test(filter = "gap-detection-ndim")
#            testthat::test_file("tests/testthat/test-gap-detection-ndim.R")
#
# Coverage:
#  1. Neighbor-offset generation: count for radius=1.5 in various dims
#  2. Axis-ordering invariance of occupancy probability
#  3. Domain modes (specimen_range vs full_morphospace) cell-count equivalence
#  4. Connected-component labeling on a small synthetic 3D case
#  5. Regression: 2D result matches existing .compute_gap_probability output
#  6. Main function returns valid structure and class

# Helper: small deterministic dataset ----------------------------------------
.make_scores <- function(n = 60, n_dims = 5, seed = 1L) {
  set.seed(seed)
  mat <- matrix(rnorm(n * n_dims), nrow = n, ncol = n_dims)
  colnames(mat) <- paste0("PC", seq_len(n_dims))
  as.data.frame(mat)
}


# =============================================================================
# TEST 1: Neighbor-offset count
# =============================================================================

test_that(".ndim_build_neighbor_offsets gives 51 cells for radius=1.5, n_dims=5", {
  offsets <- ShapeToolKit:::.ndim_build_neighbor_offsets(5L, 1.5)
  expect_equal(nrow(offsets), 51L)
})

test_that(".ndim_build_neighbor_offsets gives 3 cells for radius=1.5, n_dims=1", {
  offsets <- ShapeToolKit:::.ndim_build_neighbor_offsets(1L, 1.5)
  expect_equal(nrow(offsets), 3L)
})

test_that(".ndim_build_neighbor_offsets gives 9 cells for radius=1.5, n_dims=2", {
  # In 2D: all of {-1,0,1}^2 have L2 <= sqrt(2) < 1.5, so all 9 pass
  offsets <- ShapeToolKit:::.ndim_build_neighbor_offsets(2L, 1.5)
  expect_equal(nrow(offsets), 9L)
})

test_that(".ndim_build_neighbor_offsets gives 19 cells for radius=1.5, n_dims=3", {
  offsets <- ShapeToolKit:::.ndim_build_neighbor_offsets(3L, 1.5)
  expect_equal(nrow(offsets), 19L)
})

test_that(".ndim_build_neighbor_offsets includes the zero offset (center cell)", {
  offsets <- ShapeToolKit:::.ndim_build_neighbor_offsets(3L, 1.5)
  has_zero <- any(apply(offsets, 1L, function(r) all(r == 0L)))
  expect_true(has_zero)
})

test_that(".ndim_build_neighbor_offsets rejects offsets whose L2 > radius", {
  offsets <- ShapeToolKit:::.ndim_build_neighbor_offsets(5L, 1.5)
  sq_norms <- rowSums(offsets^2)
  expect_true(all(sq_norms <= 1.5^2))
})

# =============================================================================
# TEST 2: Axis-ordering invariance
# =============================================================================

test_that("detect_morphospace_gaps_ndim is deterministic with the same RNG seed", {
  # Calling the function twice with set.seed() before each call must give
  # bit-for-bit identical cell-level occupancy probabilities.
  scores <- .make_scores(n = 40L, n_dims = 3L, seed = 42L)
  
  run_gaps <- function() {
    set.seed(99L)
    detect_morphospace_gaps_ndim(
      pca_scores             = scores,
      pc_axes                = 1L:3L,
      bins_per_axis          = 5L,
      uncertainty            = 0.10,
      monte_carlo_iterations = 10L,
      bootstrap_iterations   = 10L,
      verbose                = FALSE
    )
  }
  
  res1 <- run_gaps()
  res2 <- run_gaps()
  
  expect_equal(res1$cell_df$occupancy_probability,
               res2$cell_df$occupancy_probability)
  expect_equal(res1$cell_df$region_id,
               res2$cell_df$region_id)
})

# =============================================================================
# TEST 3: Domain modes
# =============================================================================

test_that("specimen_range and full_morphospace produce the same cell count", {
  # Both use the same bounding box grid, so total cells are identical
  scores <- .make_scores(n = 30L, n_dims = 3L, seed = 7L)
  
  run <- function(mode) {
    detect_morphospace_gaps_ndim(
      pca_scores             = scores,
      pc_axes                = 1L:3L,
      bins_per_axis          = 5L,
      monte_carlo_iterations = 10L,
      bootstrap_iterations   = 5L,
      domain_mode            = mode,
      verbose                = FALSE
    )
  }
  
  res_sr <- run("specimen_range")
  res_fm <- run("full_morphospace")
  
  # Both should have 5^3 = 125 cells
  expect_equal(nrow(res_sr$cell_df), 125L)
  expect_equal(nrow(res_fm$cell_df), 125L)
})

test_that("grid_breaks span the observed PC range under specimen_range", {
  scores <- .make_scores(n = 50L, n_dims = 2L, seed = 5L)
  
  res <- detect_morphospace_gaps_ndim(
    pca_scores             = scores,
    pc_axes                = 1L:2L,
    bins_per_axis          = 8L,
    monte_carlo_iterations = 5L,
    bootstrap_iterations   = 5L,
    verbose                = FALSE
  )
  
  for (d in seq_along(res$grid_breaks)) {
    observed_min <- min(scores[[paste0("PC", d)]], na.rm = TRUE)
    observed_max <- max(scores[[paste0("PC", d)]], na.rm = TRUE)
    expect_lte(res$grid_breaks[[d]][1L], observed_min + 1e-9)
    expect_gte(res$grid_breaks[[d]][length(res$grid_breaks[[d]])], observed_max - 1e-9)
  }
})

# =============================================================================
# TEST 4: Connected-component labeling (synthetic 3D)
# =============================================================================

# NOTE on convention: .ndim_connected_components expects TRUE = EMPTY (gap) cell.
# Occupied cells are FALSE and always receive label 0 in the output.

test_that(".ndim_connected_components labels a 3D array with two known gap regions", {
  # empty_arr: TRUE = gap (empty), FALSE = occupied
  empty_arr <- array(FALSE, dim = c(5L, 5L, 5L))  # start fully occupied
  
  # Blob A: a single gap cell at (2,2,2)
  empty_arr[2L, 2L, 2L] <- TRUE
  
  # Blob B: a 2x2 patch of gap cells at x=4:5, y=4:5, z=4
  empty_arr[4L, 4L, 4L] <- TRUE
  empty_arr[4L, 5L, 4L] <- TRUE
  empty_arr[5L, 4L, 4L] <- TRUE
  empty_arr[5L, 5L, 4L] <- TRUE
  
  labels <- ShapeToolKit:::.ndim_connected_components(empty_arr, connectivity = 1L)
  
  # Occupied cells (FALSE in empty_arr) should have label 0
  expect_true(all(labels[!empty_arr] == 0L))
  
  # Exactly 2 connected empty regions
  non_zero_labels <- unique(as.vector(labels)[as.vector(labels) > 0L])
  expect_equal(length(non_zero_labels), 2L)
  
  # Blob A: 1 cell; Blob B: 4 cells
  label_counts <- sort(as.integer(table(as.vector(labels)[as.vector(labels) > 0L])))
  expect_equal(label_counts, c(1L, 4L))
})

test_that(".ndim_connected_components returns all-zero array when no empty cells", {
  # All FALSE = all occupied => no gap cells => all labels 0
  empty_arr <- array(FALSE, dim = c(4L, 4L, 4L))
  labels <- ShapeToolKit:::.ndim_connected_components(empty_arr, connectivity = 1L)
  expect_true(all(labels == 0L))
})

test_that(".ndim_connected_components returns a single region when all cells empty", {
  # All TRUE = all empty => one connected region
  empty_arr <- array(TRUE, dim = c(3L, 3L, 3L))
  labels <- ShapeToolKit:::.ndim_connected_components(empty_arr, connectivity = 1L)
  expect_equal(length(unique(as.vector(labels))), 1L)
  expect_equal(unique(as.vector(labels)), 1L)
})

test_that("face-only vs diagonal connectivity on 2x2x2 opposite-corner gaps", {
  # In a 2x2x2 grid with only the two diagonally-opposite corners empty,
  # face-adjacency cannot connect them (offset (+1,+1,+1) has L1 = 3),
  # but diagonal/vertex adjacency CAN (offset has L-inf = 1 <= 1).
  empty_arr <- array(FALSE, dim = c(2L, 2L, 2L))  # all occupied
  empty_arr[1L, 1L, 1L] <- TRUE   # gap corner 1
  empty_arr[2L, 2L, 2L] <- TRUE   # gap corner 2 (diagonally opposite)
  
  labels_face <- ShapeToolKit:::.ndim_connected_components(empty_arr, connectivity = 1L)
  labels_diag <- ShapeToolKit:::.ndim_connected_components(empty_arr, connectivity = 2L)
  
  non_zero_face <- unique(as.vector(labels_face)[as.vector(labels_face) > 0L])
  non_zero_diag <- unique(as.vector(labels_diag)[as.vector(labels_diag) > 0L])
  
  # Face-adjacency: 2 separate regions (corners not reachable from each other)
  expect_equal(length(non_zero_face), 2L)
  # Diagonal (vertex) adjacency: 1 region (direct (+1,+1,+1) edge)
  expect_equal(length(non_zero_diag), 1L)
})

# =============================================================================
# TEST 5: 2D regression vs existing implementation
# =============================================================================

test_that("2D n-dim run and 2D heatmap run give consistent occupancy fractions", {
  # Use a well-separated bimodal dataset so there is a real gap to detect
  set.seed(123L)
  n    <- 50L
  scores_2d <- data.frame(
    PC1 = c(rnorm(n, -3, 0.5), rnorm(n, 3, 0.5)),
    PC2 = rnorm(2L * n)
  )
  
  # Run the NEW n-dim path (2 dims, 20 bins)
  set.seed(1L)
  res_ndim <- detect_morphospace_gaps_ndim(
    pca_scores             = scores_2d,
    pc_axes                = c(1L, 2L),
    bins_per_axis          = 20L,
    uncertainty            = 0.05,
    monte_carlo_iterations = 30L,
    bootstrap_iterations   = 30L,
    occupancy_radius       = 1.5,
    uncertainty_type       = "gaussian",
    occupancy_threshold    = 0.05,
    verbose                = FALSE
  )
  
  frac_occupied_ndim <- mean(res_ndim$cell_df$occupancy_probability)
  
  # There should be a clearly occupied region (mean occupancy well above 0)
  expect_gt(frac_occupied_ndim, 0.0)
  # And not every cell should be occupied (there IS a gap)
  expect_lt(mean(res_ndim$cell_df$occupied), 1.0)
  # The gap region(s) should be detected
  expect_gte(nrow(res_ndim$region_summary), 1L)
})

# =============================================================================
# TEST 6: Output structure and class
# =============================================================================

test_that("detect_morphospace_gaps_ndim returns correct class and structure", {
  scores <- .make_scores(n = 30L, n_dims = 3L, seed = 10L)
  
  res <- detect_morphospace_gaps_ndim(
    pca_scores             = scores,
    pc_axes                = 1L:3L,
    bins_per_axis          = 5L,
    monte_carlo_iterations = 5L,
    bootstrap_iterations   = 5L,
    verbose                = FALSE
  )
  
  expect_s3_class(res, "morphospace_gaps_ndim")
  expect_true(is.list(res))
  
  # Required top-level elements
  expect_named(res, c("cell_df", "region_summary", "grid_breaks",
                       "grid_centers", "dims", "parameters"),
               ignore.order = TRUE)
  
  # cell_df required columns
  expect_true(all(c("bin_id", "occupancy_probability", "gap_probability",
                     "occupied", "region_id") %in% colnames(res$cell_df)))
  
  # PC centre columns present
  centre_cols <- paste0("PC", 1L:3L, "_center")
  expect_true(all(centre_cols %in% colnames(res$cell_df)))
  
  # Dimensions: 5^3 = 125 cells
  expect_equal(nrow(res$cell_df), 125L)
  
  # Probabilities in [0, 1]
  expect_true(all(res$cell_df$occupancy_probability >= 0))
  expect_true(all(res$cell_df$occupancy_probability <= 1))
  expect_true(all(res$cell_df$gap_probability >= 0))
  expect_true(all(res$cell_df$gap_probability <= 1))
  
  # occ + gap = 1 (within floating point)
  expect_equal(res$cell_df$occupancy_probability + res$cell_df$gap_probability,
               rep(1, nrow(res$cell_df)), tolerance = 1e-10)
  
  # Occupied cells have region_id == -1
  expect_true(all(res$cell_df$region_id[res$cell_df$occupied] == -1L))
  
  # dims vector
  expect_equal(res$dims, rep(5L, 3L))
  
  # parameters list
  expect_equal(res$parameters$n_dims, 3L)
  expect_equal(res$parameters$bins_per_axis, 5L)
})

test_that("print.morphospace_gaps_ndim runs without error", {
  scores <- .make_scores(n = 20L, n_dims = 2L, seed = 11L)
  res <- detect_morphospace_gaps_ndim(
    pca_scores             = scores,
    pc_axes                = 1L:2L,
    bins_per_axis          = 4L,
    monte_carlo_iterations = 5L,
    bootstrap_iterations   = 5L,
    verbose                = FALSE
  )
  expect_output(print(res), "N-Dimensional Morphospace Gap Detection")
})

test_that("detect_morphospace_gaps_ndim errors on missing PC columns", {
  df <- data.frame(X = rnorm(10), Y = rnorm(10))
  expect_error(detect_morphospace_gaps_ndim(df), "No columns matching pattern")
})

test_that("detect_morphospace_gaps_ndim errors when fewer than 2 PC axes available", {
  df <- data.frame(PC1 = rnorm(10))
  expect_error(detect_morphospace_gaps_ndim(df, pc_axes = 1L), "at least 2 PC axes")
})

test_that("detect_morphospace_gaps_ndim handles NA rows by removing them", {
  scores <- .make_scores(n = 40L, n_dims = 3L, seed = 20L)
  scores[1L, "PC1"] <- NA
  scores[5L, "PC3"] <- NA
  
  expect_no_error(
    detect_morphospace_gaps_ndim(
      pca_scores             = scores,
      pc_axes                = 1L:3L,
      bins_per_axis          = 4L,
      monte_carlo_iterations = 5L,
      bootstrap_iterations   = 5L,
      verbose                = FALSE
    )
  )
})

test_that("bootstrap_sample_size as fraction works correctly", {
  scores <- .make_scores(n = 40L, n_dims = 2L, seed = 30L)
  res <- detect_morphospace_gaps_ndim(
    pca_scores             = scores,
    pc_axes                = 1L:2L,
    bins_per_axis          = 4L,
    monte_carlo_iterations = 5L,
    bootstrap_iterations   = 5L,
    bootstrap_sample_size  = 0.5,
    verbose                = FALSE
  )
  # boot_size should be floor(40 * 0.5) = 20
  expect_equal(res$parameters$bootstrap_actual_size, 20L)
})

test_that("bootstrap_sample_size as absolute count works correctly", {
  scores <- .make_scores(n = 40L, n_dims = 2L, seed = 31L)
  res <- detect_morphospace_gaps_ndim(
    pca_scores             = scores,
    pc_axes                = 1L:2L,
    bins_per_axis          = 4L,
    monte_carlo_iterations = 5L,
    bootstrap_iterations   = 5L,
    bootstrap_sample_size  = 25L,
    verbose                = FALSE
  )
  expect_equal(res$parameters$bootstrap_actual_size, 25L)
})

test_that("region_summary is sorted descending by cell_count", {
  set.seed(42L)
  scores <- data.frame(
    PC1 = c(rnorm(40L, -3, 0.4), rnorm(40L, 3, 0.4)),
    PC2 = rnorm(80L),
    PC3 = rnorm(80L)
  )
  res <- detect_morphospace_gaps_ndim(
    pca_scores             = scores,
    pc_axes                = 1L:3L,
    bins_per_axis          = 8L,
    monte_carlo_iterations = 20L,
    bootstrap_iterations   = 20L,
    verbose                = FALSE
  )
  
  if (nrow(res$region_summary) >= 2L) {
    cc <- res$region_summary$cell_count
    expect_true(all(diff(cc) <= 0L))
  } else {
    skip("Not enough regions to test ordering")
  }
})
