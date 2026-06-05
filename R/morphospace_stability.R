#' Compute Morphospace Stability by Fraction-Based Resampling
#'
#' Runs repeated subsampling of shapes at user-defined fractions, computes EFA
#' and PCA for each run (strict mode) or reuses a full-data EFA basis and reruns
#' PCA only (fast mode), and quantifies convergence using PCA subspace
#' similarity and morphospace occupancy overlap.
#'
#' @param shape_dir Directory containing JPG/JPEG shape images.
#' @param sample_fractions Numeric vector of sample fractions in (0, 1].
#' @param n_repeats Number of random subsamples per fraction.
#' @param harmonics Optional number of harmonics passed to \\code{efourier}.
#' @param norm Logical; EFA normalization mode (see \\code{shape_analysis}).
#' @param start_point Start direction for outline sliding.
#' @param align_orientation Logical; whether to align orientation before EFA.
#' @param mode Analysis mode: \\code{"strict"} (full preprocess+EFA+PCA per run)
#'   or \\code{"fast"} (single full EFA, PCA rerun per subset).
#' @param reference_mode Reference used for similarity metrics. Either
#'   \\code{"full_dataset"} or \\code{"largest_fraction"}.
#' @param max_pcs Maximum number of PCs used for subspace and occupancy metrics.
#' @param grid_resolution Grid resolution for occupancy IoU.
#' @param seed Optional random seed for reproducibility.
#' @param parallel Logical; run repeats in parallel (uses \\pkg{parallel}).
#' @param n_cores Number of parallel workers if \\code{parallel = TRUE}.
#' @param verbose Print progress messages.
#'
#' @return List of class \\code{morphospace_stability}.
#' @export
compute_morphospace_stability <- function(shape_dir,
                                          sample_fractions = c(0.02, 0.05, 0.10, 0.20, 0.30, 0.50, 1.00),
                                          n_repeats = 10,
                                          harmonics = NULL,
                                          norm = TRUE,
                                          start_point = "left",
                                          align_orientation = FALSE,
                                          mode = c("fast", "strict"),
                                          reference_mode = c("full_dataset", "largest_fraction"),
                                          max_pcs = 4,
                                          grid_resolution = 60,
                                          seed = NULL,
                                          parallel = FALSE,
                                          n_cores = NULL,
                                          verbose = TRUE) {

  # Force and sanitize arguments early so parallel workers never need to
  # evaluate lazy promises that may originate from reactive expressions.
  shape_dir <- as.character(shape_dir)
  sample_fractions <- as.numeric(sample_fractions)
  n_repeats <- as.integer(n_repeats)
  max_pcs <- as.integer(max_pcs)
  grid_resolution <- as.integer(grid_resolution)
  parallel <- isTRUE(parallel)
  verbose <- isTRUE(verbose)
  if (!is.null(seed)) seed <- as.integer(seed)
  if (!is.null(n_cores)) n_cores <- as.integer(n_cores)

  mode <- match.arg(mode)
  reference_mode <- match.arg(reference_mode)

  if (!is.numeric(sample_fractions) || length(sample_fractions) == 0) {
    stop("sample_fractions must be a non-empty numeric vector")
  }
  if (any(!is.finite(sample_fractions))) {
    stop("sample_fractions must contain only finite values")
  }
  if (any(sample_fractions <= 0 | sample_fractions > 1)) {
    stop("All sample_fractions must be in the open interval (0, 1]")
  }
  if (!is.numeric(n_repeats) || length(n_repeats) != 1 || n_repeats < 1) {
    stop("n_repeats must be a positive integer")
  }
  n_repeats <- as.integer(n_repeats)

  if (!is.null(seed)) {
    set.seed(seed)
  }

  if (verbose) {
    message("Importing shapes from: ", shape_dir)
  }
  shapes <- .import_shape_files(shape_dir, verbose = verbose)
  n_total <- length(shapes$coo)
  if (n_total < 3) {
    stop("At least 3 shapes are required for stability analysis")
  }

  frac_tbl <- .prepare_fraction_schedule(sample_fractions, n_total)
  if (nrow(frac_tbl) == 0) {
    stop("No valid realized sample sizes after fraction conversion")
  }

  if (verbose) {
    message("Using fractions: ", paste(sprintf("%.3f", frac_tbl$fraction), collapse = ", "))
    message("Realized sample sizes: ", paste(frac_tbl$realized_n, collapse = ", "))
  }

  full_pre <- .compute_full_preprocessed(shapes, mode, norm, harmonics, start_point, align_orientation, verbose)

  reference <- .compute_reference_model(
    shapes = shapes,
    full_pre = full_pre,
    mode = mode,
    reference_mode = reference_mode,
    fraction_table = frac_tbl,
    norm = norm,
    harmonics = harmonics,
    start_point = start_point,
    align_orientation = align_orientation,
    seed = seed,
    verbose = verbose
  )

  tasks <- .build_stability_tasks(frac_tbl, n_repeats)

  run_one <- function(task) {
    .run_single_stability_iteration(
      shapes = shapes,
      full_pre = full_pre,
      mode = mode,
      fraction = task$fraction,
      realized_n = task$realized_n,
      repeat_id = task$repeat_id,
      norm = norm,
      harmonics = harmonics,
      start_point = start_point,
      align_orientation = align_orientation,
      reference = reference,
      max_pcs = max_pcs,
      grid_resolution = grid_resolution,
      seed = seed
    )
  }

  task_list <- split(tasks, seq_len(nrow(tasks)))

  if (parallel && requireNamespace("parallel", quietly = TRUE)) {
    if (is.null(n_cores)) {
      n_cores <- max(1, parallel::detectCores() - 1)
    }
    n_cores <- min(n_cores, nrow(tasks))
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterExport(
      cl,
      c(
        ".run_single_stability_iteration", ".compute_single_run_model",
        ".compute_axis_shift_per_axis",
        ".project_to_reference", ".compute_subspace_similarity",
        ".compute_occupancy_metrics", ".grid_iou", ".hull_iou",
        ".pairwise_pc_indices", "Out", ".normalize_shapes", ".perform_efa"
      ),
      envir = environment()
    )
    runs <- parallel::parLapply(
      cl,
      task_list,
      fun = function(task,
                     shapes,
                     full_pre,
                     mode,
                     norm,
                     harmonics,
                     start_point,
                     align_orientation,
                     reference,
                     max_pcs,
                     grid_resolution,
                     seed) {
        .run_single_stability_iteration(
          shapes = shapes,
          full_pre = full_pre,
          mode = mode,
          fraction = task$fraction,
          realized_n = task$realized_n,
          repeat_id = task$repeat_id,
          norm = norm,
          harmonics = harmonics,
          start_point = start_point,
          align_orientation = align_orientation,
          reference = reference,
          max_pcs = max_pcs,
          grid_resolution = grid_resolution,
          seed = seed
        )
      },
      shapes = shapes,
      full_pre = full_pre,
      mode = mode,
      norm = norm,
      harmonics = harmonics,
      start_point = start_point,
      align_orientation = align_orientation,
      reference = reference,
      max_pcs = max_pcs,
      grid_resolution = grid_resolution,
      seed = seed
    )
  } else {
    runs <- lapply(task_list, run_one)
  }

  run_df <- do.call(rbind, runs)
  rownames(run_df) <- NULL

  summary_df <- .summarize_stability_runs(run_df)

  result <- list(
    run_results = run_df,
    summary_table = summary_df,
    reference_info = reference$info,
    parameters = list(
      shape_dir = shape_dir,
      sample_fractions = frac_tbl$fraction,
      realized_sample_sizes = frac_tbl$realized_n,
      fraction_map = frac_tbl,
      n_total_specimens = n_total,
      n_repeats = n_repeats,
      harmonics = harmonics,
      norm = norm,
      start_point = start_point,
      align_orientation = align_orientation,
      mode = mode,
      reference_mode = reference_mode,
      max_pcs = max_pcs,
      grid_resolution = grid_resolution,
      seed = seed,
      parallel = parallel,
      n_cores = if (parallel) n_cores else NULL,
      timestamp = Sys.time()
    )
  )

  class(result) <- c("morphospace_stability", "list")
  result
}


#' Plot Morphospace Stability Convergence
#'
#' @param stability_result Output from \\code{compute_morphospace_stability}.
#' @param x_axis Use fractions or absolute counts on x-axis.
#' @param metrics Character vector of metrics to display.
#' @param show_ci Logical; show 95% CI ribbons.
#' @param show_points Logical; show mean points.
#' @param theme_name Plot theme preset.
#' @param title Plot title.
#'
#' @return A \\pkg{ggplot2} object.
#' @export
plot_morphospace_stability <- function(stability_result,
                                       x_axis = c("fraction", "count"),
                                       metrics = c("subspace_similarity", "occupancy_similarity"),
                                       show_ci = TRUE,
                                       show_points = TRUE,
                                       theme_name = "Haug",
                                       title = "Morphospace Stability Convergence") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting")
  }

  x_axis <- match.arg(x_axis)
  summary_df <- stability_result$summary_table
  plot_df <- summary_df[summary_df$metric_type %in% metrics, , drop = FALSE]

  if (nrow(plot_df) == 0) {
    stop("No matching metrics found in summary table")
  }

  x_var <- if (x_axis == "fraction") "fraction" else "realized_n"
  x_lab <- if (x_axis == "fraction") "Sample Fraction" else "Number of Specimens"

  p <- ggplot2::ggplot(plot_df, ggplot2::aes_string(x = x_var, y = "mean", color = "metric_type", fill = "metric_type"))
  if (show_ci) {
    p <- p + ggplot2::geom_ribbon(ggplot2::aes_string(ymin = "q025", ymax = "q975"), alpha = 0.2, color = NA)
  }
  p <- p + ggplot2::geom_line(size = 1.1)
  if (show_points) {
    p <- p + ggplot2::geom_point(size = 2.0)
  }

  p <- p + ggplot2::labs(
    title = title,
    x = x_lab,
    y = "Similarity (0-1)",
    color = "Metric",
    fill = "Metric"
  )

  if (theme_name == "Haug") {
    p <- p + ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 15, face = "bold"),
        axis.title = ggplot2::element_text(size = 11, face = "bold"),
        panel.grid.minor = ggplot2::element_blank()
      )
  } else if (theme_name == "publication") {
    p <- p + ggplot2::theme_classic()
  } else if (theme_name == "inverted_Haug") {
    p <- p + ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(color = "white", face = "bold"),
        axis.title = ggplot2::element_text(color = "white", face = "bold"),
        axis.text = ggplot2::element_text(color = "white"),
        panel.background = ggplot2::element_rect(fill = "black", color = NA),
        plot.background = ggplot2::element_rect(fill = "black", color = NA)
      )
  }

  p
}


#' Plot PCA Axis Shift Across Fractions
#'
#' Visualizes how individual PC axes rotate relative to the reference PCA
#' across sample fractions.
#'
#' @param stability_result Output from \code{compute_morphospace_stability}.
#' @param value_type Plot either angle in degrees or similarity.
#' @param show_ci Logical; show 95% CI ribbons.
#' @param max_axes Maximum number of PCs to display.
#'
#' @return A \pkg{ggplot2} object.
#' @export
plot_pca_axis_shift <- function(stability_result,
                                value_type = c("angle_deg", "similarity"),
                                show_ci = TRUE,
                                max_axes = 4) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting")
  }

  value_type <- match.arg(value_type)
  run_df <- stability_result$run_results

  metric_suffix <- if (value_type == "angle_deg") "_deg" else "_sim"
  axis_cols <- grep(paste0("^axis_pc[0-9]+", metric_suffix, "$"), colnames(run_df), value = TRUE)

  if (length(axis_cols) == 0) {
    stop("No per-axis shift columns found. Re-run analysis with the updated implementation.")
  }

  axis_ids <- as.integer(sub(paste0("^axis_pc([0-9]+)", metric_suffix, "$"), "\\1", axis_cols))
  keep <- axis_ids <= max_axes
  axis_cols <- axis_cols[keep]
  axis_ids <- axis_ids[keep]

  if (length(axis_cols) == 0) {
    stop("No axis columns available for selected max_axes")
  }

  long_list <- lapply(seq_along(axis_cols), function(i) {
    data.frame(
      fraction = run_df$fraction,
      realized_n = run_df$realized_n,
      repeat_id = run_df$repeat_id,
      pc_axis = paste0("PC", axis_ids[i]),
      value = run_df[[axis_cols[i]]],
      stringsAsFactors = FALSE
    )
  })
  long_df <- do.call(rbind, long_list)
  long_df <- long_df[is.finite(long_df$value), , drop = FALSE]

  if (nrow(long_df) == 0) {
    stop("No finite per-axis values to plot")
  }

  summarize_group <- function(v) {
    data.frame(
      mean = mean(v),
      q025 = stats::quantile(v, 0.025, names = FALSE),
      q975 = stats::quantile(v, 0.975, names = FALSE),
      stringsAsFactors = FALSE
    )
  }

  grp <- split(long_df$value, interaction(long_df$fraction, long_df$pc_axis, sep = "__", drop = TRUE))
  keys <- names(grp)
  sum_rows <- lapply(seq_along(grp), function(i) {
    key <- strsplit(keys[i], "__", fixed = TRUE)[[1]]
    vals <- grp[[i]]
    s <- summarize_group(vals)
    data.frame(
      fraction = as.numeric(key[1]),
      pc_axis = key[2],
      mean = s$mean,
      q025 = s$q025,
      q975 = s$q975,
      stringsAsFactors = FALSE
    )
  })
  sum_df <- do.call(rbind, sum_rows)

  y_lab <- if (value_type == "angle_deg") "Axis Shift (degrees)" else "Axis Similarity (|cos|)"
  title <- if (value_type == "angle_deg") "PC Axis Rotation vs Reference" else "PC Axis Similarity vs Reference"

  p <- ggplot2::ggplot(sum_df, ggplot2::aes_string(x = "fraction", y = "mean", color = "pc_axis", fill = "pc_axis"))
  if (show_ci) {
    p <- p + ggplot2::geom_ribbon(ggplot2::aes_string(ymin = "q025", ymax = "q975"), alpha = 0.2, color = NA)
  }
  p <- p +
    ggplot2::geom_line(size = 1.1) +
    ggplot2::geom_point(size = 2.0) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = title,
      x = "Sample Fraction",
      y = y_lab,
      color = "Axis",
      fill = "Axis"
    )

  p
}


#' Summarize Morphospace Stability
#'
#' @param stability_result Output from \\code{compute_morphospace_stability}.
#' @param threshold Minimum metric mean considered converged.
#' @param sd_threshold Maximum SD considered stable.
#'
#' @return Data frame with convergence recommendations by metric.
#' @export
summarize_morphospace_stability <- function(stability_result,
                                            threshold = 0.90,
                                            sd_threshold = 0.05) {
  summary_df <- stability_result$summary_table

  metrics <- unique(summary_df$metric_type)
  out <- lapply(metrics, function(m) {
    sub <- summary_df[summary_df$metric_type == m, , drop = FALSE]
    sub <- sub[order(sub$fraction), , drop = FALSE]
    ok <- which(sub$mean >= threshold & sub$sd <= sd_threshold)
    first_ok <- if (length(ok) == 0) NA_integer_ else ok[1]
    data.frame(
      metric_type = m,
      threshold = threshold,
      sd_threshold = sd_threshold,
      recommended_fraction = if (is.na(first_ok)) NA_real_ else sub$fraction[first_ok],
      recommended_sample_size = if (is.na(first_ok)) NA_real_ else sub$realized_n[first_ok],
      converged = !is.na(first_ok),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, out)
}


#' @export
print.morphospace_stability <- function(x, ...) {
  cat("Morphospace Stability Analysis\n")
  cat("============================\n\n")
  cat(sprintf("Total specimens: %d\n", x$parameters$n_total_specimens))
  cat(sprintf("Mode: %s\n", x$parameters$mode))
  cat(sprintf("Reference: %s\n", x$parameters$reference_mode))
  cat(sprintf("Repeats per fraction: %d\n", x$parameters$n_repeats))
  cat("Fractions:", paste(sprintf("%.3f", x$parameters$sample_fractions), collapse = ", "), "\n")
  cat("Realized sizes:", paste(x$parameters$realized_sample_sizes, collapse = ", "), "\n\n")
  cat("Summary (first 10 rows):\n")
  print(utils::head(x$summary_table, 10))
  invisible(x)
}


.prepare_fraction_schedule <- function(sample_fractions, n_total) {
  fr <- sort(unique(sample_fractions))
  realized <- pmax(2L, floor(fr * n_total))
  realized <- pmin(realized, n_total)
  keep <- !duplicated(realized)
  data.frame(
    fraction = fr[keep],
    realized_n = as.integer(realized[keep]),
    stringsAsFactors = FALSE
  )
}


.compute_full_preprocessed <- function(shapes,
                                       mode,
                                       norm,
                                       harmonics,
                                       start_point,
                                       align_orientation,
                                       verbose) {
  if (mode == "fast") {
    if (verbose) message("Fast mode: computing shared normalized outlines and EFA basis")
    normalized <- .normalize_shapes(shapes, start_point, align_orientation, verbose = FALSE)
    efa <- .perform_efa(normalized, norm = norm, harmonics = harmonics, start = TRUE, verbose = FALSE)
    coe <- efa$coe
    pca <- stats::prcomp(coe, center = TRUE, scale. = FALSE)
    return(list(normalized_shapes = normalized, efa = efa, coe = coe, pca = pca))
  }

  if (verbose) message("Strict mode: full-data reference will be computed from raw subsets")
  list(normalized_shapes = NULL, efa = NULL, coe = NULL, pca = NULL)
}


.compute_reference_model <- function(shapes,
                                     full_pre,
                                     mode,
                                     reference_mode,
                                     fraction_table,
                                     norm,
                                     harmonics,
                                     start_point,
                                     align_orientation,
                                     seed,
                                     verbose) {
  n_total <- if (!is.null(full_pre$coe)) nrow(full_pre$coe) else length(shapes$coo)
  use_n <- if (reference_mode == "full_dataset") n_total else max(fraction_table$realized_n)

  if (use_n == n_total) {
    ref_indices <- seq_len(n_total)
    source_label <- "full_dataset"
  } else {
    if (!is.null(seed)) set.seed(seed + 700001L)
    ref_indices <- sample(seq_len(n_total), size = use_n, replace = FALSE)
    source_label <- "largest_fraction"
  }

  ref_model <- .compute_single_run_model(
    shapes = shapes,
    full_pre = full_pre,
    mode = mode,
    indices = ref_indices,
    norm = norm,
    harmonics = harmonics,
    start_point = start_point,
    align_orientation = align_orientation
  )

  if (verbose) {
    message("Reference model prepared from ", source_label, " (n=", use_n, ")")
  }

  list(
    pca = ref_model$pca,
    coe = ref_model$coe,
    scores = ref_model$pca$x,
    info = list(source = source_label, n_reference = use_n)
  )
}


.build_stability_tasks <- function(fraction_table, n_repeats) {
  out <- lapply(seq_len(nrow(fraction_table)), function(i) {
    data.frame(
      fraction = fraction_table$fraction[i],
      realized_n = fraction_table$realized_n[i],
      repeat_id = seq_len(n_repeats),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, out)
}


.run_single_stability_iteration <- function(shapes,
                                            full_pre,
                                            mode,
                                            fraction,
                                            realized_n,
                                            repeat_id,
                                            norm,
                                            harmonics,
                                            start_point,
                                            align_orientation,
                                            reference,
                                            max_pcs,
                                            grid_resolution,
                                            seed) {
  n_total <- if (!is.null(full_pre$coe)) nrow(full_pre$coe) else length(shapes$coo)
  if (!is.null(seed)) {
    task_seed <- as.integer(seed + round(fraction * 100000) + repeat_id * 1009)
    set.seed(task_seed)
  }
  idx <- sample(seq_len(n_total), size = realized_n, replace = FALSE)

  run_model <- .compute_single_run_model(
    shapes = shapes,
    full_pre = full_pre,
    mode = mode,
    indices = idx,
    norm = norm,
    harmonics = harmonics,
    start_point = start_point,
    align_orientation = align_orientation
  )

  n_axes <- min(max_pcs, ncol(reference$pca$rotation), ncol(run_model$pca$rotation))
  subspace_sim <- .compute_subspace_similarity(run_model$pca$rotation, reference$pca$rotation, n_axes)
  axis_shift <- .compute_axis_shift_per_axis(run_model$pca$rotation, reference$pca$rotation, n_axes)

  axis_deg <- rep(NA_real_, max_pcs)
  axis_sim <- rep(NA_real_, max_pcs)
  if (n_axes > 0) {
    axis_deg[seq_len(n_axes)] <- axis_shift$angle_deg
    axis_sim[seq_len(n_axes)] <- axis_shift$similarity
  }

  proj_scores <- .project_to_reference(run_model$coe, reference$pca)
  occ <- .compute_occupancy_metrics(
    projected_scores = proj_scores,
    reference_scores = reference$scores,
    max_pcs = max_pcs,
    grid_resolution = grid_resolution
  )

  out <- data.frame(
    fraction = fraction,
    realized_n = realized_n,
    repeat_id = repeat_id,
    subspace_similarity = subspace_sim,
    axis_shift_mean_deg = if (n_axes > 0) mean(axis_shift$angle_deg) else NA_real_,
    axis_shift_max_deg = if (n_axes > 0) max(axis_shift$angle_deg) else NA_real_,
    axis_shift_mean_similarity = if (n_axes > 0) mean(axis_shift$similarity) else NA_real_,
    occupancy_grid_iou = occ$grid_iou,
    occupancy_hull_iou = occ$hull_iou,
    occupancy_similarity = occ$combined,
    stringsAsFactors = FALSE
  )

  for (i in seq_len(max_pcs)) {
    out[[paste0("axis_pc", i, "_deg")]] <- axis_deg[i]
    out[[paste0("axis_pc", i, "_sim")]] <- axis_sim[i]
  }

  out
}


.compute_single_run_model <- function(shapes,
                                      full_pre,
                                      mode,
                                      indices,
                                      norm,
                                      harmonics,
                                      start_point,
                                      align_orientation) {
  if (mode == "fast") {
    coe <- full_pre$coe[indices, , drop = FALSE]
    pca <- stats::prcomp(coe, center = TRUE, scale. = FALSE)
    return(list(coe = coe, pca = pca))
  }

  subset_coo <- shapes$coo[indices]
  subset_fac <- if (is.data.frame(shapes$fac) && nrow(shapes$fac) >= max(indices)) {
    shapes$fac[indices, , drop = FALSE]
  } else {
    data.frame()
  }
  subset_shapes <- Out(subset_coo, fac = subset_fac, ldk = shapes$ldk)
  normalized <- .normalize_shapes(subset_shapes, start_point, align_orientation, verbose = FALSE)
  efa <- .perform_efa(normalized, norm = norm, harmonics = harmonics, start = TRUE, verbose = FALSE)
  coe <- efa$coe
  pca <- stats::prcomp(coe, center = TRUE, scale. = FALSE)
  list(coe = coe, pca = pca)
}


.project_to_reference <- function(run_coe, reference_pca) {
  centered <- sweep(run_coe, 2, reference_pca$center, FUN = "-")
  centered %*% reference_pca$rotation
}


.compute_subspace_similarity <- function(run_rotation, ref_rotation, n_axes) {
  if (n_axes < 1) return(NA_real_)
  a <- run_rotation[, seq_len(n_axes), drop = FALSE]
  b <- ref_rotation[, seq_len(n_axes), drop = FALSE]
  qa <- qr.Q(qr(a))
  qb <- qr.Q(qr(b))
  s <- svd(t(qa) %*% qb)$d
  s <- pmin(pmax(s, 0), 1)
  mean(s)
}


.compute_axis_shift_per_axis <- function(run_rotation, ref_rotation, n_axes) {
  if (n_axes < 1) {
    return(list(angle_deg = numeric(0), similarity = numeric(0)))
  }

  sim <- numeric(n_axes)
  for (i in seq_len(n_axes)) {
    v1 <- run_rotation[, i]
    v2 <- ref_rotation[, i]
    denom <- sqrt(sum(v1^2)) * sqrt(sum(v2^2))
    if (!is.finite(denom) || denom <= 0) {
      sim[i] <- NA_real_
    } else {
      d <- sum(v1 * v2) / denom
      # Sign flips are arbitrary in PCA, so use absolute alignment.
      sim[i] <- abs(pmax(pmin(d, 1), -1))
    }
  }

  ang <- acos(sim) * 180 / pi
  list(angle_deg = ang, similarity = sim)
}


.compute_occupancy_metrics <- function(projected_scores,
                                       reference_scores,
                                       max_pcs,
                                       grid_resolution) {
  n_pcs <- min(max_pcs, ncol(projected_scores), ncol(reference_scores))
  if (n_pcs < 2) {
    return(list(grid_iou = NA_real_, hull_iou = NA_real_, combined = NA_real_))
  }

  pairs <- .pairwise_pc_indices(n_pcs)
  grid_vals <- numeric(nrow(pairs))
  hull_vals <- numeric(nrow(pairs))

  for (i in seq_len(nrow(pairs))) {
    p1 <- pairs[i, 1]
    p2 <- pairs[i, 2]

    run_xy <- projected_scores[, c(p1, p2), drop = FALSE]
    ref_xy <- reference_scores[, c(p1, p2), drop = FALSE]

    grid_vals[i] <- .grid_iou(run_xy, ref_xy, grid_resolution = grid_resolution)
    hull_vals[i] <- .hull_iou(run_xy, ref_xy)
  }

  safe_mean <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) == 0) return(NA_real_)
    mean(x)
  }

  grid_mean <- safe_mean(grid_vals)
  hull_mean <- safe_mean(hull_vals)
  combined <- safe_mean(c(grid_mean, hull_mean))

  list(grid_iou = grid_mean, hull_iou = hull_mean, combined = combined)
}


.pairwise_pc_indices <- function(n_pcs) {
  t(utils::combn(seq_len(n_pcs), 2))
}


.grid_iou <- function(run_xy, ref_xy, grid_resolution = 60) {
  all_x <- c(run_xy[, 1], ref_xy[, 1])
  all_y <- c(run_xy[, 2], ref_xy[, 2])

  if (diff(range(all_x, na.rm = TRUE)) == 0 || diff(range(all_y, na.rm = TRUE)) == 0) {
    return(NA_real_)
  }

  bx <- seq(min(all_x, na.rm = TRUE), max(all_x, na.rm = TRUE), length.out = grid_resolution + 1)
  by <- seq(min(all_y, na.rm = TRUE), max(all_y, na.rm = TRUE), length.out = grid_resolution + 1)

  run_ix <- cut(run_xy[, 1], breaks = bx, include.lowest = TRUE, labels = FALSE)
  run_iy <- cut(run_xy[, 2], breaks = by, include.lowest = TRUE, labels = FALSE)
  ref_ix <- cut(ref_xy[, 1], breaks = bx, include.lowest = TRUE, labels = FALSE)
  ref_iy <- cut(ref_xy[, 2], breaks = by, include.lowest = TRUE, labels = FALSE)

  run_cells <- unique(paste(run_ix, run_iy, sep = "_"))
  ref_cells <- unique(paste(ref_ix, ref_iy, sep = "_"))

  inter <- length(intersect(run_cells, ref_cells))
  uni <- length(union(run_cells, ref_cells))

  if (uni == 0) return(NA_real_)
  inter / uni
}


.hull_iou <- function(run_xy, ref_xy) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    return(NA_real_)
  }
  if (nrow(run_xy) < 3 || nrow(ref_xy) < 3) {
    return(NA_real_)
  }

  make_poly <- function(mat) {
    h <- grDevices::chull(mat[, 1], mat[, 2])
    pts <- mat[c(h, h[1]), , drop = FALSE]
    sf::st_polygon(list(as.matrix(pts)))
  }

  out <- tryCatch({
    p1 <- sf::st_sfc(make_poly(run_xy), crs = NA)
    p2 <- sf::st_sfc(make_poly(ref_xy), crs = NA)
    inter <- suppressWarnings(sf::st_intersection(p1, p2))
    uni <- suppressWarnings(sf::st_union(p1, p2))
    a_inter <- suppressWarnings(as.numeric(sf::st_area(inter)))
    a_uni <- suppressWarnings(as.numeric(sf::st_area(uni)))
    if (!is.finite(a_inter) || !is.finite(a_uni) || a_uni <= 0) {
      NA_real_
    } else {
      a_inter / a_uni
    }
  }, error = function(e) {
    NA_real_
  })

  out
}


.summarize_stability_runs <- function(run_df) {
  metrics <- c(
    "subspace_similarity",
    "axis_shift_mean_deg",
    "axis_shift_max_deg",
    "axis_shift_mean_similarity",
    "occupancy_grid_iou",
    "occupancy_hull_iou",
    "occupancy_similarity"
  )

  metrics <- metrics[metrics %in% colnames(run_df)]
  if (length(metrics) == 0) {
    stop("No supported metric columns found in run_df")
  }

  out <- lapply(metrics, function(m) {
    vals <- split(run_df[, c("fraction", "realized_n", m)], list(run_df$fraction, run_df$realized_n), drop = TRUE)
    rows <- lapply(vals, function(v) {
      x <- v[[m]]
      x <- x[is.finite(x)]
      if (length(x) == 0) {
        mean_val <- median_val <- sd_val <- se_val <- q025 <- q975 <- NA_real_
      } else {
        mean_val <- mean(x)
        median_val <- median(x)
        sd_val <- stats::sd(x)
        se_val <- sd_val / sqrt(length(x))
        q025 <- stats::quantile(x, 0.025, names = FALSE)
        q975 <- stats::quantile(x, 0.975, names = FALSE)
      }
      data.frame(
        fraction = v$fraction[1],
        realized_n = v$realized_n[1],
        metric_type = m,
        mean = mean_val,
        median = median_val,
        sd = sd_val,
        se = se_val,
        q025 = q025,
        q975 = q975,
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, rows)
  })

  result <- do.call(rbind, out)
  result <- result[order(result$fraction, result$metric_type), , drop = FALSE]
  rownames(result) <- NULL
  result
}