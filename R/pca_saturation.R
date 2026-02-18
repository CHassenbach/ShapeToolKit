#' Compute PCA Saturation Curve
#'
#' @param pca_object PCA object or matrix of PC scores
#' @param sample_sizes Sample sizes to test
#' @param bootstrap_iterations Number of bootstrap iterations
#' @param pcs_to_analyze Number of PCs to analyze
#' @param metric Variance metric to compute
#' @param min_sample_size Minimum sample size
#' @param seed Random seed
#' @param parallel Use parallel processing
#' @param n_cores Number of cores
#'
#' @return List with saturation data and parameters
#' @export
compute_pca_saturation <- function(pca_object,
                                   sample_sizes = seq(0.1, 1, 0.1),
                                   bootstrap_iterations = 200,
                                   pcs_to_analyze = NULL,
                                   metric = c("total_variance", "cumulative_variance"),
                                   min_sample_size = 5,
                                   seed = NULL,
                                   parallel = FALSE,
                                   n_cores = NULL) {
  
  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Extract PC scores from PCA object or use matrix directly
  if (inherits(pca_object, "PCA") || inherits(pca_object, "prcomp")) {
    pc_scores <- pca_object$x
  } else if (is.matrix(pca_object) || is.data.frame(pca_object)) {
    pc_scores <- as.matrix(pca_object)
  } else {
    stop("pca_object must be a PCA object, prcomp object, or a matrix of PC scores")
  }
  
  # Ensure all data is numeric
  if (!is.numeric(pc_scores)) {
    stop("PC scores must be numeric. Check your input data for non-numeric columns.")
  }
  
  # Check for NA values
  if (any(is.na(pc_scores))) {
    warning("PC scores contain NA values. These will be removed from analysis.")
    # Remove rows with any NA
    complete_rows <- complete.cases(pc_scores)
    pc_scores <- pc_scores[complete_rows, , drop = FALSE]
  }
  
  # Get number of specimens and PCs
  n_specimens <- nrow(pc_scores)
  n_pcs <- ncol(pc_scores)
  
  # Determine PCs to analyze
  if (is.null(pcs_to_analyze)) {
    pcs_to_analyze <- n_pcs
  } else {
    pcs_to_analyze <- min(pcs_to_analyze, n_pcs)
  }
  
  # Subset PC scores to selected PCs
  pc_scores <- pc_scores[, 1:pcs_to_analyze, drop = FALSE]
  
  # Convert proportions to absolute sample sizes
  sample_sizes_abs <- ifelse(sample_sizes <= 1, 
                              round(sample_sizes * n_specimens), 
                              sample_sizes)
  
  # Filter out sample sizes below minimum
  sample_sizes_abs <- sample_sizes_abs[sample_sizes_abs >= min_sample_size]
  sample_sizes_abs <- unique(sort(sample_sizes_abs))
  
  # Ensure maximum sample size doesn't exceed total specimens
  sample_sizes_abs <- sample_sizes_abs[sample_sizes_abs <= n_specimens]
  
  if (length(sample_sizes_abs) == 0) {
    stop(paste0("No valid sample sizes found. Minimum sample size is ", 
                min_sample_size, " and total specimens is ", n_specimens))
  }
  
  # Match metric argument
  metric <- match.arg(metric, several.ok = TRUE)
  
  # Helper function to compute morphospace coverage metrics for a subsample
  compute_variance_metrics <- function(subsample_scores, metrics_to_compute, full_scores = NULL) {
    results <- list()
    
    # Compute morphospace coverage metrics
    # We want to measure how much of the total morphospace is captured
    
    if ("total_variance" %in% metrics_to_compute) {
      # Total range across all PCs (sum of ranges)
      pc_ranges <- apply(subsample_scores, 2, function(x) {
        diff(range(x, na.rm = TRUE))
      })
      pc_ranges[is.na(pc_ranges) | is.nan(pc_ranges)] <- 0
      results$total_variance <- sum(pc_ranges, na.rm = TRUE)
    }
    
    if ("pc1_variance" %in% metrics_to_compute) {
      # Range of PC1
      results$pc1_variance <- diff(range(subsample_scores[, 1], na.rm = TRUE))
    }
    
    if ("cumulative_variance" %in% metrics_to_compute) {
      # Total morphospace volume (product of ranges, geometric measure)
      pc_ranges <- apply(subsample_scores, 2, function(x) {
        diff(range(x, na.rm = TRUE))
      })
      pc_ranges[is.na(pc_ranges) | is.nan(pc_ranges) | pc_ranges == 0] <- 1e-10
      results$cumulative_variance <- prod(pc_ranges)^(1/length(pc_ranges))  # Geometric mean
    }
    
    return(results)
  }
  
  # Function to run bootstrap for a single sample size
  bootstrap_sample_size <- function(sample_size, scores, n_iter, metrics) {
    n_samples <- nrow(scores)
    
    # Store results for each bootstrap iteration
    iter_results <- vector("list", n_iter)
    
    for (i in 1:n_iter) {
      tryCatch({
        # Resample with replacement
        boot_indices <- sample(1:n_samples, size = sample_size, replace = TRUE)
        boot_scores <- scores[boot_indices, , drop = FALSE]
        
        # Compute metrics
        iter_results[[i]] <- compute_variance_metrics(boot_scores, metrics)
      }, error = function(e) {
        warning(paste("Error in bootstrap iteration", i, ":", e$message))
        iter_results[[i]] <- NULL
      })
    }
    
    # Remove NULL results (failed iterations)
    iter_results <- iter_results[!sapply(iter_results, is.null)]
    
    if (length(iter_results) == 0) {
      stop(paste("All bootstrap iterations failed for sample size", sample_size))
    }
    
    # Aggregate results across iterations for each metric
    aggregated <- list()
    for (m in metrics) {
      metric_values <- sapply(iter_results, function(x) {
        val <- x[[m]]
        if (is.null(val) || is.na(val) || is.nan(val)) return(NA)
        return(val)
      })
      
      # Remove NA values before computing statistics
      metric_values <- metric_values[!is.na(metric_values)]
      
      if (length(metric_values) == 0) {
        warning(paste("No valid values for metric", m, "at sample size", sample_size))
        aggregated[[m]] <- data.frame(
          sample_size = sample_size,
          metric_type = m,
          mean = NA,
          median = NA,
          sd = NA,
          q025 = NA,
          q975 = NA,
          stringsAsFactors = FALSE
        )
      } else {
        aggregated[[m]] <- data.frame(
          sample_size = sample_size,
          metric_type = m,
          mean = mean(metric_values, na.rm = TRUE),
          median = median(metric_values, na.rm = TRUE),
          sd = sd(metric_values, na.rm = TRUE),
          q025 = quantile(metric_values, 0.025, na.rm = TRUE),
          q975 = quantile(metric_values, 0.975, na.rm = TRUE),
          stringsAsFactors = FALSE
        )
      }
    }
    
    return(do.call(rbind, aggregated))
  }
  
  # Run bootstrap analysis across all sample sizes
  if (parallel && requireNamespace("parallel", quietly = TRUE)) {
    # Parallel processing
    if (is.null(n_cores)) {
      n_cores <- max(1, parallel::detectCores() - 1)
    }
    
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    
    # Export necessary objects to cluster
    parallel::clusterExport(cl, 
                           c("pc_scores", "bootstrap_iterations", "metric",
                             "compute_variance_metrics", "bootstrap_sample_size"),
                           envir = environment())
    
    results_list <- parallel::parLapply(cl, sample_sizes_abs, function(ss) {
      bootstrap_sample_size(ss, pc_scores, bootstrap_iterations, metric)
    })
    
  } else {
    # Sequential processing
    results_list <- lapply(sample_sizes_abs, function(ss) {
      bootstrap_sample_size(ss, pc_scores, bootstrap_iterations, metric)
    })
  }
  
  # Combine results into single data frame
  saturation_data <- do.call(rbind, results_list)
  rownames(saturation_data) <- NULL
  
  # Calculate proportion of total specimens
  saturation_data$sample_proportion <- saturation_data$sample_size / n_specimens
  
  # Prepare parameters list
  parameters <- list(
    n_specimens = n_specimens,
    n_pcs_analyzed = pcs_to_analyze,
    sample_sizes = sample_sizes_abs,
    bootstrap_iterations = bootstrap_iterations,
    metrics = metric,
    min_sample_size = min_sample_size,
    parallel = parallel
  )
  
  # Return results
  result <- list(
    saturation_data = saturation_data,
    parameters = parameters
  )
  
  class(result) <- c("pca_saturation", "list")
  
  return(result)
}


#' Plot PCA Saturation Curve
#'
#' @param saturation_result Output from compute_pca_saturation
#' @param x_axis Type of x-axis
#' @param normalize_metrics Normalize metrics to percentage
#' @param show_ci Show confidence intervals
#' @param show_points Show individual data points
#' @param colors Color vector for metrics
#' @param theme_name Plot theme
#' @param title Plot title
#' @param subtitle Plot subtitle
#'
#' @return A ggplot2 object
#' @export
plot_pca_saturation <- function(saturation_result,
                                x_axis = c("absolute", "proportion"),
                                normalize_metrics = TRUE,
                                show_ci = TRUE,
                                show_points = TRUE,
                                colors = NULL,
                                theme_name = "Haug",
                                title = "PCA Saturation Curve",
                                subtitle = NULL) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting")
  }
  
  x_axis <- match.arg(x_axis)
  
  # Extract data
  data <- saturation_result$saturation_data
  params <- saturation_result$parameters
  
  # Normalize metrics to percentage of maximum if requested
  if (normalize_metrics) {
    # For each metric type, divide by its maximum mean value
    for (metric in unique(data$metric_type)) {
      metric_rows <- data$metric_type == metric
      max_val <- max(data$mean[metric_rows], na.rm = TRUE)
      
      if (max_val > 0) {
        data$mean[metric_rows] <- (data$mean[metric_rows] / max_val) * 100
        data$median[metric_rows] <- (data$median[metric_rows] / max_val) * 100
        data$q025[metric_rows] <- (data$q025[metric_rows] / max_val) * 100
        data$q975[metric_rows] <- (data$q975[metric_rows] / max_val) * 100
      }
    }
    y_lab <- "Morphospace Coverage (% of Maximum)"
  } else {
    y_lab <- "Morphospace Coverage"
  }
  
  # Determine x variable
  x_var <- if (x_axis == "proportion") "sample_proportion" else "sample_size"
  x_lab <- if (x_axis == "proportion") "Sample Size (Proportion of Total)" else "Number of Specimens"
  
  # Default colors if not provided
  if (is.null(colors)) {
    colors <- c(
      total_variance = "#2E86AB",
      cumulative_variance = "#A23B72",
      pc1_variance = "#F18F01"
    )
  }
  
  # Create base plot
  p <- ggplot2::ggplot(data, ggplot2::aes_string(x = x_var, y = "mean", 
                                                  color = "metric_type", 
                                                  fill = "metric_type"))
  
  # Add confidence interval ribbon if requested
  if (show_ci) {
    p <- p + ggplot2::geom_ribbon(ggplot2::aes_string(ymin = "q025", ymax = "q975"),
                                   alpha = 0.2, color = NA)
  }
  
  # Add line
  p <- p + ggplot2::geom_line(size = 1.2)
  
  # Add points if requested
  if (show_points) {
    p <- p + ggplot2::geom_point(size = 2.5)
  }
  
  # Add labels
  p <- p + ggplot2::labs(
    title = title,
    subtitle = subtitle,
    x = x_lab,
    y = y_lab,
    color = "Metric",
    fill = "Metric"
  )
  
  # Apply theme
  if (theme_name == "Haug") {
    p <- p + ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 16, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 12),
        axis.title = ggplot2::element_text(size = 12, face = "bold"),
        axis.text = ggplot2::element_text(size = 10),
        legend.title = ggplot2::element_text(size = 11, face = "bold"),
        legend.text = ggplot2::element_text(size = 10),
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = "white", color = NA),
        plot.background = ggplot2::element_rect(fill = "white", color = NA)
      )
  } else if (theme_name == "inverted_Haug") {
    p <- p + ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 16, face = "bold", color = "white"),
        plot.subtitle = ggplot2::element_text(size = 12, color = "white"),
        axis.title = ggplot2::element_text(size = 12, face = "bold", color = "white"),
        axis.text = ggplot2::element_text(size = 10, color = "white"),
        legend.title = ggplot2::element_text(size = 11, face = "bold", color = "white"),
        legend.text = ggplot2::element_text(size = 10, color = "white"),
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_line(color = "gray30"),
        panel.background = ggplot2::element_rect(fill = "black", color = NA),
        plot.background = ggplot2::element_rect(fill = "black", color = NA)
      )
  } else if (theme_name == "publication") {
    p <- p + ggplot2::theme_classic() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        axis.title = ggplot2::element_text(size = 11),
        axis.text = ggplot2::element_text(size = 9),
        legend.title = ggplot2::element_text(size = 10, face = "bold"),
        legend.text = ggplot2::element_text(size = 9)
      )
  }
  
  # Apply custom colors if provided
  if (!is.null(colors)) {
    p <- p + ggplot2::scale_color_manual(values = colors) +
      ggplot2::scale_fill_manual(values = colors)
  }
  
  return(p)
}


#' Print method for pca_saturation objects
#'
#' @param x A pca_saturation object
#' @param ... Additional arguments (not used)
#'
#' @export
print.pca_saturation <- function(x, ...) {
  cat("PCA Saturation Analysis\n")
  cat("=======================\n\n")
  cat("Parameters:\n")
  cat(sprintf("  Total specimens: %d\n", x$parameters$n_specimens))
  cat(sprintf("  PCs analyzed: %d\n", x$parameters$n_pcs_analyzed))
  cat(sprintf("  Bootstrap iterations: %d\n", x$parameters$bootstrap_iterations))
  cat(sprintf("  Sample sizes tested: %d to %d\n", 
              min(x$parameters$sample_sizes), 
              max(x$parameters$sample_sizes)))
  cat(sprintf("  Metrics: %s\n", paste(x$parameters$metrics, collapse = ", ")))
  cat("\nResults:\n")
  print(head(x$saturation_data, 10))
  if (nrow(x$saturation_data) > 10) {
    cat(sprintf("\n... and %d more rows\n", nrow(x$saturation_data) - 10))
  }
  invisible(x)
}
