.disparity_catalogue <- function() {
  categories <- list(
    size = c("convhull.volume", "convhull.surface", "diagonal", "ellipsoid.volume",
             "n.ball.volume", "centroids", "variances", "ranges", "radius", "quantiles"),
    density = c("pairwise.dist", "neighbours", "count.neighbours", "span.tree.length",
                "func.eve", "func.div"),
    position = c("displacements", "projections"),
    other = c("angles", "deviations", "roundness")
  )
  scalar <- c("convhull.volume", "convhull.surface", "diagonal", "ellipsoid.volume",
              "func.div", "func.eve", "n.ball.volume", "roundness")
  vector <- c("angles", "centroids", "count.neighbours", "deviations", "displacements",
              "neighbours", "pairwise.dist", "projections", "quantiles", "radius",
              "ranges", "variances", "span.tree.length")
  rows <- lapply(c(scalar, vector), function(primitive) {
    reducers <- if (primitive %in% scalar) "" else c("mean", "median", "sum", "prod", "mode")
    id <- gsub(".", "_", primitive, fixed = TRUE)
    data.frame(
      metric = ifelse(nzchar(reducers), paste(reducers, id, sep = "_"), id),
      primitive = primitive, reducer = reducers,
      category = names(categories)[vapply(categories, function(x) primitive %in% x, logical(1))],
      stringsAsFactors = FALSE
    )
  })
  catalogue <- do.call(rbind, rows)
  catalogue$label <- gsub("_", " ", catalogue$metric, fixed = TRUE)
  catalogue$label[catalogue$metric == "sum_variances"] <- "Sum of variances (SOV)"
  catalogue$label[catalogue$metric == "sum_ranges"] <- "Sum of ranges (SOR)"
  catalogue$label[catalogue$metric == "mean_pairwise_dist"] <- "Mean pairwise distance (MPD)"
  catalogue$label[catalogue$metric == "mean_centroids"] <- "Mean centroid distance"
  catalogue$label[catalogue$metric == "median_centroids"] <- "Median centroid distance"
  catalogue
}

.disparity_choices <- function() {
  catalogue <- .disparity_catalogue()
  lapply(split(catalogue, factor(catalogue$category,
                                levels = c("size", "density", "position", "other"))),
         function(x) stats::setNames(x$metric, x$label))
}

.disparity_ids <- function(ids) {
  if (!is.character(ids) || !length(ids) || anyNA(ids) || anyDuplicated(ids) ||
      !all(ids %in% .disparity_catalogue()$metric)) {
    stop("Select at least one distinct metric from the disparity catalogue.")
  }
  ids
}

.disparity_integer <- function(x, name, minimum) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) ||
      x < minimum || x > .Machine$integer.max || x != floor(x)) {
    stop(name, " must be a single integer between ", minimum, " and ", .Machine$integer.max, ".")
  }
  as.integer(x)
}

.disparity_coordinates <- function(df, columns) {
  if (!is.data.frame(df) || !is.character(columns) || length(columns) < 2L ||
      anyNA(columns) || anyDuplicated(columns) || !all(columns %in% names(df)) ||
      !all(vapply(df[columns], is.numeric, logical(1)))) {
    stop("Select at least two distinct numeric morphospace columns.")
  }
  mat <- as.matrix(df[columns])
  rownames(mat) <- rownames(df)
  keep <- rowSums(!is.finite(mat)) == 0L
  list(matrix = mat[keep, , drop = FALSE], keep = keep, removed = sum(!keep))
}

.disparity_metric <- function(id, omit_undefined = FALSE) {
  .disparity_ids(id)
  row <- .disparity_catalogue()
  row <- row[row$metric == id, ]
  primitive <- getExportedValue("dispRity", row$primitive)
  reducer <- switch(row$reducer, mean = mean, median = stats::median, sum = sum,
                    prod = prod, mode = dispRity::mode.val, NULL)
  function(matrix) {
    value <- primitive(matrix)
    if (!is.null(reducer)) value <- reducer(value)
    if (!is.numeric(value) || length(value) != 1L) {
      stop(id, " did not return a finite scalar; check sample size and geometry.")
    }
    if (!is.finite(value)) {
      if (!omit_undefined) {
        stop(id, " did not return a finite scalar; check sample size and geometry.")
      }
      warning(id, " is undefined on a reduced space; omitted from the shift model.")
      return(NA_real_)
    }
    unname(value)
  }
}

.disparity_pairwise <- function(mat, groups, ids, method = "permutation",
                                permutations = 999L, adjustment = "BH") {
  .disparity_ids(ids)
  if (length(method) != 1L || !method %in% c("permutation", "wilcoxon")) {
    stop("Choose permutation or wilcoxon for pairwise tests.")
  }
  if (!adjustment %in% stats::p.adjust.methods) stop("Unknown p-value adjustment.")
  permutations <- .disparity_integer(permutations, "Permutations", 1L)
  groups <- as.character(groups)
  if (length(groups) != nrow(mat) || anyNA(groups) ||
      length(unique(groups)) < 2L || any(table(groups) < 2L)) {
    stop("Pairwise tests require at least two groups with two specimens each.")
  }
  pairs <- utils::combn(unique(groups), 2L, simplify = FALSE)
  rows <- lapply(ids, function(id) {
    do.call(rbind, lapply(pairs, function(pair) {
      x <- mat[groups == pair[1], , drop = FALSE]
      y <- mat[groups == pair[2], , drop = FALSE]
      row <- data.frame(
        metric = id, method = method, group1 = pair[1], group2 = pair[2],
        N1 = nrow(x), N2 = nrow(y), observed_difference = NA_real_,
        p_value = NA_real_, p_adjusted = NA_real_, status = "ok", message = "",
        stringsAsFactors = FALSE
      )
      warnings <- character()
      tryCatch(withCallingHandlers({
        if (method == "wilcoxon" && !id %in% c("mean_centroids", "median_centroids")) {
          stop("Wilcoxon is supported only for mean/median centroid distance; use permutations for this metric.")
        }
        metric <- .disparity_metric(id)
        row$observed_difference <- metric(x) - metric(y)
        if (method == "permutation") {
          pooled <- rbind(x, y)
          null <- replicate(permutations, {
            index <- sample.int(nrow(pooled), nrow(x))
            metric(pooled[index, , drop = FALSE]) -
              metric(pooled[-index, , drop = FALSE])
          })
          p <- (1 + sum(abs(null) >= abs(row$observed_difference))) / (permutations + 1)
        } else {
          dx <- dispRity::centroids(x)
          dy <- dispRity::centroids(y)
          if (length(unique(c(dx, dy))) == 1L) {
            stop("Wilcoxon is undefined: all centroid distances are identical.")
          }
          p <- stats::wilcox.test(dx, dy, exact = FALSE)$p.value
          if (!is.finite(p)) stop("Wilcoxon did not return a finite p-value.")
        }
        row$p_value <- p
        row$message <- paste(unique(warnings), collapse = "; ")
        row
      }, warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }), error = function(e) {
        row$status <- "failed"
        row$message <- paste(unique(c(warnings, conditionMessage(e))), collapse = "; ")
        row
      })
    }))
  })
  result <- do.call(rbind, rows)
  ok <- result$status == "ok"
  result$p_adjusted[ok] <- stats::p.adjust(result$p_value[ok], method = adjustment)
  result$adjustment <- adjustment
  result$permutations <- if (method == "permutation") permutations else NA_integer_
  result
}

.disparity_groups <- function(mat, groups, ids, rarefy = FALSE, replicates = 200L) {
  .disparity_ids(ids)
  groups <- as.character(groups)
  if (length(groups) != nrow(mat) || anyNA(groups) ||
      length(unique(groups)) < 2L || any(table(groups) < 2L)) {
    stop("Select at least two groups with at least two finite specimens each.")
  }
  replicates <- .disparity_integer(replicates, "Rarefaction replicates", 1L)
  min_n <- min(table(groups))
  rows <- lapply(unique(groups), function(group) {
    sub <- mat[groups == group, , drop = FALSE]
    do.call(rbind, lapply(ids, function(id) {
      row <- data.frame(Group = group, N = nrow(sub), metric = id, observed = NA_real_,
                        rarefied = NA_real_, rarefaction_n = if (rarefy) min_n else NA_integer_,
                        status = "ok", message = "", stringsAsFactors = FALSE)
      warnings <- character()
      tryCatch(withCallingHandlers({
        metric <- .disparity_metric(id)
        row$observed <- metric(sub)
        if (rarefy) {
          row$rarefied <- if (nrow(sub) == min_n) row$observed else {
            mean(replicate(replicates, metric(sub[sample.int(nrow(sub), min_n), , drop = FALSE])))
          }
        }
        row$message <- paste(unique(warnings), collapse = "; ")
        row
      }, warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }), error = function(e) {
        row$status <- "failed"
        row$message <- paste(unique(c(warnings, conditionMessage(e))), collapse = "; ")
        row
      })
    }))
  })
  do.call(rbind, rows)
}

.disparity_screen <- function(mat, ids, shifts = c("random", "size", "density", "position"),
                              replicates = 3L, steps = 10L, max_n = 200L,
                              max_random_r2 = 0.1, min_target_r2 = 0.7, seed = 42L) {
  .disparity_ids(ids)
  replicates <- .disparity_integer(replicates, "Screening replicates", 2L)
  steps <- .disparity_integer(steps, "Screening steps", 3L)
  max_n <- .disparity_integer(max_n, "Screening sample cap", 10L)
  seed <- .disparity_integer(seed, "Random seed", 0L)
  if (!length(shifts) || anyNA(shifts) || anyDuplicated(shifts) ||
      !all(shifts %in% c("random", "size", "density", "position", "evenness"))) {
    stop("Select one or more distinct supported shifts.")
  }
  if (!is.matrix(mat) || !is.numeric(mat) || any(!is.finite(mat)) || nrow(mat) < 10L) {
    stop("Screening requires at least ten finite specimens.")
  }
  for (value in list(max_random_r2, min_target_r2)) {
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value) || value < 0 || value > 1) {
      stop("R-squared thresholds must be between zero and one.")
    }
  }
  if (nrow(mat) > max_n) {
    mat <- shiny::repeatable(function() mat[sample.int(nrow(mat), max_n), , drop = FALSE],
                             seed = seed)()
  }
  catalogue <- .disparity_catalogue()
  rows <- lapply(ids, function(id) {
    warnings <- character()
    previous <- getOption("warn")
    on.exit(options(warn = previous), add = TRUE)
    tryCatch(withCallingHandlers({
      metric <- .disparity_metric(id)
      metric(mat)
      metric <- .disparity_metric(id, omit_undefined = TRUE)
      if (catalogue$primitive[match(id, catalogue$metric)] %in% c("convhull.volume", "convhull.surface")) {
        hull_metric <- metric
        metric <- function(matrix) {
          if (nrow(matrix) <= ncol(matrix)) {
            warning("Hull reduction has fewer than dimensions + 1 points; omitted from the shift model.")
            return(NA_real_)
          }
          hull_metric(matrix)
        }
      }
      # Use the same sampled space and simulated reductions for every metric.
      result <- shiny::repeatable(function() {
        dispRity::test.metric(mat, metric = metric, shifts = shifts,
                             replicates = replicates, steps = steps, verbose = FALSE)
      }, seed = seed)()
      if (!length(result$models)) stop("No fitted shift models returned.")
      do.call(rbind, lapply(names(result$models), function(shift) {
        fit <- result$models[[shift]]
        details <- summary(fit)
        coefficients <- details$coefficients
        if (!"reduction" %in% rownames(coefficients)) stop("No estimable reduction slope.")
        valid <- is.finite(details$r.squared) && is.finite(details$adj.r.squared)
        data.frame(
          metric = id, shift = shift, r_squared = details$r.squared,
          adjusted_r_squared = details$adj.r.squared,
          slope = coefficients["reduction", "Estimate"],
          p_value = coefficients["reduction", "Pr(>|t|)"], n_model_points = stats::nobs(fit),
          screening_n = nrow(mat), status = if (valid) "ok" else "undefined_r2",
          message = paste(unique(c(warnings, if (!valid) "Undefined R-squared.")), collapse = "; "),
          stringsAsFactors = FALSE
        )
      }))
    }, warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }), error = function(e) {
      data.frame(metric = id, shift = NA_character_, r_squared = NA_real_,
                 adjusted_r_squared = NA_real_, slope = NA_real_, p_value = NA_real_,
                 n_model_points = NA_integer_, screening_n = nrow(mat), status = "failed",
                 message = paste(unique(c(warnings, conditionMessage(e))), collapse = "; "),
                 stringsAsFactors = FALSE)
    })
  })
  result <- do.call(rbind, rows)
  result$category <- catalogue$category[match(result$metric, catalogue$metric)]
  result$meets_criteria <- vapply(result$metric, function(id) {
    sub <- result[result$metric == id & !is.na(result$shift), ]
    r2 <- function(shift) {
      value <- sub$r_squared[match(shift, sub$shift)]
      if (length(value)) value else NA_real_
    }
    category <- catalogue$category[match(id, catalogue$metric)]
    required <- switch(category,
      size = c("random", "size.increase", "size.hollowness"),
      density = c("random", "size.increase"),
      position = c("random", "size.increase"),
      character()
    )
    if (anyNA(r2(required))) return(NA)
    target <- switch(category,
      size = r2("size.increase") >= min_target_r2 & r2("size.hollowness") < r2("size.increase"),
      density = {
        values <- r2(c("density.higher", "density.lower"))
        if (all(is.na(values))) return(NA)
        best <- max(values, na.rm = TRUE)
        best >= min_target_r2 & best > r2("size.increase")
      },
      position = {
        values <- r2(c("position.top", "position.bottom"))
        if (all(is.na(values))) return(NA)
        best <- max(values, na.rm = TRUE)
        best >= min_target_r2 & best > r2("size.increase")
      },
      NA
    )
    if (any(sub$status != "ok") || !nrow(sub)) return(NA)
    r2("random") <= max_random_r2 & target
  }, logical(1))
  result
}
