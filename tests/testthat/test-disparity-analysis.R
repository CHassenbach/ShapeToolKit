disparity_test_data <- function(n = 30L) {
  set.seed(57)
  data.frame(PC1 = stats::rnorm(n), PC2 = stats::rnorm(n),
             group = rep(c("A", "B", "C"), length.out = n))
}

test_that("the complete coordinate catalogue matches dispRity primitives and reducers", {
  catalogue <- .disparity_catalogue()
  expect_equal(nrow(catalogue), 73L)
  expect_equal(anyDuplicated(catalogue$metric), 0L)
  expect_setequal(unlist(.disparity_choices(), use.names = FALSE), catalogue$metric)
  data <- disparity_test_data(60)
  mat <- as.matrix(data[1:2])
  reducers <- list(mean = mean, median = stats::median, sum = sum,
                   prod = prod, mode = dispRity::mode.val)
  for (i in seq_len(nrow(catalogue))) {
    row <- catalogue[i, ]
    expected <- getExportedValue("dispRity", row$primitive)(mat)
    if (nzchar(row$reducer)) expected <- reducers[[row$reducer]](expected)
    if (is.finite(expected)) {
      expect_equal(.disparity_metric(row$metric)(mat), unname(expected), info = row$metric)
    } else {
      expect_error(.disparity_metric(row$metric)(mat), "finite scalar")
    }
  }
  expect_equal(.disparity_metric("sum_variances")(mat), sum(diag(stats::var(mat))))
  expect_equal(.disparity_metric("sum_ranges")(mat),
               sum(apply(mat, 2, function(x) diff(range(x)))))
  expect_equal(.disparity_metric("mean_pairwise_dist")(mat), mean(stats::dist(mat)))
  distances <- sqrt(rowSums(sweep(mat, 2, colMeans(mat))^2))
  expect_equal(.disparity_metric("median_centroids")(mat), stats::median(distances))
  expect_equal(.disparity_metric("mean_centroids")(mat), mean(distances))
  expect_error(.disparity_ids(character()), "Select at least")
  expect_error(.disparity_ids(c("sum_variances", "sum_variances")), "distinct")
  expect_error(.disparity_ids("ancestral.dist"), "catalogue")
})

test_that("coordinates retain original axes and explicitly exclude non-finite rows", {
  data <- disparity_test_data()
  data$PC1[1:3] <- c(NA, Inf, -Inf)
  result <- .disparity_coordinates(data, c("PC1", "PC2"))
  expect_equal(result$removed, 3L)
  expect_equal(result$matrix, as.matrix(data[4:30, 1:2]))
  expect_error(.disparity_coordinates(data, c("PC1", "group")), "numeric")
  expect_error(.disparity_coordinates(data, "PC1"), "at least two")
  expect_error(.disparity_coordinates(data, c("PC1", "missing")), "numeric")
})

test_that("group summaries compute every metric and rarefy without replacement", {
  data <- disparity_test_data()
  data <- data[-c(1, 2), ]
  mat <- as.matrix(data[1:2])
  ids <- c("sum_variances", "median_centroids")
  set.seed(9)
  result <- .disparity_groups(mat, data$group, ids, rarefy = TRUE, replicates = 4)
  expect_equal(nrow(result), 6L)
  expect_true(all(result$status == "ok"))
  expect_true(all(result$rarefaction_n == 9))
  smallest <- result$N == 9
  expect_equal(result$observed[smallest], result$rarefied[smallest])
  set.seed(9)
  expected <- unlist(lapply(unique(data$group), function(group) {
    sub <- mat[data$group == group, , drop = FALSE]
    vapply(ids, function(id) {
      metric <- .disparity_metric(id)
      if (nrow(sub) == 9) metric(sub) else
        mean(replicate(4, metric(sub[sample.int(nrow(sub), 9), , drop = FALSE])))
    }, numeric(1))
  }), use.names = FALSE)
  expect_equal(result$rarefied, expected)
  expect_error(.disparity_groups(mat[1:2, ], c("A", "B"), ids), "two finite specimens")
})

test_that("pairwise permutations use actual interleaved labels, all metrics, and plus one", {
  data <- disparity_test_data()
  mat <- as.matrix(data[1:2])
  ids <- c("sum_variances", "median_centroids")
  set.seed(12)
  result <- .disparity_pairwise(mat, data$group, ids, permutations = 19L)
  expect_equal(nrow(result), 6L)
  expect_true(all(result$status == "ok"))
  expect_true(all(result$p_value >= 1 / 20))
  expect_true(all(result$p_value <= 1))
  expect_equal(result$p_adjusted, stats::p.adjust(result$p_value, "BH"))
  expect_setequal(result$metric, ids)
  set.seed(12)
  x <- mat[data$group == "A", , drop = FALSE]
  y <- mat[data$group == "B", , drop = FALSE]
  metric <- .disparity_metric(ids[1])
  observed <- metric(x) - metric(y)
  pooled <- rbind(x, y)
  null <- replicate(19, {
    i <- sample.int(nrow(pooled), nrow(x))
    metric(pooled[i, , drop = FALSE]) - metric(pooled[-i, , drop = FALSE])
  })
  expect_equal(result$observed_difference[1], observed)
  expect_equal(result$p_value[1], (1 + sum(abs(null) >= abs(observed))) / 20)
  set.seed(12)
  reordered <- .disparity_pairwise(mat, data$group, ids, permutations = 19L)
  expect_identical(result, reordered)
  expect_error(.disparity_pairwise(mat, data$group, ids, permutations = 1.5), "integer")
})

test_that("Wilcoxon uses specimen centroid distances and reports unsupported metrics", {
  data <- disparity_test_data()
  mat <- as.matrix(data[1:2])
  result <- .disparity_pairwise(
    mat, data$group, c("median_centroids", "mean_centroids", "sum_variances"),
    method = "wilcoxon", adjustment = "holm"
  )
  expect_equal(sum(result$status == "ok"), 6L)
  failed <- result$status == "failed"
  expect_equal(result$metric[failed], rep("sum_variances", 3L))
  expect_match(result$message[failed], "only for mean/median centroid")
  expected <- stats::wilcox.test(
    dispRity::centroids(mat[data$group == "A", ]),
    dispRity::centroids(mat[data$group == "B", ]), exact = FALSE
  )$p.value
  expect_equal(result$p_value[1], expected)
  expect_equal(result$p_value[1:3], result$p_value[4:6])
  expect_equal(result$p_adjusted[!failed], stats::p.adjust(result$p_value[!failed], "holm"))
  constant <- matrix(rep(c(-1, 1), 4), ncol = 2)
  undefined <- .disparity_pairwise(constant, rep(c("A", "B"), each = 2),
                                  "median_centroids", method = "wilcoxon")
  expect_equal(undefined$status, "failed")
  expect_match(undefined$message, "identical")
})

test_that("invalid geometry is explicit and does not erase successful metrics", {
  mat <- cbind(1:8, 2 * (1:8), 3 * (1:8))
  result <- .disparity_groups(mat, rep(c("A", "B"), each = 4),
                              c("convhull_volume", "sum_variances"))
  expect_true(all(result$status[result$metric == "convhull_volume"] == "failed"))
  expect_true(all(nzchar(result$message[result$status == "failed"])))
  expect_true(all(result$status[result$metric == "sum_variances"] == "ok"))
  data <- disparity_test_data()
  data$PC1[data$group == "C"] <- 0
  data$PC2[data$group == "C"] <- 0
  pairs <- .disparity_pairwise(as.matrix(data[1:2]), data$group,
                               "convhull_volume", permutations = 3)
  expect_equal(pairs$status[1], "ok")
  expect_true(all(pairs$status[-1] == "failed"))
  expect_equal(pairs$group2[-1], c("C", "C"))
})

test_that("screening returns raw model statistics, criteria and reproducible samples", {
  data <- disparity_test_data(90)
  mat <- as.matrix(data[1:2])
  old_warn <- getOption("warn")
  result <- .disparity_screen(mat, c("median_centroids", "sum_variances"),
                              replicates = 2, steps = 3, max_n = 60, seed = 12)
  expect_identical(getOption("warn"), old_warn)
  expect_equal(nrow(result), 14L)
  expect_true(all(result$screening_n == 60L))
  expect_true(all(result$n_model_points == 6L))
  expect_true(all(result$status == "ok"))
  expect_setequal(unique(result$shift),
                  c("random", "size.increase", "size.hollowness", "density.higher",
                    "density.lower", "position.top", "position.bottom"))
  again <- .disparity_screen(mat, rev(c("median_centroids", "sum_variances")),
                             replicates = 2, steps = 3, max_n = 60, seed = 12)
  expect_equal(result$r_squared, again$r_squared[c(8:14, 1:7)])
  sampled <- shiny::repeatable(function() mat[sample.int(nrow(mat), 60), ], seed = 12)()
  raw <- shiny::repeatable(function() {
    dispRity::test.metric(sampled, metric = .disparity_metric("median_centroids"),
                         shifts = c("random", "size", "density", "position"),
                         replicates = 2, steps = 3)
  }, seed = 12)()
  fit <- summary(raw$models$size.increase)
  row <- result[result$metric == "median_centroids" & result$shift == "size.increase", ]
  expect_equal(row$r_squared, fit$r.squared)
  expect_equal(row$adjusted_r_squared, fit$adj.r.squared)
  expect_equal(row$slope, unname(fit$coefficients["reduction", "Estimate"]))
  for (id in unique(result$metric)) {
    sub <- result[result$metric == id, ]
    r2 <- stats::setNames(sub$r_squared, sub$shift)
    expected <- r2[["random"]] <= 0.1 && r2[["size.increase"]] >= 0.7 &&
      r2[["size.hollowness"]] < r2[["size.increase"]]
    expect_true(all(sub$meets_criteria == expected))
  }
  categories <- .disparity_screen(
    mat, c("mean_neighbours", "mean_displacements", "roundness"),
    replicates = 2, steps = 3, max_n = 60
  )
  expect_true(all(categories$status == "ok"))
  for (category in c("density", "position")) {
    sub <- categories[categories$category == category, ]
    r2 <- stats::setNames(sub$r_squared, sub$shift)
    target <- if (category == "density") max(r2[c("density.higher", "density.lower")]) else
      max(r2[c("position.top", "position.bottom")])
    expected <- r2[["random"]] <= 0.1 && target >= 0.7 && target > r2[["size.increase"]]
    expect_true(all(sub$meets_criteria == expected))
  }
  expect_true(all(is.na(categories$meets_criteria[categories$category == "other"])))
  incomplete <- .disparity_screen(mat, "median_centroids", shifts = "size",
                                  replicates = 2, steps = 3, max_n = 60)
  expect_true(all(is.na(incomplete$meets_criteria)))
  failed <- .disparity_screen(cbind(1:30, 1:30), c("convhull_volume", "sum_variances"),
                              shifts = "size", replicates = 2, steps = 3, max_n = 30)
  expect_equal(failed$status[failed$metric == "convhull_volume"], "failed")
  expect_true(all(failed$status[failed$metric == "sum_variances"] == "ok"))
  expect_error(.disparity_screen(mat, "sum_variances", shifts = character()), "shifts")
  expect_error(.disparity_screen(mat, "sum_variances", min_target_r2 = 2), "thresholds")
})

test_that("default screening warns about undefined reductions without losing valid models", {
  set.seed(11)
  data <- data.frame(PC1 = stats::rnorm(60), PC2 = stats::rnorm(60))
  mat <- .disparity_coordinates(data, names(data))$matrix
  ids <- c("sum_variances", "ellipsoid_volume", "median_centroids", "mean_centroids")
  result <- .disparity_screen(mat, ids)
  expect_equal(nrow(result), 28L)
  expect_true(all(result$status == "ok"))
  reduced <- result$metric %in% c("sum_variances", "ellipsoid_volume")
  expect_true(all(grepl("undefined on a reduced space", result$message[reduced])))
  expect_true(any(result$n_model_points[reduced] < 30L))
  expect_true(all(is.finite(result$r_squared)))
  old_warn <- getOption("warn")
  failed <- .disparity_screen(cbind(1:30, 2 * (1:30)), "convhull_volume")
  expect_equal(failed$status, "failed")
  expect_identical(getOption("warn"), old_warn)
  expect_error(.disparity_integer(2^32, "Seed", 0), "integer")
})

test_that("both explorer metric selectors start without selected metrics", {
  html <- as.character(data_explorer_ui("empty"))
  for (id in c("disp_metrics", "disp_screen_metrics")) {
    pattern <- paste0('(?s)<select\\b[^>]*id="empty-', id, '"[^>]*>.*?</select>')
    control <- regmatches(html, regexpr(pattern, html, perl = TRUE))
    expect_length(control, 1L)
    expect_match(control, "<option")
    expect_false(grepl("<option[^>]*\\bselected\\b", control, perl = TRUE))
  }
})

test_that("the explorer wires metrics, screening, rank tests and clears stale results", {
  data <- disparity_test_data(60)
  data$PC1[1] <- NA
  shiny::testServer(data_explorer_server, args = list(data_reactive = shiny::reactive(data)), {
    session$setInputs(
      disp_cols = c("PC1", "PC2"), disp_group = "group", disp_groupvals = c("A", "B"),
      disp_metrics = c("sum_variances", "median_centroids"), disp_rarefy = TRUE,
      disp_rare_reps = 3, disp_test = TRUE, disp_test_method = "permutation",
      disp_perms = 9, disp_padj = "BH", disp_seed = 12, disp_run = 1
    )
    expect_equal(nrow(disp_table_rv()), 4L)
    expect_equal(nrow(disp_pairwise_rv()), 2L)
    expect_match(disp_test_rv(), "Excluded 1")
    session$setInputs(disp_test_method = "wilcoxon", disp_run = 2)
    expect_equal(disp_pairwise_rv()$status, c("failed", "ok"))
    session$setInputs(disp_test = FALSE, disp_run = 3)
    expect_null(disp_pairwise_rv())
    expect_match(disp_test_rv(), "disabled")
    session$setInputs(
      disp_group = "", disp_screen_metrics = "median_centroids", disp_shifts = c("random", "size"),
      disp_screen_reps = 2, disp_screen_steps = 3, disp_screen_n = 40, disp_screen_run = 1
    )
    expect_equal(nrow(disp_screen_rv()), 3L)
    expect_true(all(disp_screen_rv()$screening_n == 40))
    expect_match(disp_screen_txt_rv(), "pooled 59")
    session$setInputs(disp_group = "group", disp_groupvals = c("A", "B"),
                      disp_screen_run = 2)
    expect_match(disp_screen_txt_rv(), "pooled 39")
    session$setInputs(disp_group = "group", disp_groupvals = character(), disp_run = 4)
    expect_null(disp_table_rv())
    expect_null(disp_pairwise_rv())
    expect_match(disp_test_rv(), "Include at least one group")
    session$setInputs(disp_screen_metrics = character(), disp_screen_run = 3)
    expect_null(disp_screen_rv())
    expect_match(disp_screen_txt_rv(), "Error:")
  })
})
