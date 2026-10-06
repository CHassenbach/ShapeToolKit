test_that("shape_analysis works correctly", {
  # This is a placeholder test
  # You'll need to add actual tests based on your functions
  expect_true(TRUE)
})

test_that("Haug_overview works correctly", {
  # This is a placeholder test
  # You'll need to add actual tests based on your functions
  expect_true(TRUE)
})

summary_test_data <- function() {
  data.frame(
    PC1 = c(0, 2, 0, 2, 10, 12, 10, 12),
    PC2 = c(0, 0, 2, 2, 10, 10, 12, 12),
    species = rep(c("A", "B"), each = 4)
  )
}

summary_test_plot <- function(data = summary_test_data(), features = list(),
                              group_vals = NULL, group_col = "species", styling = list()) {
  styling$axis <- list(central_axes = FALSE)
  shape_plot(data, "PC1", "PC2", group_col = group_col, group_vals = group_vals,
             features = features, styling = styling, verbose = FALSE)
}

test_that("centroids use finite paired coordinates and displayed groups", {
  data <- summary_test_data()
  data <- rbind(data, data.frame(PC1 = c(NA, Inf), PC2 = c(50, 50), species = "A"))
  plot <- summary_test_plot(data, list(centroids = list(show = TRUE)), group_vals = "A")
  centroid <- plot$layers[[length(plot$layers)]]$data
  expect_equal(centroid$x, 1)
  expect_equal(centroid$y, 1)
  expect_equal(sum(vapply(plot$layers, function(layer) {
    identical(layer$aes_params$shape, 4)
  }, logical(1))), 1L)
  expect_s3_class(ggplot2::ggplot_build(plot), "ggplot_built")
})

test_that("ellipse coverage, resolution and aesthetics are configurable", {
  features <- list(
    centroids = list(show = TRUE, groups = "B", colors = c(B = "#00FF00"),
                     size = 6, shape = 23, stroke = 2),
    ellipses = list(show = TRUE, groups = "A", level = 0.8, segments = 40,
                    colors = c(A = "#FF0000"), fill = TRUE, alpha = 0.25,
                    linewidth = 1.5, linetype = "dashed")
  )
  plot <- summary_test_plot(features = features)
  ellipse <- plot$layers[[1]]
  centroid <- plot$layers[[length(plot$layers)]]
  expect_equal(ellipse$stat_params$level, 0.8)
  expect_equal(ellipse$stat_params$type, "norm")
  expect_equal(ellipse$aes_params$fill, "#FF0000")
  expect_equal(ellipse$aes_params$alpha, 0.25)
  expect_equal(ellipse$aes_params$linewidth, 1.5)
  expect_equal(ellipse$aes_params$linetype, "dashed")
  expect_equal(centroid$data$x, 11)
  expect_equal(centroid$aes_params$colour, "#00FF00")
  expect_equal(centroid$aes_params$size, 6)
  expect_equal(centroid$aes_params$shape, 23)
  expect_equal(centroid$aes_params$stroke, 2)
  built <- ggplot2::ggplot_build(plot)
  expect_equal(nrow(built$data[[1]]), 41L)
  wider <- summary_test_plot(features = list(ellipses = list(show = TRUE, groups = "A", level = 0.95)))
  expect_gt(diff(range(ggplot2::ggplot_build(wider)$data[[1]]$x)), diff(range(built$data[[1]]$x)))
})

test_that("summary colors match point colors including partial overrides", {
  plot <- summary_test_plot(
    features = list(centroids = list(show = TRUE, colors = c(A = "#FF0000")),
                    ellipses = list(show = TRUE, groups = "B")),
    styling = list(point = list(color = c(A = "#111111", B = "#222222")))
  )
  expect_equal(plot$layers[[1]]$aes_params$colour, "#222222")
  expect_equal(plot$layers[[length(plot$layers) - 1L]]$aes_params$colour, "#FF0000")
  expect_equal(plot$layers[[length(plot$layers)]]$aes_params$colour, "#222222")
  expect_warning(ggplot2::ggplot_build(plot), NA)
})

test_that("invalid ellipse parameters fail clearly", {
  for (level in c(0, 1, NA_real_)) {
    expect_error(summary_test_plot(features = list(ellipses = list(show = TRUE, level = level))), "Ellipse level")
  }
  expect_error(summary_test_plot(features = list(ellipses = list(show = TRUE, type = "other"))), "Ellipse type")
  expect_error(summary_test_plot(features = list(ellipses = list(show = TRUE, segments = 3))), "Ellipse segments")
})

test_that("small and collinear groups retain centroids but skip ellipses", {
  for (data in list(summary_test_data()[1:3, ],
                    data.frame(PC1 = 1:4, PC2 = 2 * (1:4), species = "A"))) {
    expect_warning(plot <- summary_test_plot(data, list(
      centroids = list(show = TRUE), ellipses = list(show = TRUE)
    )), "skipping ellipse")
    expect_false(any(vapply(plot$layers, function(layer) inherits(layer$stat, "StatEllipse"), logical(1))))
    expect_equal(plot$layers[[length(plot$layers)]]$data$x, mean(data$PC1))
  }
})

test_that("summaries work without grouping and respect empty selections", {
  plot <- summary_test_plot(features = list(centroids = list(show = TRUE),
                                           ellipses = list(show = TRUE)), group_col = NULL)
  expect_equal(plot$layers[[length(plot$layers)]]$data$x, 6)
  expect_equal(plot$layers[[length(plot$layers)]]$data$y, 6)
  expect_warning(ggplot2::ggplot_build(plot), NA)
  plain <- summary_test_plot()
  empty <- summary_test_plot(features = list(
    centroids = list(show = TRUE, groups = character()),
    ellipses = list(show = TRUE, groups = character())
  ))
  expect_equal(length(empty$layers), length(plain$layers))
})

test_that("categorical axes skip group summaries with a clear warning", {
  data <- summary_test_data()
  data$PC1 <- factor(data$PC1)
  expect_warning(summary_test_plot(data, list(centroids = list(show = TRUE))), "numeric axes")
})

test_that("robust t ellipses build when MASS is available", {
  skip_if_not_installed("MASS")
  data <- summary_test_data()
  data <- rbind(data, data.frame(PC1 = c(0.5, 1.2, 1.8), PC2 = c(0.8, 1.7, 0.4), species = "A"))
  plot <- summary_test_plot(data, list(ellipses = list(show = TRUE, type = "t", groups = "A")))
  expect_warning(built <- ggplot2::ggplot_build(plot), NA)
  expect_equal(nrow(built$data[[1]]), 101L)
})