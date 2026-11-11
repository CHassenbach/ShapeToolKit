visibility_test_plot <- function(...) {
  data <- data.frame(
    x = c(0, 1, 0, 0, 2, 3, 2, 2),
    y = c(0, 0, 1, 0, 2, 2, 3, 2),
    z = c(0, 0, 0, 1, 2, 2, 2, 3),
    group = rep(c("A", "B"), each = 4)
  )
  shape_plot(data, "x", "y", z_col = "z", group_col = "group",
             verbose = FALSE, ...)
}

expect_visibility_controls <- function(plot) {
  menu <- plot$x$layout$updatemenus[[1]]
  expect_identical(menu$type, "buttons")
  expect_false(menu$showactive)
  expect_equal(vapply(menu$buttons, `[[`, character(1), "label"),
               c("Clear all", "Show all"))
  expect_true(all(vapply(menu$buttons, function(button) {
    identical(button$method, "restyle") && length(button$args) == 1L
  }, logical(1))))
  expect_identical(menu$buttons[[1]]$args[[1]]$visible, "legendonly")
  expect_true(menu$buttons[[2]]$args[[1]]$visible)
}

test_that("3D visibility buttons target every trace, including later overlays", {
  p <- visibility_test_plot(features = list(
    hulls_3d = list(show = TRUE, wireframe = TRUE)
  ))
  p <- plotly::add_surface(
    p, x = c(0, 1), y = c(0, 1), z = matrix(0, 2, 2),
    name = "Gap surface", inherit = FALSE
  )
  built <- plotly::plotly_build(p)
  expect_visibility_controls(built)
  types <- vapply(built$x$data, `[[`, character(1), "type")
  expect_true(all(c("scatter3d", "mesh3d", "surface") %in% types))
  expect_equal(built$x$layout$scene$xaxis$title, "x")
  expect_equal(built$x$layout$scene$yaxis$title, "y")
  expect_equal(built$x$layout$scene$zaxis$title, "z")
})

test_that("gradient and reconstruction plots retain visibility controls", {
  gradient <- plotly::plotly_build(visibility_test_plot(
    gradient = list(col = "z", low = "#000000", high = "#ffffff")
  ))
  expect_visibility_controls(gradient)
  expect_true(gradient$x$data[[1]]$marker$showscale)

  reconstruction <- visibility_test_plot(
    interactive = TRUE, pca_model = list(rotation = diag(3))
  )
  expect_true(any(vapply(reconstruction$x$attrs, function(trace) {
    identical(trace$name, "selection")
  }, logical(1))))
  expect_warning(reconstruction <- plotly::plotly_build(reconstruction),
                 "Ignoring 1 observations")
  expect_visibility_controls(reconstruction)
  expect_equal(reconstruction$x$data[[1]]$name, "morphospace_3d")
  expect_equal(reconstruction$x$data[[1]]$marker$opacity, 0)
})

test_that("exported HTML includes standalone visibility buttons", {
  file <- tempfile(fileext = ".html")
  on.exit(unlink(c(file, sub("\\.html$", "_files", file)), recursive = TRUE),
          add = TRUE)
  htmlwidgets::saveWidget(visibility_test_plot(), file, selfcontained = FALSE)
  html <- paste(readLines(file, warn = FALSE), collapse = "\n")
  expect_match(html, '"label":"Clear all"', fixed = TRUE)
  expect_match(html, '"label":"Show all"', fixed = TRUE)
  expect_match(html, '"visible":"legendonly"', fixed = TRUE)
})
