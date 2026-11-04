colour_group_data <- function() {
  data <- data.frame(
    x = c(0, 1, 0, 0, 2, 3, 2, 2, NA),
    y = c(0, 0, 1, 0, 2, 2, 3, 2, 0),
    z = c(0, 0, 0, 1, 2, 2, 2, 3, 0),
    genus = rep(c("A", "B", "filtered"), c(4, 4, 1)),
    region = c(rep("East", 4), rep("West", 4), "not exported"),
    subfamily = c("One", "One", NA, "", "Two", "Two", "Two", "Two", "not exported")
  )
  data$shape <- rep(list(matrix(c(0, 1, 0, 0, 0, 1), ncol = 2)), nrow(data))
  data
}

colour_group_plot <- function(data = colour_group_data(), ...) {
  shape_plot(data, "x", "y", z_col = "z", group_col = "genus",
             verbose = FALSE, ...)
}

test_that("additional colour modes include matching points, hulls and row keys", {
  p <- plotly::plotly_build(colour_group_plot(features = list(
    colour_groups = list(columns = c("genus", "region", "region", "subfamily")),
    hulls_3d = list(show = TRUE, groups = "A", wireframe = TRUE, opacity = 0.4)
  )))
  options <- p$jsHooks$render[[1]]$data
  expect_equal(options$base_label, "genus")
  expect_equal(vapply(options$modes, `[[`, character(1), "label"), c("region", "subfamily"))
  region <- options$modes[[1]]$traces
  expect_equal(vapply(region, `[[`, character(1), "name"), c("East", "West", "East hull", "West hull"))
  expect_equal(as.integer(region[[1]]$customdata), 1:4)
  expect_equal(as.integer(region[[2]]$customdata), 5:8)
  expect_equal(as.character(region[[3]]$facecolor), rep(region[[1]]$marker$color, 4))
  expect_equal(as.character(region[[4]]$facecolor), rep(region[[2]]$marker$color, 4))
  expect_equal(region[[3]]$opacity, 0.4)
  expect_true(region[[3]]$contour$x$show)
  expect_true(all(vapply(p$x$data, function(trace) {
    identical(trace$meta$shape_toolkit_grouping, "group")
  }, logical(1))))
  expect_equal(as.integer(p$x$data[[2]]$customdata), 5:8)
  expect_false(any(grepl("not exported", unlist(options), fixed = TRUE)))
})

test_that("switching modes preserves the displayed specimen subset and viewer keys", {
  p <- plotly::plotly_build(colour_group_plot(
    group_vals = "B", features = list(
      specimen_viewer = list(show = TRUE),
      colour_groups = list(columns = "region")
    )
  ))
  expect_length(p$jsHooks$render, 2)
  expect_length(p$jsHooks$render[[1]]$data, 4)
  expect_equal(as.integer(p$x$data[[1]]$customdata), 1:4)
  mode <- p$jsHooks$render[[2]]$data$modes[[1]]
  expect_length(mode$traces, 1)
  expect_equal(mode$traces[[1]]$name, "West")
  expect_equal(as.integer(mode$traces[[1]]$customdata), 1:4)
  expect_null(mode$traces[[1]]$shape)
  expect_false(any(grepl("East", unlist(mode), fixed = TRUE)))
})

test_that("missing grouping labels are explicit and cannot collide with real values", {
  data <- colour_group_data()
  data$subfamily[1] <- "(Missing)"
  p <- colour_group_plot(data, features = list(colour_groups = list(columns = "subfamily")))
  traces <- p$jsHooks$render[[1]]$data$modes[[1]]$traces
  expect_equal(vapply(traces, `[[`, character(1), "name"),
               c("(Missing)", "One", "(Missing)*", "Two"))
  expect_equal(as.integer(traces[[3]]$customdata), 3:4)
  expect_equal(as.integer(traces[[1]]$customdata), 1L)
})

test_that("gradient originals remain available and extra modes are categorical", {
  p <- colour_group_plot(
    gradient = list(col = "z", low = "#000000", high = "#ffffff"),
    features = list(colour_groups = list(columns = c("genus", "region")))
  )
  options <- p$jsHooks$render[[1]]$data
  expect_equal(options$base_label, "Gradient: z")
  expect_length(options$modes, 2)
  expect_false(isTRUE(options$modes[[1]]$traces[[1]]$marker$showscale))
})

test_that("missing original groups cannot create phantom specimen traces", {
  data <- colour_group_data()
  data$genus[9] <- NA
  p <- plotly::plotly_build(colour_group_plot(data, features = list(
    colour_groups = list(columns = "region")
  )))
  expect_length(p$x$data, 2)
  expect_equal(sort(unlist(lapply(p$x$data, function(trace) as.integer(trace$customdata)))), 1:8)
  mode <- p$jsHooks$render[[1]]$data$modes[[1]]
  expect_equal(sort(unlist(lapply(mode$traces, function(trace) as.integer(trace$customdata)))), 1:8)
})

test_that("ungrouped plots can opt into additional colouring modes", {
  p <- shape_plot(colour_group_data(), "x", "y", z_col = "z",
                  features = list(colour_groups = list(columns = "region")),
                  verbose = FALSE)
  expect_equal(p$jsHooks$render[[1]]$data$base_label, "Original colours")
  expect_equal(p$jsHooks$render[[1]]$data$modes[[1]]$label, "region")
})

test_that("additional modes retain alpha hull geometry settings", {
  p <- colour_group_plot(features = list(
    colour_groups = list(columns = "region"),
    hulls_3d = list(show = TRUE, hull_type = "alpha", alpha_value = 1, opacity = 0.2)
  ))
  traces <- p$jsHooks$render[[1]]$data$modes[[1]]$traces
  expect_equal(vapply(traces[3:4], `[[`, character(1), "name"),
               c("East alpha hull", "West alpha hull"))
  expect_equal(traces[[3]]$opacity, 0.2)
})

test_that("invalid colouring mode requests fail explicitly", {
  for (columns in list(NA_character_, "", 42, "absent", "shape")) {
    expect_error(colour_group_plot(features = list(colour_groups = list(columns = columns))),
                 "column")
  }
  expect_error(colour_group_plot(features = list(colour_groups = TRUE)), "must be a list")
  expect_error(shape_plot(colour_group_data(), "x", "y",
                          features = list(colour_groups = list(columns = "region"))),
               "3D")
  p <- colour_group_plot(features = list(colour_groups = list(columns = "genus")))
  expect_null(p$jsHooks$render)
})

test_that("Shiny passes selected grouping columns into the exported plot", {
  data <- colour_group_data()
  shiny::testServer(plotting_server, args = list(data_reactive = shiny::reactive(data)), {
    session$setInputs(
      mode_3d = TRUE, x_col = "x", y_col = "y", z_col = "z",
      group_col = "genus", group_vals = c("A", "B"),
      colour_group_cols = c("region", "subfamily"), gradient_col = "",
      title = "", x_label = "", y_label = "", x_adjust = "", y_adjust = "",
      gradient_legend_title = ""
    )
    session$setInputs(render = 1)
    expect_s3_class(plot_obj(), "plotly")
    modes <- plot_obj()$jsHooks$render[[1]]$data$modes
    expect_equal(vapply(modes, `[[`, character(1), "label"), c("region", "subfamily"))
  })
})

test_that("export includes offline grouping data and a working browser handler", {
  file <- tempfile(fileext = ".html")
  on.exit(unlink(c(file, sub("\\.html$", "_files", file)), recursive = TRUE), add = TRUE)
  p <- colour_group_plot(features = list(
    colour_groups = list(columns = c("region", "subfamily")),
    hulls_3d = list(show = TRUE)
  ))
  htmlwidgets::saveWidget(p, file, selfcontained = FALSE)
  html <- paste(readLines(file, warn = FALSE), collapse = "\n")
  expect_match(html, "Colour by", fixed = TRUE)
  expect_match(html, "West hull", fixed = TRUE)
  expect_match(html, "Plotly.react", fixed = TRUE)
  node <- Sys.which("node")
  if (nzchar(node)) {
    result <- system2(node, c(shQuote(testthat::test_path("colour-groups-switch.cjs")),
                             shQuote(file)), stdout = TRUE, stderr = TRUE)
    expect_null(attr(result, "status"), info = paste(result, collapse = "\n"))
  }
})
