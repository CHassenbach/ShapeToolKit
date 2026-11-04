specimen_test_data <- function() {
  data <- data.frame(
    PC1 = c(1, 1, NA, 4), PC2 = c(2, 2, 3, 5), PC3 = c(3, 3, 4, 6),
    ID = c("duplicate", "duplicate", "filtered", "last"),
    Genus = c("A", "B", "A", "B"), Species = c("alpha", "beta", "gamma", "delta"),
    latitude = c(0, -12.5, 45, 91), longitude = c(0, 179.99, 10, 20),
    antweb_id = c("casent0001", "casent0002", "", ""),
    country = c("one", "two", "three", "four"), private_note = "not exported"
  )
  outline <- matrix(c(0, 1, 0, 0, 0, 1), ncol = 2)
  data$shape <- list(structure(list(coo = list(outline)), class = "Out"),
                     outline, NULL, matrix(NA_real_, nrow = 3, ncol = 2))
  data
}

specimen_test_options <- function() {
  list(show = TRUE, id_col = "ID", genus_col = "Genus", species_col = "Species",
       latitude_col = "latitude", longitude_col = "longitude",
       antweb_col = "antweb_id", metadata_cols = "country")
}

specimen_test_plot <- function(data = specimen_test_data(), group_col = "Genus",
                               group_vals = NULL, gradient = NULL) {
  shape_plot(data, "PC1", "PC2", z_col = "PC3", group_col = group_col,
             group_vals = group_vals, gradient = gradient,
             features = list(specimen_viewer = specimen_test_options()), verbose = FALSE)
}

test_that("viewer row keys survive grouping, filtering and duplicate coordinates and IDs", {
  p <- plotly::plotly_build(specimen_test_plot())
  expect_equal(as.integer(p$x$data[[1]]$customdata), 1L)
  expect_equal(as.integer(p$x$data[[2]]$customdata), c(2L, 3L))
  records <- p$jsHooks$render[[1]]$data
  expect_length(records, 3)
  expect_equal(records[[1]]$species, "A alpha")
  expect_equal(records[[2]]$species, "B beta")
  expect_equal(records[[3]]$id, "last")
  expect_equal(records[[1]]$shape, records[[2]]$shape)
  expect_equal(records[[1]]$latitude, 0)
  expect_equal(records[[1]]$longitude, 0)
  expect_equal(records[[2]]$latitude, -12.5)
  expect_null(records[[3]]$shape)
  expect_match(records[[3]]$shape_status, "Invalid")
  expect_null(records[[3]]$latitude)
  expect_match(records[[3]]$location_status, "Invalid")
  expect_equal(records[[2]]$metadata, list(list(label = "country", value = "two")))
  expect_false(any(grepl("not exported", unlist(records))))
})

test_that("only displayed groups are embedded, including with gradient coloring", {
  for (gradient in list(NULL, list(col = "PC3", low = "#000000", high = "#ffffff"))) {
    p <- plotly::plotly_build(specimen_test_plot(group_vals = "B", gradient = gradient))
    expect_equal(as.integer(p$x$data[[1]]$customdata), c(1L, 2L))
    expect_length(p$jsHooks$render[[1]]$data, 2)
    expect_equal(p$jsHooks$render[[1]]$data[[1]]$species, "B beta")
  }
  p <- plotly::plotly_build(specimen_test_plot(group_col = NULL))
  expect_equal(as.integer(p$x$data[[1]]$customdata), 1:3)
})

test_that("missing shapes, coordinates and species have explicit statuses", {
  data <- specimen_test_data()
  options <- specimen_test_options()
  data$shape <- list(NULL, NULL, NULL, NULL)
  data$latitude[1] <- NA
  data$Species[1] <- NA
  records <- .specimen_viewer_data(data, options, c("PC1", "PC2", "PC3"))
  expect_match(records[[1]]$shape_status, "No mapped")
  expect_match(records[[1]]$location_status, "No location")
  expect_equal(records[[1]]$species, "A")
  options$genus_col <- ""
  expect_equal(.specimen_viewer_data(data, options, "PC1")[[2]]$species, "beta")
  data$latitude <- as.character(data$latitude)
  data$latitude[2] <- "not a coordinate"
  expect_match(.specimen_viewer_data(data, options, "PC1")[[2]]$location_status, "Invalid")
})

test_that("invalid viewer mappings fail explicitly", {
  data <- specimen_test_data()
  expect_error(.specimen_viewer_data(data, list(shape_col = "missing"), "PC1"), "not found")
  expect_error(.specimen_viewer_data(data, list(shape_col = "ID"), "PC1"), "list-column")
  expect_error(.specimen_viewer_data(data, list(metadata_cols = "shape"), "PC1"), "scalar")
  expect_error(.specimen_viewer_data(data, list(id_col = NA_character_), "PC1"), "column name")
  expect_error(shape_plot(data, "PC1", "PC2", features = list(
    specimen_viewer = list(show = TRUE)), verbose = FALSE), "3D")
})

test_that("unselected viewer mappings and partial coordinates do not block plotting", {
  data <- specimen_test_data()
  data$shape <- NULL
  for (location in list(list(), list(latitude_col = "latitude"),
                        list(longitude_col = "longitude"))) {
    options <- c(list(show = TRUE, shape_col = ""), location)
    p <- shape_plot(data, "PC1", "PC2", z_col = "PC3",
                    features = list(specimen_viewer = options), verbose = FALSE)
    expect_s3_class(plotly::plotly_build(p), "plotly")
    records <- p$jsHooks$render[[1]]$data
    expect_length(records, 3)
    expect_null(records[[1]]$shape)
    expect_match(records[[1]]$shape_status, "No mapped")
    expect_match(records[[1]]$location_status, "No location")
    expect_equal(records[[1]]$id, "Specimen 1")
    expect_equal(records[[1]]$species, "")
  }
})

test_that("Shiny renders viewer with unselected columns and colouring modes", {
  data <- specimen_test_data()
  data$shape <- NULL
  shiny::testServer(plotting_server, args = list(data_reactive = shiny::reactive(data)), {
    session$setInputs(
      mode_3d = TRUE, specimen_viewer = TRUE, interactive_mode = FALSE,
      x_col = "PC1", y_col = "PC2", z_col = "PC3", group_col = "", gradient_col = "",
      colour_group_cols = "Genus", specimen_shape_col = "", specimen_id_col = "",
      specimen_genus_col = "", specimen_species_col = "", specimen_latitude_col = "latitude",
      specimen_longitude_col = "", specimen_antweb_col = "", specimen_metadata_cols = character(),
      title = "", x_label = "", y_label = "", x_adjust = "", y_adjust = "",
      gradient_legend_title = ""
    )
    session$setInputs(render = 1)
    expect_s3_class(plot_obj(), "plotly")
    expect_equal(messages(), "")
    expect_length(plot_obj()$jsHooks$render, 2)
    records <- plot_obj()$jsHooks$render[[1]]$data
    expect_match(records[[1]]$shape_status, "No mapped")
    expect_match(records[[1]]$location_status, "No location")
    expect_match(output$plot_3d, "No mapped", fixed = TRUE)
  })
})

test_that("viewer excludes reconstruction grids even when a PCA model is supplied", {
  data <- specimen_test_data()
  p <- shape_plot(data, "PC1", "PC2", z_col = "PC3", interactive = TRUE,
                  pca_model = list(rotation = diag(3)),
                  features = list(specimen_viewer = specimen_test_options()), verbose = FALSE)
  traces <- plotly::plotly_build(p)$x$data
  expect_length(traces, 1)
  expect_equal(as.integer(traces[[1]]$customdata), 1:3)
})

test_that("ordinary 3D plots retain their existing behavior", {
  data <- specimen_test_data()
  p <- shape_plot(data, "PC1", "PC2", z_col = "PC3", verbose = FALSE)
  expect_null(p$jsHooks$render)
  expect_null(plotly::plotly_build(p)$x$data[[1]]$customdata)
})

test_that("Shiny renders and exports the same browser-side specimen viewer", {
  data <- specimen_test_data()
  file <- tempfile(fileext = ".html")
  on.exit(unlink(file), add = TRUE)
  shiny::testServer(plotting_server, args = list(data_reactive = shiny::reactive(data)), {
    session$setInputs(
      mode_3d = TRUE, specimen_viewer = TRUE, interactive_mode = FALSE,
      x_col = "PC1", y_col = "PC2", z_col = "PC3", group_col = "", gradient_col = "",
      specimen_shape_col = "shape", specimen_id_col = "ID", specimen_genus_col = "Genus",
      specimen_species_col = "Species", specimen_latitude_col = "latitude",
      specimen_longitude_col = "longitude", specimen_antweb_col = "antweb_id",
      specimen_metadata_cols = "country", title = "", x_label = "", y_label = "",
      x_adjust = "", y_adjust = "", gradient_legend_title = ""
    )
    session$setInputs(render = 1)
    expect_s3_class(plot_obj(), "plotly")
    expect_equal(plot_obj()$jsHooks$render[[1]]$data[[2]]$species, "B beta")
    expect_null(hover_shape_coords())
    if (rmarkdown::pandoc_available()) {
      html_export_dir(dirname(file))
      session$setInputs(html_export_filename = tools::file_path_sans_ext(basename(file)),
                        export_html_3d = 1)
      expect_true(file.exists(file))
      expect_match(paste(readLines(file, warn = FALSE), collapse = "\n"), "B beta", fixed = TRUE)
    }
  })
})

test_that("HTML export embeds the browser handler and only selected specimen data", {
  skip_if_not(rmarkdown::pandoc_available())
  file <- tempfile(fileext = ".html")
  on.exit(unlink(file), add = TRUE)
  htmlwidgets::saveWidget(specimen_test_plot(), file, selfcontained = TRUE)
  html <- paste(readLines(file, warn = FALSE), collapse = "\n")
  expect_match(html, "plotly_click", fixed = TRUE)
  expect_match(html, "casent0002", fixed = TRUE)
  expect_match(html, "B beta", fixed = TRUE)
  expect_match(html, "Equal Earth", fixed = TRUE)
  expect_match(html, "specimen_map", fixed = TRUE)
  expect_false(grepl("openstreetmap.org",
                    specimen_test_plot()$jsHooks$render[[1]]$code, fixed = TRUE))
  expect_false(grepl("not exported", html, fixed = TRUE))
  node <- Sys.which("node")
  if (nzchar(node)) {
    result <- system2(node, c(shQuote(testthat::test_path("specimen-viewer-click.cjs")),
                             shQuote(file)), stdout = TRUE, stderr = TRUE)
    expect_null(attr(result, "status"), info = paste(result, collapse = "\n"))
  }
})
