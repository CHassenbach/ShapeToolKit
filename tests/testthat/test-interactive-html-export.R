html_export_fixture <- function() {
  data <- data.frame(x = c(0, 1, 0, 0), y = c(0, 0, 1, 0),
                     z = c(0, 0, 0, 1), group = c("A", "A", "B", "B"))
  shape_plot(data, "x", "y", z_col = "z", group_col = "group",
             features = list(specimen_viewer = list(show = TRUE, shape_col = ""),
                             colour_groups = list(columns = "group")),
             verbose = FALSE)
}

test_that("folder exports are complete UTF-8 HTML with intact dependencies", {
  folder <- tempfile("html-export-")
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  file <- file.path(folder, "plot.html")
  .save_interactive_html(html_export_fixture(), file, selfcontained = FALSE)
  bytes <- readBin(file, "raw", n = file.info(file)$size)
  expect_identical(bytes[1:3], as.raw(c(0xEF, 0xBB, 0xBF)))
  html <- rawToChar(bytes[-(1:3)])
  expect_match(html, "^<!DOCTYPE html>")
  expect_match(html, '<meta charset="utf-8"', fixed = TRUE)
  expect_match(html, "</html>\\s*$")
  expect_false(grepl("header-include:", html, fixed = TRUE))
  expect_match(html, "specimen_map", fixed = TRUE)
  assets <- regmatches(html, gregexpr('(?:src|href)="plot_files/[^"]+"', html, perl = TRUE))[[1]]
  expect_gt(length(assets), 0)
  paths <- sub('^[^=]+="([^"]+)"$', "\\1", assets)
  expect_true(all(file.exists(file.path(folder, paths))))
  .save_interactive_html(html_export_fixture(), file, selfcontained = FALSE)
  expect_true(all(file.exists(file.path(folder, paths))))
  expect_equal(sort(list.files(folder)), sort(c("plot_files", "plot.html")))
})

test_that("missing Pandoc cannot overwrite an existing export", {
  skip_if(requireNamespace("rmarkdown", quietly = TRUE) && rmarkdown::pandoc_available())
  file <- tempfile(fileext = ".html")
  on.exit(unlink(file), add = TRUE)
  writeLines("previous valid export", file)
  expect_error(.save_interactive_html(html_export_fixture(), file), "uncheck")
  expect_equal(readLines(file), "previous valid export")
})

test_that("failed or incomplete conversion never publishes intermediate Markdown", {
  folder <- tempfile("html-export-failure-")
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  file <- file.path(folder, "plot.html")
  writeLines("previous valid export", file)
  testthat::local_mocked_bindings(
    saveWidget = function(widget, file, ...) writeLines(c("---", "title: plotly", "---"), file),
    .package = "htmlwidgets"
  )
  expect_error(.save_interactive_html(html_export_fixture(), file, selfcontained = FALSE),
               "complete UTF-8 HTML")
  expect_equal(readLines(file), "previous valid export")
  expect_equal(list.files(folder, all.files = TRUE, no.. = TRUE), "plot.html")
})

test_that("conversion errors preserve existing output and remove staging files", {
  folder <- tempfile("html-export-error-")
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  file <- file.path(folder, "plot.html")
  writeLines("previous valid export", file)
  testthat::local_mocked_bindings(
    saveWidget = function(widget, file, ...) {
      writeLines(c("---", "title: plotly", "---"), file)
      stop("Conversion failed")
    },
    .package = "htmlwidgets"
  )
  expect_error(.save_interactive_html(html_export_fixture(), file, selfcontained = FALSE),
               "Conversion failed")
  expect_equal(readLines(file), "previous valid export")
  expect_equal(list.files(folder, all.files = TRUE, no.. = TRUE), "plot.html")
})

test_that("single-file exports are complete when Pandoc is available", {
  skip_if_not(requireNamespace("rmarkdown", quietly = TRUE) && rmarkdown::pandoc_available())
  folder <- tempfile("html-export-single-")
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  file <- file.path(folder, "plot.html")
  .save_interactive_html(html_export_fixture(), file)
  expect_identical(readBin(file, "raw", n = 3), as.raw(c(0xEF, 0xBB, 0xBF)))
  expect_equal(list.files(folder), "plot.html")
  html <- paste(readLines(file, warn = FALSE), collapse = "\n")
  expect_false(grepl('src="plot_files/', html, fixed = TRUE))
  expect_match(html, "specimen_map", fixed = TRUE)
})
