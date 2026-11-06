fallback_shape_images <- function() {
  path <- tempfile("shape-fallback-")
  dir.create(path)
  centered <- matrix(1, 80, 80)
  centered[20:60, 30:50] <- 0.7
  offset <- matrix(1, 80, 80)
  offset[10:30, 10:25] <- 0.7
  jpeg::writeJPEG(centered, file.path(path, "centered.jpg"), quality = 1)
  jpeg::writeJPEG(offset, file.path(path, "offset.jpg"), quality = 1)
  jpeg::writeJPEG(offset, file.path(path, "offset2.jpg"), quality = 1)
  path
}

test_that("center-first import only requests points for background centers", {
  path <- fallback_shape_images()
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  centered <- file.path(path, "centered.jpg")
  offset <- file.path(path, "offset.jpg")
  expect_true(is.matrix(import_jpg1(
    centered, auto.notcentered = FALSE, threshold = 0.8, ask = FALSE
  )))
  expect_error(
    import_jpg1(offset, auto.notcentered = FALSE, threshold = 0.8, ask = FALSE),
    class = "shape_seed_required"
  )
  expect_true(is.matrix(import_jpg1(
    offset, auto.notcentered = FALSE, threshold = 0.8, ask = FALSE, seed = c(20, 15)
  )))
  expect_error(import_jpg1(centered, threshold = 0.5), "No usable black foreground")
  for (threshold in list(NA_real_, Inf, -0.1, 1, "0.8", c(0.5, 0.8))) {
    expect_error(import_jpg1(centered, threshold = threshold), "threshold")
  }
})

test_that("shape checks distinguish point requests from import failures", {
  path <- fallback_shape_images()
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  jpeg::writeJPEG(matrix(1, 40, 40), file.path(path, "blank.jpg"), quality = 1)
  writeLines("invalid image", file.path(path, "broken.jpg"))
  report <- .check_shape_files(
    path, norm = FALSE, harmonics = 4, threshold = 0.8, auto.notcentered = FALSE
  )
  expect_equal(report$status[report$file == "centered.jpg"], "passed")
  expect_equal(report$status[report$file %in% c("offset.jpg", "offset2.jpg")],
               rep("needs_point", 2))
  expect_equal(report$status[report$file %in% c("blank.jpg", "broken.jpg")],
               rep("failed", 2))
  report <- .check_shape_files(
    path, norm = FALSE, harmonics = 4, threshold = 0.8, auto.notcentered = FALSE,
    seed_points = list("offset.jpg" = c(20, 15), "offset2.jpg" = c(20, 15))
  )
  expect_true(all(report$status[grepl("offset", report$file)] == "passed"))
})

test_that("analysis import uses the same threshold and points as direct Momocs-style import", {
  path <- fallback_shape_images()
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  seeds <- list("offset.jpg" = c(20, 15), "offset2.jpg" = c(20, 15))
  shapes <- .import_shape_files(
    path, verbose = FALSE, seed_points = seeds, threshold = 0.8,
    auto.notcentered = FALSE
  )
  direct <- import_jpg(
    list.files(path, full.names = TRUE), auto.notcentered = FALSE,
    threshold = 0.8, seed_points = seeds, ask = FALSE
  )
  expect_equal(shapes$coo, direct)
  expect_error(.import_shape_files(
    path, verbose = FALSE, threshold = 0.8, auto.notcentered = FALSE
  ), "Select a point")
})

test_that("Shiny requests only missing points and resumes analysis after valid clicks", {
  path <- fallback_shape_images()
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  calls <- list()
  imported_files <- character(0)
  server <- shape_analysis_server
  server_env <- new.env(parent = environment(server))
  environment(server) <- server_env
  server_env$shape_analysis <- function(...) {
    calls[[length(calls) + 1L]] <<- list(...)
    NULL
  }
  server_env$.check_shape_files <- function(...) {
    stop("Run Analysis must not invoke the Shape Checks diagnostic routine.")
  }
  server_env$.read_shape_foreground <- function(path, threshold) {
    imported_files <<- c(imported_files, basename(path))
    .read_shape_foreground(path, threshold)
  }
  shiny::testServer(server, {
    session$setInputs(manual_seed = TRUE, threshold = 0.8, norm = FALSE,
                      start_point = "left", harmonics = 4, num_pcs = 3)
    shape_dir(path)
    session$flushReact()
    session$setInputs(run = 1)
    expect_equal(basename(seed_requests()), "offset.jpg")
    expect_equal(imported_files, c("centered.jpg", "offset.jpg"))
    expect_null(shape_checks())
    expect_equal(pending_action(), "run")
    expect_length(calls, 0)

    session$setInputs(seed_file = file.path(path, "offset.jpg"))
    session$setInputs(seed_image_click = list(x = "invalid", y = 61))
    expect_length(seed_points(), 0)
    session$setInputs(seed_image_click = list(x = 1, y = 80))
    expect_length(seed_points(), 0)
    session$setInputs(seed_image_click = list(x = 15, y = 61))
    expect_equal(seed_points()[["offset.jpg"]], c(20L, 15L))
    expect_equal(basename(seed_requests()), "offset2.jpg")
    expect_equal(imported_files, c("centered.jpg", "offset.jpg", "offset2.jpg"))
    expect_length(calls, 0)

    session$setInputs(seed_file = file.path(path, "offset2.jpg"))
    session$setInputs(seed_image_click = list(x = 15, y = 61))
    expect_length(seed_requests(), 0)
    expect_length(calls, 1)
    expect_equal(calls[[1]]$threshold, 0.8)
    expect_false(calls[[1]]$auto.notcentered)
    expect_length(calls[[1]]$seed_points, 2)

    session$setInputs(threshold = 0.5)
    expect_length(seed_points(), 0)
    expect_null(pending_action())
  })
})

test_that("Check Shapes remains a separate diagnostic action", {
  path <- fallback_shape_images()
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  server <- shape_analysis_server
  server_env <- new.env(parent = environment(server))
  environment(server) <- server_env
  server_env$shape_analysis <- function(...) {
    stop("Check Shapes must not run the analysis.")
  }
  shiny::testServer(server, {
    session$setInputs(manual_seed = TRUE, threshold = 0.8, norm = FALSE,
                      start_point = "left", harmonics = 4)
    shape_dir(path)
    session$flushReact()
    session$setInputs(check_shapes = 1)
    expect_equal(shape_checks()$status, c("passed", "needs_point", "needs_point"))
    expect_equal(basename(seed_requests()), c("offset.jpg", "offset2.jpg"))
    expect_equal(pending_action(), "check")
  })
})
