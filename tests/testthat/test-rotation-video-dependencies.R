test_that("rotation video dependency checks identify only missing packages", {
  for (missing in list("av", "webshot2", "chromote",
                       c("av", "webshot2", "chromote"), character())) {
    checked <- character()
    dependency_message <- .rotation_video_dependency_message
    environment(dependency_message) <- list2env(list(
      requireNamespace = function(package, quietly) {
        expect_true(quietly)
        checked <<- c(checked, package)
        !package %in% missing
      }
    ), parent = environment(dependency_message))
    message <- dependency_message()
    expect_identical(checked, c("av", "webshot2", "chromote"))
    if (length(missing) == 0L) {
      expect_null(message)
    } else {
      expect_match(message, paste(missing, collapse = ", "), fixed = TRUE)
      command <- paste0('install.packages(c(',
                        paste(sprintf('"%s"', missing), collapse = ", "), '))')
      expect_match(message, command, fixed = TRUE)
      expect_match(message, "not installed automatically", fixed = TRUE)
    }
  }
})

test_that("missing video packages fail before browser or filesystem side effects", {
  testthat::local_mocked_bindings(
    .rotation_video_dependency_message = function() {
      'Install missing optional package with install.packages(c("av"))'
    },
    .ensure_chromium = function() stop("Browser detection should not run")
  )
  folder <- tempfile("rotation-missing-deps-")
  file <- file.path(folder, "video.mp4")
  expect_error(
    .create_3d_rotation_video(NULL, file, frames_dir = folder),
    'install.packages(c("av"))', fixed = TRUE
  )
  expect_false(dir.exists(folder))
})

test_that("plotting starts without video packages and never installs them", {
  installations <- character()
  notifications <- character()
  state <- new.env(parent = emptyenv())
  state$video_packages_available <- FALSE
  state$exports <- 0L
  testthat::local_mocked_bindings(
    .rotation_video_dependency_message = function() {
      if (state$video_packages_available) return(NULL)
      paste("Rotation video export requires optional packages: av, webshot2, chromote.",
            'Install them with install.packages(c("av", "webshot2", "chromote")).')
    },
    .create_3d_rotation_video = function(...) {
      state$exports <- state$exports + 1L
    },
    showNotification = function(ui, ...) {
      notifications <<- c(notifications, ui)
      "dependency-notification"
    }
  )
  testthat::local_mocked_bindings(
    install.packages = function(pkgs, ...) {
      installations <<- c(installations, pkgs)
      stop("Packages must not be installed by this test")
    },
    .package = "utils"
  )
  shiny::testServer(plotting_server, args = list(
    data_reactive = shiny::reactive(NULL)
  ), {
    session$flushReact()
    expect_match(output$rotation_video_status$html, "Render a 3D plot first")
    plot_obj(plotly::plot_ly(x = 1, y = 1, z = 1, type = "scatter3d",
                            mode = "markers"))
    session$flushReact()
    expect_match(output$rotation_video_status$html,
                 "av, webshot2, chromote", fixed = TRUE)
    expect_false(grepl("Installing required", output$rotation_video_status$html))

    session$setInputs(generate_rotation_video = 1)
    expect_true(any(grepl("install.packages", notifications, fixed = TRUE)))
    expect_length(installations, 0)
    expect_equal(state$exports, 0L)

    state$video_packages_available <- TRUE
    session$setInputs(generate_rotation_video = 2)
    expect_equal(state$exports, 1L)
    expect_match(output$rotation_video_status$html, "Packages available", fixed = TRUE)
    expect_length(installations, 0)
  })
})

test_that("rotation export still renders and encodes both camera sequences", {
  for (package in c("av", "webshot2", "chromote")) {
    skip_if_not_installed(package)
  }
  folder <- tempfile("rotation-export-")
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  frames_dir <- file.path(folder, "frames")
  out_file <- file.path(folder, "video.mp4")
  snapshots <- character()
  cameras <- list()
  progress <- integer()
  closed <- FALSE
  testthat::local_mocked_bindings(.ensure_chromium = function() invisible(TRUE))
  testthat::local_mocked_bindings(
    ChromoteSession = list(new = function(width, height) {
      expect_equal(c(width, height), c(800, 600))
      list(close = function() closed <<- TRUE)
    }),
    .package = "chromote"
  )
  testthat::local_mocked_bindings(
    saveWidget = function(widget, file, selfcontained) {
      expect_true(selfcontained)
      cameras[[length(cameras) + 1L]] <<- widget$x$layoutAttrs[[1]]$scene$camera$eye
      writeLines("<html></html>", file)
    },
    .package = "htmlwidgets"
  )
  testthat::local_mocked_bindings(
    webshot = function(url, file, vwidth, vheight, delay) {
      expect_true(file.exists(url))
      expect_equal(c(vwidth, vheight), c(800, 600))
      snapshots <<- c(snapshots, file)
      writeLines("mock frame", file)
    },
    .package = "webshot2"
  )
  testthat::local_mocked_bindings(
    av_encode_video = function(input, output, framerate) {
      expect_identical(input, snapshots)
      expect_true(all(file.exists(input)))
      expect_equal(framerate, 24)
      writeLines("mock video", output)
    },
    .package = "av"
  )
  plot <- plotly::plot_ly(x = 1, y = 1, z = 1, type = "scatter3d",
                          mode = "markers")
  result <- .create_3d_rotation_video(
    plot, out_file, frames_dir = frames_dir, n_frames = 2L, axis = "both",
    progress_cb = function(i, n) {
      expect_equal(n, 4)
      progress <<- c(progress, i)
    }
  )
  expect_identical(result, out_file)
  expect_true(file.exists(out_file))
  expect_length(snapshots, 4)
  expect_identical(progress, 1:4)
  expect_equal(cameras[[1]], list(x = 2.5, y = 0, z = 1.25))
  expect_equal(cameras[[3]], list(x = 0, y = 1.25, z = -2.5),
               tolerance = 1e-10)
  expect_true(closed)
})
