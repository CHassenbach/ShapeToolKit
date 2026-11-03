mapping_progress_fixture <- function() {
  folder <- tempfile("mapping-progress-")
  dir.create(folder)
  file.create(file.path(folder, c("first.jpg", "failed.jpg", "last.jpg")))
  env <- new.env(parent = environment(map_shapes_to_data))
  env$.import_single_shape <- function(file_path, params, verbose) {
    if (basename(file_path) == "failed.jpg") {
      return(list(success = FALSE, shape = NULL, error = "Test import failure"))
    }
    list(success = TRUE, shape = list(file = basename(file_path)), error = NULL)
  }
  mapper <- map_shapes_to_data
  row_mapper <- .map_shapes_to_rows
  environment(mapper) <- env
  environment(row_mapper) <- env
  env$.map_shapes_to_rows <- row_mapper
  list(folder = folder, map = mapper)
}

test_that("mapping progress counts every row and preserves missing/failed positions", {
  fixture <- mapping_progress_fixture()
  on.exit(unlink(fixture$folder, recursive = TRUE), add = TRUE)
  data <- data.frame(ID = c("missing", "first", "failed", "last", "missing"))
  updates <- list()
  result <- fixture$map(data, "ID", fixture$folder, verbose = FALSE,
                       progress_callback = function(message, increment) {
                         updates[[length(updates) + 1L]] <<- list(message = message, increment = increment)
                       })
  increments <- vapply(updates, `[[`, numeric(1), "increment")
  expect_equal(sum(increments), 1)
  expect_equal(increments[increments > 0], rep(0.2, 5))
  expect_equal(cumsum(increments)[length(increments)], 1)
  messages <- vapply(updates, `[[`, character(1), "message")
  expect_match(messages[1], "Finding")
  expect_match(tail(messages, 1), "completed")
  expect_true(any(grepl("Processing row 4/5 \\(ID: last\\)", messages)))
  expect_true(any(grepl("import failed", messages)))
  expect_true(any(grepl("no matching file", messages)))
  expect_length(result$shape, 5)
  expect_null(result$shape[[1]])
  expect_equal(result$shape[[2]]$file, "first.jpg")
  expect_null(result$shape[[3]])
  expect_equal(result$shape[[4]]$file, "last.jpg")
  expect_null(result$shape[[5]])
  expect_equal(attr(result, "mapping_summary")$successful_mappings, 2)
  expect_equal(result, fixture$map(data, "ID", fixture$folder, verbose = FALSE))
})

test_that("progress completes when the shape directory has no matching files", {
  folder <- tempfile("mapping-empty-")
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  increments <- numeric()
  result <- map_shapes_to_data(data.frame(ID = c("a", "b")), "ID", folder, verbose = FALSE,
                               progress_callback = function(message, increment) {
                                 increments <<- c(increments, increment)
                               })
  expect_equal(increments, c(0, 1))
  expect_length(result$shape, 2)
})

test_that("invalid callbacks and callback errors are surfaced", {
  fixture <- mapping_progress_fixture()
  on.exit(unlink(fixture$folder, recursive = TRUE), add = TRUE)
  data <- data.frame(ID = "first")
  expect_error(fixture$map(data, "ID", fixture$folder, progress_callback = TRUE),
               "must be a function")
  expect_error(fixture$map(data, "ID", fixture$folder, verbose = FALSE,
                          progress_callback = function(message, increment) stop("Callback failed")),
               "Callback failed")
})

test_that("Map shapes shows advancing Shiny progress and closes it on completion", {
  fixture <- mapping_progress_fixture()
  on.exit(unlink(fixture$folder, recursive = TRUE), add = TRUE)
  env <- new.env(parent = environment(data_import_server))
  env$map_shapes_to_data <- fixture$map
  server <- data_import_server
  environment(server) <- env
  shiny::testServer(server, {
    session$flushReact()
    updates <- list()
    root <- session$rootScope()
    original <- root$sendProgress
    root$sendProgress <- function(type, message) {
      updates[[length(updates) + 1L]] <<- list(type = type, message = message)
      original(type, message)
    }
    working_data(data.frame(ID = c("first", "missing", "failed", "last")))
    shape_dir_path(fixture$folder)
    session$setInputs(id_col = "ID", map_shapes_btn = 1)
    expect_length(working_data()$shape, 4)
    expect_equal(mapping_summary()$successful_mappings, 2)
    types <- vapply(updates, `[[`, character(1), "type")
    expect_equal(types[1], "open")
    expect_equal(tail(types, 1), "close")
    values <- vapply(updates[types == "update"], function(update) update$message$value, numeric(1))
    expect_true(all(diff(values) >= 0))
    expect_equal(unique(values), seq(0, 1, by = 0.25))
    expect_true(any(vapply(updates, function(update) {
      identical(update$message$detail, "Processed 3/4 rows (import failed)")
    }, logical(1))))
  })
})
