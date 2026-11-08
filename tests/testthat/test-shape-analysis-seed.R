seeded_shape_images <- function(path) {
  for (i in seq_len(5L)) {
    pixels <- matrix(1, 80, 100)
    for (column in 8:(22 + i * 2)) {
      height <- 12 + i * 2 + (column %% (i + 2))
      pixels[8:(8 + height), column] <- 0
    }
    jpeg::writeJPEG(pixels, file.path(path, paste0("shape", i, ".jpg")), quality = 1)
  }
}

test_that("seeded analyses reproduce coefficients, PCA, and exported numerical data", {
  path <- tempfile("shape-seed-")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  input_path <- file.path(path, "input")
  dir.create(input_path)
  seeded_shape_images(input_path)
  set.seed(987)
  previous_seed <- .Random.seed
  on.exit(assign(".Random.seed", previous_seed, envir = globalenv()), add = TRUE)

  for (harmonics in list(4, NULL)) {
    first <- shape_analysis(
      input_path, output_dir = path, output_file = "first.xlsx",
      num_pcs = 2, harmonics = harmonics, verbose = FALSE
    )
    expect_identical(.Random.seed, previous_seed)
    runif(7)
    surrounding_seed <- .Random.seed
    second <- shape_analysis(
      input_path, output_dir = path, output_file = "second.xlsx",
      num_pcs = 2, harmonics = harmonics, verbose = FALSE
    )
    expect_identical(.Random.seed, surrounding_seed)
    expect_identical(first$efa_results$coe, second$efa_results$coe)
    expect_identical(first$pca_results$rotation, second$pca_results$rotation)
    expect_identical(first$pca_results$sdev, second$pca_results$sdev)
    expect_identical(first$scores, second$scores)
    expect_identical(first$metadata$seed, 42L)
    expect_equal(openxlsx::read.xlsx(first$output_path),
                 openxlsx::read.xlsx(second$output_path))
    expect_identical(readLines(first$reconstruction_files$coefficients),
                     readLines(second$reconstruction_files$coefficients))
    expect_true(any(grepl("Random Seed: 42", readLines(first$reconstruction_files$metadata))))
    model <- load_reconstruction_csv(first$reconstruction_files$rotation, verbose = FALSE)
    expect_identical(model$parameters$seed, 42L)
    previous_seed <- surrounding_seed
  }
})

test_that("analysis seeds are validated before filesystem work", {
  for (seed in list(NA_real_, NaN, Inf, -1, 0.5, 2147483648,
                   "42", TRUE, numeric(0), c(1, 2))) {
    expect_error(shape_analysis("nonexistent", seed = seed), "'seed'")
  }
})

test_that("selected seeds restore random state on import failure and NULL remains unseeded", {
  path <- tempfile("shape-seed-failure-")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (had_seed) original_seed <- get(".Random.seed", envir = globalenv())
  on.exit({
    if (had_seed) {
      assign(".Random.seed", original_seed, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  }, add = TRUE)
  draws <- numeric()
  analysis <- shape_analysis
  analysis_env <- new.env(parent = environment(analysis))
  environment(analysis) <- analysis_env
  analysis_env$.import_shape_files <- function(...) {
    draws <<- c(draws, runif(1))
    stop("test import failure")
  }
  set.seed(321)
  previous_seed <- .Random.seed
  for (seed in c(0, 123, 2147483647)) {
    expect_error(analysis(path, output_dir = path, verbose = FALSE, seed = seed),
                 "test import failure")
    expect_identical(.Random.seed, previous_seed)
    expect_error(analysis(path, output_dir = path, verbose = FALSE, seed = seed),
                 "test import failure")
    expect_identical(tail(draws, 2)[1], tail(draws, 2)[2])
  }
  expect_false(identical(draws[1], draws[3]))
  expect_error(analysis(path, output_dir = path, verbose = FALSE, seed = NULL),
               "test import failure")
  expect_false(identical(.Random.seed, previous_seed))
  rm(".Random.seed", envir = globalenv())
  expect_error(analysis(path, output_dir = path, verbose = FALSE),
               "test import failure")
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
})
