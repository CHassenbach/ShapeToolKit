shape_check_images <- function() {
  work_dir <- tempfile("shape-check-")
  dir.create(work_dir)
  image <- magick::image_composite(
    magick::image_blank(50, 50, "white"), magick::image_blank(24, 30, "black"),
    offset = "+13+10"
  )
  magick::image_write(image, file.path(work_dir, "valid.JPG"), format = "jpeg")
  magick::image_write(magick::image_blank(30, 30, "white"),
                      file.path(work_dir, "blank.jpeg"), format = "jpeg")
  writeLines("not a JPEG", file.path(work_dir, "broken.jpg"))
  work_dir
}

test_that("shape checks report failures without stopping valid files", {
  skip_if_not_installed("magick")
  work_dir <- shape_check_images()
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
  report <- .check_shape_files(work_dir, norm = FALSE, start_point = "down", harmonics = 4)
  expect_equal(nrow(report), 3)
  expect_equal(report$status[report$file == "valid.JPG"], "passed")
  expect_equal(report$status[report$file == "blank.jpeg"], "failed")
  expect_equal(report$status[report$file == "broken.jpg"], "failed")
  expect_true(all(report$stage[report$status == "failed"] == "Import"))
  expect_match(report$error_message[report$file == "blank.jpeg"], "black foreground")
  expect_true(all(file.exists(report$input_path)))
  expect_equal(length(list.files(work_dir)), 3)
})

test_that("shape checks use each selected direction and EFA normalization mode", {
  skip_if_not_installed("magick")
  work_dir <- shape_check_images()
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
  for (norm in c(FALSE, TRUE)) {
    for (direction in c("down", "up", "left", "right")) {
      report <- .check_shape_files(work_dir, norm = norm, start_point = direction, harmonics = 4)
      expect_equal(report$status[report$file == "valid.JPG"], "passed")
    }
  }
})

test_that("manually selected foreground pixels are used for outline import", {
  skip_if_not_installed("magick")
  work_dir <- shape_check_images()
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
  image_path <- file.path(work_dir, "valid.JPG")
  pixels <- jpeg::readJPEG(image_path)
  if (length(dim(pixels)) == 3L) {
    pixels <- (pixels[, , 1L] + pixels[, , 2L] + pixels[, , 3L]) / 3
  }
  foreground <- which(pixels <= 0.5, arr.ind = TRUE)
  seed <- as.integer(apply(foreground, 2L, stats::median))

  outline <- import_jpg1(image_path, seed = seed)
  expect_true(is.matrix(outline))
  expect_true(nrow(outline) >= 3L)
  expect_true(all(is.finite(outline)))
  automatic_outline <- import_jpg(image_path, seed_points = list())[[1L]]
  expect_true(nrow(automatic_outline) >= 3L)
  expect_error(
    import_jpg1(image_path, seed = c(1L, 1L)),
    "not foreground"
  )

  border_image <- file.path(work_dir, "border.jpg")
  border_pixels <- matrix(1, 60, 60)
  border_pixels[10:50, 1:20] <- 0
  jpeg::writeJPEG(border_pixels, border_image, quality = 1)
  border_outline <- import_jpg1(border_image, seed = c(30L, 10L))
  expect_true(nrow(border_outline) >= 3L)
  expect_true(all(is.finite(border_outline)))

  report <- .check_shape_files(
    work_dir, norm = FALSE, start_point = "down", harmonics = 4,
    seed_points = list("valid.JPG" = seed)
  )
  expect_equal(report$status[report$file == "valid.JPG"], "passed")
})

test_that("shape checks identify collapsed outlines and EFA failures by stage", {
  skip_if_not_installed("magick")
  work_dir <- shape_check_images()
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
  check_env <- new.env(parent = environment(.check_shape_files))
  checker <- .check_shape_files
  environment(checker) <- check_env
  check_env$import_jpg1 <- function(...) cbind(seq_len(4), rep(0, 4))
  report <- checker(work_dir, norm = FALSE, start_point = "down", harmonics = 4)
  expect_equal(report$stage[report$file == "valid.JPG"], "Preprocessing")
  expect_match(report$error_message[report$file == "valid.JPG"], "collapsed")
  check_env$import_jpg1 <- function(...) cbind(c(-1, 1, 1, -1), c(-1, -1, 1, 1))
  check_env$.perform_efa <- function(...) stop("test EFA failure")
  report <- checker(work_dir, harmonics = 4)
  expect_equal(report$stage[report$file == "valid.JPG"], "EFA")
  expect_match(report$error_message[report$file == "valid.JPG"], "test EFA failure")
  check_env$.perform_efa <- function(...) list(coe = matrix(NA_real_, 1, 4))
  report <- checker(work_dir, harmonics = 4)
  expect_match(report$error_message[report$file == "valid.JPG"], "non-finite")
})

test_that("shape checks reject invalid settings and empty folders", {
  work_dir <- tempfile("shape-check-empty-")
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
  expect_error(.check_shape_files(work_dir), "No JPG/JPEG")
  expect_error(.check_shape_files(work_dir, start_point = "invalid"), "start_point")
  expect_error(.check_shape_files(work_dir, harmonics = 0), "harmonics")
})

test_that("automatic harmonic selection checks single shapes without false failures", {
  skip_if_not_installed("magick")
  work_dir <- shape_check_images()
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
  for (norm in c(FALSE, TRUE)) {
    report <- .check_shape_files(work_dir, norm = norm, start_point = "down")
    expect_equal(report$status[report$file == "valid.JPG"], "passed")
    expect_equal(report$stage[report$file == "valid.JPG"], "Complete")
    expect_equal(report$status[report$file == "blank.jpeg"], "failed")
    expect_equal(report$status[report$file == "broken.jpg"], "failed")
  }
})

test_that("harmonic calibration counts and labels actual outlines", {
  angles <- seq(0, 2 * pi, length.out = 121)[-121]
  outline <- cbind(cos(angles) + 0.1 * cos(3 * angles), sin(angles))
  for (count in c(1L, 2L, 5L)) {
    outlines <- setNames(rep(list(outline), count), paste0("specimen_", seq_len(count)))
    shapes <- Out(outlines)
    calibration <- calibrate_harmonicpower_efourier(shapes, nb.h = 12, thresh = 99, plot = FALSE)
    expect_equal(nrow(calibration$q), count)
    expect_identical(rownames(calibration$q), names(outlines))
    expect_true(all(is.finite(calibration$minh)))
  }
  selected <- calibrate_harmonicpower_efourier(
    shapes, id = c(5, 2), nb.h = 12, thresh = 99, plot = FALSE
  )
  expect_identical(rownames(selected$q), c("specimen_5", "specimen_2"))
})

test_that("automatic EFA harmonic selection is independent of specimen order", {
  angles <- seq(0, 2 * pi, length.out = 121)[-121]
  simple <- cbind(cos(angles) + 0.1 * cos(3 * angles), sin(angles))
  detailed <- cbind(cos(angles) + 0.1 * cos(9 * angles), sin(angles))
  outlines <- setNames(
    c(rep(list(simple), 3), rep(list(detailed), 3)),
    paste0("specimen_", seq_len(6))
  )
  shapes <- Out(outlines)
  reordered <- Out(outlines[c(4:6, 1:3)])
  calibrate <- function(x, ...) {
    calibrate_harmonicpower_efourier(x, thresh = 99, plot = FALSE, ...)
  }

  first_three <- calibrate(shapes, id = 1:3)
  last_three <- calibrate(shapes, id = 4:6)
  expect_false(identical(first_three$minh, last_three$minh))

  automatic <- calibrate(shapes)
  explicit <- calibrate(shapes, id = seq_along(outlines))
  permuted <- calibrate(reordered)
  expect_equal(automatic$q, explicit$q)
  expect_equal(automatic$minh, explicit$minh)
  expect_equal(automatic$minh, permuted$minh)
  expect_equal(automatic$q, permuted$q[names(outlines), , drop = FALSE])

  for (norm in c(FALSE, TRUE)) {
    original <- .perform_efa(shapes, norm, NULL, start = TRUE, verbose = FALSE)
    reordered_efa <- .perform_efa(reordered, norm, NULL, start = TRUE, verbose = FALSE)
    fixed <- .perform_efa(shapes, norm, 12, start = TRUE, verbose = FALSE)
    expect_equal(ncol(original$coe), 4 * unname(automatic$minh))
    expect_equal(original$coe, reordered_efa$coe[names(outlines), , drop = FALSE])
    expect_equal(ncol(fixed$coe), 48L)
    expect_equal(fixed$coe, efourier(shapes, nb.h = 12, norm = norm, start = TRUE)$coe)
  }
})