completion_test_image <- function(background = "white") {
  magick::image_composite(
    magick::image_blank(30, 24, background),
    magick::image_blank(10, 14, "black"),
    offset = "+8+5", operator = "over"
  )
}

test_that("completion crops white, near-white and transparent margins", {
  skip_if_not_installed("magick")
  for (background in c("white", "#FAFAFA", "none")) {
    cropped <- .crop_to_content(completion_test_image(background))
    info <- magick::image_info(cropped)
    expect_equal(info$width, 10)
    expect_equal(info$height, 14)
    expect_true(all(as.integer(as.vector(magick::image_data(cropped, "gray"))) == 0))
  }
  blank <- magick::image_blank(20, 12, "white")
  expect_equal(magick::image_info(.crop_to_content(blank)), magick::image_info(blank))
})

test_that("overlap defaults to five pixels and rejects invalid values", {
  params <- .setup_complete_shape_params(list(), list(), list(), list(), FALSE)
  expect_equal(params$completion_options$overlap, 5)
  for (overlap in list(-1, 1.5, NA_real_, Inf, c(1, 2), "5")) {
    expect_error(
      .setup_complete_shape_params(list(overlap = overlap), list(), list(), list(), FALSE),
      "'overlap' must be a single non-negative integer"
    )
  }
})

test_that("overlapping halves remove white seams and preserve symmetry", {
  skip_if_not_installed("magick")
  half <- magick::image_composite(
    magick::image_blank(10, 12, "white"),
    magick::image_blank(9, 12, "black")
  )
  for (method in c("mirror_left", "mirror_right", "both_sides")) {
    for (side in c("left", "right")) {
      options <- list(method = method, overlap = 5, blend_seam = FALSE)
      completed <- .complete_shape_image(half, options, side)
      expect_equal(magick::image_info(completed)$width, 15)
      expect_equal(magick::image_info(completed)$height, 12)
      expect_identical(magick::image_data(completed),
                       magick::image_data(magick::image_flop(completed)))
      pixels <- magick::image_data(completed, "gray")
      expect_true(all(as.integer(as.vector(pixels[, 8, ])) == 0))
      options$overlap <- 0
      unoverlapped <- .complete_shape_image(half, options, side)
      expect_equal(magick::image_info(unoverlapped)$width, 20)
      expected <- if (method == "mirror_left" || (method == "both_sides" && side == "right")) {
        magick::image_append(c(magick::image_flop(half), half))
      } else {
        magick::image_append(c(half, magick::image_flop(half)))
      }
      expect_identical(magick::image_data(unoverlapped, "rgb"),
               magick::image_data(expected, "rgb"))
    }
  }
})

test_that("overlap is safe for narrow images and retains transparency", {
  skip_if_not_installed("magick")
  options <- list(method = "mirror_right", overlap = 5, blend_seam = FALSE)
  for (width in c(1, 2, 4)) {
    half <- magick::image_blank(width, 10, "black")
    completed <- .complete_shape_image(half, options, "left")
    expect_equal(magick::image_info(completed)$width, width + 1)
  }
  completed <- .complete_shape_image(completion_test_image("none"), options, "left")
  expect_equal(as.integer(magick::image_data(completed, "rgba")[4, 1, 1]), 0)
})

test_that("JPEG files are cropped before completion and written successfully", {
  skip_if_not_installed("magick")
  work_dir <- tempfile("completion-")
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
  for (extension in c("jpg", "jpeg", "JPG")) {
    input_path <- file.path(work_dir, paste0("half.", extension))
    magick::image_write(completion_test_image(), input_path, format = "jpeg", quality = 95)
    result <- complete_halved_shape(
      input_path, tempfile("output-", tmpdir = work_dir),
      alignment_options = list(auto_align = FALSE), verbose = FALSE
    )
    expect_equal(result$summary$successful_count, 1)
    expect_equal(result$summary$overlap, 5)
    expect_true(file.exists(result$processed_files$output_path))
    expect_equal(result$completion_info[[1]]$original_dimensions, list(width = 30L, height = 24L))
    expect_equal(result$completion_info[[1]]$completed_dimensions, list(width = 15L, height = 14L))
    expect_equal(magick::image_info(magick::image_read(result$processed_files$output_path))$format, "JPEG")
  }
})

test_that("hole filling fills enclosed background but preserves open notches", {
  skip_if_not_installed("magick")
  for (background in c("white", "none")) {
    img <- magick::image_composite(
      magick::image_blank(24, 24, background),
      magick::image_blank(16, 16, "black"), offset = "+4+4", operator = "over"
    )
    img <- magick::image_composite(
      img, magick::image_blank(4, 4, background), offset = "+10+10", operator = "copy"
    )
    img <- magick::image_composite(
      img, magick::image_blank(3, 10, background), offset = "+6+0", operator = "copy"
    )
    filled <- .fill_shape_holes(img)
    pixels <- magick::image_data(filled, "rgba")
    expect_equal(as.integer(pixels[1, 12, 12]), 0)
    expect_equal(as.integer(pixels[4, 12, 12]), 255)
    original <- magick::image_data(img, "rgba")
    expect_identical(pixels[, 7, 7], original[, 7, 7])
    expect_identical(pixels[, 1, 1], original[, 1, 1])
    expect_equal(magick::image_info(filled)$width, 24)
    expect_equal(magick::image_info(filled)$height, 24)
  }
})

test_that("hole filling is optional and applied by the public completion function", {
  skip_if_not_installed("magick")
  params <- .setup_complete_shape_params(list(), list(), list(), list(), FALSE)
  expect_false(params$completion_options$fill_holes)
  for (invalid in list(NA, 1, "yes", c(TRUE, FALSE))) {
    expect_error(
      .setup_complete_shape_params(list(fill_holes = invalid), list(), list(), list(), FALSE),
      "'fill_holes' must be a single logical value"
    )
  }
  work_dir <- tempfile("holes-")
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
  img <- magick::image_composite(
    magick::image_blank(20, 20, "black"),
    magick::image_blank(4, 4, "white"), offset = "+5+5", operator = "over"
  )
  input_path <- file.path(work_dir, "half.png")
  magick::image_write(img, input_path)
  for (fill_holes in c(FALSE, TRUE)) {
    result <- complete_halved_shape(
      input_path, tempfile("output-", tmpdir = work_dir),
      completion_options = list(method = "mirror_right", overlap = 0, fill_holes = fill_holes),
      verbose = FALSE
    )
    expect_equal(result$summary$successful_count, 1)
    expect_identical(result$summary$fill_holes, fill_holes)
    pixels <- magick::image_data(magick::image_read(result$processed_files$output_path), "gray")
    expect_equal(as.integer(pixels[1, 7, 7]), if (fill_holes) 0 else 255)
  }
})