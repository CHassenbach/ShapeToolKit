test_that("the outline overlay preserves all imports and supports size comparison", {
  outlines <- list(
    "upper.jpg" = cbind(c(10, 14, 14, 10), c(20, 20, 24, 24)),
    "lower.jpg" = cbind(c(30, 42, 42, 30), c(50, 50, 62, 62))
  )
  result <- list(imported_outlines = outlines, pc_contribution_plot = ggplot2::ggplot())
  shiny::testServer(shape_analysis_server, {
    results(result)
    session$setInputs(outline_scale = TRUE, outline_file = "")
    scaled <- outline_coordinates()
    expect_named(scaled, names(outlines))
    expect_length(scaled, length(outlines))
    for (i in seq_along(outlines)) {
      expect_equal(scaled[[i]], coo_scale(coo_center(outlines[[i]])))
      expect_equal(unname(colMeans(scaled[[i]])), c(0, 0), tolerance = 1e-12)
      expect_equal(coo_centsize(scaled[[i]]), 1)
    }
    expect_true(nzchar(output$outline_overlay$src))
    choices <- output$outline_file_ui$html
    expect_match(choices, "upper.jpg", fixed = TRUE)
    expect_match(choices, "lower.jpg", fixed = TRUE)
    session$setInputs(outline_file = "2", outline_scale = FALSE)
    centered <- outline_coordinates()
    expect_equal(centered[[1]], coo_center(outlines[[1]]))
    expect_equal(centered[[2]], coo_center(outlines[[2]]))
    expect_true(nzchar(output$outline_overlay$src))
    expect_identical(results()$imported_outlines, outlines)

    result$imported_outlines <- outlines[1]
    results(result)
    expect_length(outline_coordinates(), 1)
    expect_true(nzchar(output$outline_overlay$src))
    session$setInputs(outline_file = "1")
    expect_true(nzchar(output$outline_overlay$src))
  })
})
