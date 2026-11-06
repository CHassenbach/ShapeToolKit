#' Shape Analysis Module
#'
#' UI and server for running shape_analysis() with all parameters.
#'
#' @details The foreground threshold controls which image pixels belong to the shape.
#'   Enable center-first manual fallback to import centered shapes automatically and
#'   request browser clicks only for background-centered images. Valid clicks are
#'   saved immediately, and a pending analysis resumes after all requested points.
#'   Blank or unreadable images are reported as failures rather than requesting a click.
#'   Run Shape Analysis checks foreground centers sequentially without performing the
#'   separate Shape Checks diagnostic routine; it pauses at the first required point.
#'
#' @param id Module id
#' @export
shape_analysis_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        box(
          title = "Shape Analysis",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,

          # Input directory chooser
          uiOutput(ns("shape_dir_ui")),
          helpText("Folder must contain JPG/JPEG files (non-recursive)."),

          # Output directory chooser and file name
          uiOutput(ns("out_dir_ui")),
          textInput(ns("output_file"), "Output Excel file name", value = "shape_analysis.xlsx"),

          # Parameters
          checkboxInput(ns("norm"), "Normalize Fourier coefficients by first harmonic", value = TRUE),
          selectInput(ns("start_point"), "Start point for alignment", choices = c("up","left","down","right"), selected = "left"),
          numericInput(ns("num_pcs"), "Number of PCs to plot (1-50)", value = 10, min = 1, max = 50, step = 1),
          numericInput(ns("harmonics"), "Harmonics (NULL for auto)", value = NA, min = 1, max = 100, step = 1),
          numericInput(ns("threshold"), "Foreground threshold", value = 0.5, min = 0, max = 0.99, step = 0.05),
          helpText("Pixels at or below the threshold are foreground. Higher thresholds include lighter gray pixels."),
          checkboxInput(ns("manual_seed"), "Ask for a foreground point only when the image center is background", value = FALSE),
          conditionalPanel(
            condition = sprintf("input['%s']", ns("manual_seed")),
            helpText("Click Run Shape Analysis to start. Centered images are handled automatically; the run pauses at each image needing a point. Click inside the shape to save and continue. Check Shapes is a separate diagnostic action."),
            uiOutput(ns("seed_file_ui")),
            plotOutput(ns("seed_image"), height = 450, click = ns("seed_image_click")),
            textOutput(ns("seed_status"))
          ),

          actionButton(ns("check_shapes"), "Check Shapes", icon = icon("check")),
          actionButton(ns("run"), "Run Shape Analysis", class = "btn-success")
        ),
        box(
          title = "Shape Checks",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          textOutput(ns("check_summary")),
          checkboxInput(ns("failed_only"), "Show failures and images needing a point", value = TRUE),
          downloadButton(ns("download_checks"), "Download Check Report"),
          DT::dataTableOutput(ns("check_table"))
        ),
        box(
          title = "Results",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          uiOutput(ns("run_meta")),
          br(),
          plotOutput(ns("pc_plot"), height = 400),
          br(),
          tags$h4("PCA Summary"),
          tags$pre(style = "max-height: 320px; overflow-y: auto;", textOutput(ns("summary")))
        )
      )
    )
  )
}

#' Shape Analysis Module Server
#'
#' Server counterpart for the Shape Analysis Module.
#'
#' @param id Module id
#' @export
shape_analysis_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # package availability
    shinyfiles_ready <- reactiveVal(FALSE)
    openxlsx_ready <- reactiveVal(FALSE)

    # Try to ensure dependencies
    observe({
      ready <- requireNamespace("shinyFiles", quietly = TRUE)
      if (!isTRUE(ready)) {
        try(install.packages("shinyFiles", repos = "https://cran.r-project.org", quiet = TRUE), silent = TRUE)
        ready <- requireNamespace("shinyFiles", quietly = TRUE)
      }
      shinyfiles_ready(isTRUE(ready))
    })
    observe({
      r2 <- requireNamespace("openxlsx", quietly = TRUE)
      if (!isTRUE(r2)) {
        try(install.packages("openxlsx", repos = "https://cran.r-project.org", quiet = TRUE), silent = TRUE)
        r2 <- requireNamespace("openxlsx", quietly = TRUE)
      }
      openxlsx_ready(isTRUE(r2))
    })

    # Input directory chooser
    output$shape_dir_ui <- renderUI({
      if (isTRUE(shinyfiles_ready())) {
        tagList(
          shinyFiles::shinyDirButton(ns("shape_dir_btn"), label = "Choose input folder", title = "Select a folder with JPG files"),
          br(), strong("Selected folder: "), textOutput(ns("shape_dir_selected"), inline = TRUE)
        )
      } else {
        textInput(ns("shape_dir_fallback"), "Input folder (JPG)", value = "")
      }
    })
    shape_dir <- reactiveVal("")
    selected_shape_dir <- reactive({
      if (isTRUE(shinyfiles_ready())) shape_dir() else input$shape_dir_fallback
    })
    seed_points <- reactiveVal(list())
    seed_requests <- reactiveVal(character(0))
    pending_action <- reactiveVal(NULL)
    run_files <- reactiveVal(character(0))
    run_index <- reactiveVal(1L)
    observeEvent(list(selected_shape_dir(), input$threshold, input$manual_seed,
                      input$norm, input$start_point, input$harmonics), {
      seed_points(list())
      seed_requests(character(0))
      pending_action(NULL)
      run_files(character(0))
      run_index(1L)
    }, ignoreInit = TRUE)

    output$seed_file_ui <- renderUI({
      req(isTRUE(input$manual_seed))
      files <- seed_requests()
      req(length(files) > 0L)
      selectInput(
        ns("seed_file"), "Image to select",
        choices = stats::setNames(files, basename(files)),
        selected = if (length(files)) files[[1L]] else character(0)
      )
    })
    observeEvent(input$seed_image_click, {
      req(isTRUE(input$manual_seed), input$seed_file)
      req(input$seed_file %in% seed_requests())
      click <- input$seed_image_click
      if (!is.list(click) || !is.numeric(click$x) || length(click$x) != 1L ||
          !is.numeric(click$y) || length(click$y) != 1L ||
          !is.finite(click$x) || !is.finite(click$y)) {
        showNotification("Could not read the image click coordinates. Please click inside the shape again.",
                         type = "warning")
        return(invisible(NULL))
      }
      pixels <- jpeg::readJPEG(input$seed_file)
      if (length(dim(pixels)) == 3L) {
        pixels <- (pixels[, , 1L] + pixels[, , 2L] + pixels[, , 3L]) / 3
      }
      row <- as.integer(round(nrow(pixels) + 1 - click$y))
      column <- as.integer(round(click$x))
      if (row < 1L || row > nrow(pixels) ||
          column < 1L || column > ncol(pixels)) {
        showNotification("The click was outside the image. Please click inside the shape.",
                         type = "warning")
        return(invisible(NULL))
      }
      if (!is.finite(pixels[row, column]) || pixels[row, column] > input$threshold) {
        showNotification(paste0("The selected pixel is not foreground at the import threshold (",
                                input$threshold, ")."),
                         type = "warning")
        return(invisible(NULL))
      }

      selected_file <- input$seed_file
      points <- seed_points()
      points[[basename(selected_file)]] <- c(row, column)
      seed_points(points)

      action <- pending_action()
      if (identical(action, "run")) continue_run() else check_shapes()
    }, ignoreInit = TRUE)

    output$seed_image <- renderPlot({
      req(isTRUE(input$manual_seed), input$seed_file, file.exists(input$seed_file))
      req(input$seed_file %in% seed_requests())
      pixels <- jpeg::readJPEG(input$seed_file)
      if (length(dim(pixels)) == 3L) {
        pixels <- (pixels[, , 1L] + pixels[, , 2L] + pixels[, , 3L]) / 3
      }
      height <- nrow(pixels)
      width <- ncol(pixels)
      graphics::par(mar = rep(0.25, 4))
      graphics::plot(
        NA, xlim = c(0.5, width + 0.5), ylim = c(0.5, height + 0.5),
        asp = 1, axes = FALSE, xlab = "", ylab = "", frame.plot = FALSE
      )
      graphics::rasterImage(
        pixels, 0.5, 0.5, width + 0.5, height + 0.5,
        interpolate = FALSE
      )
      graphics::box()
    })
    output$seed_status <- renderText({
      selected_dir <- selected_shape_dir()
      files <- if (!is.null(selected_dir) && dir.exists(selected_dir)) {
        list.files(selected_dir, pattern = "\\.(jpg|jpeg)$", ignore.case = TRUE)
      } else {
        character(0)
      }
      sprintf(
        "%d image(s) have a saved point; %d still need a click.",
        length(intersect(names(seed_points()), files)), length(seed_requests())
      )
    })

    observeEvent(shinyfiles_ready(), ignoreInit = FALSE, handlerExpr = {
      if (!isTRUE(shinyfiles_ready())) return()
      vols <- try(shinyFiles::getVolumes()(), silent = TRUE)
      if (inherits(vols, "try-error") || is.null(vols) || length(vols) == 0) vols <- c(Home = normalizePath("~"))
      shinyFiles::shinyDirChoose(input, id = "shape_dir_btn", roots = vols, session = session)
    })
    observeEvent(input$shape_dir_btn, {
      if (!isTRUE(shinyfiles_ready())) return()
      vols <- try(shinyFiles::getVolumes()(), silent = TRUE)
      if (inherits(vols, "try-error") || is.null(vols) || length(vols) == 0) vols <- c(Home = normalizePath("~"))
      sel <- try(shinyFiles::parseDirPath(vols, input$shape_dir_btn), silent = TRUE)
      if (!inherits(sel, "try-error") && length(sel) == 1) shape_dir(as.character(sel))
    })
    output$shape_dir_selected <- renderText({
      sd <- shape_dir(); if (!is.null(sd) && nzchar(sd)) sd else "(none)"
    })

    # Output directory chooser
    output$out_dir_ui <- renderUI({
      if (isTRUE(shinyfiles_ready())) {
        tagList(
          shinyFiles::shinyDirButton(ns("out_dir_btn"), label = "Choose output folder", title = "Select output folder"),
          br(), strong("Output folder: "), textOutput(ns("out_dir_selected"), inline = TRUE)
        )
      } else {
        textInput(ns("out_dir_fallback"), "Output folder", value = getwd())
      }
    })
    out_dir <- reactiveVal(getwd())
    observeEvent(shinyfiles_ready(), ignoreInit = FALSE, handlerExpr = {
      if (!isTRUE(shinyfiles_ready())) return()
      vols <- try(shinyFiles::getVolumes()(), silent = TRUE)
      if (inherits(vols, "try-error") || is.null(vols) || length(vols) == 0) vols <- c(Home = normalizePath("~"))
      shinyFiles::shinyDirChoose(input, id = "out_dir_btn", roots = vols, session = session)
    })
    observeEvent(input$out_dir_btn, {
      if (!isTRUE(shinyfiles_ready())) return()
      vols <- try(shinyFiles::getVolumes()(), silent = TRUE)
      if (inherits(vols, "try-error") || is.null(vols) || length(vols) == 0) vols <- c(Home = normalizePath("~"))
      sel <- try(shinyFiles::parseDirPath(vols, input$out_dir_btn), silent = TRUE)
      if (!inherits(sel, "try-error") && length(sel) == 1) out_dir(as.character(sel))
    })
    output$out_dir_selected <- renderText({
      od <- out_dir(); if (!is.null(od) && nzchar(od)) od else "(none)"
    })

    # Run analysis
    results <- reactiveVal(NULL)
    shape_checks <- reactiveVal(NULL)

    continue_run <- function() {
      files <- run_files()
      index <- run_index()
      seed_requests(character(0))
      while (index <= length(files)) {
        path <- files[[index]]
        if (is.null(seed_points()[[basename(path)]])) {
          pixels <- tryCatch(
            .read_shape_foreground(path, input$threshold),
            error = function(error) {
              showNotification(
                paste("Failed to import", basename(path), ":", conditionMessage(error)),
                type = "error", duration = 10
              )
              NULL
            }
          )
          if (is.null(pixels)) {
            pending_action(NULL)
            return(invisible(NULL))
          }
          center <- round(dim(pixels) / 2)
          if (any(center < 1L) || pixels[center[1L], center[2L]] > input$threshold) {
            run_index(index)
            seed_requests(path)
            pending_action("run")
            return(invisible(NULL))
          }
        }
        index <- index + 1L
        run_index(index)
      }
      pending_action(NULL)
      run_analysis()
    }

    observeEvent(list(shape_dir(), input$shape_dir_fallback, input$norm,
                      input$start_point, input$harmonics, input$manual_seed,
                      input$threshold), {
      shape_checks(NULL)
    }, ignoreInit = TRUE)

    check_shapes <- function() {
      shape_checks(NULL)
      selected_dir <- if (isTRUE(shinyfiles_ready())) shape_dir() else input$shape_dir_fallback
      if (is.null(selected_dir) || !nzchar(selected_dir)) {
        showNotification("Please choose an input folder.", type = "warning")
        return(invisible(NULL))
      }
      harm <- input$harmonics
      if (length(harm) == 0L || is.na(harm)) harm <- NULL
      report <- tryCatch({
        withProgress(message = "Checking shapes...", value = 0.1, {
          checked <- .check_shape_files(
            shape_dir = selected_dir,
            norm = isTRUE(input$norm),
            start_point = input$start_point,
            harmonics = harm,
            seed_points = if (isTRUE(input$manual_seed)) seed_points() else NULL,
            threshold = input$threshold,
            auto.notcentered = !isTRUE(input$manual_seed)
          )
          incProgress(0.9)
          checked
        })
      }, error = function(error) {
        showNotification(paste("Shape check failed:", conditionMessage(error)),
                         type = "error", duration = 10)
        NULL
      })
      shape_checks(report)
      if (is.null(report)) return(invisible(NULL))
      requests <- report$input_path[report$status == "needs_point"]
      seed_requests(requests)
      if (length(requests) > 0L) {
        pending_action("check")
        showNotification(paste(length(requests), "image(s) need a foreground point. Click inside the displayed shape."),
                         type = "warning", duration = 10)
      } else {
        pending_action(NULL)
      }
    }
    observeEvent(input$check_shapes, check_shapes())

    output$check_summary <- renderText({
      report <- shape_checks(); req(report)
      sprintf("%d files checked: %d passed, %d failed, %d need a point. PCA not checked.",
              nrow(report), sum(report$status == "passed"), sum(report$status == "failed"),
              sum(report$status == "needs_point"))
    })

    output$check_table <- DT::renderDataTable({
      report <- shape_checks(); req(report)
      if (isTRUE(input$failed_only)) report <- report[report$status != "passed", , drop = FALSE]
      display <- report[, c("file", "status", "stage", "error_message"), drop = FALSE]
      names(display) <- c("File", "Status", "Stage", "Reason")
      DT::datatable(display, rownames = FALSE,
                    options = list(pageLength = 10, scrollX = TRUE))
    })

    output$download_checks <- downloadHandler(
      filename = function() "shape_check_report.csv",
      content = function(file) {
        report <- shape_checks(); req(report)
        utils::write.csv(report, file, row.names = FALSE)
      }
    )

    # Run analysis with the selected normalization and seed settings.
    run_analysis <- function() {
      # Validate inputs
      sd <- if (isTRUE(shinyfiles_ready())) shape_dir() else input$shape_dir_fallback
      if (is.null(sd) || !nzchar(sd)) {
        showNotification("Please choose an input folder.", type = "warning"); return(invisible(NULL))
      }
      sd <- normalizePath(sd, winslash = "/", mustWork = FALSE)
      if (!dir.exists(sd)) { showNotification("Input folder does not exist.", type = "error"); return(invisible(NULL)) }

      od <- if (isTRUE(shinyfiles_ready())) out_dir() else input$out_dir_fallback
      if (is.null(od) || !nzchar(od)) od <- getwd()
      od <- normalizePath(od, winslash = "/", mustWork = FALSE)
      if (!dir.exists(od)) {
        ok <- try(dir.create(od, recursive = TRUE, showWarnings = FALSE), silent = TRUE)
        if (inherits(ok, "try-error") || !dir.exists(od)) {
          showNotification("Cannot create output folder.", type = "error"); return(invisible(NULL))
        }
      }

      of <- input$output_file
      if (is.null(of) || !nzchar(of)) of <- "shape_analysis.xlsx"
      if (!grepl("\\.(xlsx|xls)$", of, ignore.case = TRUE)) of <- paste0(of, ".xlsx")

      # Ensure packages
      if (!isTRUE(openxlsx_ready())) {
        showNotification("Installing required packages...", type = "message")
      }
      if (!requireNamespace("openxlsx", quietly = TRUE)) {
        showNotification("Package 'openxlsx' is required.", type = "error", duration = 8)
        return(invisible(NULL))
      }

      # Parse harmonics (NA => NULL)
      harm <- input$harmonics
      if (is.na(harm)) harm <- NULL

      withProgress(message = "Running shape analysis...", value = 0, {
        incProgress(0.2, detail = "Processing shapes")
          res <- tryCatch({
          shape_analysis(
            shape_dir = sd,
            norm = isTRUE(input$norm),
            output_dir = od,
            output_file = of,
            num_pcs = as.integer(input$num_pcs),
            start_point = input$start_point,
            harmonics = if (is.null(harm)) NULL else as.integer(harm),
            seed_points = if (isTRUE(input$manual_seed)) seed_points() else NULL,
            threshold = input$threshold,
            auto.notcentered = !isTRUE(input$manual_seed)
          )
        }, error = function(e) {
          showNotification(paste("Analysis failed:", conditionMessage(e)), type = "error", duration = 8)
          NULL
        })
        incProgress(0.8, detail = "Finalizing")
        results(res)
      })

      if (!is.null(results())) {
        showNotification("Shape analysis completed.", type = "message")
      }
    }
    observeEvent(input$run, {
      if (!isTRUE(input$manual_seed)) {
        run_analysis()
      } else {
        selected_dir <- selected_shape_dir()
        if (is.null(selected_dir) || !nzchar(selected_dir) || !dir.exists(selected_dir)) {
          showNotification("Please choose an existing input folder.", type = "warning")
          return(invisible(NULL))
        }
        files <- list.files(
          selected_dir, pattern = "\\.(jpg|jpeg)$",
          full.names = TRUE, ignore.case = TRUE
        )
        if (length(files) == 0L) {
          showNotification("No JPG/JPEG files found in the selected folder.", type = "error")
          return(invisible(NULL))
        }
        run_files(files)
        run_index(1L)
        continue_run()
      }
    })

    # Outputs
    output$run_meta <- renderUI({
      res <- results(); req(res)
      tags$div(
        tags$p(sprintf("Analyzed %d shapes.", res$metadata$n_shapes)),
        tags$h5("Output Files:"),
        tags$ul(
          tags$li(tags$strong("PCA Scores: "), tags$code(basename(res$output_path))),
          tags$li(tags$strong("Summary: "), tags$code(basename(res$summary_txt_path))),
          tags$li(tags$strong("PC Plot: "), tags$code(basename(res$pc_plot_jpg_path))),
          if (!is.null(res$reconstruction_files))
            tags$li(tags$strong("Reconstruction Files: "),
                    tags$code(basename(res$reconstruction_files$rotation)))
        ),
        tags$p(tags$em("All files saved to: ", dirname(res$output_path)))
      )
    })

    output$summary <- renderText({
      res <- results(); req(res)
      res$summary
    })

    output$pc_plot <- renderPlot({
      res <- results(); req(res)
      # If stored plot exists and is plottable, print it; otherwise, attempt to re-generate
      if (!is.null(res$pc_contribution_plot)) {
        try({
          if (is.list(res$pc_contribution_plot) && !is.null(res$pc_contribution_plot$gg)) {
            print(res$pc_contribution_plot$gg)
          } else {
            print(res$pc_contribution_plot)
          }
        }, silent = TRUE)
      } else {
        max_pcs <- min(input$num_pcs, ncol(res$pca_results$x))
        pc_obj <- PCcontrib(res$pca_results, nax = 1:max_pcs, sd.r = c(-2,-1,0,1,2))
        if (is.list(pc_obj) && !is.null(pc_obj$gg)) {
          print(pc_obj$gg)
        }
      }
    })

    invisible(results)
  })
}
