#' Shape Analysis Module
#'
#' UI and server for running shape_analysis() with all parameters.
#'
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
          checkboxInput(ns("norm"), "Align by first harmonic (unchecked: longest radius)", value = TRUE),
          selectInput(ns("start_point"), "Start point for alignment", choices = c("up","left","down","right"), selected = "left"),
          numericInput(ns("num_pcs"), "Number of PCs to plot (1-50)", value = 10, min = 1, max = 50, step = 1),
          numericInput(ns("harmonics"), "Harmonics (NULL for auto)", value = NA, min = 1, max = 100, step = 1),

          actionButton(ns("run"), "Run Shape Analysis", class = "btn-success")
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

    # No dynamic disabling required; single checkbox controls EFA normalization mode.
    observeEvent(input$run, {
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
            harmonics = if (is.null(harm)) NULL else as.integer(harm)
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
          tags$li(tags$strong("Reconstruction Model: "), tags$code(basename(res$reconstruction_model_path))),
          tags$li(tags$strong("Reconstruction Info: "), tags$code(basename(res$reconstruction_info_path)))
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
        try(print(res$pc_contribution_plot), silent = TRUE)
      } else {
        max_pcs <- min(input$num_pcs, ncol(res$pca_results$x))
        PCcontrib(res$pca_results, nax = 1:max_pcs, sd.r = c(-2,-1,0,1,2))
      }
    })

    invisible(results)
  })
}
