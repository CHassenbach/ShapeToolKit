#' Morphospace Stability Module UI
#'
#' @param id Module namespace id.
#'
#' @return Shiny UI for morphospace stability analysis.
#' @export
morphospace_stability_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shinydashboard::box(
          title = "Step 1: Input Folder",
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          collapsible = TRUE,
          shiny::uiOutput(ns("shape_dir_ui")),
          shiny::helpText("Folder must contain JPG/JPEG shape images.")
        ),
        shinydashboard::box(
          title = "Step 2: Parameters",
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          collapsible = TRUE,
          shiny::textInput(
            ns("sample_fractions"),
            "Sample fractions (comma-separated, 0-1]",
            value = "0.02,0.05,0.10,0.20,0.30,0.50,1.00"
          ),
          shiny::numericInput(ns("n_repeats"), "Repeats per fraction", value = 10, min = 1, max = 200, step = 1),
          shiny::selectInput(ns("mode"), "Analysis mode", choices = c("fast", "strict"), selected = "fast"),
          shiny::selectInput(ns("reference_mode"), "Reference mode", choices = c("full_dataset", "largest_fraction"), selected = "full_dataset"),
          shiny::numericInput(ns("max_pcs"), "Max PCs for similarity", value = 4, min = 2, max = 20, step = 1),
          shiny::numericInput(ns("grid_resolution"), "Grid resolution", value = 60, min = 20, max = 250, step = 5),
          shiny::checkboxInput(ns("norm"), "EFA normalize by first harmonic", value = TRUE),
          shiny::selectInput(ns("start_point"), "Start point", choices = c("up", "left", "down", "right"), selected = "left"),
          shiny::checkboxInput(ns("align_orientation"), "Align orientation", value = FALSE),
          shiny::numericInput(ns("harmonics"), "Harmonics (NA = automatic)", value = NA, min = 1, step = 1),
          shiny::numericInput(ns("seed"), "Random seed", value = 42, min = 1, step = 1),
          shiny::checkboxInput(ns("parallel"), "Use parallel processing", value = FALSE)
        ),
        shinydashboard::box(
          title = "Step 3: Run",
          status = "success",
          solidHeader = TRUE,
          width = NULL,
          shiny::actionButton(ns("run_analysis"), "Run Stability Analysis", class = "btn-success btn-lg btn-block", icon = shiny::icon("play")),
          shiny::br(),
          shiny::uiOutput(ns("analysis_status"))
        )
      ),
      shiny::column(
        width = 8,
        shinydashboard::box(
          title = "Stability Convergence",
          status = "info",
          solidHeader = TRUE,
          width = NULL,
          shiny::fluidRow(
            shiny::column(
              width = 4,
              shiny::selectInput(ns("x_axis"), "X axis", choices = c("fraction", "count"), selected = "fraction")
            ),
            shiny::column(
              width = 4,
              shiny::checkboxGroupInput(
                ns("metrics"),
                "Metrics",
                choices = c(
                  "Subspace similarity" = "subspace_similarity",
                  "Occupancy similarity" = "occupancy_similarity",
                  "Grid IoU" = "occupancy_grid_iou",
                  "Hull IoU" = "occupancy_hull_iou"
                ),
                selected = c("subspace_similarity", "occupancy_similarity")
              )
            ),
            shiny::column(
              width = 4,
              shiny::checkboxInput(ns("show_ci"), "Show CI", value = TRUE)
            )
          ),
          shiny::plotOutput(ns("stability_plot"), height = "450px"),
          shiny::hr(),
          shiny::fluidRow(
            shiny::column(width = 6, shiny::downloadButton(ns("download_summary_csv"), "Download Summary CSV", class = "btn-primary btn-block")),
            shiny::column(width = 6, shiny::downloadButton(ns("download_results_rds"), "Download Full Results RDS", class = "btn-primary btn-block"))
          )
        ),
        shinydashboard::box(
          title = "PC Axis Shift",
          status = "info",
          solidHeader = TRUE,
          width = NULL,
          collapsible = TRUE,
          shiny::fluidRow(
            shiny::column(
              width = 4,
              shiny::selectInput(
                ns("axis_value_type"),
                "Axis shift view",
                choices = c("Angle (degrees)" = "angle_deg", "Similarity (|cos|)" = "similarity"),
                selected = "angle_deg"
              )
            ),
            shiny::column(
              width = 4,
              shiny::numericInput(ns("axis_max_plot"), "Max PC axes to display", value = 4, min = 1, max = 20, step = 1)
            ),
            shiny::column(
              width = 4,
              shiny::checkboxInput(ns("axis_show_ci"), "Show CI", value = TRUE)
            )
          ),
          shiny::plotOutput(ns("axis_shift_plot"), height = "320px")
        ),
        shinydashboard::box(
          title = "Summary Table",
          status = "info",
          solidHeader = TRUE,
          width = NULL,
          collapsible = TRUE,
          DT::dataTableOutput(ns("summary_table"))
        ),
        shinydashboard::box(
          title = "Convergence Recommendation",
          status = "info",
          solidHeader = TRUE,
          width = NULL,
          collapsible = TRUE,
          shiny::numericInput(ns("threshold"), "Target mean similarity", value = 0.90, min = 0, max = 1, step = 0.01),
          shiny::numericInput(ns("sd_threshold"), "Max SD", value = 0.05, min = 0, max = 1, step = 0.01),
          DT::dataTableOutput(ns("recommendation_table"))
        ),
        shinydashboard::box(
          title = "Metric Help and Interpretation",
          status = "warning",
          solidHeader = TRUE,
          width = NULL,
          collapsible = TRUE,
          collapsed = TRUE,
          shiny::tags$h5("What the metrics quantify"),
          shiny::tags$ul(
            shiny::tags$li(
              shiny::tags$b("Subspace similarity"),
              ": Compares PCA loading subspaces between a subset run and the reference. Values near 1 indicate similar PCA orientation/structure."
            ),
            shiny::tags$li(
              shiny::tags$b("Grid IoU"),
              ": Compares occupied cells in PC space between subset and reference (intersection over union). Higher values indicate more similar occupied regions."
            ),
            shiny::tags$li(
              shiny::tags$b("Hull IoU"),
              ": Compares overlap of convex hull areas in PC space. Can be NA for very small subsets or degenerate hull geometry."
            ),
            shiny::tags$li(
              shiny::tags$b("Occupancy similarity"),
              ": Combined occupancy measure from available occupancy metrics (Grid/Hull)."
            )
          ),
          shiny::tags$h5("How to read convergence"),
          shiny::tags$ul(
            shiny::tags$li("Across fractions, increasing means and decreasing SD suggest stabilization."),
            shiny::tags$li("The recommendation table reports the first fraction where mean >= threshold and SD <= max SD."),
            shiny::tags$li("Suggested starting defaults: threshold = 0.90 and max SD = 0.05.")
          ),
          shiny::helpText("Tip: If Hull IoU is often NA at low fractions, rely on Subspace similarity and Grid IoU trends for early-sample interpretation.")
        )
      )
    )
  )
}


#' Morphospace Stability Module Server
#'
#' @param id Module namespace id.
#'
#' @return Reactive value holding stability results.
#' @export
morphospace_stability_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shape_dir <- shiny::reactiveVal("")
    stability_results <- shiny::reactiveVal(NULL)

    output$shape_dir_ui <- shiny::renderUI({
      if (requireNamespace("shinyFiles", quietly = TRUE)) {
        shiny::tagList(
          shinyFiles::shinyDirButton(ns("shape_dir_btn"), label = "Choose input folder", title = "Select folder with shape images"),
          shiny::br(),
          shiny::strong("Selected folder: "),
          shiny::textOutput(ns("shape_dir_selected"), inline = TRUE)
        )
      } else {
        shiny::textInput(ns("shape_dir_fallback"), "Input folder", value = "")
      }
    })

    shiny::observe({
      if (!requireNamespace("shinyFiles", quietly = TRUE)) return()
      vols <- try(shinyFiles::getVolumes()(), silent = TRUE)
      if (inherits(vols, "try-error") || is.null(vols) || length(vols) == 0) {
        vols <- c(Home = normalizePath("~"))
      }
      shinyFiles::shinyDirChoose(input, id = "shape_dir_btn", roots = vols, session = session)
    })

    shiny::observeEvent(input$shape_dir_btn, {
      if (!requireNamespace("shinyFiles", quietly = TRUE)) return()
      vols <- try(shinyFiles::getVolumes()(), silent = TRUE)
      if (inherits(vols, "try-error") || is.null(vols) || length(vols) == 0) {
        vols <- c(Home = normalizePath("~"))
      }
      sel <- try(shinyFiles::parseDirPath(vols, input$shape_dir_btn), silent = TRUE)
      if (!inherits(sel, "try-error") && length(sel) == 1) {
        shape_dir(as.character(sel))
      }
    })

    output$shape_dir_selected <- shiny::renderText({
      sel <- shape_dir()
      if (!is.null(sel) && nzchar(sel)) sel else "(none)"
    })

    parse_fraction_input <- function(x) {
      vals <- unlist(strsplit(x, ","), use.names = FALSE)
      vals <- trimws(vals)
      vals <- vals[nzchar(vals)]
      as.numeric(vals)
    }

    shiny::observeEvent(input$run_analysis, {
      input_dir <- if (requireNamespace("shinyFiles", quietly = TRUE)) shape_dir() else input$shape_dir_fallback

      if (is.null(input_dir) || !nzchar(input_dir) || !dir.exists(input_dir)) {
        shiny::showNotification("Please choose a valid input folder.", type = "error", duration = 6)
        return()
      }

      fr <- suppressWarnings(parse_fraction_input(input$sample_fractions))
      if (length(fr) == 0 || any(!is.finite(fr))) {
        shiny::showNotification("Sample fractions must be a comma-separated numeric list.", type = "error", duration = 6)
        return()
      }

      # Capture all reactive inputs eagerly before any parallel work starts.
      harm <- input$harmonics
      if (is.na(harm)) harm <- NULL
      n_repeats_val <- as.integer(input$n_repeats)
      norm_val <- isTRUE(input$norm)
      start_point_val <- input$start_point
      align_orientation_val <- isTRUE(input$align_orientation)
      mode_val <- input$mode
      reference_mode_val <- input$reference_mode
      max_pcs_val <- as.integer(input$max_pcs)
      grid_resolution_val <- as.integer(input$grid_resolution)
      seed_val <- as.integer(input$seed)
      parallel_val <- isTRUE(input$parallel)

      shiny::withProgress(message = "Computing morphospace stability...", value = 0, {
        shiny::incProgress(0.15, detail = "Preparing analysis")
        res <- tryCatch({
          compute_morphospace_stability(
            shape_dir = input_dir,
            sample_fractions = fr,
            n_repeats = n_repeats_val,
            harmonics = if (is.null(harm)) NULL else as.integer(harm),
            norm = norm_val,
            start_point = start_point_val,
            align_orientation = align_orientation_val,
            mode = mode_val,
            reference_mode = reference_mode_val,
            max_pcs = max_pcs_val,
            grid_resolution = grid_resolution_val,
            seed = seed_val,
            parallel = parallel_val,
            verbose = FALSE
          )
        }, error = function(e) {
          shiny::showNotification(paste("Analysis failed:", conditionMessage(e)), type = "error", duration = 8)
          NULL
        })
        shiny::incProgress(0.85, detail = "Finalizing")
        stability_results(res)
      })

      if (!is.null(stability_results())) {
        shiny::showNotification("Morphospace stability analysis completed.", type = "message", duration = 5)
      }
    })

    output$analysis_status <- shiny::renderUI({
      if (is.null(stability_results())) {
        shiny::tags$div(style = "padding: 8px; background-color: #fff3cd; border-radius: 4px;", shiny::icon("info-circle"), shiny::strong(" Ready to run"))
      } else {
        shiny::tags$div(style = "padding: 8px; background-color: #d4edda; border-radius: 4px;", shiny::icon("check-circle"), shiny::strong(" Analysis complete"))
      }
    })

    output$stability_plot <- shiny::renderPlot({
      req(stability_results())
      req(length(input$metrics) > 0)
      plot_morphospace_stability(
        stability_result = stability_results(),
        x_axis = input$x_axis,
        metrics = input$metrics,
        show_ci = isTRUE(input$show_ci)
      )
    })

    output$summary_table <- DT::renderDataTable({
      req(stability_results())
      x <- stability_results()$summary_table
      x$mean <- round(x$mean, 4)
      x$median <- round(x$median, 4)
      x$sd <- round(x$sd, 4)
      x$se <- round(x$se, 4)
      x$q025 <- round(x$q025, 4)
      x$q975 <- round(x$q975, 4)
      DT::datatable(x, options = list(pageLength = 12, scrollX = TRUE), rownames = FALSE)
    })

    output$axis_shift_plot <- shiny::renderPlot({
      req(stability_results())
      plot_pca_axis_shift(
        stability_result = stability_results(),
        value_type = input$axis_value_type,
        show_ci = isTRUE(input$axis_show_ci),
        max_axes = as.integer(input$axis_max_plot)
      )
    })

    output$recommendation_table <- DT::renderDataTable({
      req(stability_results())
      rec <- summarize_morphospace_stability(
        stability_result = stability_results(),
        threshold = input$threshold,
        sd_threshold = input$sd_threshold
      )
      DT::datatable(rec, options = list(dom = "t"), rownames = FALSE)
    })

    output$download_summary_csv <- shiny::downloadHandler(
      filename = function() {
        paste0("morphospace_stability_summary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        utils::write.csv(stability_results()$summary_table, file, row.names = FALSE)
      }
    )

    output$download_results_rds <- shiny::downloadHandler(
      filename = function() {
        paste0("morphospace_stability_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
      },
      content = function(file) {
        saveRDS(stability_results(), file)
      }
    )

    invisible(stability_results)
  })
}