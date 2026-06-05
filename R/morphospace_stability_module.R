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
          shiny::radioButtons(
            ns("sampling_mode"),
            "Sampling schedule",
            choices = c(
              "Fractions" = "fractions",
              "Specimen-size steps" = "steps"
            ),
            selected = "fractions",
            inline = TRUE
          ),
          shiny::conditionalPanel(
            condition = "input.sampling_mode == 'fractions'",
            ns = ns,
            shiny::textInput(
              ns("sample_fractions"),
              "Sample fractions (comma-separated, 0-1]",
              value = "0.02,0.05,0.10,0.20,0.30,0.50,1.00"
            )
          ),
          shiny::conditionalPanel(
            condition = "input.sampling_mode == 'steps'",
            ns = ns,
            shiny::numericInput(ns("sample_start_n"), "Start at specimen count", value = 20, min = 2, step = 1),
            shiny::numericInput(ns("sample_step_n"), "Step size (specimens)", value = 50, min = 1, step = 1),
            shiny::helpText("Builds sample sizes like start, start+step, ... and always includes full N.")
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
                  "Occupancy similarity (strict: requires hull)" = "occupancy_similarity_strict",
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
            shiny::column(width = 6, shiny::downloadButton(ns("download_results_rds"), "Download Analysis Bundle (RDS)", class = "btn-primary btn-block"))
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
          title = "Replicate SD Outline Overlays",
          status = "info",
          solidHeader = TRUE,
          width = NULL,
          collapsible = TRUE,
          collapsed = TRUE,
          shiny::fluidRow(
            shiny::column(
              width = 4,
              shiny::textInput(ns("pc_contrib_pcs"), "PCs (comma-separated)", value = "1,2,3,4")
            ),
            shiny::column(
              width = 4,
              shiny::textInput(ns("pc_contrib_sd_values"), "SD levels (comma-separated)", value = "-2,-1,0,1,2")
            ),
            shiny::column(
              width = 4,
              shiny::numericInput(ns("pc_contrib_nb_pts"), "Outline resolution", value = 160, min = 50, max = 500, step = 25)
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::sliderInput(ns("pc_contrib_alpha"), "Line opacity", min = 0.05, max = 1, value = 0.20, step = 0.05)
            ),
            shiny::column(
              width = 6,
              shiny::sliderInput(ns("pc_contrib_line_width"), "Line width", min = 0.1, max = 2, value = 0.4, step = 0.1)
            )
          ),
          shiny::plotOutput(ns("pc_contrib_plot"), height = "700px")
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
          shiny::textInput(
            ns("thresholds"),
            "Similarity thresholds (comma-separated)",
            value = "0.95,0.90,0.80"
          ),
          shiny::selectInput(
            ns("similarity_uncertainty"),
            "Similarity uncertainty basis",
            choices = c(
              "Replicate SD" = "sd",
              "Standard error of the mean" = "se",
              "Approx. 95% CI half-width" = "ci_halfwidth"
            ),
            selected = "ci_halfwidth"
          ),
          shiny::numericInput(ns("sd_threshold"), "Similarity uncertainty cutoff", value = 0.05, min = 0, max = 1, step = 0.01),
          shiny::numericInput(ns("angle_threshold_deg"), "Angle max mean shift (deg)", value = 5, min = 0, max = 90, step = 0.5),
          DT::dataTableOutput(ns("recommendation_table"))
        ),
        shinydashboard::box(
          title = "Metric Help and Interpretation",
          status = "warning",
          solidHeader = TRUE,
          width = NULL,
          collapsible = TRUE,
          collapsed = TRUE,
          shiny::tags$h5("Methods-Level Definition of Stability Metrics"),
          shiny::tags$p(
            "Each replicate is generated by subsampling specimens at a fixed fraction without replacement,",
            "running EFA plus PCA (strict mode) or PCA on a fixed EFA basis (fast mode),",
            "and comparing replicate structure to a reference PCA (full dataset or largest fraction)."
          ),
          shiny::tags$h6("1) Subspace Similarity (global PCA structure)"),
          shiny::tags$p(
            "Let A and B be loading matrices (coefficients x k axes) for replicate and reference.",
            "Orthonormal bases Qa and Qb are computed from A and B.",
            "Singular values s_i of t(Qa) %*% Qb are cosine principal angles between subspaces."
          ),
          shiny::tags$pre("Subspace similarity = mean(s_i), i = 1..k, with 0 <= s_i <= 1"),
          shiny::tags$p(
            "Interpretation: values near 1 indicate highly concordant multivariate axis geometry.",
            "This metric is sign-invariant and summarizes global orientation agreement."
          ),

          shiny::tags$h6("2) Per-Axis Shift (axis-level PCA rotation)"),
          shiny::tags$p(
            "For each PC axis j, similarity is computed from unit loadings v_j (replicate) and r_j (reference):"
          ),
          shiny::tags$pre("axis_similarity_j = abs( dot(v_j, r_j) / (||v_j|| * ||r_j||) )"),
          shiny::tags$pre("axis_angle_j_deg = acos(axis_similarity_j) * 180 / pi"),
          shiny::tags$p(
            "Absolute cosine removes arbitrary PCA sign flips.",
            "Lower angle implies stronger axis-level alignment.",
            "Reported summaries include mean and max axis shift (degrees) and mean axis similarity."
          ),

          shiny::tags$h6("3) Occupancy Grid IoU (morphospace occupancy overlap)"),
          shiny::tags$p(
            "For each PC pair, both replicate and reference scores are rasterized to a shared grid.",
            "Occupied cells define two sets, G_rep and G_ref."
          ),
          shiny::tags$pre("Grid IoU = |G_rep INTERSECT G_ref| / |G_rep UNION G_ref|"),
          shiny::tags$p(
            "Interpretation: near 1 indicates similar occupied morphospace regions",
            "(coverage congruence), not just similar PCA orientation."
          ),

          shiny::tags$h6("4) Hull IoU (morphospace extent overlap)"),
          shiny::tags$p(
            "For each PC pair, convex hull polygons are built for replicate and reference point sets.",
            "Intersection and union areas are computed in 2D PC space."
          ),
          shiny::tags$pre("Hull IoU = Area(Intersection) / Area(Union)"),
          shiny::tags$p(
            "This reflects agreement in outer envelope/extent.",
            "NA can occur for small subsets or degenerate geometry."
          ),

          shiny::tags$h6("5) Occupancy Similarity (composite occupancy metric)"),
          shiny::tags$p(
            "Occupancy similarity uses the mean of available occupancy components",
            "(Grid IoU and Hull IoU), so it can equal Grid IoU when Hull IoU is unavailable."
          ),
          shiny::tags$p(
            "Use 'Occupancy similarity (strict: requires hull)' to force both components;",
            "it returns NA when Hull IoU cannot be computed."
          ),

          shiny::tags$h5("Convergence Rule Used in Recommendation Table"),
          shiny::tags$p("Recommendations are threshold-crossing estimates, not fitted asymptotic means."),
          shiny::tags$ul(
            shiny::tags$li(
              shiny::tags$b("Similarity-family metrics"),
              ": first sample size where mean >= similarity threshold and the selected uncertainty basis is <= the cutoff."
            ),
            shiny::tags$li(
              shiny::tags$b("Angle-family metrics (_deg)"),
              ": first sample size where mean <= angle threshold (degrees)."
            )
          ),
          shiny::tags$p(
            "The default is approximate 95% CI half-width, which is usually the safest choice when users",
            "do not know which uncertainty threshold to pick.",
            "Use SD if you want replicate spread, or SE if you want uncertainty on the mean.",
            "Because this is a first-pass threshold crossing on the tested schedule,",
            "results depend on fraction/step granularity and number of replicates."
          ),
          shiny::helpText(
            "Practical guidance: use subspace and axis metrics for structural PCA stability;",
            "use occupancy metrics for conservative morphospace coverage stability."
          )
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

    build_fraction_schedule_from_steps <- function(input_dir, start_n, step_n) {
      files <- list.files(
        input_dir,
        pattern = "\\.(jpg|jpeg)$",
        full.names = FALSE,
        ignore.case = TRUE
      )
      n_total <- length(files)
      if (n_total < 3) {
        stop("Need at least 3 JPG/JPEG files in selected folder")
      }

      start_n <- max(2L, as.integer(start_n))
      step_n <- as.integer(step_n)
      if (!is.finite(step_n) || step_n < 1) {
        stop("Step size must be a positive integer")
      }

      if (start_n > n_total) {
        start_n <- n_total
      }

      sample_sizes <- seq(from = start_n, to = n_total, by = step_n)
      if (length(sample_sizes) == 0 || sample_sizes[length(sample_sizes)] != n_total) {
        sample_sizes <- c(sample_sizes, n_total)
      }
      sample_sizes <- sort(unique(sample_sizes))

      list(
        fractions = sample_sizes / n_total,
        sample_sizes = sample_sizes,
        n_total = n_total
      )
    }

    shiny::observeEvent(input$run_analysis, {
      input_dir <- if (requireNamespace("shinyFiles", quietly = TRUE)) shape_dir() else input$shape_dir_fallback

      if (is.null(input_dir) || !nzchar(input_dir) || !dir.exists(input_dir)) {
        shiny::showNotification("Please choose a valid input folder.", type = "error", duration = 6)
        return()
      }

      sampling_mode_val <- input$sampling_mode
      fr <- NULL
      if (identical(sampling_mode_val, "steps")) {
        step_schedule <- tryCatch({
          build_fraction_schedule_from_steps(
            input_dir = input_dir,
            start_n = input$sample_start_n,
            step_n = input$sample_step_n
          )
        }, error = function(e) {
          shiny::showNotification(paste("Invalid step schedule:", conditionMessage(e)), type = "error", duration = 7)
          NULL
        })

        if (is.null(step_schedule)) return()
        fr <- step_schedule$fractions

        shiny::showNotification(
          paste0(
            "Using step-based schedule with N=", step_schedule$n_total,
            ": ", paste(step_schedule$sample_sizes, collapse = ", "), " specimens"
          ),
          type = "message",
          duration = 5
        )
      } else {
        fr <- suppressWarnings(parse_fraction_input(input$sample_fractions))
        if (length(fr) == 0 || any(!is.finite(fr))) {
          shiny::showNotification("Sample fractions must be a comma-separated numeric list.", type = "error", duration = 6)
          return()
        }
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
        hull_available_n <- sum(is.finite(stability_results()$run_results$occupancy_hull_iou))
        if (isTRUE(hull_available_n == 0)) {
          shiny::showNotification(
            "Hull IoU was unavailable for all runs; occupancy similarity metrics are hidden because they collapse to Grid IoU without hull overlap.",
            type = "warning",
            duration = 8
          )
        }
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

      metrics_to_plot <- input$metrics
      hull_all_na <- all(!is.finite(stability_results()$run_results$occupancy_hull_iou))
      if (hull_all_na) {
        metrics_to_plot <- setdiff(metrics_to_plot, c("occupancy_similarity", "occupancy_similarity_strict"))
      }
      req(length(metrics_to_plot) > 0)

      plot_morphospace_stability(
        stability_result = stability_results(),
        x_axis = input$x_axis,
        metrics = metrics_to_plot,
        show_ci = isTRUE(input$show_ci)
      )
    })

    output$summary_table <- DT::renderDataTable({
      req(stability_results())
      x <- stability_results()$summary_table
      hull_all_na <- all(!is.finite(stability_results()$run_results$occupancy_hull_iou))
      if (hull_all_na) {
        x <- x[!x$metric_type %in% c("occupancy_similarity", "occupancy_similarity_strict"), , drop = FALSE]
      }
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

    output$pc_contrib_plot <- shiny::renderPlot({
      req(stability_results())

      parse_num_csv <- function(x) {
        v <- unlist(strsplit(x, ","), use.names = FALSE)
        v <- trimws(v)
        v <- v[nzchar(v)]
        suppressWarnings(as.numeric(v))
      }

      pcs <- suppressWarnings(as.integer(parse_num_csv(input$pc_contrib_pcs)))
      pcs <- pcs[is.finite(pcs)]
      if (length(pcs) == 0) pcs <- 1:4

      sd_vals <- parse_num_csv(input$pc_contrib_sd_values)
      sd_vals <- sd_vals[is.finite(sd_vals)]
      if (length(sd_vals) == 0) sd_vals <- c(-2, -1, 0, 1, 2)

      plot_morphospace_replicate_sd_overlays(
        stability_result = stability_results(),
        pcs = pcs,
        sd_values = sd_vals,
        nb_pts = as.integer(input$pc_contrib_nb_pts),
        alpha = input$pc_contrib_alpha,
        line_width = input$pc_contrib_line_width
      )
    })

    output$recommendation_table <- DT::renderDataTable({
      req(stability_results())

      thr_vals <- suppressWarnings(parse_fraction_input(input$thresholds))
      thr_vals <- thr_vals[is.finite(thr_vals)]
      thr_vals <- thr_vals[thr_vals > 0 & thr_vals <= 1]
      if (length(thr_vals) == 0) {
        thr_vals <- c(0.95, 0.90, 0.80)
      }

      rec <- summarize_morphospace_stability(
        stability_result = stability_results(),
        threshold = thr_vals,
        sd_threshold = input$sd_threshold,
        similarity_uncertainty = input$similarity_uncertainty,
        angle_threshold_deg = input$angle_threshold_deg
      )
      hull_all_na <- all(!is.finite(stability_results()$run_results$occupancy_hull_iou))
      if (hull_all_na) {
        rec <- rec[!rec$metric_type %in% c("occupancy_similarity", "occupancy_similarity_strict"), , drop = FALSE]
      }
      DT::datatable(
        rec,
        options = list(
          dom = "t",
          paging = FALSE,
          scrollX = TRUE,
          autoWidth = TRUE
        ),
        rownames = FALSE,
        class = "compact nowrap"
      )
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
        paste0("morphospace_stability_bundle_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
      },
      content = function(file) {
        shiny::req(stability_results())

        thr_vals <- suppressWarnings(parse_fraction_input(input$thresholds))
        thr_vals <- thr_vals[is.finite(thr_vals)]
        thr_vals <- thr_vals[thr_vals > 0 & thr_vals <= 1]
        if (length(thr_vals) == 0) {
          thr_vals <- c(0.95, 0.90, 0.80)
        }

        rec_tbl <- summarize_morphospace_stability(
          stability_result = stability_results(),
          threshold = thr_vals,
          sd_threshold = input$sd_threshold,
          similarity_uncertainty = input$similarity_uncertainty,
          angle_threshold_deg = input$angle_threshold_deg
        )

        p_stability <- NULL
        if (length(input$metrics) > 0) {
          p_stability <- plot_morphospace_stability(
            stability_result = stability_results(),
            x_axis = input$x_axis,
            metrics = input$metrics,
            show_ci = isTRUE(input$show_ci)
          )
        }

        p_axis <- plot_pca_axis_shift(
          stability_result = stability_results(),
          value_type = input$axis_value_type,
          show_ci = isTRUE(input$axis_show_ci),
          max_axes = as.integer(input$axis_max_plot)
        )

        parse_num_csv <- function(x) {
          v <- unlist(strsplit(x, ","), use.names = FALSE)
          v <- trimws(v)
          v <- v[nzchar(v)]
          suppressWarnings(as.numeric(v))
        }

        pc_vals <- suppressWarnings(as.integer(parse_num_csv(input$pc_contrib_pcs)))
        pc_vals <- pc_vals[is.finite(pc_vals)]
        if (length(pc_vals) == 0) pc_vals <- 1:4

        pc_sd_vals <- parse_num_csv(input$pc_contrib_sd_values)
        pc_sd_vals <- pc_sd_vals[is.finite(pc_sd_vals)]
        if (length(pc_sd_vals) == 0) pc_sd_vals <- c(-2, -1, 0, 1, 2)

        p_pc_contrib <- plot_morphospace_replicate_sd_overlays(
          stability_result = stability_results(),
          pcs = pc_vals,
          sd_values = pc_sd_vals,
          nb_pts = as.integer(input$pc_contrib_nb_pts),
          alpha = input$pc_contrib_alpha,
          line_width = input$pc_contrib_line_width
        )

        bundle <- list(
          version = "morphospace_stability_bundle_v1",
          created_at = Sys.time(),
          settings = list(
            sampling_mode = input$sampling_mode,
            sample_fractions_input = input$sample_fractions,
            sample_start_n = input$sample_start_n,
            sample_step_n = input$sample_step_n,
            n_repeats = input$n_repeats,
            mode = input$mode,
            reference_mode = input$reference_mode,
            max_pcs = input$max_pcs,
            grid_resolution = input$grid_resolution,
            norm = input$norm,
            start_point = input$start_point,
            align_orientation = input$align_orientation,
            harmonics = input$harmonics,
            seed = input$seed,
            parallel = input$parallel,
            selected_metrics = input$metrics,
            pc_contribution_pcs = pc_vals,
            pc_contribution_sd_values = pc_sd_vals,
            pc_contribution_nb_pts = input$pc_contrib_nb_pts,
            pc_contribution_alpha = input$pc_contrib_alpha,
            pc_contribution_line_width = input$pc_contrib_line_width,
            recommendation_similarity_thresholds = thr_vals,
            recommendation_similarity_uncertainty = input$similarity_uncertainty,
            recommendation_similarity_sd_threshold = input$sd_threshold,
            recommendation_angle_threshold_deg = input$angle_threshold_deg
          ),
          results = stability_results(),
          recommendation_table = rec_tbl,
          plots = list(
            stability_convergence = p_stability,
            axis_shift = p_axis,
            pc_contributions = p_pc_contrib
          )
        )

        saveRDS(bundle, file)
      }
    )

    invisible(stability_results)
  })
}