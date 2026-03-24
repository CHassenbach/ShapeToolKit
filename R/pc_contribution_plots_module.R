#' PC Contribution Plots Module
#'
#' UI and server for generating enhanced PC contribution panels.
#' Loads a reconstruction model (from shape analysis CSV files) and creates a
#' panel showing reconstructed shapes along each selected PC axis at
#' -2 SD, -1 SD, 0, +1 SD, +2 SD plus a blue/-2 SD vs red/+2 SD overlay.
#'
#' @export
pc_contribution_plots_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,

        # --- Load model box ---
        box(
          title = "Load Reconstruction Model",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,

          uiOutput(ns("model_file_ui")),
          helpText("Select any CSV file from a shape analysis output folder (e.g., *_pca_rotation.csv), or point to the folder directly."),

          actionButton(ns("load_model"), "Load Model", class = "btn-primary"),

          hr(),

          uiOutput(ns("model_info_ui"))
        ),

        # --- Settings box ---
        box(
          title = "Plot Settings",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,

          uiOutput(ns("pc_select_ui")),

          hr(),

          numericInput(ns("nb_pts"), "Outline resolution (points per shape)", value = 200, min = 50, max = 500, step = 50),
          numericInput(ns("plot_height_per_row"), "Height per row (pixels)", value = 160, min = 80, max = 400, step = 20),

          hr(),

          actionButton(ns("generate"), "Generate Panel", class = "btn-success"),
          downloadButton(ns("download_jpg"), "Download Panel (JPG)"),
          downloadButton(ns("download_pdf"), "Download Panel (PDF)")
        ),

        # --- Panel output box ---
        box(
          title = "PC Contribution Panel",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          collapsible = FALSE,

          uiOutput(ns("panel_plot_ui"))
        )
      )
    )
  )
}


#' @export
pc_contribution_plots_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Reactive state ---
    loaded_model  <- reactiveVal(NULL)
    panel_data    <- reactiveVal(NULL)
    shinyfiles_ready <- reactiveVal(FALSE)

    # --- Try shinyFiles ---
    observe({
      ready <- requireNamespace("shinyFiles", quietly = TRUE)
      shinyfiles_ready(isTRUE(ready))
    })

    # --- Model file chooser UI ---
    output$model_file_ui <- renderUI({
      if (isTRUE(shinyfiles_ready())) {
        tagList(
          shinyFiles::shinyFilesButton(
            ns("model_file_btn"),
            label = "Choose reconstruction model file",
            title = "Select CSV file (*_pca_rotation.csv)",
            multiple = FALSE
          ),
          br(), br(),
          strong("Selected file: "),
          textOutput(ns("model_file_selected"), inline = TRUE)
        )
      } else {
        textInput(ns("model_file_fallback"), "Model file or folder path", value = "")
      }
    })

    model_file_path <- reactiveVal("")

    observeEvent(shinyfiles_ready(), {
      if (!isTRUE(shinyfiles_ready())) return()
      roots <- .pc_contrib_get_roots()
      shinyFiles::shinyFileChoose(
        input,
        id = "model_file_btn",
        roots = roots,
        session = session,
        filetypes = c("csv", "CSV")
      )
    })

    observeEvent(input$model_file_btn, {
      req(shinyfiles_ready())
      roots <- .pc_contrib_get_roots()
      sel <- try(shinyFiles::parseFilePaths(roots, input$model_file_btn), silent = TRUE)
      if (!inherits(sel, "try-error") && nrow(sel) > 0) {
        model_file_path(as.character(sel$datapath[1]))
      }
    })

    observe({
      if (!isTRUE(shinyfiles_ready()) &&
          !is.null(input$model_file_fallback) &&
          nzchar(input$model_file_fallback)) {
        model_file_path(input$model_file_fallback)
      }
    })

    output$model_file_selected <- renderText({
      path <- model_file_path()
      if (is.null(path) || !nzchar(path)) "No file selected" else basename(path)
    })

    # --- Load model button ---
    observeEvent(input$load_model, {
      path <- model_file_path()
      if (is.null(path) || !nzchar(path)) {
        showNotification("Please select a model file or folder first.", type = "warning")
        return()
      }
      if (!file.exists(path) && !dir.exists(path)) {
        showNotification("Path does not exist.", type = "error")
        return()
      }

      withProgress(message = "Loading reconstruction model...", value = 0.5, {
        model <- tryCatch(
          load_reconstruction_csv(path, validate = TRUE, verbose = FALSE),
          error = function(e) {
            showNotification(paste("Failed to load model:", conditionMessage(e)),
                             type = "error", duration = 8)
            NULL
          }
        )
        if (!is.null(model)) {
          loaded_model(model)
          panel_data(NULL)   # reset panel when a new model is loaded
          showNotification("Model loaded successfully!", type = "message")
        }
      })
    })

    # --- Model info display ---
    output$model_info_ui <- renderUI({
      model <- loaded_model()
      req(model)

      var_lines <- if (!is.null(model$variance_explained)) {
        n_show <- min(10, length(model$variance_explained))
        sapply(seq_len(n_show), function(i) {
          sprintf("PC%d: %.2f%%", i, model$variance_explained[i])
        })
      } else {
        "Variance information not available"
      }

      tagList(
        tags$h4("Model Information", style = "color: #3c8dbc;"),
        tags$table(
          class = "table table-condensed",
          tags$tr(tags$td(tags$strong("Coefficients:")),        tags$td(length(model$center))),
          tags$tr(tags$td(tags$strong("Principal Components:")), tags$td(ncol(model$rotation))),
          if (!is.null(model$parameters$n_harmonics))
            tags$tr(tags$td(tags$strong("Harmonics:")), tags$td(model$parameters$n_harmonics))
        ),
        tags$h5("Variance Explained:"),
        tags$pre(
          style = "max-height: 150px; overflow-y: auto; font-size: 11px;",
          paste(var_lines, collapse = "\n")
        )
      )
    })

    # --- PC selection checkboxes (populated after model load) ---
    output$pc_select_ui <- renderUI({
      model <- loaded_model()
      req(model)

      n_pcs <- model$parameters$n_components
      var_pct <- model$variance_explained

      choices <- setNames(
        as.character(seq_len(n_pcs)),
        sapply(seq_len(n_pcs), function(i) {
          pct <- if (!is.null(var_pct) && i <= length(var_pct)) {
            sprintf(" (%.2f%%)", var_pct[i])
          } else ""
          paste0("PC", i, pct)
        })
      )

      # Default: select first 5 PCs (or all if fewer)
      default_sel <- as.character(seq_len(min(5, n_pcs)))

      tagList(
        tags$h4("Select PCs to include"),
        checkboxGroupInput(
          ns("selected_pcs"),
          label    = NULL,
          choices  = choices,
          selected = default_sel,
          inline   = TRUE
        )
      )
    })

    # --- Generate panel ---
    observeEvent(input$generate, {
      model <- loaded_model()
      if (is.null(model)) {
        showNotification("Please load a reconstruction model first.", type = "warning")
        return()
      }

      sel_pcs <- as.integer(input$selected_pcs)
      if (length(sel_pcs) == 0) {
        showNotification("Please select at least one PC.", type = "warning")
        return()
      }
      sel_pcs <- sort(sel_pcs)

      nb_pts <- input$nb_pts %||% 200

      withProgress(message = "Reconstructing shapes...", value = 0, {
        sd_values <- c(-2, -1, 0, 1, 2)
        n_total   <- length(sel_pcs) * length(sd_values)
        done      <- 0

        shapes_by_pc <- lapply(sel_pcs, function(pc_idx) {
          shapes <- lapply(sd_values, function(sd_val) {
            done <<- done + 1
            incProgress(1 / n_total, detail = sprintf("PC%d @ %+dSD", pc_idx, sd_val))

            n_pcs_model <- ncol(model$rotation)
            scores <- rep(0, n_pcs_model)
            scores[pc_idx] <- sd_val

            tryCatch(
              .pc_contrib_reconstruct(model, scores, nb_pts = nb_pts),
              error = function(e) NULL
            )
          })
          names(shapes) <- paste0("sd", sd_values)
          shapes
        })
        names(shapes_by_pc) <- paste0("PC", sel_pcs)

        panel_data(list(
          shapes_by_pc = shapes_by_pc,
          sel_pcs      = sel_pcs,
          variance     = model$variance_explained,
          nb_pts       = nb_pts
        ))
      })
    })

    # --- Dynamic plot height ---
    plot_height_px <- reactive({
      pd <- panel_data()
      if (is.null(pd)) return(400)
      ppr <- input$plot_height_per_row %||% 160
      length(pd$sel_pcs) * ppr
    })

    output$panel_plot_ui <- renderUI({
      plotOutput(ns("pc_panel"), height = paste0(plot_height_px(), "px"))
    })

    # --- Render panel ---
    output$pc_panel <- renderPlot({
      pd <- panel_data()
      req(pd)
      .pc_contrib_draw_panel(pd)
    }, res = 96)

    # --- Download JPG ---
    output$download_jpg <- downloadHandler(
      filename = function() {
        paste0("pc_contribution_panel_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".jpg")
      },
      content = function(file) {
        pd <- panel_data()
        req(pd)
        n_rows   <- length(pd$sel_pcs)
        ppr      <- input$plot_height_per_row %||% 160
        h_px     <- n_rows * ppr
        grDevices::jpeg(filename = file, width = 1400, height = max(h_px, 200),
                        quality = 95, res = 96, bg = "white")
        .pc_contrib_draw_panel(pd)
        grDevices::dev.off()
      }
    )

    # --- Download PDF ---
    output$download_pdf <- downloadHandler(
      filename = function() {
        paste0("pc_contribution_panel_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
      },
      content = function(file) {
        pd <- panel_data()
        req(pd)
        n_rows <- length(pd$sel_pcs)
        grDevices::pdf(file = file, width = 14, height = max(n_rows * 1.8, 3))
        .pc_contrib_draw_panel(pd)
        grDevices::dev.off()
      }
    )

  })
}


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Build file-system roots for shinyFiles
#' @noRd
.pc_contrib_get_roots <- function() {
  roots <- try(shinyFiles::getVolumes()(), silent = TRUE)
  if (inherits(roots, "try-error") || is.null(roots) || length(roots) == 0) roots <- c()
  if (.Platform$OS.type == "windows" && dir.exists("C:/")) {
    roots <- c(`C:` = "C:/", roots)
  }
  c(roots, Home = normalizePath("~"), `Working Dir` = normalizePath(getwd()))
}


#' Reconstruct a shape outline from a model and a PC score vector (SD units)
#'
#' @param model   Reconstruction model from \code{load_reconstruction_csv}.
#' @param scores  Numeric vector of PC scores in SD units (length = n_components).
#'   All unspecified PCs default to 0. Values are scaled by \code{model$sdev}.
#' @param nb_pts  Number of outline points (default 200).
#' @return A 2-column matrix of (x, y) coordinates.
#' @noRd
.pc_contrib_reconstruct <- function(model, scores, nb_pts = 200) {
  n_pcs <- ncol(model$rotation)

  # Pad / trim scores to match model dimension
  if (length(scores) < n_pcs) {
    full <- rep(0, n_pcs)
    full[seq_along(scores)] <- scores
    scores <- full
  } else if (length(scores) > n_pcs) {
    scores <- scores[seq_len(n_pcs)]
  }

  # Scale SD scores by standard deviations, then project back to coefficient space
  scaled  <- scores * model$sdev[seq_len(n_pcs)]
  coefs   <- model$center + as.vector(scaled %*% t(model$rotation))

  # Split flat coefficient vector into named list (an, bn, cn, dn)
  n_harmonics <- model$parameters$n_harmonics
  coef_list   <- coeff_split(coefs)

  if (is.null(coef_list$ao)) coef_list$ao <- 0
  if (is.null(coef_list$co)) coef_list$co <- 0

  efourier_i(coef_list, nb.h = n_harmonics, nb.pts = nb_pts)
}


#' Draw the full PC contribution panel using base R graphics
#'
#' @param pd Panel data list produced inside the server reactive.
#' @noRd
.pc_contrib_draw_panel <- function(pd) {

  sel_pcs      <- pd$sel_pcs
  shapes_by_pc <- pd$shapes_by_pc
  variance     <- pd$variance

  n_pcs  <- length(sel_pcs)
  # Columns: label | -2SD | -1SD | 0 | +1SD | +2SD | overlay
  n_cols <- 7
  sd_values <- c(-2, -1, 0, 1, 2)
  sd_keys   <- paste0("sd", sd_values)
  sd_labels <- c("-2 SD", "-1 SD", "0", "+1 SD", "+2 SD")

  # Build layout matrix: n_pcs rows x n_cols columns
  mat <- matrix(seq_len(n_pcs * n_cols), nrow = n_pcs, ncol = n_cols, byrow = TRUE)

  # Label column slightly narrower than shape columns; overlay same width
  col_widths <- c(1.2, rep(1.8, 5), 1.8)

  graphics::layout(mat, widths = col_widths, heights = rep(1, n_pcs))

  for (row_i in seq_len(n_pcs)) {
    pc_idx   <- sel_pcs[row_i]
    pc_name  <- paste0("PC", pc_idx)
    pc_shapes <- shapes_by_pc[[pc_name]]

    var_pct <- if (!is.null(variance) && pc_idx <= length(variance)) {
      sprintf("%.2f%%", variance[pc_idx])
    } else {
      ""
    }

    label_text <- if (nzchar(var_pct)) {
      paste0(pc_name, "\n(", var_pct, ")")
    } else {
      pc_name
    }

    # --- Label cell ---
    graphics::par(mar = c(0.2, 0.2, 0.2, 0.2), bg = "white")
    graphics::plot.new()
    graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1))
    graphics::text(0.5, 0.5, label_text, cex = 1.5, font = 2, adj = c(0.5, 0.5))

    # --- Shape cells (-2SD, -1SD, 0, +1SD, +2SD) ---
    for (col_i in seq_along(sd_values)) {
      key    <- sd_keys[col_i]
      coords <- pc_shapes[[key]]
      ttl    <- sd_labels[col_i]

      graphics::par(mar = c(0.5, 0.5, 1.8, 0.5), bg = "white")

      if (!is.null(coords) && is.matrix(coords) && nrow(coords) > 2) {
        graphics::plot(
          coords, type = "n", asp = 1,
          axes = FALSE, xlab = "", ylab = "",
          main = ttl, cex.main = 1.2, font.main = 1
        )
        graphics::polygon(coords, col = "black", border = "black", lwd = 1)
      } else {
        graphics::plot.new()
        graphics::plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
        graphics::title(main = ttl, cex.main = 1.2, font.main = 1)
        graphics::text(0, 0, "error", col = "red", cex = 1.0)
      }
    }

    # --- Overlay cell: -2SD (blue) vs +2SD (red) ---
    coords_neg2 <- pc_shapes[["sd-2"]]
    coords_pos2 <- pc_shapes[["sd2"]]

    graphics::par(mar = c(0.5, 0.5, 1.8, 0.5), bg = "white")

    if (!is.null(coords_neg2) && !is.null(coords_pos2) &&
        is.matrix(coords_neg2) && is.matrix(coords_pos2) &&
        nrow(coords_neg2) > 2 && nrow(coords_pos2) > 2) {

      all_x <- c(coords_neg2[, 1], coords_pos2[, 1])
      all_y <- c(coords_neg2[, 2], coords_pos2[, 2])
      pad_x <- diff(range(all_x)) * 0.05
      pad_y <- diff(range(all_y)) * 0.05

      graphics::plot(
        range(all_x) + c(-pad_x, pad_x),
        range(all_y) + c(-pad_y, pad_y),
        type = "n", asp = 1,
        axes = FALSE, xlab = "", ylab = "",
        main = "Overlay", cex.main = 1.2, font.main = 1
      )
      graphics::polygon(coords_neg2, col = NA, border = "blue",  lwd = 1.5)
      graphics::polygon(coords_pos2, col = NA, border = "red",   lwd = 1.5)

      if (row_i == ceiling(n_pcs / 2)) {
        graphics::legend(
          "bottomright",
          legend = c("-2 SD", "+2 SD"),
          col    = c("blue", "red"),
          lty    = 1, lwd = 1.5, cex = 0.9, bty = "n"
        )
      }

    } else {
      graphics::plot.new()
      graphics::plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      graphics::title(main = "Overlay", cex.main = 1.2, font.main = 1)
      graphics::text(0, 0, "error", col = "red", cex = 1.0)
    }
  }

  # Reset layout
  graphics::layout(1)
}
