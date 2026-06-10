#' Shape Panel Module
#'
#' UI and server for generating a specimen shape panel from mapped shape data.
#' Displays all specimens as outline shapes arranged in a figure grid,
#' optionally coloured by a group column. Shapes must have been mapped to the
#' data using the "Map shapes to data" feature in the Data Import module.
#'
#' @param id Module id
#' @param data_reactive A reactive returning a data.frame that includes a
#'   'shape' list column (produced by the Data Import module after mapping
#'   shapes to data).
#' @name shape_panel
NULL

#' @rdname shape_panel
#' @export
shape_panel_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        box(
          title = "Shape Panel Settings",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = FALSE,
          helpText(
            "Generates a high-definition figure plate of all specimens with mapped shapes.",
            "Use 'Map shapes to data' in the Data Import module first."
          ),
          uiOutput(ns("status_ui")),
          hr(),
          fluidRow(
            column(6, uiOutput(ns("label_col_ui"))),
            column(6, uiOutput(ns("group_col_ui")))
          ),
          uiOutput(ns("group_color_ui")),
          hr(),
          fluidRow(
            column(3, numericInput(
              ns("ncols"), "Columns in grid",
              value = 5, min = 1, max = 30, step = 1
            )),
            column(3, numericInput(
              ns("label_size"), "Label font size",
              value = 7, min = 4, max = 24, step = 1
            )),
            column(3, checkboxInput(
              ns("show_labels"), "Show specimen labels", value = TRUE
            )),
            column(3, numericInput(
              ns("panel_spacing"), "Panel spacing (lines)",
              value = 0.5, min = 0, max = 5, step = 0.1
            ))
          ),
          fluidRow(
            column(4, selectInput(
              ns("border_color"), "Shape border colour",
              choices = c(
                "Black"  = "black",
                "White"  = "white",
                "Grey"   = "grey50",
                "None"   = "none"
              ),
              selected = "black"
            )),
            column(4, numericInput(
              ns("fill_alpha"), "Fill transparency",
              value = 0.85, min = 0, max = 1, step = 0.05
            )),
            column(4, numericInput(
              ns("border_size"), "Border line width",
              value = 0.25, min = 0, max = 3, step = 0.05
            ))
          ),
          hr(),
          tags$h5("Export settings"),
          fluidRow(
            column(3, numericInput(
              ns("export_width"), "Width (inches)",
              value = 12, min = 2, max = 80, step = 0.5
            )),
            column(3, numericInput(
              ns("export_height"), "Height (inches)",
              value = 10, min = 2, max = 80, step = 0.5
            )),
            column(3, selectInput(
              ns("export_dpi"), "Resolution (DPI)",
              choices = c("150" = 150, "300" = 300, "600" = 600),
              selected = 300
            )),
            column(3, selectInput(
              ns("export_format"), "Format",
              choices = c(
                "PNG"  = "png",
                "TIFF" = "tiff",
                "SVG"  = "svg",
                "PDF"  = "pdf"
              ),
              selected = "png"
            ))
          ),
          fluidRow(
            column(4, div(
              style = "margin-top: 8px;",
              actionButton(ns("generate"), "Generate Panel", class = "btn-success")
            )),
            column(8, div(
              style = "margin-top: 8px;",
              uiOutput(ns("download_ui"))
            ))
          )
        ),
        box(
          title = "Panel Preview",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          plotOutput(ns("panel_plot"), height = "600px")
        )
      )
    )
  )
}

#' @rdname shape_panel
#' @export
shape_panel_server <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Local null-coalesce helper
    `%||%` <- function(a, b) if (is.null(a)) b else a

    # Safe CSS-id from arbitrary string
    safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", as.character(x))

    # --- colourpicker availability -------------------------------------------
    colourpicker_ready <- reactiveVal(FALSE)
    observe({
      ready <- requireNamespace("colourpicker", quietly = TRUE)
      if (!isTRUE(ready)) {
        try(
          install.packages("colourpicker",
                           repos = "https://cran.r-project.org",
                           quiet = TRUE),
          silent = TRUE
        )
        ready <- requireNamespace("colourpicker", quietly = TRUE)
      }
      colourpicker_ready(isTRUE(ready))
    })

    # --- Status alert --------------------------------------------------------
    output$status_ui <- renderUI({
      df <- data_reactive()
      if (is.null(df)) {
        return(div(
          class = "alert alert-warning",
          "No data loaded. Import and map shapes in the Data Import module."
        ))
      }
      if (!"shape" %in% names(df)) {
        return(div(
          class = "alert alert-warning",
          "No 'shape' column found. Use 'Map shapes to data' in the Data Import module."
        ))
      }
      n_shapes <- sum(!sapply(df[["shape"]], is.null))
      if (n_shapes == 0) {
        return(div(
          class = "alert alert-warning",
          "No shapes were successfully mapped. Check your shape folder and ID column."
        ))
      }
      div(
        class = "alert alert-success",
        sprintf(
          "%d of %d rows have mapped shapes ready for the panel.",
          n_shapes, nrow(df)
        )
      )
    })

    # --- Column selectors ----------------------------------------------------
    non_shape_cols <- reactive({
      df <- data_reactive()
      if (is.null(df)) return(character(0))
      setdiff(names(df), "shape")
    })

    output$label_col_ui <- renderUI({
      cols <- non_shape_cols()
      if (length(cols) == 0) return(NULL)
      selectInput(
        ns("label_col"), "Specimen label column",
        choices  = cols,
        selected = cols[1]
      )
    })

    output$group_col_ui <- renderUI({
      cols <- non_shape_cols()
      if (length(cols) == 0) return(NULL)
      selectInput(
        ns("group_col"), "Group column (for colouring)",
        choices  = c("(none)" = "", cols),
        selected = ""
      )
    })

    # --- Group values and palette --------------------------------------------
    group_vals <- reactive({
      df  <- data_reactive()
      gc  <- input$group_col
      if (is.null(df) || is.null(gc) || !nzchar(gc) || !gc %in% names(df)) {
        return(character(0))
      }
      sort(unique(as.character(df[[gc]][!is.na(df[[gc]])])))
    })

    auto_palette <- reactive({
      gv <- group_vals()
      n  <- length(gv)
      if (n == 0) return(character(0))
      cols <- tryCatch(
        scales::hue_pal()(n),
        error = function(e) grDevices::hcl.colors(n, palette = "Dynamic")
      )
      stats::setNames(cols, gv)
    })

    # --- Per-group colour pickers --------------------------------------------
    output$group_color_ui <- renderUI({
      gc <- input$group_col
      if (is.null(gc) || !nzchar(gc)) return(NULL)
      if (!isTRUE(colourpicker_ready())) {
        return(helpText("Install the 'colourpicker' package to enable per-group colour picking."))
      }
      gv  <- group_vals()
      pal <- auto_palette()
      if (length(gv) == 0) return(NULL)
      tags$div(
        tags$h6("Group colours:"),
        fluidRow(
          lapply(seq_along(gv), function(i) {
            g   <- gv[[i]]
            col <- if (g %in% names(pal)) pal[[g]] else "#333333"
            column(
              2,
              colourpicker::colourInput(
                ns(paste0("grp_color_", safe_id(g))),
                label = g,
                value = col
              )
            )
          })
        )
      )
    })

    resolved_colors <- reactive({
      gv <- group_vals()
      if (length(gv) == 0) return(stats::setNames("#2b6cb0", "All"))
      pal <- auto_palette()
      cols <- vapply(gv, function(g) {
        key <- paste0("grp_color_", safe_id(g))
        val <- input[[key]]
        if (!is.null(val) && nzchar(val)) val
        else if (g %in% names(pal)) pal[[g]]
        else "#333333"
      }, character(1))
      stats::setNames(cols, gv)
    })

    # --- Panel data builder --------------------------------------------------
    build_panel_df <- function() {
      df <- data_reactive()
      if (is.null(df) || !"shape" %in% names(df)) return(NULL)

      # Determine label column
      label_col <- input$label_col %||% setdiff(names(df), "shape")[1]
      if (is.null(label_col) || !label_col %in% names(df)) {
        label_col <- setdiff(names(df), "shape")[1]
      }

      gc         <- input$group_col
      use_groups <- !is.null(gc) && nzchar(gc) && gc %in% names(df)

      # Rows with non-null shapes
      has_shape <- !sapply(df[["shape"]], is.null)
      if (!any(has_shape)) return(NULL)
      sub_df <- df[has_shape, , drop = FALSE]

      # Pre-build unique specimen labels (append row index if duplicated)
      raw_labels <- as.character(sub_df[[label_col]])
      if (anyDuplicated(raw_labels)) {
        spec_labels <- paste0(raw_labels, " (", seq_along(raw_labels), ")")
      } else {
        spec_labels <- raw_labels
      }

      # Build coordinate rows
      rows <- vector("list", nrow(sub_df))
      for (i in seq_len(nrow(sub_df))) {
        shape_obj <- sub_df[["shape"]][[i]]

        # Extract xy matrix
        coords <- tryCatch({
          if (inherits(shape_obj, "Out") &&
              !is.null(shape_obj$coo) &&
              length(shape_obj$coo) > 0) {
            shape_obj$coo[[1]]
          } else if (is.matrix(shape_obj) && ncol(shape_obj) >= 2) {
            shape_obj[, 1:2, drop = FALSE]
          } else {
            NULL
          }
        }, error = function(e) NULL)

        if (is.null(coords) || !is.matrix(coords) || nrow(coords) < 3) next
        if (!all(is.finite(coords))) next

        # Centre and normalise to unit range
        cx <- mean(coords[, 1])
        cy <- mean(coords[, 2])
        coords[, 1] <- coords[, 1] - cx
        coords[, 2] <- coords[, 2] - cy
        max_r <- max(max(abs(coords[, 1])), max(abs(coords[, 2])))
        if (max_r > 0) {
          coords[, 1] <- coords[, 1] / max_r
          coords[, 2] <- coords[, 2] / max_r
        }

        grp_val <- if (use_groups) {
          as.character(sub_df[[gc]][i])
        } else {
          "All specimens"
        }

        rows[[i]] <- data.frame(
          panel_label = spec_labels[i],
          group       = grp_val,
          x           = coords[, 1],
          y           = coords[, 2],
          stringsAsFactors = FALSE
        )
      }

      panel_df <- do.call(rbind, rows[!sapply(rows, is.null)])
      if (is.null(panel_df) || nrow(panel_df) == 0) return(NULL)

      # Preserve order for facets
      panel_df$panel_label <- factor(
        panel_df$panel_label,
        levels = unique(panel_df$panel_label)
      )

      panel_df
    }

    # --- Generate panel ------------------------------------------------------
    panel_plot_rv <- reactiveVal(NULL)

    observeEvent(input$generate, {
      df <- data_reactive()
      if (is.null(df)) {
        showNotification("No data available.", type = "warning")
        return()
      }
      if (!"shape" %in% names(df)) {
        showNotification("No shape column found. Map shapes in the Data Import module.", type = "warning")
        return()
      }

      panel_df <- tryCatch(
        build_panel_df(),
        error = function(e) {
          showNotification(
            paste("Error building panel:", conditionMessage(e)),
            type = "error"
          )
          NULL
        }
      )
      if (is.null(panel_df)) {
        showNotification("No valid shapes to display.", type = "warning")
        return()
      }

      gc         <- input$group_col
      use_groups <- !is.null(gc) && nzchar(gc)

      # Resolve fill colours
      rcols <- if (use_groups) {
        resolved_colors()
      } else {
        c("All specimens" = "#2b6cb0")
      }

      # Fall back colour for any group not in rcols
      all_grps    <- unique(as.character(panel_df$group))
      missing_grps <- setdiff(all_grps, names(rcols))
      if (length(missing_grps) > 0) {
        extra <- stats::setNames(
          rep("#888888", length(missing_grps)),
          missing_grps
        )
        rcols <- c(rcols, extra)
      }

      ncols_val  <- max(1L, as.integer(input$ncols  %||% 5L))
      lbl_size   <- max(4,  as.numeric(input$label_size %||% 7))
      show_lbl   <- isTRUE(input$show_labels)
      spacing    <- max(0,  as.numeric(input$panel_spacing %||% 0.5))
      border_col <- input$border_color %||% "black"
      if (border_col == "none") border_col <- NA_character_
      fill_alpha <- as.numeric(input$fill_alpha %||% 0.85)
      border_w   <- as.numeric(input$border_size %||% 0.25)

      p <- ggplot2::ggplot(
        panel_df,
        ggplot2::aes(x = x, y = y, fill = group)
      ) +
        ggplot2::geom_polygon(
          color     = border_col,
          linewidth = border_w,
          alpha     = fill_alpha
        ) +
        ggplot2::facet_wrap(~panel_label, ncol = ncols_val) +
        ggplot2::coord_fixed() +
        ggplot2::scale_fill_manual(
          values = rcols,
          name   = if (use_groups) gc else NULL,
          guide  = if (use_groups) {
            ggplot2::guide_legend(title = gc)
          } else {
            "none"
          }
        ) +
        ggplot2::theme_void() +
        ggplot2::theme(
          strip.text = if (show_lbl) {
            ggplot2::element_text(
              size   = lbl_size,
              margin = ggplot2::margin(b = 2)
            )
          } else {
            ggplot2::element_blank()
          },
          strip.background = ggplot2::element_blank(),
          legend.position  = if (use_groups) "right" else "none",
          legend.title     = ggplot2::element_text(size = lbl_size + 2),
          legend.text      = ggplot2::element_text(size = lbl_size + 1),
          panel.spacing    = ggplot2::unit(spacing, "lines"),
          plot.margin      = ggplot2::margin(10, 10, 10, 10)
        )

      panel_plot_rv(p)
      showNotification(
        sprintf(
          "Panel generated with %d specimens.",
          length(levels(panel_df$panel_label))
        ),
        type = "message"
      )
    })

    # --- Preview -------------------------------------------------------------
    output$panel_plot <- renderPlot({
      p <- panel_plot_rv()
      req(p)
      print(p)
    }, res = 150)

    # --- Download ------------------------------------------------------------
    output$download_ui <- renderUI({
      if (is.null(panel_plot_rv())) return(NULL)
      downloadButton(ns("download_panel"), "Download Panel", class = "btn-primary")
    })

    output$download_panel <- downloadHandler(
      filename = function() {
        fmt <- input$export_format %||% "png"
        paste0("shape_panel_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", fmt)
      },
      content = function(file) {
        p   <- panel_plot_rv()
        req(p)
        w   <- as.numeric(input$export_width  %||% 12)
        h   <- as.numeric(input$export_height %||% 10)
        dpi <- as.integer(input$export_dpi    %||% 300)
        fmt <- input$export_format %||% "png"
        if (fmt %in% c("svg", "pdf")) {
          ggplot2::ggsave(file, plot = p, device = fmt, width = w, height = h)
        } else {
          ggplot2::ggsave(
            file, plot = p, device = fmt,
            width = w, height = h, dpi = dpi
          )
        }
      }
    )

    invisible(NULL)
  })
}
