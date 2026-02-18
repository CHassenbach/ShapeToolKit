#' Overview Module (Haug_overview)
#'
#' UI and server to configure and render the Haug_overview() results on the
#' imported dataset. Users select an even number of columns (paired as 1-2,
#' 3-4, ...), optional grouping, and basic options for hulls/contours/export.
#'
#' @param id Module id
#' @param data_reactive A reactive function returning a data.frame from Data Import
#' @export
overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        box(
          title = "Data mapping",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = FALSE,
          helpText("Select an even number of columns; pairs are (1,2), (3,4), ..."),
          uiOutput(ns("cols_ui")),
          selectInput(ns("group_col"), "Group column (optional)", choices = c("(none)" = "")),
          uiOutput(ns("group_vals_ui"))
        ),
        box(
          title = "Options",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          selectInput(ns("plot_style"), "Plot style", choices = c("Haug", "inverted_Haug", "publication"), selected = "Haug"),
          numericInput(ns("point_size"), "Point size", value = 2, min = 0.1, step = 0.1),
          selectInput(ns("point_shape"), "Point shape", choices = c(
            "Circle filled (21)" = 21,
            "Circle solid (16)" = 16,
            "Square filled (22)" = 22,
            "Diamond filled (23)" = 23,
            "Triangle up (24)" = 24
          ), selected = 21),
          numericInput(ns("title_size"), "Title size", value = 24, min = 6, step = 1),
          numericInput(ns("label_size"), "Axis label size", value = 20, min = 6, step = 1),
          numericInput(ns("tick_size"), "Tick label size", value = 15, min = 6, step = 1)
        ),
        box(
          title = "Hulls & Contours",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          checkboxInput(ns("hulls_show_all"), "Show individual hull plots per group", value = FALSE),
          numericInput(ns("hull_alpha"), "Hull alpha", value = 0.3, min = 0, max = 1, step = 0.05),
          checkboxInput(ns("contours_show_all"), "Show individual contour plots per group", value = FALSE),
          numericInput(ns("contour_linewidth"), "Contour linewidth", value = 1, min = 0, step = 0.1)
        ),
        box(
          title = "Export & Tables",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          checkboxInput(ns("export_pdf"), "Export Overview to PDF", value = FALSE),
          textInput(ns("export_filename"), "PDF filename", value = "overview_plots.pdf"),
          numericInput(ns("export_width"), "Width (inches)", value = 10, min = 1, step = 0.5),
          numericInput(ns("export_height"), "Height (inches)", value = 10, min = 1, step = 0.5),
          uiOutput(ns("export_dir_ui")),
          textOutput(ns("export_dir_txt")),
          checkboxInput(ns("show_table"), "Generate hull specimen tables", value = FALSE)
        ),
        div(style = "margin: 10px 0;",
            actionButton(ns("run"), "Run Overview", class = "btn-success btn-lg")
        ),
        box(
          title = "Overview Results",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          collapsible = FALSE,
          uiOutput(ns("pair_selector_ui")),
          plotOutput(ns("pair_plot"), height = 500),
          br(),
          tags$h4("Combined Boxplot"),
          plotOutput(ns("boxplot"), height = 400),
          br(),
          verbatimTextOutput(ns("messages"))
        )
      )
    )
  )
}

#' @export
overview_server <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Optional directory picker for export
    shinyfiles_ready <- reactiveVal(FALSE)
    observe({
      ready <- requireNamespace("shinyFiles", quietly = TRUE)
      if (!isTRUE(ready)) {
        try(install.packages("shinyFiles", repos = "https://cran.r-project.org", quiet = TRUE), silent = TRUE)
        ready <- requireNamespace("shinyFiles", quietly = TRUE)
      }
      shinyfiles_ready(isTRUE(ready))
    })

    output$export_dir_ui <- renderUI({
      if (!isTRUE(shinyfiles_ready())) return(helpText("Directory picker unavailable; using working directory"))
      shinyFiles::shinyDirButton(ns("export_dir"), label = "Choose export folder", title = "Select export folder")
    })
    export_dir_path <- reactiveVal("")
    observeEvent(shinyfiles_ready(), {
      if (!isTRUE(shinyfiles_ready())) return()
      roots <- try(shinyFiles::getVolumes()(), silent = TRUE)
      if (inherits(roots, "try-error") || is.null(roots) || length(roots) == 0) {
        roots <- c()
      }
      if (.Platform$OS.type == "windows" && dir.exists("C:/")) {
        roots <- c(`C:` = "C:/", roots)
      }
      roots <- c(roots, Home = normalizePath("~"), `Working Dir` = normalizePath(getwd()))
      shinyFiles::shinyDirChoose(input, id = "export_dir", roots = roots, session = session)
    })
    observeEvent(input$export_dir, {
      req(shinyfiles_ready())
      roots <- try(shinyFiles::getVolumes()(), silent = TRUE)
      if (inherits(roots, "try-error") || is.null(roots) || length(roots) == 0) {
        roots <- c()
      }
      if (.Platform$OS.type == "windows" && dir.exists("C:/")) {
        roots <- c(`C:` = "C:/", roots)
      }
      roots <- c(roots, Home = normalizePath("~"), `Working Dir` = normalizePath(getwd()))
      sel <- try(shinyFiles::parseDirPath(roots, input$export_dir), silent = TRUE)
      if (!inherits(sel, "try-error") && length(sel) > 0) export_dir_path(as.character(sel))
    })
    output$export_dir_txt <- renderText({
      p <- export_dir_path()
      if (is.null(p) || !nzchar(p)) "No folder selected (using working directory)" else p
    })

    # Update inputs when data changes
    output$cols_ui <- renderUI({
      df <- data_reactive(); req(df)
      cols <- names(df)
      # Prefer numeric columns by default
      num_cols <- cols[vapply(df, is.numeric, logical(1))]
      selectizeInput(ns("cols"), "Columns (even number)", choices = cols, selected = head(num_cols, 2), multiple = TRUE, options = list(maxItems = NULL))
    })
    observe({
      df <- data_reactive(); if (is.null(df) || !is.data.frame(df) || ncol(df) == 0) return()
      cols <- names(df)
      updateSelectInput(session, "group_col", choices = c("(none)" = "", cols), selected = if (!is.null(input$group_col) && input$group_col %in% cols) input$group_col else "")
    })
    output$group_vals_ui <- renderUI({
      df <- data_reactive(); req(df)
      gcol <- input$group_col
      if (is.null(gcol) || gcol == "" || !gcol %in% names(df)) return(NULL)
      vals <- unique(df[[gcol]])
      selectInput(ns("group_vals"), "Group values (optional)", choices = vals, selected = vals, multiple = TRUE)
    })

    results <- reactiveVal(NULL)
    messages <- reactiveVal("")

    observeEvent(input$run, {
      df <- data_reactive()
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
        showNotification("No data available. Please import an Excel file in the Data Import tab.", type = "warning")
        return()
      }
      cols <- input$cols
      if (is.null(cols) || length(cols) < 2 || length(cols) %% 2 != 0) {
        showNotification("Please select an even number of columns (at least two).", type = "warning")
        return()
      }
      gcol <- input$group_col
      gvals <- NULL
      if (!is.null(gcol) && nzchar(gcol)) {
        gv <- input$group_vals
        if (!is.null(gv) && length(gv) > 0) gvals <- gv
      }

      # Colors per group (auto if grouped)
      colors <- NULL
      if (!is.null(gcol) && nzchar(gcol)) {
        grps <- if (!is.null(gvals) && length(gvals)) gvals else unique(df[[gcol]])
        colors <- tryCatch({ if (requireNamespace("scales", quietly = TRUE)) scales::hue_pal()(length(grps)) else rep("#1f77b4", length(grps)) }, error = function(...) rep("#1f77b4", length(grps)))
        # Name the colors by group for reliable mapping
        if (length(colors) == length(grps)) names(colors) <- as.character(grps)
      }

      # Build style + options lists
      point_style <- list(size = input$point_size, shape = as.numeric(input$point_shape))
      text_style <- list(title_size = input$title_size, label_size = input$label_size, tick_size = input$tick_size)
      hull_opts <- list(alpha = input$hull_alpha, show_all = isTRUE(input$hulls_show_all))
      contour_opts <- list(linewidth = input$contour_linewidth, show_all = isTRUE(input$contours_show_all))
      export_opts <- list(
        pdf = isTRUE(input$export_pdf),
        filename = if (nzchar(input$export_filename)) input$export_filename else "overview_plots.pdf",
        width = input$export_width,
        height = input$export_height,
        output_dir = {
          p <- export_dir_path(); if (!is.null(p) && nzchar(p)) p else NULL
        }
      )

      messages("")
      res <- tryCatch({
        Haug_overview(
          data = df,
          cols = cols,
          group_col = if (nzchar(gcol)) gcol else NULL,
          group_vals = gvals,
          colors = colors,
          point_style = point_style,
          text_style = text_style,
          plot_style = input$plot_style,
          hull_options = hull_opts,
          contour_options = contour_opts,
          export_options = export_opts,
          show_table = isTRUE(input$show_table),
          verbose = TRUE
        )
      }, error = function(e) {
        messages(paste0("Error: ", conditionMessage(e)))
        NULL
      })
      results(res)
      if (!is.null(res)) showNotification("Overview complete.", type = "message")
    })

    # Pair selector UI based on results
    output$pair_selector_ui <- renderUI({
      res <- results(); if (is.null(res) || is.null(res$plots)) return(NULL)
      pairs <- names(res$plots)
      if (length(pairs) == 0) return(NULL)
      selectInput(ns("pair_select"), "Column pair", choices = pairs, selected = pairs[1])
    })

    output$pair_plot <- renderPlot({
      res <- results(); req(res)
      pair <- input$pair_select
      if (is.null(pair) || !pair %in% names(res$plots)) {
        # Fallback: first available
        if (length(res$plots) == 0) return(invisible(NULL))
        pair <- names(res$plots)[1]
      }
      p <- res$plots[[pair]]$hull_plot
      req(p)
      print(p)
    })

    output$boxplot <- renderPlot({
      res <- results(); req(res)
      if (is.null(res$boxplot)) return(invisible(NULL))
      print(res$boxplot)
    })

    output$messages <- renderText({ messages() })

    invisible(list(results = results))
  })
}
