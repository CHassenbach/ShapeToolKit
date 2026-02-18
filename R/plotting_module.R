#' Plotting Module (shape_plot)
#'
#' UI and server to configure and render plots using `shape_plot()`.
#' Exposes the main parameters and consumes data from the Data Import module.
#'
#' @param id Module id
#' @param data_reactive A reactive function returning a data.frame from Data Import
#' @export
plotting_ui <- function(id) {
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
          selectInput(ns("x_col"), "X column", choices = NULL),
          selectInput(ns("y_col"), "Y column", choices = NULL),
          selectInput(ns("group_col"), "Group column (optional)", choices = c("(none)" = "")),
          uiOutput(ns("group_vals_ui"))
        ),
        box(
          title = "Styling",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          selectInput(ns("plot_style"), "Plot style", choices = c("Haug", "inverted_Haug", "publication"), selected = "Haug"),
          numericInput(ns("point_size"), "Point size", value = 2, min = 0.1, step = 0.1),
          uiOutput(ns("point_group_color_pickers")),
          uiOutput(ns("point_group_fill_pickers")),
          uiOutput(ns("point_group_shape_pickers")),
          numericInput(ns("title_size"), "Title size", value = 24, min = 6, step = 1),
          numericInput(ns("label_size"), "Axis label size", value = 20, min = 6, step = 1),
          numericInput(ns("tick_size"), "Tick label size", value = 15, min = 6, step = 1),
          numericInput(ns("axis_linewidth"), "Axis linewidth", value = 1, min = 0, step = 0.25),
          numericInput(ns("tick_length"), "Tick length (npc)", value = 0.005, min = 0, step = 0.001),
          numericInput(ns("tick_margin"), "Tick margin", value = 0.05, min = 0, step = 0.01)
        ),
        box(
          title = "Axis & Aspect",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          selectInput(
            ns("aspect"),
            "Plot aspect ratio",
            choices = c(
              "Auto (free unless shapes)" = "auto",
              "Free (no lock)" = "free",
              "1:1" = "1:1",
              "2:1" = "2:1"
            ),
            selected = "auto"
          )
        ),
        box(
          title = "Features - Hulls",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          checkboxInput(ns("hulls_show"), "Show hulls", value = FALSE),
          uiOutput(ns("hull_groups_ui")),
          # Per-group controls for hulls
          uiOutput(ns("hull_group_fill_pickers")),
          uiOutput(ns("hull_group_border_pickers")),
          uiOutput(ns("hull_group_alpha_inputs")),
          uiOutput(ns("hull_group_linetype_inputs")),
          uiOutput(ns("hull_group_linewidth_inputs"))
        ),
        box(
          title = "Features - Contours",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          checkboxInput(ns("contours_show"), "Show contours", value = FALSE),
          uiOutput(ns("contour_groups_ui")),
          uiOutput(ns("contour_group_color_pickers")),
          numericInput(ns("contour_linewidth"), "Contour linewidth", value = 0.5, min = 0, step = 0.1)
        ),
        box(
          title = "Features - Shape overlays",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          checkboxInput(ns("shapes_show"), "Overlay shapes (requires 'shape' column)", value = FALSE),
          checkboxInput(ns("shapes_only_hull"), "Only show shapes at convex hull points", value = TRUE),
          uiOutput(ns("shape_groups_ui")),
          textInput(ns("shape_col"), "Shape column name", value = "shape"),
          numericInput(ns("shape_size"), "Shape overlay size", value = 0.01, min = 0, step = 0.01),
          numericInput(ns("shape_shift"), "Shape overlay shift", value = 0.1, min = 0, step = 0.01),
          numericInput(ns("shape_x_adjust"), "Shape x adjust", value = 0, step = 0.01),
          numericInput(ns("shape_y_adjust"), "Shape y adjust", value = 0, step = 0.01)
        ),
        box(
          title = "Features - Gap Overlay",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          checkboxInput(ns("gaps_show"), "Show morphospace gaps", value = FALSE),
          helpText("Load gap detection results from the Gap Detection module to overlay gap regions."),
          conditionalPanel(
            condition = sprintf("input['%s'] == true", ns("gaps_show")),
            fileInput(
              ns("gap_results_file"),
              "Select Gap Results File (.rds)",
              accept = c(".rds"),
              placeholder = "No file selected"
            ),
            checkboxInput(
              ns("gap_auto_detect_pair"),
              "Auto-detect PC pair from plot axes",
              value = TRUE
            ),
            conditionalPanel(
              condition = sprintf("!input['%s']", ns("gap_auto_detect_pair")),
              uiOutput(ns("gap_pc_pair_selector"))
            ),
            uiOutput(ns("gap_threshold_selector")),
            selectInput(
              ns("gap_display_mode"),
              "Display Mode",
              choices = c(
                "Certainty Heatmap" = "heatmap",
                "Polygon Outlines" = "polygons",
                "Both" = "both"
              ),
              selected = "both"
            ),
            numericInput(
              ns("gap_alpha"),
              "Gap Overlay Alpha",
              value = 0.5,
              min = 0,
              max = 1,
              step = 0.1
            ),
            colourpicker::colourInput(
              ns("gap_low_color"),
              "Low Certainty Color",
              value = "#FFFFFF"
            ),
            colourpicker::colourInput(
              ns("gap_mid_color"),
              "Mid Certainty Color",
              value = "#FFFF00"
            ),
            colourpicker::colourInput(
              ns("gap_high_color"),
              "High Certainty Color",
              value = "#FF0000"
            ),
            colourpicker::colourInput(
              ns("gap_polygon_color"),
              "Polygon Border Color",
              value = "#000000"
            ),
            numericInput(
              ns("gap_polygon_width"),
              "Polygon Border Width",
              value = 1.2,
              min = 0.1,
              max = 5,
              step = 0.1
            ),
            uiOutput(ns("gap_status_ui"))
          )
        ),
        box(
          title = "Labels",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          textInput(ns("title"), "Plot title", value = NULL, placeholder = "Optional title"),
          textInput(ns("x_label"), "X axis label", value = NULL, placeholder = "Default: X column name"),
          textInput(ns("y_label"), "Y axis label", value = NULL, placeholder = "Default: Y column name"),
          textInput(ns("x_adjust"), "X label adjust (x,y)", value = "0,0"),
          textInput(ns("y_adjust"), "Y label adjust (x,y)", value = "0,0"),
          numericInput(ns("x_size"), "X label size", value = 5, min = 1, step = 0.5),
          numericInput(ns("y_size"), "Y label size", value = 5, min = 1, step = 0.5),
          checkboxInput(ns("rotate_y"), "Rotate Y label", value = FALSE),
          checkboxInput(ns("show_borders"), "Show borders", value = TRUE)
        ),
        box(
          title = "Export",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          textInput(ns("export_filename"), "Filename (without extension)", value = "shape_plot_output"),
          helpText("Download the plot as an RDS file (R ggplot object) for export in RStudio."),
          tags$hr(),
          tags$div(
            style = "background-color: #f8f9fa; padding: 12px; border-radius: 4px; margin: 10px 0;",
            tags$strong("How to export in RStudio:"),
            tags$ol(
              tags$li("Download the .rds file using the button below"),
              tags$li("Open RStudio and load the plot:", tags$br(),
                     tags$code("plot <- readRDS('shape_plot_output.rds')")),
              tags$li("Display the plot:", tags$br(),
                     tags$code("print(plot)")),
              tags$li("In the Plots pane, click", tags$strong("Export"), "→ choose your format"),
              tags$li("Available formats: SVG (editable!), PNG, TIFF, PDF, EPS")
            ),
            tags$p(style = "margin-top: 8px; margin-bottom: 0; font-style: italic;",
                  "💡 SVG exports from RStudio preserve individual plot elements for editing in vector graphics software!")
          ),
          downloadButton(ns("download_plot"), "Download ggplot (.rds)", class = "btn-primary")
        ),
        box(
          title = "Interactive Mode",
          status = "warning",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          checkboxInput(ns("interactive_mode"), "Enable Interactive Mode", value = FALSE),
          helpText("Interactive mode requires plotly. Hover over points to see IDs, hover over morphospace to see reconstructed shapes."),
          conditionalPanel(
            condition = sprintf("input['%s'] == true", ns("interactive_mode")),
            helpText("💡 PCA model will be auto-loaded if reconstruction CSV files are found alongside your data."),
            hr(),
            tags$strong("Manual PCA Model Selection:"),
            uiOutput(ns("pca_model_file_ui")),
            helpText("Select any of the PCA model CSV files (rotation, center, or sdev). The other files will be loaded automatically from the same directory."),
            actionButton(ns("load_pca_model_btn"), "Load PCA Model", class = "btn-primary"),
            hr(),
            uiOutput(ns("pca_model_status")),
            hr(),
            numericInput(ns("shape_preview_size"), "Preview shape size (pixels)", value = 400, min = 200, max = 800, step = 50)
          )
        ),
        div(style = "margin: 10px 0;",
            actionButton(ns("render"), "Render plot", class = "btn-success btn-lg")
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == true", ns("interactive_mode")),
          div(style = "margin: 10px 0;",
              actionButton(ns("open_interactive_window"), "Open Interactive Plot in New Window", 
                          class = "btn-info btn-lg", icon = icon("external-link-alt"))
          ),
          box(
            title = "Interactive Plot",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            plotly::plotlyOutput(ns("interactive_plot"), height = 600),
            br(),
            verbatimTextOutput(ns("messages"))
          ),
          box(
            title = "Shape Preview",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            collapsed = FALSE,
            helpText("Hover over the plot to see shapes. Points show actual data, empty space shows reconstructed hypothetical shapes."),
            plotOutput(ns("shape_preview"), height = "auto"),
            verbatimTextOutput(ns("hover_info"))
          )
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == false", ns("interactive_mode")),
          box(
            title = "Plot",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            plotOutput(ns("plot"), height = 600),
            br(),
            verbatimTextOutput(ns("messages"))
          )
        )
      ),
      column(
        width = 12,
        box(
          title = "Legend / Info",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = FALSE,
          helpText("Summary of what is currently displayed in the plot."),
          htmlOutput(ns("legend_html"))
        )
      )
    )
  )
}

#' @export
plotting_server <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Helper: null-or-empty coalesce
    `%||%` <- function(a, b) {
      if (is.null(a)) return(b)
      if (is.character(a) && identical(length(a), 1L) && !nzchar(a)) return(b)
      a
    }

    # Ensure colourpicker is available (auto-install quietly like other modules)
    colourpicker_ready <- reactiveVal(FALSE)
    observe({
      ready <- requireNamespace("colourpicker", quietly = TRUE)
      if (!isTRUE(ready)) {
        try(install.packages("colourpicker", repos = "https://cran.r-project.org", quiet = TRUE), silent = TRUE)
        ready <- requireNamespace("colourpicker", quietly = TRUE)
      }
      colourpicker_ready(isTRUE(ready))
    })

    # Reactive values for interactive mode
    pca_model <- reactiveVal(NULL)
    plot_obj <- reactiveVal(NULL)
    data_file_path <- reactiveVal(NULL)
    hover_shape_coords <- reactiveVal(NULL)
    hover_point_info <- reactiveVal(NULL)
    pca_model_file_path <- reactiveVal("")
    
    # Reactive values for gap overlay
    gap_results <- reactiveVal(NULL)
    
    # Check for shinyFiles availability
    shinyfiles_ready <- reactiveVal(FALSE)
    observe({
      ready <- requireNamespace("shinyFiles", quietly = TRUE)
      if (!isTRUE(ready)) {
        try(install.packages("shinyFiles", repos = "https://cran.r-project.org", quiet = TRUE), silent = TRUE)
        ready <- requireNamespace("shinyFiles", quietly = TRUE)
      }
      shinyfiles_ready(isTRUE(ready))
    })
    
    # PCA model file chooser UI
    output$pca_model_file_ui <- renderUI({
      if (isTRUE(shinyfiles_ready())) {
        tagList(
          shinyFiles::shinyFilesButton(
            ns("pca_model_file_btn"), 
            label = "Choose PCA model file", 
            title = "Select CSV file (*_pca_rotation.csv, *_pca_center.csv, or *_pca_sdev.csv)",
            multiple = FALSE
          ),
          br(), br(),
          strong("Selected file: "), 
          textOutput(ns("pca_model_file_selected"), inline = TRUE)
        )
      } else {
        tagList(
          textInput(ns("pca_model_file_fallback"), "PCA model file path (.csv)", value = ""),
          helpText("Enter the full path to any of the PCA model CSV files (*_pca_rotation.csv, *_pca_center.csv, or *_pca_sdev.csv)")
        )
      }
    })
    
    # Setup file chooser for PCA model
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
      
      shinyFiles::shinyFileChoose(
        input, 
        id = "pca_model_file_btn", 
        roots = roots, 
        session = session,
        filetypes = c("csv", "CSV")
      )
    })
    
    # Handle PCA model file selection
    observeEvent(input$pca_model_file_btn, {
      req(shinyfiles_ready())
      
      roots <- try(shinyFiles::getVolumes()(), silent = TRUE)
      if (inherits(roots, "try-error") || is.null(roots) || length(roots) == 0) {
        roots <- c()
      }
      if (.Platform$OS.type == "windows" && dir.exists("C:/")) {
        roots <- c(`C:` = "C:/", roots)
      }
      roots <- c(roots, Home = normalizePath("~"), `Working Dir` = normalizePath(getwd()))
      
      sel <- try(shinyFiles::parseFilePaths(roots, input$pca_model_file_btn), silent = TRUE)
      if (!inherits(sel, "try-error") && nrow(sel) > 0) {
        pca_model_file_path(as.character(sel$datapath[1]))
      }
    })
    
    # Fallback file path for PCA model
    observe({
      if (!isTRUE(shinyfiles_ready()) && !is.null(input$pca_model_file_fallback) && nzchar(input$pca_model_file_fallback)) {
        pca_model_file_path(input$pca_model_file_fallback)
      }
    })
    
    # Display selected PCA model file
    output$pca_model_file_selected <- renderText({
      path <- pca_model_file_path()
      if (is.null(path) || !nzchar(path)) {
        return("(No file selected)")
      }
      basename(path)
    })
    
    # Load PCA model when button is clicked
    observeEvent(input$load_pca_model_btn, {
      path <- pca_model_file_path()
      
      if (is.null(path) || !nzchar(path)) {
        showNotification("Please select a PCA model file first.", type = "warning", duration = 5)
        return()
      }
      
      if (!file.exists(path)) {
        showNotification(paste("File not found:", path), type = "error", duration = 5)
        return()
      }
      
      tryCatch({
        # Load the model using the file path
        model <- load_pca_model_for_plotting(path)
        
        if (!is.null(model)) {
          pca_model(model)
          showNotification("PCA model loaded successfully!", type = "message", duration = 3)
        } else {
          showNotification("Failed to load PCA model. Check that CSV files exist in the same directory.", 
                         type = "error", duration = 5)
        }
      }, error = function(e) {
        showNotification(paste("Error loading PCA model:", e$message), type = "error", duration = 5)
      })
    })
    
    # Try to load PCA model when data changes (auto-load attempt)
    observe({
      df <- data_reactive()
      req(df)
      
      # Check if data frame has attributes that indicate file path
      file_path <- attr(df, "source_file", exact = TRUE)
      if (!is.null(file_path)) {
        data_file_path(file_path)
        
        # Auto-load PCA model if interactive mode is enabled
        if (isTRUE(input$interactive_mode)) {
          model <- load_pca_model_for_plotting(file_path)
          pca_model(model)
        }
      }
    })
    
    # Load PCA model when interactive mode is toggled on
    observeEvent(input$interactive_mode, {
      if (isTRUE(input$interactive_mode)) {
        file_path <- data_file_path()
        if (!is.null(file_path)) {
          model <- load_pca_model_for_plotting(file_path)
          pca_model(model)
        }
      }
    })
    
    # PCA model status display
    output$pca_model_status <- renderUI({
      model <- pca_model()
      if (is.null(model)) {
        tags$div(
          style = "padding: 10px; background-color: #fff3cd; border: 1px solid #ffc107; border-radius: 4px;",
          tags$strong("⚠️ No PCA model loaded"),
          tags$p("Reconstruction CSV files not found. Interactive mode will show IDs only.", style = "margin: 5px 0 0 0;")
        )
      } else {
        tags$div(
          style = "padding: 10px; background-color: #d4edda; border: 1px solid #28a745; border-radius: 4px;",
          tags$strong("✓ PCA model loaded"),
          tags$ul(
            style = "margin: 5px 0 0 0;",
            tags$li("Method: ", model$method),
            tags$li("Harmonics: ", model$n_harmonics),
            tags$li("PCs: ", length(model$sdev))
          )
        )
      }
    })
    
    # Load gap results when file is selected
    observeEvent(input$gap_results_file, {
      req(input$gap_results_file)
      
      file_path <- input$gap_results_file$datapath
      
      tryCatch({
        results <- readRDS(file_path)
        
        # Validate it's a morphospace_gaps object
        if (!inherits(results, "morphospace_gaps")) {
          showNotification(
            "Invalid file format. Please select a gap detection results file (.rds)",
            type = "error",
            duration = 5
          )
          gap_results(NULL)
          return()
        }
        
        gap_results(results)
        
        showNotification(
          sprintf("Loaded gap results: %d PC pairs, %d gap regions",
                 length(results$results), nrow(results$summary_table)),
          type = "message",
          duration = 5
        )
        
      }, error = function(e) {
        showNotification(
          paste("Error loading gap results:", e$message),
          type = "error",
          duration = 8
        )
        gap_results(NULL)
      })
    })
    
    # Gap PC pair selector
    output$gap_pc_pair_selector <- renderUI({
      results <- gap_results()
      req(results)
      
      pair_names <- names(results$results)
      
      # Try to match current x and y columns
      x_col <- input$x_col
      y_col <- input$y_col
      default_pair <- pair_names[1]
      
      if (!is.null(x_col) && !is.null(y_col)) {
        # Extract PC numbers from column names
        x_pc <- sub("PC", "", x_col)
        y_pc <- sub("PC", "", y_col)
        
        # Try to find matching pair
        match_pair <- sprintf("PC%s-PC%s", x_pc, y_pc)
        if (match_pair %in% pair_names) {
          default_pair <- match_pair
        }
      }
      
      selectInput(
        ns("gap_pc_pair"),
        "Gap PC Pair",
        choices = pair_names,
        selected = default_pair
      )
    })
    
    # Gap threshold selector
    output$gap_threshold_selector <- renderUI({
      results <- gap_results()
      req(results)
      
      thresholds <- results$parameters$certainty_thresholds

      method <- results$parameters$estimation_method
      threshold_label <- if (!is.null(method) && identical(method, "bootstrap_mc")) {
        "Probability Threshold"
      } else {
        "Certainty Threshold"
      }
      
      selectInput(
        ns("gap_threshold"),
        threshold_label,
        choices = thresholds,
        selected = thresholds[length(thresholds)]
      )
    })
    
    # Gap status UI
    output$gap_status_ui <- renderUI({
      results <- gap_results()
      
      if (is.null(results)) {
        tags$div(
          style = "padding: 10px; background-color: #fff3cd; border: 1px solid #ffc107; border-radius: 4px; margin-top: 10px;",
          tags$strong("⚠️ No gap results loaded"),
          tags$p("Please select a gap detection results file (.rds)", style = "margin: 5px 0 0 0;")
        )
      } else {
        tags$div(
          style = "padding: 10px; background-color: #d4edda; border: 1px solid #28a745; border-radius: 4px; margin-top: 10px;",
          tags$strong("✓ Gap results loaded"),
          tags$ul(
            style = "margin: 5px 0 0 0;",
            tags$li("PC Pairs: ", length(results$results)),
            tags$li("Gap Regions: ", nrow(results$summary_table)),
            tags$li("Grid Resolution: ", results$parameters$grid_resolution),
            tags$li("Uncertainty: ", sprintf("%.1f%%", results$parameters$uncertainty * 100))
          )
        )
      }
    })

    # Dynamic per-group point color/fill/shape pickers
    output$point_group_color_pickers <- renderUI({
      if (!isTRUE(colourpicker_ready())) return(NULL)
      df <- data_reactive(); req(df)
      gcol <- input$group_col
      if (is.null(gcol) || gcol == "" || !gcol %in% names(df)) return(NULL)
      groups <- if (!is.null(input$group_vals) && length(input$group_vals)) input$group_vals else unique(df[[gcol]])
      pal <- tryCatch({ if (requireNamespace("scales", quietly = TRUE)) scales::hue_pal()(length(groups)) else rep("#1f77b4", length(groups)) }, error = function(...) rep("#1f77b4", length(groups)))
      safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", as.character(x))
      picker_list <- mapply(function(g, default_col) {
        colourpicker::colourInput(ns(paste0("point_color_", safe_id(g))), paste0("Point color: ", g), value = default_col)
      }, groups, pal, SIMPLIFY = FALSE)
      do.call(tagList, picker_list)
    })
    output$point_group_fill_pickers <- renderUI({
      if (!isTRUE(colourpicker_ready())) return(NULL)
      df <- data_reactive(); req(df)
      gcol <- input$group_col
      if (is.null(gcol) || gcol == "" || !gcol %in% names(df)) return(NULL)
      groups <- if (!is.null(input$group_vals) && length(input$group_vals)) input$group_vals else unique(df[[gcol]])
      pal <- tryCatch({ if (requireNamespace("scales", quietly = TRUE)) scales::hue_pal()(length(groups)) else rep("#1f77b4", length(groups)) }, error = function(...) rep("#1f77b4", length(groups)))
      safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", as.character(x))
      
      # Only show fill picker for shapes that support separate fill (21-25)
      picker_list <- lapply(seq_along(groups), function(i) {
        g <- groups[i]
        default_col <- pal[i]
        shape_val <- input[[paste0("point_shape_", safe_id(g))]]
        
        # Only create picker if shape is 21-25 (fillable shapes)
        if (!is.null(shape_val) && as.numeric(shape_val) >= 21 && as.numeric(shape_val) <= 25) {
          colourpicker::colourInput(ns(paste0("point_fill_", safe_id(g))), 
                                   paste0("Point fill: ", g, " (shape ", shape_val, ")"), 
                                   value = default_col)
        } else {
          NULL
        }
      })
      
      # Remove NULL entries
      picker_list <- picker_list[!sapply(picker_list, is.null)]
      
      if (length(picker_list) == 0) {
        return(helpText("Fill color only applies to shapes 21-25 (filled shapes with borders)"))
      }
      
      do.call(tagList, picker_list)
    })
    output$point_group_shape_pickers <- renderUI({
      df <- data_reactive(); req(df)
      gcol <- input$group_col
      if (is.null(gcol) || gcol == "" || !gcol %in% names(df)) return(NULL)
      groups <- if (!is.null(input$group_vals) && length(input$group_vals)) input$group_vals else unique(df[[gcol]])
      choices <- c(
        "0: Square open" = 0,
        "1: Circle open" = 1,
        "2: Triangle up open" = 2,
        "3: Plus" = 3,
        "4: Cross" = 4,
        "5: Diamond open" = 5,
        "6: Triangle down open" = 6,
        "7: Square cross" = 7,
        "8: Star" = 8,
        "9: Diamond plus" = 9,
        "10: Circle plus" = 10,
        "11: Triangles up/down" = 11,
        "12: Square plus" = 12,
        "13: Circle cross" = 13,
        "14: Triangle square" = 14,
        "15: Square filled" = 15,
        "16: Circle solid" = 16,
        "17: Triangle up solid" = 17,
        "18: Diamond solid" = 18,
        "19: Circle solid (small)" = 19,
        "20: Circle dot" = 20,
        "21: Circle filled" = 21,
        "22: Square filled" = 22,
        "23: Diamond filled" = 23,
        "24: Triangle up filled" = 24,
        "25: Triangle down filled" = 25
      )
      safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", as.character(x))
      picker_list <- lapply(groups, function(g) {
        selectInput(ns(paste0("point_shape_", safe_id(g))), paste0("Point shape: ", g), choices = choices, selected = 21)
      })
      do.call(tagList, picker_list)
    })
    # Dynamic per-group hull fill color pickers
    output$hull_group_fill_pickers <- renderUI({
      if (!isTRUE(colourpicker_ready())) return(NULL)
      df <- data_reactive(); req(df)
      gcol <- input$group_col
      if (is.null(gcol) || gcol == "" || !gcol %in% names(df)) return(NULL)
      if (!isTRUE(input$hulls_show)) return(NULL)
      groups <- input$hull_groups
      if (is.null(groups) || length(groups) == 0) return(NULL)
      # Default palette
      pal <- tryCatch({
        if (requireNamespace("scales", quietly = TRUE)) scales::hue_pal()(length(groups)) else rep("#1f77b4", length(groups))
      }, error = function(...) rep("#1f77b4", length(groups)))
      # Sanitize ID helper
      safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", as.character(x))
      # Build pickers
      picker_list <- mapply(function(g, default_col) {
        inputId <- ns(paste0("hull_fill_", safe_id(g)))
        label <- paste0("Hull fill color: ", g)
        colourpicker::colourInput(inputId, label, value = default_col)
      }, groups, pal, SIMPLIFY = FALSE)
      do.call(tagList, picker_list)
    })
    output$hull_group_border_pickers <- renderUI({
      if (!isTRUE(colourpicker_ready())) return(NULL)
      df <- data_reactive(); req(df)
      gcol <- input$group_col
      if (is.null(gcol) || gcol == "" || !gcol %in% names(df)) return(NULL)
      if (!isTRUE(input$hulls_show)) return(NULL)
      groups <- input$hull_groups
      if (is.null(groups) || length(groups) == 0) return(NULL)
      safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", as.character(x))
      picker_list <- lapply(groups, function(g) {
        inputId <- ns(paste0("hull_border_", safe_id(g)))
        label <- paste0("Hull border color: ", g)
        colourpicker::colourInput(inputId, label, value = "black")
      })
      do.call(tagList, picker_list)
    })
    # Per-group hull alpha inputs
    output$hull_group_alpha_inputs <- renderUI({
      df <- data_reactive(); req(df)
      gcol <- input$group_col
      if (is.null(gcol) || gcol == "" || !gcol %in% names(df)) return(NULL)
      if (!isTRUE(input$hulls_show)) return(NULL)
      groups <- input$hull_groups
      if (is.null(groups) || length(groups) == 0) return(NULL)
      safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", as.character(x))
      picker_list <- lapply(groups, function(g) {
        numericInput(ns(paste0("hull_alpha_", safe_id(g))), paste0("Hull alpha: ", g), value = 0.1, min = 0, max = 1, step = 0.05)
      })
      do.call(tagList, picker_list)
    })
    # Per-group hull linetype inputs
    output$hull_group_linetype_inputs <- renderUI({
      df <- data_reactive(); req(df)
      gcol <- input$group_col
      if (is.null(gcol) || gcol == "" || !gcol %in% names(df)) return(NULL)
      if (!isTRUE(input$hulls_show)) return(NULL)
      groups <- input$hull_groups
      if (is.null(groups) || length(groups) == 0) return(NULL)
      choices <- c("solid","dashed","dotted","dotdash","longdash","twodash")
      safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", as.character(x))
      picker_list <- lapply(groups, function(g) {
        selectInput(ns(paste0("hull_linetype_", safe_id(g))), paste0("Hull linetype: ", g), choices = choices, selected = "solid")
      })
      do.call(tagList, picker_list)
    })
    # Per-group hull linewidth inputs
    output$hull_group_linewidth_inputs <- renderUI({
      df <- data_reactive(); req(df)
      gcol <- input$group_col
      if (is.null(gcol) || gcol == "" || !gcol %in% names(df)) return(NULL)
      if (!isTRUE(input$hulls_show)) return(NULL)
      groups <- input$hull_groups
      if (is.null(groups) || length(groups) == 0) return(NULL)
      safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", as.character(x))
      picker_list <- lapply(groups, function(g) {
        numericInput(ns(paste0("hull_linewidth_", safe_id(g))), paste0("Hull linewidth: ", g), value = 0.5, min = 0, step = 0.1)
      })
      do.call(tagList, picker_list)
    })
    # Dynamic per-group contour color pickers
    output$contour_group_color_pickers <- renderUI({
      if (!isTRUE(colourpicker_ready())) return(NULL)
      df <- data_reactive(); req(df)
      gcol <- input$group_col
      if (is.null(gcol) || gcol == "" || !gcol %in% names(df)) return(NULL)
      if (!isTRUE(input$contours_show)) return(NULL)
      groups <- input$contour_groups
      if (is.null(groups) || length(groups) == 0) return(NULL)
      # Default to a palette like hulls
      pal <- tryCatch({ if (requireNamespace("scales", quietly = TRUE)) scales::hue_pal()(length(groups)) else rep("#1f77b4", length(groups)) }, error = function(...) rep("#1f77b4", length(groups)))
      safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", as.character(x))
      picker_list <- mapply(function(g, default_col) {
        inputId <- ns(paste0("contour_color_", safe_id(g)))
        label <- paste0("Contour color: ", g)
        colourpicker::colourInput(inputId, label, value = default_col)
      }, groups, pal, SIMPLIFY = FALSE)
      do.call(tagList, picker_list)
    })

    # Helper: parse comma-separated values into vector (legacy UI removed, keep in case of inputs from sessions)
    parse_csv <- function(x) {
      if (is.null(x) || !nzchar(x)) return(character())
      parts <- unlist(strsplit(x, ","))
      trimws(parts)
    }

    # Update column selectors when data changes
    observe({
      df <- data_reactive()
      if (is.null(df) || !is.data.frame(df) || ncol(df) == 0) return()
      cols <- names(df)
      updateSelectInput(session, "x_col", choices = cols, selected = if (!is.null(input$x_col) && input$x_col %in% cols) input$x_col else cols[1])
      updateSelectInput(session, "y_col", choices = cols, selected = if (!is.null(input$y_col) && input$y_col %in% cols) input$y_col else cols[min(2, length(cols))])
      updateSelectInput(session, "group_col", choices = c("(none)" = "", cols), selected = if (!is.null(input$group_col) && input$group_col %in% cols) input$group_col else "")
    })

    # Group values UI updates based on group column
    output$group_vals_ui <- renderUI({
      df <- data_reactive(); req(df)
      gcol <- input$group_col
      if (is.null(gcol) || gcol == "" || !gcol %in% names(df)) return(NULL)
      vals <- unique(df[[gcol]])
      selectInput(ns("group_vals"), "Group values (optional)", choices = vals, selected = vals, multiple = TRUE)
    })

    # Hull and contour groups UIs
    output$hull_groups_ui <- renderUI({
      df <- data_reactive(); req(df)
      gcol <- input$group_col
      if (is.null(gcol) || gcol == "" || !gcol %in% names(df)) return(NULL)
      vals <- unique(df[[gcol]])
      selectInput(ns("hull_groups"), "Hull groups (optional)", choices = vals, selected = vals, multiple = TRUE)
    })
    output$contour_groups_ui <- renderUI({
      df <- data_reactive(); req(df)
      gcol <- input$group_col
      if (is.null(gcol) || gcol == "" || !gcol %in% names(df)) return(NULL)
      vals <- unique(df[[gcol]])
      selectInput(ns("contour_groups"), "Contour groups (optional)", choices = vals, selected = vals, multiple = TRUE)
    })
    output$shape_groups_ui <- renderUI({
      df <- data_reactive(); req(df)
      gcol <- input$group_col
      if (is.null(gcol) || gcol == "" || !gcol %in% names(df)) return(NULL)
      vals <- unique(df[[gcol]])
      selectInput(ns("shape_groups"), "Shape overlay groups (optional)", choices = vals, selected = vals, multiple = TRUE)
    })

    # Render plot - reactive values (plot_obj already declared above)
    messages <- reactiveVal("")
    legend_info <- reactiveVal(NULL)

    observeEvent(input$render, {
      df <- data_reactive()
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
        showNotification("No data available. Please import an Excel file in the Data Import tab.", type = "warning")
        return()
      }

      # Validate mappings
      x_col <- input$x_col; y_col <- input$y_col
      if (is.null(x_col) || !nzchar(x_col) || is.null(y_col) || !nzchar(y_col)) {
        showNotification("Please select X and Y columns.", type = "warning"); return()
      }

      gcol <- input$group_col
      gvals <- NULL
      if (!is.null(gcol) && nzchar(gcol)) {
        gvals_sel <- input$group_vals
        if (!is.null(gvals_sel) && length(gvals_sel) > 0) gvals <- gvals_sel
      }

      # Build styling list from per-group pickers; fallback to defaults if none
      # Collect groups used for styling (use group filter if provided)
      style_groups <- NULL
      if (!is.null(gcol) && nzchar(gcol)) {
        style_groups <- if (!is.null(gvals) && length(gvals)) gvals else unique(df[[gcol]])
      }
      # Collect per-group point colors/fills/shapes
      point_colors <- NULL
      point_fills <- NULL
      point_shapes <- NULL
      if (!is.null(style_groups) && length(style_groups)) {
        safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", as.character(x))
        if (isTRUE(colourpicker_ready())) {
          cols <- vapply(style_groups, function(g) input[[paste0("point_color_", safe_id(g))]] %||% NA_character_, character(1))
          fills <- vapply(style_groups, function(g) input[[paste0("point_fill_", safe_id(g))]] %||% NA_character_, character(1))
          if (any(!is.na(cols))) { point_colors <- cols[!is.na(cols)]; names(point_colors) <- as.character(style_groups[!is.na(cols)]) }
          if (any(!is.na(fills))) { point_fills <- fills[!is.na(fills)]; names(point_fills) <- as.character(style_groups[!is.na(fills)]) }
        }
        shapes <- vapply(style_groups, function(g) {
          val <- input[[paste0("point_shape_", safe_id(g))]]
          if (is.null(val)) return(NA_real_)
          # Coerce both character and numeric to numeric safely
          num <- suppressWarnings(as.numeric(val))
          if (is.na(num)) NA_real_ else num
        }, numeric(1))
        if (any(!is.na(shapes))) {
          point_shapes <- shapes[!is.na(shapes)]
          names(point_shapes) <- as.character(style_groups[!is.na(shapes)])
        }
      }

      styling <- list(
        plot_style = input$plot_style,
        point = list(
          color = point_colors,
          fill = point_fills,
          shape = point_shapes,
          size = input$point_size
        ),
        text = list(
          title_size = input$title_size,
          label_size = input$label_size,
          tick_size = input$tick_size
        ),
        axis = list(
          linewidth = input$axis_linewidth,
          tick_length = input$tick_length,
          tick_margin = input$tick_margin,
          aspect = input$aspect
        )
      )

    # Build features list
      # Collect per-group hull fill colors if provided
      hull_fill_by_group <- NULL
      if (isTRUE(colourpicker_ready()) && !is.null(input$hull_groups) && length(input$hull_groups) > 0) {
        groups <- input$hull_groups
        safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", as.character(x))
        cols <- vapply(groups, function(g) {
          val <- input[[paste0("hull_fill_", safe_id(g))]]
          if (is.null(val) || !nzchar(val)) NA_character_ else val
        }, character(1))
        names(cols) <- as.character(groups)
        # If at least one color is provided, keep vector (missing handled downstream)
        if (any(!is.na(cols))) hull_fill_by_group <- cols[!is.na(cols)]
      }
      # Collect per-group hull border colors if provided
      hull_color_by_group <- NULL
      if (isTRUE(colourpicker_ready()) && !is.null(input$hull_groups) && length(input$hull_groups) > 0) {
        groups <- input$hull_groups
        safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", as.character(x))
        cols <- vapply(groups, function(g) {
          val <- input[[paste0("hull_border_", safe_id(g))]]
          if (is.null(val) || !nzchar(val)) NA_character_ else val
        }, character(1))
        names(cols) <- as.character(groups)
        if (any(!is.na(cols))) hull_color_by_group <- cols[!is.na(cols)]
      }
      # Collect per-group contour colors if provided
      contour_color_by_group <- NULL
      if (isTRUE(colourpicker_ready()) && !is.null(input$contour_groups) && length(input$contour_groups) > 0) {
        groups <- input$contour_groups
        safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", as.character(x))
        cols <- vapply(groups, function(g) {
          val <- input[[paste0("contour_color_", safe_id(g))]]
          if (is.null(val) || !nzchar(val)) NA_character_ else val
        }, character(1))
        names(cols) <- as.character(groups)
        if (any(!is.na(cols))) contour_color_by_group <- cols[!is.na(cols)]
      }

      # Collect per-group hull alpha/linetype/linewidth
      hull_alpha_by_group <- NULL
      hull_linetype_by_group <- NULL
      hull_linewidth_by_group <- NULL
      if (!is.null(input$hull_groups) && length(input$hull_groups) > 0) {
        groups <- input$hull_groups
        safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", as.character(x))
        hull_alpha_vals <- vapply(groups, function(g) {
          val <- input[[paste0("hull_alpha_", safe_id(g))]]
          if (is.null(val)) NA_real_ else as.numeric(val)
        }, numeric(1))
        names(hull_alpha_vals) <- as.character(groups)
        if (any(!is.na(hull_alpha_vals))) hull_alpha_by_group <- hull_alpha_vals[!is.na(hull_alpha_vals)]

        hull_linetype_vals <- vapply(groups, function(g) {
          val <- input[[paste0("hull_linetype_", safe_id(g))]]
          if (is.null(val) || !nzchar(val)) NA_character_ else as.character(val)
        }, character(1))
        names(hull_linetype_vals) <- as.character(groups)
        if (any(!is.na(hull_linetype_vals))) hull_linetype_by_group <- hull_linetype_vals[!is.na(hull_linetype_vals)]

        hull_linewidth_vals <- vapply(groups, function(g) {
          val <- input[[paste0("hull_linewidth_", safe_id(g))]]
          if (is.null(val)) NA_real_ else as.numeric(val)
        }, numeric(1))
        names(hull_linewidth_vals) <- as.character(groups)
        if (any(!is.na(hull_linewidth_vals))) hull_linewidth_by_group <- hull_linewidth_vals[!is.na(hull_linewidth_vals)]
      }

      # Build hulls list without fallbacks; let shape_plot defaults apply when not provided
      hulls_list <- list(
        show = isTRUE(input$hulls_show),
        groups = input$hull_groups,
        fill = if (!is.null(hull_fill_by_group)) hull_fill_by_group else NULL,
        color = if (!is.null(hull_color_by_group)) hull_color_by_group else NULL
      )
      if (!is.null(hull_alpha_by_group)) hulls_list$alpha <- hull_alpha_by_group
      if (!is.null(hull_linetype_by_group)) hulls_list$linetype <- hull_linetype_by_group
      if (!is.null(hull_linewidth_by_group)) hulls_list$linewidth <- hull_linewidth_by_group

      features <- list(
        hulls = hulls_list,
        contours = list(
          show = isTRUE(input$contours_show),
          groups = input$contour_groups,
          colors = if (!is.null(contour_color_by_group)) contour_color_by_group else NULL,
          linewidth = input$contour_linewidth
        ),
        shapes = list(
          show = isTRUE(input$shapes_show),
          groups = input$shape_groups,
          shape_col = input$shape_col,
          only_hull = isTRUE(input$shapes_only_hull),
          size = input$shape_size,
          shift = input$shape_shift,
          x_adjust = input$shape_x_adjust,
          y_adjust = input$shape_y_adjust
        )
      )

      # Build labels list
      x_adj <- suppressWarnings(as.numeric(parse_csv(input$x_adjust)))
      y_adj <- suppressWarnings(as.numeric(parse_csv(input$y_adjust)))
      if (length(x_adj) != 2 || any(is.na(x_adj))) x_adj <- c(0, 0)
      if (length(y_adj) != 2 || any(is.na(y_adj))) y_adj <- c(0, 0)

      labels <- list(
        title = if (nzchar(input$title)) input$title else NULL,
        x_label = if (nzchar(input$x_label)) input$x_label else x_col,
        y_label = if (nzchar(input$y_label)) input$y_label else y_col,
        x_adjust = x_adj,
        y_adjust = y_adj,
        x_size = input$x_size,
        y_size = input$y_size,
        rotate_y = isTRUE(input$rotate_y),
        show_borders = isTRUE(input$show_borders)
      )

      # Call shape_plot (disable internal export, we use downloadHandler)
      messages("")
      p <- tryCatch({
        shape_plot(
          data = df,
          x_col = x_col,
          y_col = y_col,
          group_col = if (nzchar(gcol)) gcol else NULL,
          group_vals = gvals,
          styling = styling,
          features = features,
          labels = labels,
          export_options = list(export = FALSE),
          interactive = isTRUE(input$interactive_mode),
          pca_model = if (isTRUE(input$interactive_mode)) pca_model() else NULL,
          verbose = TRUE
        )
      }, error = function(e) {
        messages(paste0("Error: ", conditionMessage(e)))
        NULL
      })
      
      # Add gap overlay if enabled
      if (!is.null(p) && isTRUE(input$gaps_show)) {
        results <- gap_results()
        
        if (!is.null(results)) {
          selected_pc_pair <- NULL
          
          # Auto-detect PC pair from plot axes if enabled
          if (isTRUE(input$gap_auto_detect_pair)) {
            # Try to extract PC numbers from column names
            if (grepl("^PC[0-9]+$", x_col) && grepl("^PC[0-9]+$", y_col)) {
              x_pc <- as.integer(gsub("PC", "", x_col))
              y_pc <- as.integer(gsub("PC", "", y_col))
              
              # Try both possible orderings
              pc_pair_name_1 <- sprintf("PC%d-PC%d", x_pc, y_pc)
              pc_pair_name_2 <- sprintf("PC%d-PC%d", y_pc, x_pc)
              
              # Find which one exists in the results
              if (pc_pair_name_1 %in% names(results$results)) {
                selected_pc_pair <- pc_pair_name_1
              } else if (pc_pair_name_2 %in% names(results$results)) {
                selected_pc_pair <- pc_pair_name_2
              }
            }
          } else {
            # Use manual selection
            selected_pc_pair <- input$gap_pc_pair
          }
          
          if (!is.null(selected_pc_pair) && !is.null(input$gap_threshold)) {
            tryCatch({
              p <- .add_gap_overlay_to_plot(
                plot = p,
                gap_results = results,
                pc_pair = selected_pc_pair,
                threshold = as.numeric(input$gap_threshold),
                display_mode = input$gap_display_mode,
                alpha = input$gap_alpha,
                low_color = input$gap_low_color,
                mid_color = input$gap_mid_color,
                high_color = input$gap_high_color,
                polygon_color = input$gap_polygon_color,
                polygon_width = input$gap_polygon_width
              )
            }, error = function(e) {
              messages(paste0("Gap overlay error: ", conditionMessage(e)))
            })
          } else if (isTRUE(input$gap_auto_detect_pair) && is.null(selected_pc_pair) && grepl("^PC[0-9]+$", x_col) && grepl("^PC[0-9]+$", y_col)) {
            # PC columns but no gap data for this pair (only warn in auto mode)
            messages(sprintf("Note: No gap data available for %s vs %s. Available PC pairs: %s",
                           x_col, y_col, paste(names(results$results), collapse = ", ")))
          }
        }
      }

      plot_obj(p)

      # Build legend info summary with actual colors and hull specimens per group
      lg <- list()
      if (!is.null(gcol) && nzchar(gcol)) {
        used_groups <- if (!is.null(gvals) && length(gvals)) gvals else unique(df[[gcol]])
        used_groups <- as.character(used_groups)
        safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", as.character(x))
        get_or <- function(id, default) { val <- input[[id]]; if (is.null(val) || (is.character(val) && !nzchar(val))) default else val }

        # Base palette used by plotting defaults when user didn't override
        auto_pal <- tryCatch({ if (requireNamespace("scales", quietly = TRUE)) scales::hue_pal()(length(used_groups)) else rep("#1f77b4", length(used_groups)) }, error = function(...) rep("#1f77b4", length(used_groups)))
        names(auto_pal) <- used_groups

        # Colors from pickers or fallback auto palette
        if (isTRUE(colourpicker_ready())) {
          cols <- setNames(vapply(used_groups, function(g) get_or(paste0("point_color_", safe_id(g)), auto_pal[[g]]), character(1)), used_groups)
          fills <- setNames(vapply(used_groups, function(g) get_or(paste0("point_fill_", safe_id(g)), auto_pal[[g]]), character(1)), used_groups)
        } else {
          cols <- auto_pal
          fills <- auto_pal
        }
        # Shapes from pickers or default 21
        shapes <- setNames(vapply(used_groups, function(g) {
          v <- input[[paste0("point_shape_", safe_id(g))]]
          if (is.null(v)) 21 else suppressWarnings(as.numeric(v))
        }, numeric(1)), used_groups)

        # Compute hull specimens only for groups that have hulls enabled/selected
        xcol <- x_col; ycol <- y_col
        hulls_on <- isTRUE(input$hulls_show)
        hull_groups <- input$hull_groups
        specimen_groups <- if (hulls_on) {
          if (!is.null(hull_groups) && length(hull_groups) > 0) intersect(used_groups, as.character(hull_groups)) else used_groups
        } else character(0)
        specimen_rows <- list()
        for (gv in specimen_groups) {
          gd <- df[df[[gcol]] == gv, , drop = FALSE]
          gd <- gd[is.finite(gd[[xcol]]) & is.finite(gd[[ycol]]), , drop = FALSE]
          if (nrow(gd) >= 3) {
            idx <- tryCatch(grDevices::chull(gd[[xcol]], gd[[ycol]]), error = function(...) integer())
            sp <- if (length(idx)) gd[idx, c(1, match(xcol, names(gd)), match(ycol, names(gd))) , drop = FALSE] else gd[0, , drop = FALSE]
          } else if (nrow(gd) > 0) {
            sp <- gd[, c(1, match(xcol, names(gd)), match(ycol, names(gd))) , drop = FALSE]
          } else {
            sp <- gd
          }
          specimen_rows[[gv]] <- sp
        }

        lg$groups <- used_groups
        lg$colors <- cols
        lg$fills <- fills
        lg$shapes <- shapes
  lg$specimens <- specimen_rows
  lg$specimen_groups <- specimen_groups
        lg$group_col <- gcol
        lg$x_col <- xcol; lg$y_col <- ycol
      }

      legend_info(lg)
    })

    output$plot <- renderPlot({
      p <- plot_obj(); req(p)
      print(p)
    })
    
    # Render interactive plotly plot
    output$interactive_plot <- plotly::renderPlotly({
      p <- plot_obj()
      req(p)
      req(inherits(p, "plotly"))
      p
    })
    
    # Handle hover events for shape reconstruction
    observeEvent(plotly::event_data("plotly_hover", source = "morphospace"), {
      hover_data <- plotly::event_data("plotly_hover", source = "morphospace")
      req(hover_data)
      
      # Get hover coordinates
      pc1 <- hover_data$x
      pc2 <- hover_data$y
      point_number <- hover_data$pointNumber
      curve_number <- hover_data$curveNumber
      
      # Check if hovering over actual data point or empty space
      # plotly returns curveNumber for the trace - we need to identify if it's a point trace
      df <- data_reactive()
      req(df)
      x_col <- input$x_col
      y_col <- input$y_col
      
      # Determine if this is a hover over an actual data point
      # We check if the hover event has a valid pointNumber AND if the coordinates
      # match an actual data point within tolerance
      is_data_point <- FALSE
      point_idx <- NULL
      
      if (!is.null(point_number) && !is.null(pc1) && !is.null(pc2)) {
        # Check if coordinates match any data point
        tolerance <- 0.001  # Very small tolerance for exact matches
        x_matches <- abs(df[[x_col]] - pc1) < tolerance
        y_matches <- abs(df[[y_col]] - pc2) < tolerance
        matches <- which(x_matches & y_matches)
        
        if (length(matches) > 0) {
          is_data_point <- TRUE
          point_idx <- matches[1]
        }
      }
      
      if (is_data_point && !is.null(point_idx)) {
        # Hovering over an actual point - show ID and shape if available
        point_id <- if ("ID" %in% names(df)) df$ID[point_idx] else paste("Point", point_idx)
        point_info <- list(
          type = "data_point",
          id = point_id,
          pc1 = df[[x_col]][point_idx],
          pc2 = df[[y_col]][point_idx],
          x_col = x_col,
          y_col = y_col
        )
        
        # Check if shape column exists
        if ("shape" %in% names(df) && !is.null(df$shape[[point_idx]])) {
          # Extract shape coordinates from Out object
          shape_obj <- df$shape[[point_idx]]
          if (inherits(shape_obj, "Out") && !is.null(shape_obj$coo) && length(shape_obj$coo) > 0) {
            point_info$shape_coords <- shape_obj$coo[[1]]
            point_info$shape_source <- "data"
          }
        }
        
        hover_point_info(point_info)
        hover_shape_coords(point_info$shape_coords)
        
      } else {
        # Hovering over empty morphospace - reconstruct hypothetical shape
        model <- pca_model()
        
        if (!is.null(model)) {
          tryCatch({
            # Extract PC indices from column names (e.g., "PC1" -> 1, "PC3" -> 3)
            x_pc_index <- as.integer(gsub("PC", "", x_col))
            y_pc_index <- as.integer(gsub("PC", "", y_col))
            
            # Reconstruct shape from hover coordinates with correct PC axes
            coords <- .reconstruct_shape_from_hover(
              model, 
              x_value = pc1, 
              y_value = pc2, 
              x_pc_index = x_pc_index, 
              y_pc_index = y_pc_index,
              nb_pts = 120
            )
            
            point_info <- list(
              type = "reconstructed",
              pc1 = pc1,
              pc2 = pc2,
              x_col = x_col,
              y_col = y_col,
              shape_source = "reconstruction"
            )
            
            hover_point_info(point_info)
            hover_shape_coords(coords)
            
          }, error = function(e) {
            # Show error in console for debugging
            message("Reconstruction error: ", e$message)
            hover_point_info(list(type = "error", message = e$message, pc1 = pc1, pc2 = pc2))
            hover_shape_coords(NULL)
          })
        } else {
          hover_point_info(list(type = "no_model", pc1 = pc1, pc2 = pc2))
          hover_shape_coords(NULL)
        }
      }
    })
    
    # Render shape preview
    output$shape_preview <- renderPlot({
      coords <- hover_shape_coords()
      info <- hover_point_info()
      
      if (is.null(coords) || is.null(info)) {
        plot.new()
        text(0.5, 0.5, "Hover over the plot to see shapes", cex = 1.2)
        return()
      }
      
      # Plot the shape
      par(mar = c(1, 1, 2, 1))
      plot(coords, type = "l", lwd = 2, asp = 1, 
           xlab = "", ylab = "", axes = FALSE,
           main = if (info$type == "data_point") {
             paste("Shape:", info$id)
           } else {
             paste("Reconstructed Shape")
           })
      
      # Add polygon fill
      polygon(coords, col = "lightblue", border = "darkblue", lwd = 2)
      
      # Add grid
      grid(col = "gray80", lty = "dotted")
      
    }, height = function() input$shape_preview_size %||% 400)
    
    # Render hover info text
    output$hover_info <- renderText({
      info <- hover_point_info()
      
      if (is.null(info)) {
        return("Hover over the plot to see information")
      }
      
      if (info$type == "data_point") {
        x_label <- if (!is.null(info$x_col)) info$x_col else "PC1"
        y_label <- if (!is.null(info$y_col)) info$y_col else "PC2"
        paste0(
          "Data Point\n",
          "ID: ", info$id, "\n",
          x_label, ": ", round(info$pc1, 3), "\n",
          y_label, ": ", round(info$pc2, 3), "\n",
          if (!is.null(info$shape_coords)) "Source: Original shape from data" else "No shape data available"
        )
      } else if (info$type == "reconstructed") {
        x_label <- if (!is.null(info$x_col)) info$x_col else "PC1"
        y_label <- if (!is.null(info$y_col)) info$y_col else "PC2"
        paste0(
          "Hypothetical Shape (Reconstructed)\n",
          x_label, ": ", round(info$pc1, 3), "\n",
          y_label, ": ", round(info$pc2, 3), "\n",
          "Other PCs: 0 (at mean)\n",
          "Source: Real-time reconstruction from PCA model"
        )
      } else if (info$type == "no_model") {
        x_label <- if (!is.null(info$x_col)) info$x_col else "PC1"
        y_label <- if (!is.null(info$y_col)) info$y_col else "PC2"
        paste0(
          "Position: ", x_label, " = ", round(info$pc1, 3), ", ", y_label, " = ", round(info$pc2, 3), "\n",
          "No PCA model loaded - reconstruction not available"
        )
      } else if (info$type == "error") {
        paste0(
          "Reconstruction Error\n",
          "PC1: ", round(info$pc1, 3), "\n",
          "PC2: ", round(info$pc2, 3), "\n",
          "Error: ", info$message
        )
      } else {
        "Hover over the plot"
      }
    })

    output$messages <- renderText({ messages() })
    
    # Open interactive plot in modal window
    observeEvent(input$open_interactive_window, {
      p <- plot_obj()
      
      if (is.null(p)) {
        showNotification("Please render the plot first by clicking 'Render plot'.", 
                        type = "warning", duration = 5)
        return()
      }
      
      if (!inherits(p, "plotly")) {
        showNotification("Interactive mode must be enabled to open in a new window.", 
                        type = "warning", duration = 5)
        return()
      }
      
      # Calculate dynamic heights based on window size
      plot_height <- "calc(100vh - 200px)"  # Full viewport height minus header/footer
      preview_height <- "400px"  # Fixed height to ensure space for info text and avoid margin errors
      
      showModal(modalDialog(
        title = "Interactive Morphospace Explorer",
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close"),
        
        # Add custom CSS for full-screen modal
        tags$head(tags$style(HTML("
          .modal-dialog {
            width: 95vw !important;
            max-width: 95vw !important;
            height: 95vh !important;
            max-height: 95vh !important;
            margin: 2.5vh auto !important;
          }
          .modal-content {
            height: 95vh !important;
          }
          .modal-body {
            height: calc(95vh - 120px) !important;
            overflow-y: auto;
          }
        "))),
        
        fluidRow(
          column(
            width = 8,
            tags$div(
              style = "border: 1px solid #ddd; padding: 10px; border-radius: 4px; height: 100%;",
              tags$h4("Interactive Plot", style = "margin-top: 0;"),
              plotly::plotlyOutput(ns("interactive_plot_modal"), height = plot_height)
            )
          ),
          column(
            width = 4,
            tags$div(
              style = "border: 1px solid #ddd; padding: 10px; border-radius: 4px; height: 100%;",
              tags$h4("Shape Preview", style = "margin-top: 0;"),
              helpText("Hover over the plot to see shapes"),
              plotOutput(ns("shape_preview_modal"), height = preview_height),
              tags$hr(),
              verbatimTextOutput(ns("hover_info_modal"))
            )
          )
        )
      ))
    })
    
    # Render modal plotly (same as main)
    output$interactive_plot_modal <- plotly::renderPlotly({
      p <- plot_obj()
      req(p)
      req(inherits(p, "plotly"))
      p
    })
    
    # Render modal shape preview (same as main)
    output$shape_preview_modal <- renderPlot({
      coords <- hover_shape_coords()
      info <- hover_point_info()
      
      if (is.null(coords) || is.null(info)) {
        plot.new()
        text(0.5, 0.5, "Hover over the plot to see shapes", cex = 1.2)
        return()
      }
      
      # Plot the shape
      par(mar = c(1, 1, 2, 1))
      plot(coords, type = "l", lwd = 2, asp = 1, 
           xlab = "", ylab = "", axes = FALSE,
           main = if (info$type == "data_point") {
             paste("Shape:", info$id)
           } else {
             paste("Reconstructed Shape")
           })
      
      # Add polygon fill
      polygon(coords, col = "lightblue", border = "darkblue", lwd = 2)
      
      # Add grid
      grid(col = "gray80", lty = "dotted")
      
    })
    
    # Render modal hover info (same as main)
    output$hover_info_modal <- renderText({
      info <- hover_point_info()
      
      if (is.null(info)) {
        return("Hover over the plot to see information")
      }
      
      if (info$type == "data_point") {
        x_label <- if (!is.null(info$x_col)) info$x_col else "PC1"
        y_label <- if (!is.null(info$y_col)) info$y_col else "PC2"
        paste0(
          "Data Point\n",
          "ID: ", info$id, "\n",
          x_label, ": ", round(info$pc1, 3), "\n",
          y_label, ": ", round(info$pc2, 3), "\n",
          if (!is.null(info$shape_coords)) "Source: Original shape from data" else "No shape data available"
        )
      } else if (info$type == "reconstructed") {
        x_label <- if (!is.null(info$x_col)) info$x_col else "PC1"
        y_label <- if (!is.null(info$y_col)) info$y_col else "PC2"
        paste0(
          "Hypothetical Shape (Reconstructed)\n",
          x_label, ": ", round(info$pc1, 3), "\n",
          y_label, ": ", round(info$pc2, 3), "\n",
          "Other PCs: 0 (at mean)\n",
          "Source: Real-time reconstruction from PCA model"
        )
      } else if (info$type == "no_model") {
        x_label <- if (!is.null(info$x_col)) info$x_col else "PC1"
        y_label <- if (!is.null(info$y_col)) info$y_col else "PC2"
        paste0(
          "Position: ", x_label, " = ", round(info$pc1, 3), ", ", y_label, " = ", round(info$pc2, 3), "\n",
          "No PCA model loaded - reconstruction not available"
        )
      } else if (info$type == "error") {
        x_label <- if (!is.null(info$x_col)) info$x_col else "PC1"
        y_label <- if (!is.null(info$y_col)) info$y_col else "PC2"
        paste0(
          "Reconstruction Error\n",
          x_label, ": ", round(info$pc1, 3), "\n",
          y_label, ": ", round(info$pc2, 3), "\n",
          "Error: ", info$message
        )
      } else {
        "Hover over the plot"
      }
    })

    output$legend_html <- renderUI({
      lg <- legend_info()
      if (is.null(lg) || is.null(lg$groups)) return(HTML("<em>No grouping applied; legend not required.</em>"))
      # Build HTML with color swatches and specimen tables per group
      rows <- list()
      rows[[length(rows)+1]] <- HTML(paste0("<div><strong>Grouping by:</strong> ", htmltools::htmlEscape(lg$group_col), "</div>"))
      for (g in lg$groups) {
        col <- lg$colors[[g]]; fill <- lg$fills[[g]]; shp <- lg$shapes[[g]]
        swatch <- paste0('<span style="display:inline-block;width:14px;height:14px;border:1px solid #888;background:', htmltools::htmlEscape(fill), ';"></span>')
        header <- HTML(paste0("<div style=\"margin-top:6px;\">", swatch, " <strong>", htmltools::htmlEscape(col), "</strong>: group ", htmltools::htmlEscape(g), " (shape ", htmltools::htmlEscape(as.character(shp)), ")</div>"))
        rows[[length(rows)+1]] <- header
        # Specimen table
        sp <- lg$specimens[[g]]
        if (g %in% (lg$specimen_groups %||% character(0)) && !is.null(sp) && nrow(sp) > 0) {
          # Ensure columns have names: first col is ID (guess), then x,y
          colnames(sp)[1:3] <- c("ID", lg$x_col, lg$y_col)
          # Build compact HTML table
          tbl_head <- paste0("<table class=\"table table-sm\" style=\"margin-left:18px;\"><thead><tr><th>ID</th><th>", htmltools::htmlEscape(lg$x_col), "</th><th>", htmltools::htmlEscape(lg$y_col), "</th></tr></thead><tbody>")
          tbl_rows <- apply(sp, 1, function(r) {
            sprintf("<tr><td>%s</td><td>%s</td><td>%s</td></tr>", htmltools::htmlEscape(as.character(r[1])), htmltools::htmlEscape(format(as.numeric(r[2]), digits = 4)), htmltools::htmlEscape(format(as.numeric(r[3]), digits = 4)))
          })
          tbl <- HTML(paste0(tbl_head, paste(tbl_rows, collapse = ""), "</tbody></table>"))
          rows[[length(rows)+1]] <- tbl
        }
      }
      do.call(tagList, rows)
    })

    # Download handler for plot export as RDS
    output$download_plot <- downloadHandler(
      filename = function() {
        stem <- input$export_filename
        if (is.null(stem) || !nzchar(stem)) stem <- "shape_plot_output"
        paste0(stem, ".rds")
      },
      content = function(file) {
        p <- plot_obj()
        validate(need(!is.null(p), "No plot has been rendered yet. Click 'Render plot' first."))
        saveRDS(p, file)
      }
    )

    # Removed: hull specimens modal button and observer (now shown inline in the legend)

    invisible(list(plot = plot_obj))
  })
}

#' Add Gap Overlay to Plot
#'
#' Internal helper function to add morphospace gap overlay to an existing ggplot
#'
#' @param plot ggplot2 object
#' @param gap_results morphospace_gaps object
#' @param pc_pair Character, PC pair name (e.g., "PC1-PC2")
#' @param threshold Numeric, certainty threshold
#' @param display_mode Character: "heatmap", "polygons", or "both"
#' @param alpha Numeric, overlay alpha
#' @param low_color Color for low certainty
#' @param mid_color Color for mid certainty
#' @param high_color Color for high certainty
#' @param polygon_color Color for polygon borders
#' @param polygon_width Width for polygon borders
#'
#' @keywords internal
.add_gap_overlay_to_plot <- function(plot,
                                     gap_results,
                                     pc_pair,
                                     threshold,
                                     display_mode = "both",
                                     alpha = 0.5,
                                     low_color = "#FFFFFF",
                                     mid_color = "#FFFF00",
                                     high_color = "#FF0000",
                                     polygon_color = "#000000",
                                     polygon_width = 1.2) {
  
  # Extract result for this PC pair
  pair_result <- gap_results$results[[pc_pair]]
  
  if (is.null(pair_result)) {
    warning(sprintf("No gap results found for PC pair: %s", pc_pair))
    return(plot)
  }
  
  # Check if plot is plotly (interactive mode)
  is_plotly <- inherits(plot, "plotly")
  
  if (is_plotly) {
    # For plotly, we need to add traces
    # This is more complex, so for now we'll skip interactive mode overlay
    warning("Gap overlay not yet supported in interactive plotly mode")
    return(plot)
  }
  
  # For ggplot2, add layers
  if (display_mode %in% c("heatmap", "both")) {
    # Add heatmap layer
    gap_certainty <- pair_result$gap_certainty
    grid_x <- pair_result$grid_x
    grid_y <- pair_result$grid_y
    
    # Create data frame for heatmap
    gap_df <- expand.grid(x = grid_x, y = grid_y)
    gap_df$certainty <- as.vector(gap_certainty)
    
    # Filter out NA values
    gap_df <- gap_df[!is.na(gap_df$certainty), ]
    
    # Add heatmap layer
    plot <- plot +
      ggplot2::geom_raster(
        data = gap_df,
        ggplot2::aes(x = x, y = y, fill = certainty),
        alpha = alpha,
        inherit.aes = FALSE
      ) +
      ggplot2::scale_fill_gradient2(
        low = low_color,
        mid = mid_color,
        high = high_color,
        midpoint = 0.5,
        limits = c(0, 1),
        name = {
          method <- gap_results$parameters$estimation_method
          if (!is.null(method) && identical(method, "bootstrap_mc")) {
            "Gap\nProbability"
          } else {
            "Gap\nCertainty"
          }
        }
      )
  }
  
  if (display_mode %in% c("polygons", "both")) {
    # Add polygon outlines
    gap_polygons <- pair_result$gap_polygons
    
    if (!is.null(gap_polygons) && nrow(gap_polygons) > 0) {
      # Filter to selected threshold
      gap_at_threshold <- gap_polygons[gap_polygons$threshold == threshold, ]
      
      if (nrow(gap_at_threshold) > 0) {
        # Convert sf to data frame for ggplot
        gap_coords_list <- lapply(seq_len(nrow(gap_at_threshold)), function(i) {
          coords <- sf::st_coordinates(gap_at_threshold[i, ])
          data.frame(
            x = coords[, 1],
            y = coords[, 2],
            group = i
          )
        })
        
        gap_coords <- do.call(rbind, gap_coords_list)
        
        # Add polygon layer
        plot <- plot +
          ggplot2::geom_polygon(
            data = gap_coords,
            ggplot2::aes(x = x, y = y, group = group),
            fill = NA,
            color = polygon_color,
            size = polygon_width,
            inherit.aes = FALSE
          )
      }
    }
  }
  
  return(plot)
}