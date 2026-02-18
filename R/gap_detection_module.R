# Suppress R CMD check notes for ggplot2 NSE
utils::globalVariables(c("x", "y", "certainty", "group"))

#' Gap Detection Module UI
#'
#' @param id Module namespace ID
#'
#' @import shiny
#' @import shinydashboard
#' @importFrom shinyWidgets pickerInput
#' @importFrom shinyFiles shinyDirButton shinyDirChoose parseDirPath
#'
#' @export
gap_detection_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 12,
        
        # Data Input Box
        box(
          title = "1. Load PCA Data",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = NULL,
          
          helpText("Load a data frame containing PC scores (columns: PC1, PC2, PC3, ...)"),
          
          fileInput(
            ns("pca_data_file"),
            "Select PCA Scores File (CSV, Excel, RDS)",
            accept = c(".csv", ".xlsx", ".xls", ".rds"),
            placeholder = "No file selected"
          ),
          
          conditionalPanel(
            condition = sprintf("output['%s']", ns("data_loaded")),
            ns = ns,
            
            uiOutput(ns("data_preview_ui"))
          )
        )
      )
    ),
    
    fluidRow(
      column(
        width = 12,
        
        # Parameters Box
        box(
          title = "2. Configure Parameters",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,
          width = NULL,
          
          fluidRow(
            column(
              width = 6,
              numericInput(
                ns("uncertainty"),
                "Measurement Uncertainty (%)",
                value = 5,
                min = 0,
                max = 50,
                step = 1
              ),
              
              numericInput(
                ns("grid_resolution"),
                "Grid Resolution",
                value = 150,
                min = 50,
                max = 500,
                step = 10
              ),
              
              numericInput(
                ns("monte_carlo_iterations"),
                "Monte Carlo Iterations",
                value = 100,
                min = 10,
                max = 1000,
                step = 10
              )
            ),
            
            column(
              width = 6,
              numericInput(
                ns("bootstrap_iterations"),
                "Bootstrap Iterations",
                value = 200,
                min = 10,
                max = 1000,
                step = 10
              ),
              
              checkboxInput(
                ns("use_bootstrap_subsample"),
                "Subsample for bootstrap",
                value = FALSE
              ),
              
              conditionalPanel(
                condition = "input.use_bootstrap_subsample == true",
                ns = ns,
                
                numericInput(
                  ns("bootstrap_sample_size"),
                  "Sample Size (count or fraction ≤1)",
                  value = 50,
                  min = 2,
                  step = 1
                ),
                
                helpText(HTML("<small>For fractions, use values ≤1 (e.g., 0.5 = 50%).<br>",
                            "For absolute counts, use values >1 (e.g., 50 specimens).<br>",
                            "<strong>Use case:</strong> Normalize sample sizes when comparing datasets.</small>"))
              ),
              
              checkboxInput(
                ns("manual_pc_pairs"),
                "Manually select PC pairs",
                value = FALSE
              ),
              
              uiOutput(ns("pc_selection_ui")),

              selectInput(
                ns("domain_mode"),
                "Analysis Domain",
                choices = c(
                  "Constrain to hull" = "hull",
                  "Full morphospace (bounding box)" = "full"
                ),
                selected = "hull"
              ),
              
              selectInput(
                ns("hull_type"),
                "Domain Hull Type",
                choices = c("Alpha Hull (Concave)" = "alpha",
                           "Convex Hull" = "convex"),
                selected = "alpha"
              ),
              br(),
              h5("Group Filtering (optional)"),
              uiOutput(ns("group_filter_ui")),
              uiOutput(ns("domain_reference_ui"))
            )
          ),
          
          hr(),
          
          fluidRow(
            column(
              width = 6,
              numericInput(
                ns("hull_buffer"),
                "Hull Buffer (%)",
                value = 5,
                min = 0,
                max = 20,
                step = 1
              ),
              
              selectInput(
                ns("uncertainty_type"),
                "Uncertainty Model",
                choices = c("Gaussian" = "gaussian",
                           "Uniform" = "uniform"),
                selected = "gaussian"
              )
            ),
            
            column(
              width = 6,
              selectInput(
                ns("occupancy_method"),
                "Occupancy Method",
                choices = c("Radius" = "radius",
                           "Kernel Density" = "kde"),
                selected = "radius"
              ),
              
              numericInput(
                ns("occupancy_radius"),
                "Occupancy Radius (× cell size)",
                value = 1.5,
                min = 0.5,
                max = 5,
                step = 0.1
              )
            )
          ),
          
          hr(),
          
          h5("Gap Probability Thresholds for Polygon Extraction"),
          fluidRow(
            column(
              width = 4,
              numericInput(
                ns("threshold_1"),
                "Threshold 1",
                value = 0.80,
                min = 0,
                max = 1,
                step = 0.05
              )
            ),
            column(
              width = 4,
              numericInput(
                ns("threshold_2"),
                "Threshold 2",
                value = 0.90,
                min = 0,
                max = 1,
                step = 0.05
              )
            ),
            column(
              width = 4,
              numericInput(
                ns("threshold_3"),
                "Threshold 3",
                value = 0.95,
                min = 0,
                max = 1,
                step = 0.05
              )
            )
          ),
          
          hr(),
          
          checkboxInput(
            ns("use_parallel"),
            "Use Parallel Processing (experimental)",
            value = FALSE
          ),
          
          conditionalPanel(
            condition = sprintf("input['%s']", ns("use_parallel")),
            ns = ns,
            
            numericInput(
              ns("n_cores"),
              "Number of Cores",
              value = 2,
              min = 1,
              max = parallel::detectCores(),
              step = 1
            )
          )
        )
      )
    ),
    
    fluidRow(
      column(
        width = 12,
        
        # Execution Box
        box(
          title = "3. Run Analysis",
          status = "warning",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = NULL,
          
          h5("Output Settings"),
          
          uiOutput(ns("output_folder_ui")),
          
          hr(),
          
          actionButton(
            ns("run_analysis"),
            "Detect Gaps",
            icon = icon("play"),
            class = "btn-success btn-lg btn-block"
          ),
          
          br(),
          
          conditionalPanel(
            condition = sprintf("output['%s']", ns("analysis_running")),
            ns = ns,
            
            div(
              style = "text-align: center;",
              h4("Analysis in progress..."),
              shinycssloaders::withSpinner(
                uiOutput(ns("progress_text")),
                type = 6,
                color = "#3c8dbc"
              )
            )
          ),
          
          conditionalPanel(
            condition = sprintf("output['%s']", ns("analysis_complete")),
            ns = ns,
            
            hr(),
            
            h4(icon("check-circle"), "Analysis Complete!"),
            
            uiOutput(ns("analysis_summary_ui")),
            
            br(),
            
            uiOutput(ns("saved_file_display")),
            
            downloadButton(
              ns("download_results_rds"),
              "Download Results (RDS)",
              class = "btn-primary"
            ),
            
            downloadButton(
              ns("download_summary_csv"),
              "Download Summary Table (CSV)",
              class = "btn-primary"
            ),
            
            br(), br(),
            
            actionButton(
              ns("clear_results"),
              "Clear Results",
              icon = icon("trash"),
              class = "btn-warning"
            )
          )
        )
      )
    )
  )
}


#' Gap Detection Module Server
#'
#' @param id Module namespace ID
#' @param pca_data Optional reactive containing PCA scores data frame. If provided,
#'   this data will be used instead of requiring file upload.
#'
#' @import shiny
#' @importFrom DT renderDataTable datatable
#'
#' @export
gap_detection_server <- function(id, pca_data = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values
    rv <- reactiveValues(
      pca_data = NULL,
      gap_results = NULL,
      analysis_running = FALSE,
      analysis_complete = FALSE,
      saved_file_path = NULL
    )
    
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
    
    # Setup directory chooser
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
      
      shinyFiles::shinyDirChoose(
        input,
        id = "browse_folder",
        roots = roots,
        session = session
      )
    })
    
    # Handle folder selection
    observeEvent(input$browse_folder, {
      req(shinyfiles_ready())
      
      roots <- try(shinyFiles::getVolumes()(), silent = TRUE)
      if (inherits(roots, "try-error") || is.null(roots) || length(roots) == 0) {
        roots <- c()
      }
      if (.Platform$OS.type == "windows" && dir.exists("C:/")) {
        roots <- c(`C:` = "C:/", roots)
      }
      roots <- c(roots, Home = normalizePath("~"), `Working Dir` = normalizePath(getwd()))
      
      sel <- try(shinyFiles::parseDirPath(roots, input$browse_folder), silent = TRUE)
      if (!inherits(sel, "try-error") && length(sel) > 0) {
        updateTextInput(session, "output_folder", value = as.character(sel))
      }
    })
    
    # Load PCA data from file OR use provided reactive data
    observeEvent(input$pca_data_file, {
      req(input$pca_data_file)
      
      file_path <- input$pca_data_file$datapath
      file_ext <- tools::file_ext(input$pca_data_file$name)
      
      tryCatch({
        if (file_ext == "rds") {
          rv$pca_data <- readRDS(file_path)
        } else if (file_ext == "csv") {
          rv$pca_data <- read.csv(file_path, stringsAsFactors = FALSE)
        } else if (file_ext %in% c("xlsx", "xls")) {
          if (!requireNamespace("readxl", quietly = TRUE)) {
            showNotification("Package 'readxl' required for Excel files",
                           type = "error", duration = 5)
            return()
          }
          rv$pca_data <- as.data.frame(readxl::read_excel(file_path))
        } else {
          showNotification("Unsupported file format", type = "error", duration = 5)
          return()
        }
        
        # Validate data
        pc_cols <- grep("^PC[0-9]+$", colnames(rv$pca_data), value = TRUE)
        
        if (length(pc_cols) < 2) {
          showNotification(
            "Data must contain at least 2 columns named PC1, PC2, etc.",
            type = "error",
            duration = 8
          )
          rv$pca_data <- NULL
          return()
        }
        
        showNotification(
          sprintf("Loaded %d specimens with %d PCs from file", 
                  nrow(rv$pca_data), length(pc_cols)),
          type = "message",
          duration = 5
        )
        
      }, error = function(e) {
        showNotification(
          paste("Error loading file:", conditionMessage(e)),
          type = "error",
          duration = 8
        )
        rv$pca_data <- NULL
      })
    })
    
    # If external PCA data is provided via reactive, use it
    observe({
      if (!is.null(pca_data) && is.reactive(pca_data)) {
        external_data <- pca_data()
        if (!is.null(external_data)) {
          # Validate external data
          pc_cols <- grep("^PC[0-9]+$", colnames(external_data), value = TRUE)
          
          if (length(pc_cols) >= 2) {
            rv$pca_data <- external_data
            showNotification(
              sprintf("Using PCA scores from Shape Analysis: %d specimens with %d PCs", 
                      nrow(external_data), length(pc_cols)),
              type = "message",
              duration = 5
            )
          }
        }
      }
    })
    
    # Data loaded indicator
    output$data_loaded <- reactive({
      !is.null(rv$pca_data)
    })
    outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
    
    # Data preview UI
    output$data_preview_ui <- renderUI({
      req(rv$pca_data)
      
      pc_cols <- grep("^PC[0-9]+$", colnames(rv$pca_data), value = TRUE)
      
      tagList(
        hr(),
        h5(icon("check"), "Data Loaded Successfully"),
        p(sprintf("Specimens: %d | PC columns: %s", 
                 nrow(rv$pca_data), paste(pc_cols, collapse = ", "))),
        
        verbatimTextOutput(session$ns("data_head"))
      )
    })
    
    output$data_head <- renderPrint({
      req(rv$pca_data)
      head(rv$pca_data, 3)
    })

    # Group filter UI (column + values)
    output$group_filter_ui <- renderUI({
      if (is.null(rv$pca_data)) {
        return(selectInput(session$ns("group_column"), "Group column", choices = c("None" = ""), selected = ""))
      }

      pc_cols <- grep("^PC[0-9]+$", colnames(rv$pca_data), value = TRUE)
      candidate_cols <- setdiff(colnames(rv$pca_data), pc_cols)

      if (length(candidate_cols) == 0) {
        return(tagList(
          selectInput(session$ns("group_column"), "Group column", choices = c("None" = ""), selected = ""),
          helpText("No non-PC columns available for grouping.")
        ))
      }

      selected_col <- input$group_column
      if (is.null(selected_col)) selected_col <- ""

      choices <- c("None" = "", stats::setNames(candidate_cols, candidate_cols))

      # Build group values UI if a column is selected
      group_values_ui <- NULL
      if (nzchar(selected_col) && selected_col %in% colnames(rv$pca_data)) {
        vals <- rv$pca_data[[selected_col]]
        vals <- vals[!is.na(vals)]
        vals_unique <- sort(unique(as.character(vals)))

        group_values_ui <- shinyWidgets::pickerInput(
          session$ns("group_values"),
          "Groups to analyze",
          choices = vals_unique,
          selected = vals_unique,
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `selected-text-format` = "count > 3",
            `live-search` = TRUE
          )
        )
      }

      tagList(
        selectInput(
          session$ns("group_column"),
          "Group column",
          choices = choices,
          selected = selected_col
        ),
        group_values_ui
      )
    })

    # Domain reference UI (only relevant when grouping)
    output$domain_reference_ui <- renderUI({
      selected_col <- input$group_column
      if (is.null(selected_col) || !nzchar(selected_col)) {
        return(NULL)
      }

      tagList(
        radioButtons(
          session$ns("domain_reference"),
          "Domain definition",
          choices = c(
            "Use morphospace of selected groups" = "subset",
            "Use morphospace of full dataset" = "all"
          ),
          selected = "subset"
        ),
        helpText("Tip: choose 'full dataset' to compare groups in the same morphospace extent.")
      )
    })
    
    # PC selection UI - switches between automatic and manual modes
    output$pc_selection_ui <- renderUI({
      if (isTRUE(input$manual_pc_pairs)) {
        # Manual mode: show PC pair selectors
        req(rv$pca_data)
        
        pc_cols <- grep("^PC[0-9]+$", colnames(rv$pca_data), value = TRUE)
        pc_numbers <- as.integer(gsub("PC", "", pc_cols))
        
        tagList(
          helpText("Select specific PC pairs to analyze:"),
          fluidRow(
            column(
              width = 6,
              selectInput(
                session$ns("manual_pc_x"),
                "PC X:",
                choices = pc_numbers,
                selected = pc_numbers[1],
                multiple = TRUE
              )
            ),
            column(
              width = 6,
              selectInput(
                session$ns("manual_pc_y"),
                "PC Y:",
                choices = pc_numbers,
                selected = if(length(pc_numbers) > 1) pc_numbers[2] else pc_numbers[1],
                multiple = TRUE
              )
            )
          ),
          helpText("All combinations of selected X and Y PCs will be analyzed."),
          uiOutput(session$ns("manual_pairs_preview"))
        )
      } else {
        # Automatic mode: show max_pcs input
        numericInput(
          session$ns("max_pcs"),
          "Maximum PC to Analyze (all pairs)",
          value = 4,
          min = 2,
          max = 10,
          step = 1
        )
      }
    })
    
    # Preview of manual PC pairs
    output$manual_pairs_preview <- renderUI({
      req(input$manual_pc_x, input$manual_pc_y)
      
      x_pcs <- as.integer(input$manual_pc_x)
      y_pcs <- as.integer(input$manual_pc_y)
      
      # Generate all combinations
      pairs <- expand.grid(x = x_pcs, y = y_pcs)
      # Remove pairs where x >= y (avoid duplicates and self-pairs)
      pairs <- pairs[pairs$x < pairs$y, ]
      
      if (nrow(pairs) == 0) {
        return(tags$p(style = "color: orange;", icon("exclamation-triangle"), " No valid pairs selected (ensure X < Y)"))
      }
      
      pair_names <- sprintf("PC%d-PC%d", pairs$x, pairs$y)
      
      tags$div(
        style = "padding: 10px; background-color: #d1ecf1; border: 1px solid #bee5eb; border-radius: 4px;",
        tags$strong(sprintf("%d pair(s) will be analyzed:", nrow(pairs))),
        tags$br(),
        tags$code(paste(pair_names, collapse = ", "))
      )
    })
    
    # Output folder UI - always shown
    output$output_folder_ui <- renderUI({
      tagList(
        fluidRow(
          column(
            width = 9,
            textInput(
              session$ns("output_folder"),
              "Output Folder",
              value = getwd(),
              placeholder = "Path to save results"
            )
          ),
          column(
            width = 3,
            br(),
            if (isTRUE(shinyfiles_ready())) {
              shinyFiles::shinyDirButton(
                session$ns("browse_folder"),
                "Browse...",
                title = "Select output folder",
                icon = icon("folder-open"),
                class = "btn-default"
              )
            } else {
              actionButton(
                session$ns("browse_folder_disabled"),
                "Browse...",
                icon = icon("folder-open"),
                class = "btn-default",
                disabled = TRUE
              )
            }
          )
        ),
        helpText("Results will be saved as: gap_results_YYYYMMDD_HHMMSS.rds")
      )
    })
    
    # Run analysis
    observeEvent(input$run_analysis, {
      req(rv$pca_data)
      
      # Validate inputs
      if (input$uncertainty <= 0 || input$uncertainty > 50) {
        showNotification("Uncertainty must be between 0 and 50%",
                        type = "error", duration = 5)
        return()
      }
      
      if (input$grid_resolution < 50 || input$grid_resolution > 500) {
        showNotification("Grid resolution must be between 50 and 500",
                        type = "error", duration = 5)
        return()
      }
      
      # Set analysis running
      rv$analysis_running <- TRUE
      rv$analysis_complete <- FALSE
      
      # Determine PC pairs to analyze
      pc_pairs_to_analyze <- NULL
      
      if (isTRUE(input$manual_pc_pairs)) {
        # Manual mode: build custom PC pairs matrix
        req(input$manual_pc_x, input$manual_pc_y)
        
        x_pcs <- as.integer(input$manual_pc_x)
        y_pcs <- as.integer(input$manual_pc_y)
        
        # Generate all combinations
        pairs <- expand.grid(x = x_pcs, y = y_pcs)
        # Remove pairs where x >= y (avoid duplicates and self-pairs)
        pairs <- pairs[pairs$x < pairs$y, ]
        
        if (nrow(pairs) == 0) {
          showNotification(
            "No valid PC pairs selected. Ensure X PCs are less than Y PCs.",
            type = "error",
            duration = 5
          )
          rv$analysis_running <- FALSE
          return()
        }
        
        pc_pairs_to_analyze <- as.matrix(pairs)
        colnames(pc_pairs_to_analyze) <- NULL
      }
      # If not manual mode, pc_pairs_to_analyze remains NULL and max_pcs will be used
      
      # Gather parameters
      certainty_thresholds <- c(
        input$threshold_1,
        input$threshold_2,
        input$threshold_3
      )
      certainty_thresholds <- sort(unique(certainty_thresholds))
      
      n_cores <- NULL
      if (input$use_parallel) {
        n_cores <- input$n_cores
      }
      
      # Run analysis with progress
      withProgress(message = "Detecting gaps in morphospace...", value = 0, {
        
        incProgress(0.05, detail = "Validating data and parameters...")
        
        tryCatch({
          
          # Create a custom progress callback function
          progress_fn <- function(msg, increment) {
            if (increment > 0) {
              incProgress(increment, detail = msg)
            } else {
              setProgress(value = NULL, detail = msg)
            }
          }
          
          # Determine bootstrap sample size if subsampling is enabled
          bootstrap_sample_size <- if (isTRUE(input$use_bootstrap_subsample)) {
            # Only use if checkbox is checked and value exists
            if (!is.null(input$bootstrap_sample_size) && !is.na(input$bootstrap_sample_size)) {
              input$bootstrap_sample_size
            } else {
              NULL
            }
          } else {
            NULL  # Use full dataset when unchecked
          }
          
          results <- detect_morphospace_gaps(
            pca_scores = rv$pca_data,
            uncertainty = input$uncertainty / 100,
            grid_resolution = input$grid_resolution,
            monte_carlo_iterations = input$monte_carlo_iterations,
            bootstrap_iterations = input$bootstrap_iterations,
            bootstrap_sample_size = bootstrap_sample_size,
            group_column = if (!is.null(input$group_column) && nzchar(input$group_column)) input$group_column else NULL,
            groups = if (!is.null(input$group_values) && length(input$group_values) > 0) input$group_values else NULL,
            domain_reference = if (!is.null(input$group_column) && nzchar(input$group_column)) {
              if (!is.null(input$domain_reference) && nzchar(input$domain_reference)) input$domain_reference else "subset"
            } else {
              "subset"
            },
            certainty_thresholds = certainty_thresholds,
            pc_pairs = pc_pairs_to_analyze,  # NULL for automatic mode, custom matrix for manual
            max_pcs = if (is.null(pc_pairs_to_analyze)) input$max_pcs else NULL,
            domain_mode = input$domain_mode,
            hull_type = input$hull_type,
            hull_buffer = input$hull_buffer / 100,
            uncertainty_type = input$uncertainty_type,
            occupancy_method = input$occupancy_method,
            occupancy_radius = input$occupancy_radius,
            use_parallel = input$use_parallel,
            n_cores = n_cores,
            progress_callback = progress_fn,
            verbose = TRUE
          )
          
          rv$gap_results <- results
          rv$analysis_running <- FALSE
          rv$analysis_complete <- TRUE
          
          # Always save results to selected output folder
          output_folder <- input$output_folder
          
          # Validate output folder
          if (is.null(output_folder) || !nzchar(output_folder)) {
            output_folder <- getwd()
          }
          
          # Create directory if it doesn't exist
          if (!dir.exists(output_folder)) {
            tryCatch({
              dir.create(output_folder, recursive = TRUE)
            }, error = function(e) {
              showNotification(
                paste("Could not create output folder:", e$message),
                type = "warning",
                duration = 5
              )
              output_folder <- getwd()
            })
          }
          
          # Generate filename with timestamp
          timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
          filename <- sprintf("gap_results_%s.rds", timestamp)
          filepath <- file.path(output_folder, filename)
          
          # Save results
          tryCatch({
            saveRDS(results, filepath)
            rv$saved_file_path <- filepath
            
            showNotification(
              sprintf("Results saved to: %s", filepath),
              type = "message",
              duration = 8
            )
          }, error = function(e) {
            showNotification(
              paste("Error saving results:", e$message),
              type = "error",
              duration = 8
            )
            rv$saved_file_path <- NULL
          })
          
          showNotification(
            sprintf("Analysis complete! Detected %d gap regions across %d PC pairs.",
                   nrow(results$summary_table), length(results$results)),
            type = "message",
            duration = 8
          )
          
        }, error = function(e) {
          rv$analysis_running <- FALSE
          rv$analysis_complete <- FALSE
          
          showNotification(
            paste("Analysis error:", e$message),
            type = "error",
            duration = 10
          )
        })
      })
    })
    
    # Analysis running indicator
    output$analysis_running <- reactive({
      rv$analysis_running
    })
    outputOptions(output, "analysis_running", suspendWhenHidden = FALSE)
    
    # Analysis complete indicator
    output$analysis_complete <- reactive({
      rv$analysis_complete
    })
    outputOptions(output, "analysis_complete", suspendWhenHidden = FALSE)
    
    # Progress text
    output$progress_text <- renderUI({
      if (rv$analysis_running) {
        p("This may take several minutes depending on parameters...")
      }
    })
    
    # Analysis summary
    output$analysis_summary_ui <- renderUI({
      req(rv$gap_results)
      
      n_pairs <- length(rv$gap_results$results)
      n_gaps <- nrow(rv$gap_results$summary_table)

      group_info <- NULL
      if (!is.null(rv$gap_results$parameters$group_column) && nzchar(rv$gap_results$parameters$group_column)) {
        grp_col <- rv$gap_results$parameters$group_column
        grp_vals <- rv$gap_results$parameters$groups
        grp_txt <- if (is.null(grp_vals) || length(grp_vals) == 0) "(all non-NA)" else paste(grp_vals, collapse = ", ")
        domain_ref <- rv$gap_results$parameters$domain_reference
        group_info <- tagList(
          p(sprintf("Group column: %s", grp_col)),
          p(sprintf("Groups: %s", grp_txt)),
          p(sprintf("Domain reference: %s", domain_ref))
        )
      }
      
      tagList(
        p(sprintf("Analyzed %d PC pairs", n_pairs)),
        p(sprintf("Detected %d gap regions", n_gaps)),
        group_info,
        {
          method <- rv$gap_results$parameters$estimation_method
          thr_label <- if (!is.null(method) && identical(method, "bootstrap_mc")) {
            "Probability thresholds"
          } else {
            "Certainty thresholds"
          }
          p(sprintf("%s: %s",
                    thr_label,
                    paste(rv$gap_results$parameters$certainty_thresholds, collapse = ", ")))
        }
      )
    })
    
    # Display saved file path
    output$saved_file_path <- renderText({
      if (is.null(rv$saved_file_path)) {
        return("(Results not saved)")
      }
      rv$saved_file_path
    })
    
    # Saved file display UI
    output$saved_file_display <- renderUI({
      req(rv$saved_file_path)
      
      tagList(
        hr(),
        div(
          style = "padding: 10px; background-color: #d4edda; border: 1px solid #28a745; border-radius: 4px;",
          tags$strong(icon("check"), " Results saved to:"),
          br(),
          tags$code(rv$saved_file_path, style = "font-size: 11px; word-break: break-all;")
        )
      )
    })
    
    # Download results RDS
    output$download_results_rds <- downloadHandler(
      filename = function() {
        paste0("morphospace_gaps_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
      },
      content = function(file) {
        saveRDS(rv$gap_results, file)
      }
    )
    
    # Download summary CSV
    output$download_summary_csv <- downloadHandler(
      filename = function() {
        paste0("gap_summary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        write.csv(rv$gap_results$summary_table, file, row.names = FALSE)
      }
    )
    
    # Clear results
    observeEvent(input$clear_results, {
      rv$gap_results <- NULL
      rv$analysis_complete <- FALSE
      rv$analysis_running <- FALSE
      rv$saved_file_path <- NULL
      
      showNotification("Results cleared", type = "message", duration = 3)
    })
  })
}
