#' PCA Saturation Curve Module - UI
#'
#' Shiny module UI for PCA saturation curve analysis
#'
#' @param id Module namespace ID
#'
#' @return Shiny UI elements
#'
#' @export
pca_saturation_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::fluidRow(
      # Input panel
      shiny::column(
        width = 4,
        shinydashboard::box(
          title = "Step 1: Load PCA Results",
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          collapsible = TRUE,
          
          shiny::helpText("Load PC scores from the Excel file saved by 'Run Analysis'."),
          shiny::helpText("The file should contain columns: ID, PC1, PC2, PC3, etc."),
          
          shiny::fileInput(
            ns("pca_file"),
            "Select Excel file from Run Analysis:",
            accept = c(".xlsx", ".xls", ".rds", ".RDS")
          ),
          
          shiny::uiOutput(ns("file_info"))
        ),
        
        shinydashboard::box(
          title = "Step 2: Analysis Parameters",
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          collapsible = TRUE,
          collapsed = TRUE,
          
          shiny::numericInput(
            ns("bootstrap_iterations"),
            "Bootstrap Iterations:",
            value = 200,
            min = 10,
            max = 1000,
            step = 10
          ),
          
          shiny::numericInput(
            ns("min_sample_size"),
            "Minimum Sample Size:",
            value = 5,
            min = 3,
            max = 50,
            step = 1
          ),
          
          shiny::numericInput(
            ns("n_steps"),
            "Number of Sample Size Steps:",
            value = 10,
            min = 5,
            max = 20,
            step = 1
          ),
          
          shiny::numericInput(
            ns("n_pcs"),
            "Number of PCs to Analyze:",
            value = 10,
            min = 1,
            max = 50,
            step = 1
          ),
          
          shiny::checkboxGroupInput(
            ns("metrics"),
            "Variance Metrics:",
            choices = c(
              "Total Variance" = "total_variance",
              "Cumulative Variance" = "cumulative_variance"
            ),
            selected = c("total_variance", "cumulative_variance")
          ),
          
          shiny::checkboxInput(
            ns("parallel"),
            "Use Parallel Processing",
            value = FALSE
          ),
          
          shiny::numericInput(
            ns("seed"),
            "Random Seed (for reproducibility):",
            value = 42,
            min = 1,
            max = 10000
          )
        ),
        
        shinydashboard::box(
          title = "Step 3: Run Analysis",
          status = "success",
          solidHeader = TRUE,
          width = NULL,
          
          shiny::actionButton(
            ns("run_analysis"),
            "Compute Saturation Curve",
            icon = shiny::icon("chart-line"),
            class = "btn-success btn-lg btn-block"
          ),
          
          shiny::br(),
          
          shiny::uiOutput(ns("analysis_status"))
        )
      ),
      
      # Results panel
      shiny::column(
        width = 8,
        shinydashboard::box(
          title = "Saturation Curve Plot",
          status = "info",
          solidHeader = TRUE,
          width = NULL,
          
          shiny::plotOutput(ns("saturation_plot"), height = "500px"),
          
          shiny::hr(),
          
          shiny::fluidRow(
            shiny::column(
              width = 3,
              shiny::selectInput(
                ns("x_axis_type"),
                "X-axis Type:",
                choices = c("Absolute" = "absolute", "Proportion" = "proportion"),
                selected = "absolute"
              )
            ),
            shiny::column(
              width = 3,
              shiny::selectInput(
                ns("theme"),
                "Plot Theme:",
                choices = c("Haug", "inverted_Haug", "publication"),
                selected = "Haug"
              )
            ),
            shiny::column(
              width = 2,
              shiny::checkboxInput(
                ns("normalize"),
                "Normalize Metrics",
                value = TRUE
              )
            ),
            shiny::column(
              width = 2,
              shiny::checkboxInput(
                ns("show_ci"),
                "Show CI",
                value = TRUE
              )
            ),
            shiny::column(
              width = 2,
              shiny::checkboxInput(
                ns("show_points"),
                "Show Points",
                value = TRUE
              )
            )
          ),
          
          shiny::hr(),
          
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::downloadButton(
                ns("download_plot_rds"),
                "Download Plot (RDS)",
                class = "btn-primary btn-block"
              )
            ),
            shiny::column(
              width = 6,
              shiny::downloadButton(
                ns("download_data"),
                "Download Data (CSV)",
                class = "btn-primary btn-block"
              )
            )
          )
        ),
        
        shinydashboard::box(
          title = "Summary Statistics",
          status = "info",
          solidHeader = TRUE,
          width = NULL,
          collapsible = TRUE,
          collapsed = TRUE,
          
          DT::dataTableOutput(ns("summary_table"))
        ),
        
        shinydashboard::box(
          title = "Analysis Information",
          status = "info",
          solidHeader = TRUE,
          width = NULL,
          collapsible = TRUE,
          collapsed = TRUE,
          
          shiny::verbatimTextOutput(ns("analysis_info"))
        )
      )
    )
  )
}


#' PCA Saturation Curve Module - Server
#'
#' Shiny module server for PCA saturation curve analysis
#'
#' @param id Module namespace ID
#'
#' @return Reactive values with analysis results
#'
#' @export
pca_saturation_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Reactive values to store results
    pca_data <- shiny::reactiveVal(NULL)
    saturation_results <- shiny::reactiveVal(NULL)
    
    # File info output
    output$file_info <- shiny::renderUI({
      req(input$pca_file)
      req(pca_data())
      
      file_ext <- tools::file_ext(input$pca_file$name)
      data <- pca_data()
      
      # Get dimensions
      if (inherits(data, "PCA") || inherits(data, "prcomp")) {
        n_specimens <- nrow(data$x)
        n_pcs <- ncol(data$x)
      } else {
        n_specimens <- nrow(data)
        n_pcs <- ncol(data)
      }
      
      shiny::tagList(
        shiny::tags$div(
          style = "padding: 10px; background-color: #d4edda; border-radius: 5px;",
          shiny::icon("check-circle", class = "fa-lg"),
          shiny::strong(" File loaded successfully"),
          shiny::br(),
          shiny::tags$small(paste0("Format: ", toupper(file_ext))),
          shiny::br(),
          shiny::tags$small(paste0("Specimens: ", n_specimens)),
          shiny::br(),
          shiny::tags$small(paste0("PC axes: ", n_pcs))
        )
      )
    })
    
    # Load PCA data when file is uploaded
    shiny::observeEvent(input$pca_file, {
      file_path <- input$pca_file$datapath
      file_ext <- tools::file_ext(input$pca_file$name)
      
      tryCatch({
        if (file_ext %in% c("rds", "RDS")) {
          # Load RDS file
          pca_obj <- readRDS(file_path)
          pca_data(pca_obj)
          
        } else if (file_ext %in% c("xlsx", "xls")) {
          # Load Excel file - expect PC scores from shape_analysis output
          # Excel file from "Run Analysis" has structure: ID, PC1, PC2, PC3, ...
          
          if (requireNamespace("openxlsx", quietly = TRUE)) {
            # Read Excel file (single sheet expected from shape_analysis)
            scores <- openxlsx::read.xlsx(file_path, sheet = 1)
            
            # Identify PC columns by name pattern (PC1, PC2, PC3, ...)
            # This is more robust than just selecting all numeric columns
            all_cols <- names(scores)
            pc_pattern <- "^PC[0-9]+$"  # Matches PC1, PC2, PC10, etc.
            pc_cols <- all_cols[grepl(pc_pattern, all_cols, ignore.case = TRUE)]
            
            # Sort PC columns numerically (PC1, PC2, ..., PC10, PC11, ...)
            if (length(pc_cols) > 0) {
              pc_numbers <- as.numeric(gsub("^PC", "", pc_cols, ignore.case = TRUE))
              pc_cols <- pc_cols[order(pc_numbers)]
            }
            
            if (length(pc_cols) == 0) {
              shiny::showNotification(
                "No PC columns found (looking for PC1, PC2, PC3, ...)",
                type = "error",
                duration = 10
              )
              return()
            }
            
            # Verify they are numeric
            non_numeric <- pc_cols[!sapply(scores[, pc_cols], is.numeric)]
            if (length(non_numeric) > 0) {
              shiny::showNotification(
                paste("Warning: These PC columns are not numeric:", 
                      paste(non_numeric, collapse = ", ")),
                type = "warning",
                duration = 10
              )
              # Remove non-numeric PC columns
              pc_cols <- pc_cols[sapply(scores[, pc_cols], is.numeric)]
            }
            
            # Extract PC scores as numeric matrix
            scores_matrix <- as.matrix(scores[, pc_cols, drop = FALSE])
            
            # Ensure it's actually numeric (force conversion if needed)
            mode(scores_matrix) <- "numeric"
            
            # Check for any conversion issues
            if (any(is.na(scores_matrix)) && !any(is.na(scores[, pc_cols]))) {
              shiny::showNotification(
                "Warning: Some values could not be converted to numeric",
                type = "warning",
                duration = 10
              )
            }
            
            # Set rownames from ID column if it exists
            if ("ID" %in% names(scores)) {
              rownames(scores_matrix) <- scores$ID
            }
            
            pca_data(scores_matrix)
            
            shiny::showNotification(
              paste0("Loaded ", nrow(scores_matrix), " specimens with ", 
                     ncol(scores_matrix), " PCs"),
              type = "message",
              duration = 5
            )
            
          } else {
            shiny::showNotification(
              "Package 'openxlsx' required for Excel files. Please install it.",
              type = "error",
              duration = 10
            )
          }
        }
        
      }, error = function(e) {
        shiny::showNotification(
          paste("Error loading file:", e$message),
          type = "error",
          duration = 10
        )
      })
    })
    
    # Run analysis
    shiny::observeEvent(input$run_analysis, {
      req(pca_data())
      
      # Validate inputs
      if (length(input$metrics) == 0) {
        shiny::showNotification(
          "Please select at least one variance metric",
          type = "warning"
        )
        return()
      }
      
      # Run with progress
      shiny::withProgress(message = "Computing saturation curve...", value = 0, {
        
        tryCatch({
          # Create sample size sequence
          sample_sizes <- seq(1/input$n_steps, 1, by = 1/input$n_steps)
          
          shiny::incProgress(0.1, detail = "Initializing bootstrap...")
          
          # Run saturation analysis
          results <- compute_pca_saturation(
            pca_object = pca_data(),
            sample_sizes = sample_sizes,
            bootstrap_iterations = input$bootstrap_iterations,
            pcs_to_analyze = input$n_pcs,
            metric = input$metrics,
            min_sample_size = input$min_sample_size,
            seed = input$seed,
            parallel = input$parallel,
            n_cores = NULL
          )
          
          shiny::incProgress(0.8, detail = "Finalizing results...")
          
          # Store results
          saturation_results(results)
          
          shiny::incProgress(0.1, detail = "Complete!")
          
          shiny::showNotification(
            "Saturation curve analysis completed successfully!",
            type = "message",
            duration = 5
          )
          
        }, error = function(e) {
          shiny::showNotification(
            paste("Analysis error:", e$message),
            type = "error",
            duration = 10
          )
        })
      })
    })
    
    # Analysis status
    output$analysis_status <- shiny::renderUI({
      if (is.null(saturation_results())) {
        shiny::tags$div(
          style = "padding: 10px; background-color: #fff3cd; border-radius: 5px;",
          shiny::icon("info-circle"),
          shiny::strong(" Ready to analyze")
        )
      } else {
        shiny::tags$div(
          style = "padding: 10px; background-color: #d4edda; border-radius: 5px;",
          shiny::icon("check-circle"),
          shiny::strong(" Analysis complete")
        )
      }
    })
    
    # Create saturation plot
    output$saturation_plot <- shiny::renderPlot({
      req(saturation_results())
      
      plot_pca_saturation(
        saturation_results(),
        x_axis = input$x_axis_type,
        normalize_metrics = input$normalize,
        show_ci = input$show_ci,
        show_points = input$show_points,
        theme_name = input$theme
      )
    })
    
    # Summary table
    output$summary_table <- DT::renderDataTable({
      req(saturation_results())
      
      data <- saturation_results()$saturation_data
      
      # Format for display
      data_display <- data
      data_display$mean <- round(data_display$mean, 3)
      data_display$median <- round(data_display$median, 3)
      data_display$sd <- round(data_display$sd, 3)
      data_display$q025 <- round(data_display$q025, 3)
      data_display$q975 <- round(data_display$q975, 3)
      data_display$sample_proportion <- round(data_display$sample_proportion, 3)
      
      DT::datatable(
        data_display,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          autoWidth = TRUE
        ),
        rownames = FALSE
      )
    })
    
    # Analysis information
    output$analysis_info <- shiny::renderPrint({
      req(saturation_results())
      print(saturation_results())
    })
    
    # Download handlers
    output$download_plot_rds <- shiny::downloadHandler(
      filename = function() {
        paste0("pca_saturation_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
      },
      content = function(file) {
        p <- plot_pca_saturation(
          saturation_results(),
          x_axis = input$x_axis_type,
          normalize_metrics = input$normalize,
          show_ci = input$show_ci,
          show_points = input$show_points,
          theme_name = input$theme
        )
        saveRDS(p, file)
      }
    )
    
    output$download_data <- shiny::downloadHandler(
      filename = function() {
        paste0("pca_saturation_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        data <- saturation_results()$saturation_data
        write.csv(data, file, row.names = FALSE)
      }
    )
    
    # Return reactive values
    return(saturation_results)
  })
}
