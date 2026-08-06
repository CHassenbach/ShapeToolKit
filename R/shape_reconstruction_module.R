#' Shape Reconstruction Module
#'
#' UI and server for interactive shape reconstruction from PCA models.
#' Allows loading reconstruction models and generating shapes from PC scores.
#'
#' @param id Module id
#' @export
shape_reconstruction_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        box(
          title = "Load Reconstruction Model",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          
          # Model file chooser
          uiOutput(ns("model_file_ui")),
          helpText("Select a reconstruction model folder containing CSV files, or any CSV file from the folder (e.g., *_pca_rotation.csv)"),
          
          actionButton(ns("load_model"), "Load Model", class = "btn-primary"),
          
          hr(),
          
          # Model information display
          uiOutput(ns("model_info_ui"))
        ),
        
        box(
          title = "Shape Reconstruction Parameters",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          
          # PC score inputs (dynamically generated based on loaded model)
          uiOutput(ns("pc_score_inputs_ui")),
          
          hr(),
          
          # Reconstruction options
          radioButtons(ns("score_type"), "Input type:",
                      choices = c("Standard Deviation units" = "sd",
                                  "Absolute PC values" = "absolute"),
                      selected = "sd", inline = TRUE),
          helpText("SD units: 0 = mean, +/-1 = one SD from mean. Absolute: use actual PC score values from your data."),
          
          checkboxInput(ns("show_original"), "Show original shape (if available)", value = FALSE),
          numericInput(ns("plot_size"), "Plot size (pixels)", value = 600, min = 300, max = 1200, step = 50),
          
          hr(),
          
          actionButton(ns("reconstruct"), "Reconstruct Shape", class = "btn-success"),
          downloadButton(ns("download_coords"), "Download Coordinates (CSV)"),
          downloadButton(ns("download_plot"), "Download Plot (RDS)"),
          downloadButton(ns("download_jpg"), "Download Shape (JPG)")
        ),
        
        box(
          title = "Reconstructed Shape",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          collapsible = FALSE,
          
          plotOutput(ns("shape_plot"), height = "auto"),
          br(),
          verbatimTextOutput(ns("reconstruction_info"))
        ),
        
        box(
          title = "Batch Reconstruction",
          status = "warning",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          
          helpText("Generate multiple shapes along PC axes for morphospace exploration."),
          
          selectInput(ns("batch_pc"), "PC axis to vary", choices = NULL),
          numericInput(ns("batch_min"), "Minimum value (SD units)", value = -3, step = 0.5),
          numericInput(ns("batch_max"), "Maximum value (SD units)", value = 3, step = 0.5),
          numericInput(ns("batch_steps"), "Number of steps", value = 7, min = 3, max = 20, step = 1),
          
          checkboxInput(ns("batch_hold_others"), "Hold other PCs at zero", value = TRUE),
          
          actionButton(ns("batch_reconstruct"), "Generate Batch", class = "btn-warning"),
          
          hr(),
          
          plotOutput(ns("batch_plot"), height = 800),
          downloadButton(ns("download_batch"), "Download Batch Grid (RDS)")
        ),
        
        # ---- New box: Batch Reconstruction from PC Score File ----
        box(
          title = "Batch Reconstruction from PC Score File",
          status = "success",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          
          helpText("Load a file containing PC scores (e.g., from gap detection output) and reconstruct a shape for every row, saving results to a folder."),
          
          # PC scores file chooser
          uiOutput(ns("pc_scores_file_ui")),
          helpText("Supported formats: .xlsx, .ods, .csv"),
          actionButton(ns("load_pc_scores_file"), "Load PC Score File", class = "btn-info"),
          
          hr(),
          
          # Preview of loaded data
          uiOutput(ns("pc_scores_preview_ui")),
          
          # Column mapping (shown after file is loaded)
          uiOutput(ns("pc_column_mapping_ui")),
          
          # Name/ID column selector
          uiOutput(ns("pc_name_column_ui")),
          
          # Score type
          radioButtons(ns("file_batch_score_type"), "PC score input type:",
                       choices = c("Absolute PC values" = "absolute",
                                   "Standard Deviation units" = "sd"),
                       selected = "absolute", inline = TRUE),
          helpText("Use 'Absolute' for real PC scores from your analysis output. Use 'SD units' if values are expressed as multiples of each PC's standard deviation."),
          
          hr(),
          
          # Output folder chooser
          uiOutput(ns("output_dir_ui")),
          helpText("Select the folder where reconstructed shapes will be saved."),
          
          checkboxGroupInput(
            ns("file_batch_formats"), "Output formats:",
            choices = c("JPG image" = "jpg", "PNG image" = "png", "Coordinates CSV" = "csv"),
            selected = "jpg", inline = TRUE
          ),
          numericInput(ns("file_batch_img_size"), "Image size (pixels)", value = 800, min = 200, max = 2000, step = 100),
          
          hr(),
          
          actionButton(ns("reconstruct_from_file"), "Reconstruct All & Save", class = "btn-success btn-lg"),
          
          hr(),
          
          tags$strong("Reconstruction log:"),
          verbatimTextOutput(ns("file_batch_log"))
        )
      )
    )
  )
}

#' Shape Reconstruction Module Server
#'
#' Server counterpart for the Shape Reconstruction Module.
#'
#' @param id Module id
#' @export
shape_reconstruction_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values
    loaded_model <- reactiveVal(NULL)
    reconstructed_shape <- reactiveVal(NULL)
    batch_shapes <- reactiveVal(NULL)
    
    # Package availability
    shinyfiles_ready <- reactiveVal(FALSE)
    momocs_ready <- reactiveVal(FALSE)
    
    # Try to ensure dependencies
    observe({
      ready <- requireNamespace("shinyFiles", quietly = TRUE)
      if (!isTRUE(ready)) {
        try(install.packages("shinyFiles", repos = "https://cran.r-project.org", quiet = TRUE), silent = TRUE)
        ready <- requireNamespace("shinyFiles", quietly = TRUE)
      }
      shinyfiles_ready(isTRUE(ready))
    })
    
    # Model file chooser UI
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
        textInput(ns("model_file_fallback"), "Model file path (.csv)", value = "")
      }
    })
    
    # Model file path reactive
    model_file_path <- reactiveVal("")
    
    # Setup file chooser
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
        id = "model_file_btn", 
        roots = roots, 
        session = session,
        filetypes = c("csv", "CSV")
      )
    })
    
    # Handle file selection
    observeEvent(input$model_file_btn, {
      req(shinyfiles_ready())
      
      roots <- try(shinyFiles::getVolumes()(), silent = TRUE)
      if (inherits(roots, "try-error") || is.null(roots) || length(roots) == 0) {
        roots <- c()
      }
      if (.Platform$OS.type == "windows" && dir.exists("C:/")) {
        roots <- c(`C:` = "C:/", roots)
      }
      roots <- c(roots, Home = normalizePath("~"), `Working Dir` = normalizePath(getwd()))
      
      sel <- try(shinyFiles::parseFilePaths(roots, input$model_file_btn), silent = TRUE)
      if (!inherits(sel, "try-error") && nrow(sel) > 0) {
        model_file_path(as.character(sel$datapath[1]))
      }
    })
    
    # Fallback file path
    observe({
      if (!isTRUE(shinyfiles_ready()) && !is.null(input$model_file_fallback) && nzchar(input$model_file_fallback)) {
        model_file_path(input$model_file_fallback)
      }
    })
    
    # Display selected file
    output$model_file_selected <- renderText({
      path <- model_file_path()
      if (is.null(path) || !nzchar(path)) {
        "No file selected"
      } else {
        basename(path)
      }
    })
    
    # Load model
    observeEvent(input$load_model, {
      path <- model_file_path()
      
      if (is.null(path) || !nzchar(path)) {
        showNotification("Please select a model file first.", type = "warning")
        return()
      }
      
      if (!file.exists(path)) {
        showNotification("Model file does not exist.", type = "error")
        return()
      }
      
      withProgress(message = "Loading reconstruction model...", value = 0.5, {
        model <- tryCatch({
          load_reconstruction_csv(path, validate = TRUE, verbose = FALSE)
        }, error = function(e) {
          showNotification(paste("Failed to load model:", conditionMessage(e)), type = "error", duration = 8)
          NULL
        })
        
        if (!is.null(model)) {
          loaded_model(model)
          showNotification("Model loaded successfully!", type = "message")
        }
      })
    })
    
    # Display model information
    output$model_info_ui <- renderUI({
      model <- loaded_model()
      req(model)
      
      # Build table rows conditionally
      table_rows <- list(
        tags$tr(tags$td(tags$strong("Coefficients:")), tags$td(length(model$center))),
        tags$tr(tags$td(tags$strong("Principal Components:")), tags$td(ncol(model$rotation)))
      )
      
      if (!is.null(model$parameters$n_harmonics)) {
        table_rows <- c(table_rows, list(
          tags$tr(tags$td(tags$strong("Harmonics:")), tags$td(model$parameters$n_harmonics))
        ))
      }
      
      if (!is.null(model$parameters$norm)) {
        table_rows <- c(table_rows, list(
          tags$tr(tags$td(tags$strong("Normalization:")), tags$td(as.character(model$parameters$norm)))
        ))
      }
      
      if (!is.null(model$parameters$start_point)) {
        table_rows <- c(table_rows, list(
          tags$tr(tags$td(tags$strong("Start Point:")), tags$td(model$parameters$start_point))
        ))
      }
      
      tagList(
        tags$h4("Model Information", style = "color: #3c8dbc;"),
        do.call(tags$table, c(list(class = "table table-condensed"), table_rows)),
        tags$hr(),
        tags$h5("Variance Explained:"),
        tags$pre(style = "max-height: 150px; overflow-y: auto; font-size: 11px;", {
          if (!is.null(model$variance_explained)) {
            n_show <- min(10, length(model$variance_explained))
            lines <- sapply(1:n_show, function(i) {
              sprintf("PC%d: %.2f%%", i, model$variance_explained[i])
            })
            paste(lines, collapse = "\n")
          } else {
            "Variance information not available"
          }
        })
      )
    })
    
    # Generate PC score inputs dynamically
    output$pc_score_inputs_ui <- renderUI({
      model <- loaded_model()
      req(model)
      
      score_type <- input$score_type
      if (is.null(score_type)) score_type <- "sd"
      
      n_pcs <- min(10, model$parameters$n_components)  # Limit to first 10 PCs for UI
      
      inputs <- lapply(1:n_pcs, function(i) {
        var_pct <- if (!is.null(model$variance_explained) && i <= length(model$variance_explained)) {
          sprintf(" (%.1f%%)", model$variance_explained[i])
        } else {
          ""
        }
        
        numericInput(
          ns(paste0("pc", i)),
          paste0("PC", i, var_pct),
          value = 0,
          step = 0.1
        )
      })
      
      title_text <- if (score_type == "sd") "PC Scores (in SD units)" else "PC Scores (absolute values)"
      help_text <- if (score_type == "sd") {
        "Enter desired PC scores. 0 = mean shape, +/-1 = one standard deviation from mean."
      } else {
        "Enter absolute PC score values (e.g., from your morphospace plot)."
      }
      
      tagList(
        tags$h4(title_text),
        helpText(help_text),
        do.call(fluidRow, lapply(inputs, function(inp) column(width = 3, inp)))
      )
    })
    
    # Update batch PC choices when model is loaded
    observe({
      model <- loaded_model()
      req(model)
      
      n_pcs <- min(10, model$parameters$n_components)
      choices <- setNames(1:n_pcs, paste0("PC", 1:n_pcs))
      updateSelectInput(session, "batch_pc", choices = choices, selected = 1)
    })
    
    # Reconstruct shape
    observeEvent(input$reconstruct, {
      model <- loaded_model()
      
      if (is.null(model)) {
        showNotification("Please load a reconstruction model first.", type = "warning")
        return()
      }
      
      score_type <- input$score_type
      if (is.null(score_type)) score_type <- "sd"
      
      # Collect PC scores from inputs
      n_pcs <- min(10, model$parameters$n_components)
      pc_scores <- sapply(1:n_pcs, function(i) {
        val <- input[[paste0("pc", i)]]
        if (is.null(val)) return(0)
        as.numeric(val)
      })
      names(pc_scores) <- paste0("PC", 1:n_pcs)
      
      # Debug: check collected scores
      if (any(is.na(pc_scores))) {
        showNotification("Invalid PC scores detected (NA values). Using 0 for missing values.", 
                        type = "warning", duration = 5)
        pc_scores[is.na(pc_scores)] <- 0
      }
      
      withProgress(message = "Reconstructing shape...", value = 0.5, {
        shape <- tryCatch({
          .reconstruct_single_shape(model, pc_scores, score_type)
        }, error = function(e) {
          # More detailed error message
          err_msg <- conditionMessage(e)
          showNotification(
            paste0("Reconstruction failed: ", err_msg, 
                   "\nModel dimensions: center=", length(model$center), 
                   ", rotation=", paste(dim(model$rotation), collapse="x")),
            type = "error", 
            duration = 10
          )
          NULL
        })
        
        if (!is.null(shape)) {
          reconstructed_shape(list(coords = shape, pc_scores = pc_scores, score_type = score_type))
          showNotification("Shape reconstructed!", type = "message")
        }
      })
    })
    
    # Helper function to denormalize efourier coefficients
    # When norm=TRUE, efourier produces A,B,C,D normalized coefficients
    # efourier_i expects an,bn,cn,dn raw coefficients
    # This function reverses the normalization
    .efourier_denorm <- function(A, B, C, D, size, theta, psi) {
      nb.h <- length(A)
      
      # Inverse of the normalization transformations
      scale <- 1/size  # size in efourier_norm is 1/scale
      
      # Inverse rotation matrix
      inv_rotation <- matrix(c(cos(psi), sin(psi), -sin(psi), cos(psi)), 2, 2)
      
      an <- bn <- cn <- dn <- numeric(nb.h)
      
      for (i in 1:nb.h) {
        # Inverse phase shift for this harmonic
        inv_phase <- matrix(c(cos(i * theta), -sin(i * theta),
                             sin(i * theta), cos(i * theta)), 2, 2)
        
        # Apply inverse transformations: raw = inv_rotation * normalized * inv_phase / size
        mat <- inv_rotation %*%
               matrix(c(A[i], C[i], B[i], D[i]), 2, 2) %*%
               inv_phase / scale
        
        an[i] <- mat[1, 1]
        cn[i] <- mat[2, 1]
        bn[i] <- mat[1, 2]
        dn[i] <- mat[2, 2]
      }
      
      return(list(an = an, bn = bn, cn = cn, dn = dn))
    }
    
    # Helper function to reconstruct a single shape
    .reconstruct_single_shape <- function(model, pc_scores, score_type = "sd") {
      # Ensure pc_scores is numeric vector
      pc_scores <- as.numeric(pc_scores)
      
      # Get dimensions
      n_coefs <- length(model$center)
      n_pcs <- ncol(model$rotation)
      
      # Ensure pc_scores has correct length and pad with zeros if needed
      if (length(pc_scores) > n_pcs) {
        pc_scores <- pc_scores[1:n_pcs]
      } else if (length(pc_scores) < n_pcs) {
        full_scores <- rep(0, n_pcs)
        full_scores[1:length(pc_scores)] <- pc_scores
        pc_scores <- full_scores
      }
      
      # Reconstruct Fourier coefficients using PCA
      # If score_type is "sd", scale by standard deviations
      # If score_type is "absolute", use scores directly
      if (score_type == "sd") {
        # Formula: reconstructed_coefs = center + (pc_scores * sdev) %*% t(rotation)
        scaled_scores <- pc_scores * model$sdev[1:length(pc_scores)]
      } else {
        # Use absolute PC values directly
        scaled_scores <- pc_scores
      }
      
      contribution <- as.vector(scaled_scores %*% t(model$rotation))
      reconstructed_coefs <- model$center + contribution
      
      # Get EFA parameters from model
      n_harmonics <- model$parameters$n_harmonics
      is_normalized <- isTRUE(model$parameters$norm)
      
      # Split coefficients into A, B, C, D components
      # coeff_split calculates nb.h automatically from length
      coef_list <- coeff_split(reconstructed_coefs)
      
      # IMPORTANT: If normalization was used during analysis, the reconstructed 
      # coefficients are in the normalized space. We need to work with them directly
      # without attempting denormalization, as denormalization can cause flips.
      # The coefficients from PCA reconstruction represent valid shape variations.
      
      # Add DC offset components (typically 0 after centering)
      if (is.null(coef_list$ao)) coef_list$ao <- 0
      if (is.null(coef_list$co)) coef_list$co <- 0
      
      # Reconstruct shape outline using inverse Fourier transform
      coords <- tryCatch({
        efourier_i(coef_list, nb.h = n_harmonics, nb.pts = 120)
      }, error = function(e) {
        stop("Shape reconstruction failed: ", conditionMessage(e), call. = FALSE)
      })
      
      return(coords)
    }
    
    # Plot reconstructed shape
    output$shape_plot <- renderPlot({
      shape_data <- reconstructed_shape()
      req(shape_data)
      
      coords <- shape_data$coords
      
      # Create plot with white background and black shape
      par(bg = "white")
      plot(coords, type = "l", lwd = 2, col = "black", 
           asp = 1, xlab = "", ylab = "", main = "Reconstructed Shape",
           axes = FALSE, frame.plot = TRUE)
      polygon(coords, col = "black", border = "black", lwd = 2)
      
      # Add PC scores as subtitle
      score_type_label <- if (!is.null(shape_data$score_type) && shape_data$score_type == "absolute") "(absolute)" else "(SD)"
      pc_text <- paste(names(shape_data$pc_scores), "=", round(shape_data$pc_scores, 2), collapse = ", ")
      mtext(paste(pc_text, score_type_label), side = 3, line = 0.5, cex = 0.8, col = "gray30")
      
    }, height = function() {
      size <- input$plot_size
      if (is.null(size) || !is.numeric(size)) return(600)
      return(as.integer(size))
    })
    
    # Display reconstruction info
    output$reconstruction_info <- renderText({
      shape_data <- reconstructed_shape()
      req(shape_data)
      
      coords <- shape_data$coords
      score_type_label <- if (!is.null(shape_data$score_type) && shape_data$score_type == "absolute") "Absolute PC values" else "SD units"
      paste0(
        "Reconstruction successful\n",
        "Number of outline points: ", nrow(coords), "\n",
        "X range: [", round(min(coords[,1]), 2), ", ", round(max(coords[,1]), 2), "]\n",
        "Y range: [", round(min(coords[,2]), 2), ", ", round(max(coords[,2]), 2), "]\n",
        "\nInput type: ", score_type_label, "\n",
        "\nPC Scores used:\n",
        paste(names(shape_data$pc_scores), "=", round(shape_data$pc_scores, 3), collapse = "\n")
      )
    })
    
    # Download coordinates
    output$download_coords <- downloadHandler(
      filename = function() {
        paste0("reconstructed_shape_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        shape_data <- reconstructed_shape()
        req(shape_data)
        
        coords <- as.data.frame(shape_data$coords)
        colnames(coords) <- c("x", "y")
        coords$point <- 1:nrow(coords)
        
        # Add PC scores as header comments
        header_lines <- paste0("# ", names(shape_data$pc_scores), " = ", shape_data$pc_scores)
        
        writeLines(c(header_lines, ""), file)
        write.csv(coords[, c("point", "x", "y")], file, row.names = FALSE, append = TRUE)
      }
    )
    
    # Download plot
    output$download_plot <- downloadHandler(
      filename = function() {
        paste0("reconstructed_shape_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
      },
      content = function(file) {
        shape_data <- reconstructed_shape()
        req(shape_data)
        
        coords <- shape_data$coords
        coords_df <- as.data.frame(coords)
        colnames(coords_df) <- c("x", "y")
        
        # Create ggplot2 object
        score_type_label <- if (!is.null(shape_data$score_type) && shape_data$score_type == "absolute") "(absolute)" else "(SD)"
        pc_text <- paste(names(shape_data$pc_scores), "=", round(shape_data$pc_scores, 2), collapse = ", ")
        subtitle_text <- paste(pc_text, score_type_label)
        
        p <- ggplot2::ggplot(coords_df, ggplot2::aes(x = x, y = y)) +
          ggplot2::geom_polygon(fill = "black", color = "black", linewidth = 0.5) +
          ggplot2::coord_fixed() +
          ggplot2::labs(title = "Reconstructed Shape", subtitle = subtitle_text) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            axis.title = ggplot2::element_blank(),
            axis.text = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_blank(),
            panel.grid = ggplot2::element_blank(),
            panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 1),
            plot.subtitle = ggplot2::element_text(color = "gray30", size = 10)
          )
        
        saveRDS(p, file)
      }
    )
    
    # Download shape as JPG (clean, no labels)
    output$download_jpg <- downloadHandler(
      filename = function() {
        paste0("reconstructed_shape_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".jpg")
      },
      content = function(file) {
        shape_data <- reconstructed_shape()
        req(shape_data)
        
        coords <- shape_data$coords
        
        # Create clean JPG with just the black shape on white background
        jpeg(file, width = 800, height = 800, quality = 100, bg = "white")
        par(mar = c(0, 0, 0, 0), bg = "white")
        plot(coords, type = "n", asp = 1, xlab = "", ylab = "",
             axes = FALSE, frame.plot = FALSE)
        polygon(coords, col = "black", border = "black", lwd = 1)
        dev.off()
      }
    )
    
    # Batch reconstruction
    observeEvent(input$batch_reconstruct, {
      model <- loaded_model()
      
      if (is.null(model)) {
        showNotification("Please load a reconstruction model first.", type = "warning")
        return()
      }
      
      pc_axis <- as.integer(input$batch_pc)
      min_val <- input$batch_min
      max_val <- input$batch_max
      n_steps <- input$batch_steps
      
      withProgress(message = "Generating batch reconstruction...", value = 0, {
        # Generate PC score combinations
        pc_values <- seq(min_val, max_val, length.out = n_steps)
        
        batch_results <- list()
        
        for (i in seq_along(pc_values)) {
          incProgress(1 / n_steps, detail = sprintf("Shape %d/%d", i, n_steps))
          
          # Create PC score vector
          n_pcs <- min(10, model$parameters$n_components)
          pc_scores <- rep(0, n_pcs)
          
          if (!input$batch_hold_others) {
            # Use current UI values for other PCs
            pc_scores <- sapply(1:n_pcs, function(j) {
              input[[paste0("pc", j)]] %||% 0
            })
          }
          
          # Set the varying PC
          pc_scores[pc_axis] <- pc_values[i]
          names(pc_scores) <- paste0("PC", 1:n_pcs)
          
          # Reconstruct
          coords <- tryCatch({
            .reconstruct_single_shape(model, pc_scores)
          }, error = function(e) NULL)
          
          if (!is.null(coords)) {
            batch_results[[i]] <- list(coords = coords, pc_score = pc_values[i])
          }
        }
        
        batch_shapes(list(results = batch_results, pc_axis = pc_axis))
        showNotification(paste("Generated", length(batch_results), "shapes"), type = "message")
      })
    })
    
    # Plot batch results
    output$batch_plot <- renderPlot({
      batch_data <- batch_shapes()
      req(batch_data)
      
      results <- batch_data$results
      pc_axis <- batch_data$pc_axis
      n_shapes <- length(results)
      
      # Calculate grid dimensions
      ncols <- ceiling(sqrt(n_shapes))
      nrows <- ceiling(n_shapes / ncols)
      
      par(mfrow = c(nrows, ncols), mar = c(2, 2, 2, 1))
      
      for (i in seq_along(results)) {
        coords <- results[[i]]$coords
        pc_val <- results[[i]]$pc_score
        
        plot(coords, type = "l", lwd = 1.5, col = "steelblue", 
             asp = 1, xlab = "", ylab = "", 
             main = sprintf("PC%d = %.2f", pc_axis, pc_val),
             axes = FALSE, frame.plot = TRUE, cex.main = 0.9)
        polygon(coords, col = rgb(0.25, 0.55, 0.75, 0.2), border = "steelblue", lwd = 1.5)
      }
    })
    
    # Download batch grid
    output$download_batch <- downloadHandler(
      filename = function() {
        paste0("batch_reconstruction_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
      },
      content = function(file) {
        batch_data <- batch_shapes()
        req(batch_data)
        
        results <- batch_data$results
        pc_axis <- batch_data$pc_axis
        
        # Create list of ggplot objects
        plot_list <- lapply(seq_along(results), function(i) {
          coords <- results[[i]]$coords
          pc_val <- results[[i]]$pc_score
          coords_df <- as.data.frame(coords)
          colnames(coords_df) <- c("x", "y")
          
          ggplot2::ggplot(coords_df, ggplot2::aes(x = x, y = y)) +
            ggplot2::geom_polygon(fill = ggplot2::alpha("steelblue", 0.2), 
                                 color = "steelblue", linewidth = 0.5) +
            ggplot2::coord_fixed() +
            ggplot2::labs(title = sprintf("PC%d = %.2f", pc_axis, pc_val)) +
            ggplot2::theme_minimal() +
            ggplot2::theme(
              axis.title = ggplot2::element_blank(),
              axis.text = ggplot2::element_blank(),
              axis.ticks = ggplot2::element_blank(),
              panel.grid = ggplot2::element_blank(),
              panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 1),
              plot.title = ggplot2::element_text(size = 9)
            )
        })
        
        # Combine plots using patchwork if available, otherwise save as list
        if (requireNamespace("patchwork", quietly = TRUE)) {
          combined_plot <- patchwork::wrap_plots(plot_list, ncol = ceiling(sqrt(length(plot_list))))
          saveRDS(combined_plot, file)
        } else {
          # Save as list of plots if patchwork not available
          saveRDS(plot_list, file)
        }
      }
    )
    
    # ============================================================
    # BATCH RECONSTRUCTION FROM PC SCORE FILE
    # ============================================================
    
    pc_file_data       <- reactiveVal(NULL)
    pc_scores_file_path <- reactiveVal("")
    output_dir_path    <- reactiveVal("")
    file_batch_log_text <- reactiveVal("No reconstruction run yet.")
    
    # Helper: build shinyFiles roots
    get_file_roots <- function() {
      roots <- try(shinyFiles::getVolumes()(), silent = TRUE)
      if (inherits(roots, "try-error") || is.null(roots) || length(roots) == 0) roots <- c()
      if (.Platform$OS.type == "windows" && dir.exists("C:/")) {
        roots <- c(`C:` = "C:/", roots)
      }
      c(roots, Home = normalizePath("~"), `Working Dir` = normalizePath(getwd()))
    }
    
    # ---- PC scores file chooser ----
    output$pc_scores_file_ui <- renderUI({
      if (isTRUE(shinyfiles_ready())) {
        tagList(
          shinyFiles::shinyFilesButton(
            ns("pc_scores_file_btn"),
            label = "Choose PC scores file",
            title = "Select PC scores file (.xlsx, .ods, .csv)",
            multiple = FALSE
          ),
          br(), br(),
          strong("Selected: "),
          textOutput(ns("pc_scores_file_selected"), inline = TRUE)
        )
      } else {
        tagList(
          textInput(ns("pc_scores_file_fallback"), "PC scores file path (.xlsx / .ods / .csv)", value = ""),
          strong("Selected: "),
          textOutput(ns("pc_scores_file_selected"), inline = TRUE)
        )
      }
    })
    
    observeEvent(shinyfiles_ready(), {
      if (!isTRUE(shinyfiles_ready())) return()
      shinyFiles::shinyFileChoose(
        input,
        id = "pc_scores_file_btn",
        roots = get_file_roots(),
        session = session,
        filetypes = c("xlsx", "ods", "csv", "XLSX", "ODS", "CSV")
      )
    })
    
    observeEvent(input$pc_scores_file_btn, {
      req(shinyfiles_ready())
      sel <- try(shinyFiles::parseFilePaths(get_file_roots(), input$pc_scores_file_btn), silent = TRUE)
      if (!inherits(sel, "try-error") && nrow(sel) > 0) {
        pc_scores_file_path(as.character(sel$datapath[1]))
      }
    })
    
    observe({
      if (!isTRUE(shinyfiles_ready()) && !is.null(input$pc_scores_file_fallback) && nzchar(input$pc_scores_file_fallback)) {
        pc_scores_file_path(input$pc_scores_file_fallback)
      }
    })
    
    output$pc_scores_file_selected <- renderText({
      path <- pc_scores_file_path()
      if (is.null(path) || !nzchar(path)) "No file selected" else basename(path)
    })
    
    # ---- Load PC scores file ----
    observeEvent(input$load_pc_scores_file, {
      path <- pc_scores_file_path()
      if (is.null(path) || !nzchar(path)) {
        showNotification("Please select a PC scores file first.", type = "warning")
        return()
      }
      if (!file.exists(path)) {
        showNotification("File does not exist.", type = "error")
        return()
      }
      
      ext <- tolower(tools::file_ext(path))
      
      df <- tryCatch({
        if (ext == "xlsx") {
          if (!requireNamespace("readxl", quietly = TRUE))
            stop("Package 'readxl' is required for .xlsx files. Install with: install.packages('readxl')")
          as.data.frame(readxl::read_xlsx(path))
        } else if (ext == "ods") {
          if (!requireNamespace("readODS", quietly = TRUE))
            stop("Package 'readODS' is required for .ods files. Install with: install.packages('readODS')")
          as.data.frame(readODS::read_ods(path))
        } else if (ext == "csv") {
          read.csv(path, stringsAsFactors = FALSE)
        } else {
          stop("Unsupported format: ", ext, ". Use .xlsx, .ods, or .csv.")
        }
      }, error = function(e) {
        showNotification(paste("Failed to load file:", conditionMessage(e)), type = "error", duration = 10)
        NULL
      })
      
      if (!is.null(df)) {
        pc_file_data(df)
        showNotification(paste("Loaded", nrow(df), "rows and", ncol(df), "columns."), type = "message")
        file_batch_log_text("File loaded. Configure columns, choose output folder, then click Reconstruct All & Save.")
      }
    })
    
    # ---- Data preview ----
    output$pc_scores_preview_ui <- renderUI({
      df <- pc_file_data()
      if (is.null(df)) return(NULL)
      
      preview <- head(df, 5)
      col_headers <- lapply(colnames(preview), function(col) tags$th(col))
      body_rows <- lapply(seq_len(nrow(preview)), function(r) {
        tags$tr(lapply(seq_len(ncol(preview)), function(c) {
          tags$td(as.character(preview[r, c]))
        }))
      })
      
      tagList(
        tags$h5(paste0("File preview (first ", min(5, nrow(df)), " of ", nrow(df), " rows):")),
        tags$div(
          style = "overflow-x:auto; max-height:200px;",
          tags$table(
            class = "table table-striped table-bordered table-condensed",
            style = "font-size:12px;",
            tags$thead(tags$tr(col_headers)),
            tags$tbody(body_rows)
          )
        ),
        tags$hr()
      )
    })
    
    # ---- Column mapping UI ----
    output$pc_column_mapping_ui <- renderUI({
      df    <- pc_file_data()
      model <- loaded_model()
      if (is.null(df) || is.null(model)) return(NULL)
      
      cols        <- colnames(df)
      col_choices <- c("(not used / zero)" = "", setNames(cols, cols))
      n_pcs       <- min(10, model$parameters$n_components)
      
      # Auto-detect obvious PC columns
      auto_match <- function(i) {
        candidates <- c(paste0("PC", i), paste0("pc", i),
                        paste0("Dim.", i), paste0("Dim", i),
                        paste0("Axis", i), paste0("axis", i))
        matched <- intersect(candidates, cols)
        if (length(matched) > 0) matched[1] else ""
      }
      
      inputs <- lapply(1:n_pcs, function(i) {
        var_pct <- if (!is.null(model$variance_explained) && i <= length(model$variance_explained)) {
          sprintf(" (%.1f%%)", model$variance_explained[i])
        } else ""
        column(
          width = 3,
          selectInput(
            ns(paste0("pc_col_", i)),
            paste0("PC", i, var_pct, ":"),
            choices  = col_choices,
            selected = auto_match(i)
          )
        )
      })
      
      tagList(
        tags$h5("Map file columns to PC axes:"),
        helpText("Select which column in your file corresponds to each PC axis. Unmapped PCs are treated as 0."),
        do.call(fluidRow, inputs),
        tags$hr()
      )
    })
    
    # ---- Name / ID column ----
    output$pc_name_column_ui <- renderUI({
      df <- pc_file_data()
      if (is.null(df)) return(NULL)
      
      cols        <- colnames(df)
      col_choices <- c("(use row number)" = "", setNames(cols, cols))
      
      name_candidates <- c("name", "Name", "ID", "id", "label", "Label",
                           "specimen", "Specimen", "taxon", "Taxon")
      auto_name <- intersect(name_candidates, cols)
      auto_val  <- if (length(auto_name) > 0) auto_name[1] else ""
      
      selectInput(
        ns("pc_name_col"),
        "Shape name / ID column (used for output file names):",
        choices  = col_choices,
        selected = auto_val
      )
    })
    
    # ---- Output folder chooser ----
    output$output_dir_ui <- renderUI({
      if (isTRUE(shinyfiles_ready())) {
        tagList(
          shinyFiles::shinyDirButton(
            ns("output_dir_btn"),
            label = "Choose output folder",
            title = "Select folder to save reconstructed shapes"
          ),
          br(), br(),
          strong("Output folder: "),
          textOutput(ns("output_dir_selected"), inline = TRUE)
        )
      } else {
        tagList(
          textInput(ns("output_dir_fallback"), "Output folder path", value = getwd()),
          strong("Output folder: "),
          textOutput(ns("output_dir_selected"), inline = TRUE)
        )
      }
    })
    
    observeEvent(shinyfiles_ready(), {
      if (!isTRUE(shinyfiles_ready())) return()
      shinyFiles::shinyDirChoose(
        input,
        id = "output_dir_btn",
        roots = get_file_roots(),
        session = session
      )
    })
    
    observeEvent(input$output_dir_btn, {
      req(shinyfiles_ready())
      sel <- try(shinyFiles::parseDirPath(get_file_roots(), input$output_dir_btn), silent = TRUE)
      if (!inherits(sel, "try-error") && length(sel) > 0 && nzchar(sel)) {
        output_dir_path(as.character(sel))
      }
    })
    
    observe({
      if (!isTRUE(shinyfiles_ready()) && !is.null(input$output_dir_fallback) && nzchar(input$output_dir_fallback)) {
        output_dir_path(input$output_dir_fallback)
      }
    })
    
    output$output_dir_selected <- renderText({
      path <- output_dir_path()
      if (is.null(path) || !nzchar(path)) "No folder selected" else path
    })
    
    # ---- Reconstruct all shapes from file ----
    observeEvent(input$reconstruct_from_file, {
      model   <- loaded_model()
      df      <- pc_file_data()
      out_dir <- output_dir_path()
      
      if (is.null(model)) {
        showNotification("Please load a reconstruction model first.", type = "warning")
        return()
      }
      if (is.null(df)) {
        showNotification("Please load a PC scores file first.", type = "warning")
        return()
      }
      if (is.null(out_dir) || !nzchar(out_dir)) {
        showNotification("Please select an output folder.", type = "warning")
        return()
      }
      
      formats    <- input$file_batch_formats
      if (is.null(formats) || length(formats) == 0) formats <- "jpg"
      img_size   <- input$file_batch_img_size
      if (is.null(img_size) || !is.numeric(img_size)) img_size <- 800
      score_type <- input$file_batch_score_type
      if (is.null(score_type)) score_type <- "absolute"
      name_col   <- input$pc_name_col
      n_pcs      <- min(10, model$parameters$n_components)
      
      # Get column mappings
      col_map <- sapply(1:n_pcs, function(i) {
        val <- input[[paste0("pc_col_", i)]]
        if (is.null(val)) "" else val
      })
      
      # Create output directory if needed
      if (!dir.exists(out_dir)) {
        tryCatch(dir.create(out_dir, recursive = TRUE), error = function(e) {
          showNotification(paste("Cannot create output folder:", conditionMessage(e)), type = "error")
        })
      }
      if (!dir.exists(out_dir)) return()
      
      n_rows       <- nrow(df)
      log_lines    <- character(0)
      success_count <- 0L
      fail_count    <- 0L
      
      withProgress(message = "Reconstructing shapes from file...", value = 0, {
        for (row_i in seq_len(n_rows)) {
          incProgress(1 / n_rows, detail = sprintf("Row %d / %d", row_i, n_rows))
          
          # Determine output file base name
          shape_name <- if (!is.null(name_col) && nzchar(name_col) && name_col %in% colnames(df)) {
            as.character(df[row_i, name_col])
          } else {
            sprintf("shape_%04d", row_i)
          }
          # Sanitize for use as file name
          shape_name <- gsub("[^A-Za-z0-9._-]", "_", shape_name)
          if (!nzchar(shape_name)) shape_name <- sprintf("shape_%04d", row_i)
          
          # Build PC score vector
          pc_scores <- numeric(n_pcs)
          for (i in seq_len(n_pcs)) {
            if (nzchar(col_map[i]) && col_map[i] %in% colnames(df)) {
              val <- suppressWarnings(as.numeric(df[row_i, col_map[i]]))
              pc_scores[i] <- if (is.na(val)) 0 else val
            }
          }
          names(pc_scores) <- paste0("PC", seq_len(n_pcs))
          
          # Reconstruct shape
          coords <- tryCatch(
            .reconstruct_single_shape(model, pc_scores, score_type),
            error = function(e) {
              log_lines   <<- c(log_lines, sprintf("FAIL [%s]: %s", shape_name, conditionMessage(e)))
              fail_count  <<- fail_count + 1L
              NULL
            }
          )
          
          if (is.null(coords)) next
          
          # Save requested formats
          save_ok <- tryCatch({
            if ("jpg" %in% formats) {
              jpeg(file.path(out_dir, paste0(shape_name, ".jpg")),
                   width = img_size, height = img_size, quality = 95, bg = "white")
              par(mar = c(0, 0, 0, 0), bg = "white")
              plot(coords, type = "n", asp = 1, xlab = "", ylab = "", axes = FALSE, frame.plot = FALSE)
              polygon(coords, col = "black", border = "black", lwd = 1)
              dev.off()
            }
            if ("png" %in% formats) {
              png(file.path(out_dir, paste0(shape_name, ".png")),
                  width = img_size, height = img_size, bg = "white")
              par(mar = c(0, 0, 0, 0), bg = "white")
              plot(coords, type = "n", asp = 1, xlab = "", ylab = "", axes = FALSE, frame.plot = FALSE)
              polygon(coords, col = "black", border = "black", lwd = 1)
              dev.off()
            }
            if ("csv" %in% formats) {
              coords_df           <- as.data.frame(coords)
              colnames(coords_df) <- c("x", "y")
              write.csv(coords_df, file.path(out_dir, paste0(shape_name, ".csv")), row.names = FALSE)
            }
            TRUE
          }, error = function(e) {
            log_lines  <<- c(log_lines, sprintf("FAIL [%s] (save error): %s", shape_name, conditionMessage(e)))
            fail_count <<- fail_count + 1L
            FALSE
          })
          
          if (isTRUE(save_ok)) {
            log_lines     <- c(log_lines, sprintf("OK   [%s]", shape_name))
            success_count <- success_count + 1L
          }
        }
      })
      
      summary_line <- sprintf(
        "Done: %d saved, %d failed. Output: %s",
        success_count, fail_count, out_dir
      )
      file_batch_log_text(paste(c(summary_line, "", log_lines), collapse = "\n"))
      
      if (success_count > 0)
        showNotification(sprintf("Saved %d shapes to %s", success_count, out_dir), type = "message", duration = 8)
      if (fail_count > 0)
        showNotification(sprintf("%d shapes failed. See log below.", fail_count), type = "warning", duration = 8)
    })
    
    output$file_batch_log <- renderText({
      file_batch_log_text()
    })
    
  })
}
