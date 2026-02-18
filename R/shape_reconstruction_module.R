#' Shape Reconstruction Module
#'
#' UI and server for interactive shape reconstruction from PCA models.
#' Allows loading reconstruction models and generating shapes from PC scores.
#'
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
          helpText("SD units: 0 = mean, ±1 = one SD from mean. Absolute: use actual PC score values from your data."),
          
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
        )
      )
    )
  )
}

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
        "Enter desired PC scores. 0 = mean shape, ±1 = one standard deviation from mean."
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
  })
}
