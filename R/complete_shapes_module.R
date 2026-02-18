#' Complete Halved Shapes Module
#'
#' UI and server logic to complete halved shapes into symmetrical images
#' using the `complete_halved_shape()` function with batch processing support.
#'
#' @param id Module id
#' @return Invisibly returns a reactive containing the last results
#' @name complete_shapes_module
NULL

#' @rdname complete_shapes_module
#' @export
complete_shapes_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        box(
          title = "Complete Halved Shapes",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          
          p("This tool takes halved images (left or right half of symmetrical shapes) and completes them into full symmetrical images by mirroring."),
          
          uiOutput(ns("folder_chooser_ui")),
          
          uiOutput(ns("output_dir_ui")),
          
          h4("Completion Options"),
          
          selectInput(
            ns("method"),
            "Completion Method",
            choices = c(
              "Mirror left half to right" = "mirror_right",
              "Mirror right half to left" = "mirror_left",
              "Both sides" = "both_sides"
            ),
            selected = "mirror_right"
          ),
          
          selectInput(
            ns("half_side"),
            "Input represents which half?",
            choices = c(
              "Auto-detect" = "auto",
              "Left half" = "left",
              "Right half" = "right"
            ),
            selected = "auto"
          ),
          
          checkboxInput(ns("blend_seam"), "Blend seam between halves", value = FALSE),
          
          conditionalPanel(
            condition = sprintf("input['%s']", ns("blend_seam")),
            sliderInput(
              ns("seam_width"),
              "Seam width (pixels)",
              min = 1,
              max = 10,
              value = 2,
              step = 1
            )
          ),
          
          h4("Alignment Options"),
          
          checkboxInput(ns("auto_align"), "Auto-align halves", value = TRUE),
          
          conditionalPanel(
            condition = sprintf("input['%s']", ns("auto_align")),
            selectInput(
              ns("alignment_method"),
              "Alignment Method",
              choices = c(
                "Edge-based" = "edge",
                "Centroid-based" = "centroid",
                "None" = "none"
              ),
              selected = "edge"
            )
          ),
          
          checkboxInput(ns("crop_to_content"), "Crop to content", value = TRUE),
          
          h4("Output Options"),
          
          selectInput(
            ns("format"),
            "Output format",
            choices = c("Auto (same as input)" = "auto", "PNG" = "png", "JPG" = "jpg", "TIFF" = "tiff"),
            selected = "auto"
          ),
          
          conditionalPanel(
            condition = sprintf("input['%s'] == 'jpg'", ns("format")),
            sliderInput(
              ns("quality"),
              "JPEG quality",
              min = 10,
              max = 100,
              value = 90,
              step = 1
            )
          ),
          
          checkboxInput(ns("preserve_transparency"), "Preserve transparency (PNG/TIFF)", value = TRUE),
          
          textInput(
            ns("suffix"),
            "Output filename suffix",
            value = "_completed"
          ),
          
          checkboxInput(ns("overwrite"), "Overwrite existing outputs", value = FALSE),
          
          hr(),
          
          actionButton(ns("process"), "Complete Shapes", class = "btn-success", icon = icon("magic"))
        )
      )
    ),
    
    fluidRow(
      column(
        width = 12,
        box(
          title = "Processing Results",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          
          verbatimTextOutput(ns("results_summary")),
          
          hr(),
          
          DT::dataTableOutput(ns("results_table"))
        )
      )
    )
  )
}

#' @rdname complete_shapes_module
#' @export
complete_shapes_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values
    rv <- reactiveValues(
      selected_folder = "",
      selected_output_dir = file.path(getwd(), "completed_shapes"),
      shinyfiles_ready = FALSE,
      magick_ready = FALSE,
      last_result = NULL
    )
    
    # Check for shinyFiles availability
    observe({
      ready <- requireNamespace("shinyFiles", quietly = TRUE)
      if (!ready) {
        try({
          install.packages("shinyFiles", repos = "https://cran.r-project.org", quiet = TRUE)
        }, silent = TRUE)
        ready <- requireNamespace("shinyFiles", quietly = TRUE)
        if (ready) {
          showNotification("Installed 'shinyFiles'. Folder chooser enabled.", type = "message")
        }
      }
      rv$shinyfiles_ready <- ready
    })
    
    # Check for magick availability
    observe({
      ready <- requireNamespace("magick", quietly = TRUE)
      if (!ready) {
        try({
          install.packages("magick", repos = "https://cran.r-project.org", quiet = TRUE)
        }, silent = TRUE)
        ready <- requireNamespace("magick", quietly = TRUE)
        if (ready) {
          showNotification("Installed 'magick'. Image processing enabled.", type = "message")
        }
      }
      rv$magick_ready <- ready
    })
    
    # Render folder chooser UI
    output$folder_chooser_ui <- renderUI({
      if (rv$shinyfiles_ready) {
        tagList(
          shinyFiles::shinyDirButton(
            ns("folder_btn"),
            label = "Choose input folder",
            title = "Select a folder with halved shape images"
          ),
          br(),
          strong("Selected folder: "),
          textOutput(ns("folder_selected"), inline = TRUE)
        )
      } else {
        tagList(
          div(class = "text-warning", "Folder chooser not available. Enter a folder path manually."),
          br(),
          textInput(
            ns("folder_path_fallback"),
            "Folder path with halved shape images",
            value = ""
          )
        )
      }
    })
    
    # Render output directory chooser UI
    output$output_dir_ui <- renderUI({
      if (rv$shinyfiles_ready) {
        tagList(
          shinyFiles::shinyDirButton(
            ns("outdir_btn"),
            label = "Choose output directory",
            title = "Select output directory for completed shapes"
          ),
          br(),
          strong("Output directory: "),
          textOutput(ns("outdir_selected"), inline = TRUE)
        )
      } else {
        textInput(
          ns("output_dir_fallback"),
          "Output directory",
          value = rv$selected_output_dir
        )
      }
    })
    
    # Setup folder choosers
    observeEvent(rv$shinyfiles_ready, ignoreInit = FALSE, {
      if (!rv$shinyfiles_ready) return()
      
      vols <- try(shinyFiles::getVolumes()(), silent = TRUE)
      if (inherits(vols, "try-error") || is.null(vols) || length(vols) == 0) {
        vols <- c(Home = normalizePath("~"))
      }
      
      shinyFiles::shinyDirChoose(input, id = "folder_btn", roots = vols, session = session)
      shinyFiles::shinyDirChoose(input, id = "outdir_btn", roots = vols, session = session)
    })
    
    # Handle folder selection
    observeEvent(input$folder_btn, {
      if (!rv$shinyfiles_ready) return()
      
      vols <- try(shinyFiles::getVolumes()(), silent = TRUE)
      if (inherits(vols, "try-error") || is.null(vols) || length(vols) == 0) {
        vols <- c(Home = normalizePath("~"))
      }
      
      sel <- try(shinyFiles::parseDirPath(vols, input$folder_btn), silent = TRUE)
      if (!inherits(sel, "try-error") && length(sel) == 1) {
        rv$selected_folder <- as.character(sel)
      }
    })
    
    # Handle output directory selection
    observeEvent(input$outdir_btn, {
      if (!rv$shinyfiles_ready) return()
      
      vols <- try(shinyFiles::getVolumes()(), silent = TRUE)
      if (inherits(vols, "try-error") || is.null(vols) || length(vols) == 0) {
        vols <- c(Home = normalizePath("~"))
      }
      
      sel <- try(shinyFiles::parseDirPath(vols, input$outdir_btn), silent = TRUE)
      if (!inherits(sel, "try-error") && length(sel) == 1) {
        rv$selected_output_dir <- as.character(sel)
      }
    })
    
    # Display selected paths
    output$folder_selected <- renderText({
      if (!is.null(rv$selected_folder) && nzchar(rv$selected_folder)) {
        rv$selected_folder
      } else {
        "(none)"
      }
    })
    
    output$outdir_selected <- renderText({
      if (!is.null(rv$selected_output_dir) && nzchar(rv$selected_output_dir)) {
        rv$selected_output_dir
      } else {
        "(none)"
      }
    })
    
    # Process button handler
    observeEvent(input$process, {
      # Validate inputs
      if (rv$shinyfiles_ready) {
        if (!nzchar(rv$selected_folder)) {
          showNotification("Please choose an input folder.", type = "warning")
          return()
        }
        folder_path <- rv$selected_folder
      } else {
        if (is.null(input$folder_path_fallback) || trimws(input$folder_path_fallback) == "") {
          showNotification("Please enter a valid folder path.", type = "warning")
          return()
        }
        folder_path <- input$folder_path_fallback
      }
      
      # Ensure magick is available
      if (!rv$magick_ready) {
        try({
          install.packages("magick", repos = "https://cran.r-project.org", quiet = TRUE)
        }, silent = TRUE)
        if (!requireNamespace("magick", quietly = TRUE)) {
          showNotification(
            "Package 'magick' is required and could not be installed automatically. Please install it: install.packages('magick')",
            type = "error",
            duration = 10
          )
          return()
        } else {
          rv$magick_ready <- TRUE
          showNotification("'magick' installed. Proceeding with processing.", type = "message", duration = 5)
        }
      }
      
      # Resolve output directory
      out_dir <- if (rv$shinyfiles_ready) rv$selected_output_dir else input$output_dir_fallback
      if (is.null(out_dir) || !nzchar(out_dir)) {
        out_dir <- file.path(getwd(), "completed_shapes")
      }
      out_dir <- normalizePath(out_dir, winslash = "/", mustWork = FALSE)
      
      if (!dir.exists(out_dir)) {
        ok_create <- try(dir.create(out_dir, recursive = TRUE, showWarnings = FALSE), silent = TRUE)
        if (inherits(ok_create, "try-error") || !dir.exists(out_dir)) {
          showNotification(sprintf("Cannot create output directory: %s", out_dir), type = "error")
          return()
        }
      }
      
      # Validate input folder
      folder_path <- normalizePath(folder_path, winslash = "/", mustWork = FALSE)
      if (!dir.exists(folder_path)) {
        showNotification(sprintf("Input folder does not exist: %s", folder_path), type = "error")
        return()
      }
      
      # Find image files
      files <- list.files(
        folder_path,
        pattern = "\\.(png|jpg|jpeg|tiff|tif|bmp)$",
        full.names = TRUE,
        ignore.case = TRUE,
        recursive = FALSE
      )
      
      if (length(files) == 0) {
        showNotification(
          sprintf("No image files found in folder: %s", folder_path),
          type = "warning",
          duration = 6
        )
        return()
      }
      
      withProgress(message = "Completing shapes...", value = 0, {
        incProgress(0.1, detail = sprintf("Found %d images", length(files)))
        
        result <- tryCatch({
          complete_halved_shape(
            input_paths = files,
            output_dir = out_dir,
            completion_options = list(
              method = input$method,
              half_side = input$half_side,
              blend_seam = input$blend_seam,
              seam_width = if (input$blend_seam) input$seam_width else 2
            ),
            alignment_options = list(
              auto_align = input$auto_align,
              alignment_method = if (input$auto_align) input$alignment_method else "none",
              crop_to_content = input$crop_to_content
            ),
            naming_options = list(
              suffix = input$suffix,
              prefix = "",
              preserve_name = FALSE,
              numbering = FALSE
            ),
            processing_options = list(
              overwrite = input$overwrite,
              quality = if (input$format == "jpg") input$quality else 90,
              format = input$format,
              preserve_transparency = input$preserve_transparency
            ),
            verbose = FALSE
          )
        }, error = function(e) {
          showNotification(
            paste("Error during processing:", e$message),
            type = "error",
            duration = 10
          )
          NULL
        })
        
        incProgress(0.9, detail = "Finalizing...")
        
        if (!is.null(result)) {
          rv$last_result <- result
          
          showNotification(
            sprintf(
              "Processing complete! Successfully processed %d/%d images.",
              result$summary$successful_count,
              result$summary$total_files
            ),
            type = if (result$summary$successful_count > 0) "message" else "warning",
            duration = 8
          )
        }
      })
    })
    
    # Display results summary
    output$results_summary <- renderText({
      req(rv$last_result)
      
      result <- rv$last_result
      
      paste0(
        "Processing Summary\n",
        "==================\n\n",
        "Total files: ", result$summary$total_files, "\n",
        "Successfully processed: ", result$summary$successful_count, "\n",
        "Failed: ", result$summary$failed_count, "\n\n",
        "Completion Configuration:\n",
        "  Method: ", result$summary$completion_method, "\n",
        "  Seam blending: ", result$summary$blend_seam, "\n",
        "  Auto alignment: ", result$summary$auto_align, "\n\n",
        if (!is.null(result$summary$average_processing_time)) {
          paste0("Average processing time: ", round(result$summary$average_processing_time, 3), " seconds\n")
        } else "",
        if (!is.null(result$summary$total_output_size)) {
          paste0("Total output size: ", round(result$summary$total_output_size / 1024^2, 2), " MB\n")
        } else ""
      )
    })
    
    # Display results table
    output$results_table <- DT::renderDataTable({
      req(rv$last_result)
      
      df <- rv$last_result$processed_files
      
      # Format for display
      display_df <- data.frame(
        Input = basename(df$input_path),
        Output = basename(df$output_path),
        Method = df$completion_method,
        Status = df$status,
        Time = sprintf("%.3f s", df$processing_time),
        stringsAsFactors = FALSE
      )
      
      if (any(df$status == "failed")) {
        display_df$Error <- df$error_message
      }
      
      DT::datatable(
        display_df,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          autoWidth = TRUE
        ),
        rownames = FALSE
      )
    })
    
    invisible(reactive(rv$last_result))
  })
}
