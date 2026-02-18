#' Morph Shapes Module - Split and Morph Images
#'
#' UI and server logic for uploading images, splitting them at a specified position,
#' and morphing the resulting halves back together using advanced morphing algorithms.
#'
#' @param id Module id
#' @return Server returns a reactive containing processing results
#' @name morph_shapes_module
NULL

#' @rdname morph_shapes_module
#' @export
morph_shapes_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        box(
          title = icon("info-circle"),
          status = "info",
          solidHeader = FALSE,
          width = 12,
          p(strong("Tip:"), "For best results, use images with at least 30 pixels of padding around the shapes.")
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        box(
          title = "1. Upload Image",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          
          fileInput(
            ns("upload_image"),
            "Choose a JPG image file",
            accept = c("image/jpeg", "image/jpg"),
            multiple = FALSE
          ),
          
          conditionalPanel(
            condition = sprintf("output['%s']", ns("image_uploaded")),
            box(
              title = "Image Preview",
              status = "info",
              width = 12,
              plotOutput(ns("preview_plot"), height = "400px"),
              br(),
              verbatimTextOutput(ns("image_info"))
            )
          )
        )
      ),
      
      column(
        width = 6,
        box(
          title = "2. Split Configuration",
          status = "warning",
          solidHeader = TRUE,
          width = 12,
          
          sliderInput(
            ns("rotate_image"),
            "Rotate Image Before Splitting (degrees clockwise)",
            min = 0,
            max = 360,
            value = 0,
            step = 0.5
          ),
          
          selectInput(
            ns("split_direction"),
            "Split Direction",
            choices = c("Vertical (left/right)" = "vertical", 
                       "Horizontal (top/bottom)" = "horizontal"),
            selected = "vertical"
          ),
          
          sliderInput(
            ns("split_position"),
            "Split Position (0 = left/top, 1 = right/bottom)",
            min = 0.1,
            max = 0.9,
            value = 0.5,
            step = 0.001
          ),
          
          radioButtons(
            ns("mirror_parts"),
            "Mirror which part?",
            choices = c("None" = "none",
                       "First part (left/top)" = "first",
                       "Second part (right/bottom)" = "second"),
            selected = "second"
          ),
          
          hr(),
          
          h4("Morphing Settings"),
          
          fluidRow(
            column(
              width = 6,
              numericInput(
                ns("n_steps"),
                "Number of morphing steps",
                value = 5,
                min = 1,
                max = 20,
                step = 1
              )
            ),
            
            column(
              width = 6,
              numericInput(
                ns("threshold"),
                "Binary threshold (0-1)",
                value = 0.1,
                min = 0,
                max = 1,
                step = 0.05
              )
            )
          ),
          
          hr(),
          
          uiOutput(ns("output_dir_ui")),
          
          textInput(
            ns("output_folder_name"),
            "Output subfolder name",
            value = format(Sys.time(), "morphed_%Y%m%d_%H%M%S")
          ),
          
          actionButton(
            ns("btn_process"),
            "Split & Morph Image",
            icon = icon("wand-magic-sparkles"),
            class = "btn-success",
            style = "width: 100%;"
          )
        )
      )
    ),
    
    fluidRow(
      column(
        width = 12,
        conditionalPanel(
          condition = sprintf("output['%s']", ns("processing_completed")),
          box(
            title = "Processing Results",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            
            verbatimTextOutput(ns("process_summary")),
            
            hr(),
            
            h4("Split Images Preview"),
            
            fluidRow(
              column(
                width = 6,
                box(
                  title = "First Half",
                  width = 12,
                  status = "info",
                  plotOutput(ns("split_preview_first"), height = "300px")
                )
              ),
              column(
                width = 6,
                box(
                  title = "Second Half",
                  width = 12,
                  status = "info",
                  plotOutput(ns("split_preview_second"), height = "300px")
                )
              )
            ),
            
            hr(),
            
            h4("Morphed Results"),
            
            sliderInput(
              ns("morph_preview_step"),
              "Preview morphing step:",
              min = 1,
              max = 5,
              value = 1,
              step = 1,
              animate = animationOptions(interval = 500)
            ),
            
            plotOutput(ns("morph_preview"), height = "500px")
          )
        )
      )
    )
  )
}

#' @rdname morph_shapes_module
#' @export
morph_shapes_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values to store state
    rv <- reactiveValues(
      uploaded_image = NULL,
      uploaded_path = NULL,
      split_result = NULL,
      morph_result = NULL,
      split_paths = list(first = NULL, second = NULL),
      magick_ready = FALSE,
      imager_ready = FALSE,
      shinyfiles_ready = FALSE
    )
    
    # Check package availability
    observe({
      rv$magick_ready <- requireNamespace("magick", quietly = TRUE)
      rv$imager_ready <- requireNamespace("imager", quietly = TRUE)
      rv$shinyfiles_ready <- requireNamespace("shinyFiles", quietly = TRUE)
      
      # Try to install missing packages silently
      if (!rv$magick_ready) {
        try({
          install.packages("magick", repos = "https://cran.r-project.org", quiet = TRUE)
          rv$magick_ready <- requireNamespace("magick", quietly = TRUE)
        }, silent = TRUE)
      }
      
      if (!rv$imager_ready) {
        try({
          install.packages("imager", repos = "https://cran.r-project.org", quiet = TRUE)
          rv$imager_ready <- requireNamespace("imager", quietly = TRUE)
        }, silent = TRUE)
      }
    })
    
    # Output directory UI
    output$output_dir_ui <- renderUI({
      if (rv$shinyfiles_ready) {
        tagList(
          shinyFiles::shinyDirButton(
            ns("output_dir_btn"),
            label = "Choose output directory",
            title = "Select output directory"
          ),
          br(),
          strong("Output directory: "),
          textOutput(ns("output_dir_display"), inline = TRUE)
        )
      } else {
        textInput(
          ns("output_dir_fallback"),
          "Output directory",
          value = file.path(getwd(), "morph_output")
        )
      }
    })
    
    # Selected output directory
    selected_output_dir <- reactiveVal(file.path(getwd(), "morph_output"))
    
    # Setup directory chooser
    observeEvent(rv$shinyfiles_ready, ignoreInit = FALSE, {
      if (!rv$shinyfiles_ready) return()
      
      vols <- try(shinyFiles::getVolumes()(), silent = TRUE)
      if (inherits(vols, "try-error") || is.null(vols) || length(vols) == 0) {
        vols <- c(Home = normalizePath("~"))
      }
      shinyFiles::shinyDirChoose(input, id = "output_dir_btn", roots = vols, session = session)
    })
    
    observeEvent(input$output_dir_btn, {
      if (!rv$shinyfiles_ready) return()
      
      vols <- try(shinyFiles::getVolumes()(), silent = TRUE)
      if (inherits(vols, "try-error") || is.null(vols) || length(vols) == 0) {
        vols <- c(Home = normalizePath("~"))
      }
      
      sel <- try(shinyFiles::parseDirPath(vols, input$output_dir_btn), silent = TRUE)
      if (!inherits(sel, "try-error") && length(sel) == 1) {
        selected_output_dir(as.character(sel))
      }
    })
    
    output$output_dir_display <- renderText({
      dir_path <- selected_output_dir()
      if (!is.null(dir_path) && nzchar(dir_path)) dir_path else "(none)"
    })
    
    # Handle image upload
    observeEvent(input$upload_image, {
      req(input$upload_image)
      
      if (!rv$magick_ready) {
        showNotification(
          "Package 'magick' is required. Please install it: install.packages('magick')",
          type = "error",
          duration = 10
        )
        return()
      }
      
      tryCatch({
        # Load image using magick
        img <- magick::image_read(input$upload_image$datapath)
        rv$uploaded_image <- img
        rv$uploaded_path <- input$upload_image$datapath
        rv$uploaded_filename <- tools::file_path_sans_ext(input$upload_image$name)
        
        showNotification("Image uploaded successfully!", type = "message")
      }, error = function(e) {
        showNotification(
          paste("Error loading image:", e$message),
          type = "error",
          duration = 10
        )
      })
    })
    
    # Image uploaded flag
    output$image_uploaded <- reactive({
      !is.null(rv$uploaded_image)
    })
    outputOptions(output, "image_uploaded", suspendWhenHidden = FALSE)
    
    # Preview uploaded image with split line
    output$preview_plot <- renderPlot({
      req(rv$uploaded_image)
      
      # Make reactive to rotation, split_position and split_direction
      rotation <- as.numeric(input$rotate_image)
      split_pos <- input$split_position
      split_dir <- input$split_direction
      
      tryCatch({
        # Apply rotation if needed
        img_display <- rv$uploaded_image
        if (!is.null(rotation) && rotation > 0) {
          img_display <- magick::image_rotate(img_display, rotation)
        }
        
        # Get image info
        img_info <- magick::image_info(img_display)
        img_width <- img_info$width
        img_height <- img_info$height
        
        # Convert to raster for plotting
        img_raster <- as.raster(img_display)
        
        # Set up plot with proper dimensions
        par(mar = c(0, 0, 0, 0))
        plot(0:1, 0:1, type = "n", axes = FALSE, xlab = "", ylab = "", asp = img_height/img_width)
        rasterImage(img_raster, 0, 0, 1, 1)
        
        # Draw split line
        if (!is.null(split_pos) && !is.na(split_pos)) {
          if (split_dir == "vertical") {
            # Vertical line at split position
            abline(v = split_pos, col = "red", lwd = 3)
          } else {
            # Horizontal line at split position (from bottom)
            abline(h = split_pos, col = "red", lwd = 3)
          }
        }
      }, error = function(e) {
        plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, paste("Error displaying image:", e$message))
      })
    })    
    # Display image info
    output$image_info <- renderText({
      req(rv$uploaded_image)
      
      info <- magick::image_info(rv$uploaded_image)
      paste0(
        "Format: ", info$format, "\n",
        "Dimensions: ", info$width, " x ", info$height, " pixels\n",
        "Colorspace: ", info$colorspace, "\n",
        "Filesize: ", round(info$filesize / 1024, 2), " KB"
      )
    })
    
    # Process: Split and Morph
    observeEvent(input$btn_process, {
      req(rv$uploaded_image)
      
      if (!rv$magick_ready) {
        showNotification(
          "Package 'magick' is required. Please install it.",
          type = "error"
        )
        return()
      }
      
      if (!rv$imager_ready) {
        showNotification(
          "Package 'imager' is required for morphing. Please install it: install.packages('imager')",
          type = "error",
          duration = 10
        )
        return()
      }
      
      # Get output directory
      output_dir <- if (rv$shinyfiles_ready) {
        selected_output_dir()
      } else {
        input$output_dir_fallback
      }
      
      if (is.null(output_dir) || !nzchar(output_dir)) {
        output_dir <- file.path(getwd(), "morph_output")
      }
      
      # Create output subfolder
      final_output_dir <- file.path(output_dir, input$output_folder_name)
      
      withProgress(message = "Processing...", value = 0, {
        tryCatch({
          # Prepare temporary file for the uploaded image
          temp_input <- tempfile(fileext = paste0(".", tools::file_ext(input$upload_image$name)))
          
          # Apply rotation if needed
          img_to_process <- rv$uploaded_image
          rotation <- as.numeric(input$rotate_image)
          if (!is.null(rotation) && rotation > 0) {
            img_to_process <- magick::image_rotate(img_to_process, rotation)
          }
          
          magick::image_write(img_to_process, temp_input)
          
          incProgress(0.1, detail = "Splitting image...")
          
          # Create temporary directory for split images
          temp_split_dir <- tempfile(pattern = "split_")
          dir.create(temp_split_dir, recursive = TRUE)
          
          # Call split_image function
          split_result <- split_image(
            input_paths = temp_input,
            output_dir = temp_split_dir,
            split_options = list(
              direction = input$split_direction,
              split_position = input$split_position,
              mirror_parts = input$mirror_parts
            ),
            naming_options = list(
              suffix_first = if (input$split_direction == "vertical") "_left" else "_top",
              suffix_second = if (input$split_direction == "vertical") "_right" else "_bottom",
              prefix = "",
              numbering = FALSE
            ),
            processing_options = list(
              overwrite = TRUE,
              quality = 95,
              format = "jpg",
              preserve_metadata = TRUE
            ),
            verbose = TRUE  # Enable verbose for debugging
          )
          
          incProgress(0.2, detail = "Split complete. Starting morphing...")
          
          # Store split results and paths
          rv$split_result <- split_result
          
          # Debug: Print split result structure
          cat("\n=== SPLIT DEBUG INFO ===\n")
          cat("Processed files rows:", nrow(split_result$processed_files), "\n")
          cat("Processed files columns:", ncol(split_result$processed_files), "\n")
          if (nrow(split_result$processed_files) > 0) {
            cat("Processed files content:\n")
            print(split_result$processed_files)
          }
          cat("Split info length:", length(split_result$split_info), "\n")
          cat("========================\n\n")
          
          # Check if split was successful
          if (is.null(split_result$processed_files) || nrow(split_result$processed_files) < 2) {
            stop("Split operation did not produce 2 images. Check split_result: ", 
                 nrow(split_result$processed_files), " files created")
          }
          
          # Check for successful status
          success_files <- split_result$processed_files[split_result$processed_files$status == "success", ]
          if (nrow(success_files) < 2) {
            stop("Split images were not successfully created. Status check failed.")
          }
          
          # Get split image paths
          rv$split_paths$first <- success_files$output_path[1]
          rv$split_paths$second <- success_files$output_path[2]
          
          # Normalize paths for Windows
          rv$split_paths$first <- normalizePath(rv$split_paths$first, winslash = "\\", mustWork = FALSE)
          rv$split_paths$second <- normalizePath(rv$split_paths$second, winslash = "\\", mustWork = FALSE)
          
          # Verify files exist
          if (!file.exists(rv$split_paths$first)) {
            stop("First split image not found at: ", rv$split_paths$first)
          }
          if (!file.exists(rv$split_paths$second)) {
            stop("Second split image not found at: ", rv$split_paths$second)
          }
          
          # Convert images to RGB using magick before morphing
          # This ensures compatibility with imager's morphing functions (needs 3 color channels)
          incProgress(0.05, detail = "Converting to RGB...")
          
          img1 <- magick::image_read(rv$split_paths$first)
          img2 <- magick::image_read(rv$split_paths$second)
          
          # Convert to RGB (3 channels) - required by imager
          img1 <- magick::image_convert(img1, colorspace = "sRGB", type = "TrueColor")
          img2 <- magick::image_convert(img2, colorspace = "sRGB", type = "TrueColor")
          
          # Save back to temp files
          temp_rgb1 <- tempfile(fileext = ".png")
          temp_rgb2 <- tempfile(fileext = ".png")
          magick::image_write(img1, temp_rgb1)
          magick::image_write(img2, temp_rgb2)
          
          # Update paths to use RGB versions
          rv$split_paths$first <- temp_rgb1
          rv$split_paths$second <- temp_rgb2
          
          incProgress(0.05, detail = "Starting morphing...")
          
          # Now proceed with morphing
          if (TRUE) {
            
            # Call morph_shapes function
            morph_result <- morph_shapes(
              input_paths = c(rv$split_paths$first, rv$split_paths$second),
              output_dir = final_output_dir,
              morphing_options = list(
                method = "distance_transform",
                n_steps = input$n_steps,
                morph_type = "pair",
                interpolation = "linear"
              ),
              processing_options = list(
                threshold = input$threshold,
                gamma = 1.0,
                blur_sigma = 0,
                auto_align = FALSE
              ),
              distance_options = list(
                distance_metric = "euclidean",
                normalize_distances = TRUE,
                invert_distances = FALSE
              ),
              blending_options = list(
                blend_mode = "average",
                weights = NULL,
                edge_enhancement = FALSE
              ),
              output_options = list(
                format = "jpg",
                naming_pattern = paste0(rv$uploaded_filename, "_morphed_{step}"),
                validate_binary = TRUE,
                similarity_threshold = 0
              ),
              export_options = list(),
              verbose = FALSE
            )
            
            incProgress(0.5, detail = "Morphing complete!")
            
            # Store morph results
            rv$morph_result <- morph_result
            
            incProgress(0.2, detail = "Finalizing...")
            
            showNotification(
              paste0("Processing complete! ", 
                    length(morph_result$morphed_images), 
                    " morphed images created in: ", 
                    final_output_dir),
              type = "message",
              duration = 5
            )
          }
          
          # Clean up temp files (but keep split files temporarily for preview)
          unlink(temp_input)
          # Don't delete split_dir yet - we need it for preview
          
        }, error = function(e) {
          # More detailed error message
          error_msg <- paste("Error processing image:", e$message)
          
          # Add debug info if available
          if (!is.null(rv$split_result)) {
            error_msg <- paste0(error_msg, 
                              "\n\nDebug Info:",
                              "\nSplit files created: ", nrow(rv$split_result$processed_files),
                              "\nSplit directory: ", temp_split_dir)
          }
          
          showNotification(
            error_msg,
            type = "error",
            duration = 15
          )
          
          # Clean up on error
          if (exists("temp_input")) unlink(temp_input)
          if (exists("temp_split_dir")) unlink(temp_split_dir, recursive = TRUE)
        })
      })
    })
    
    # Processing completed flag
    output$processing_completed <- reactive({
      !is.null(rv$split_result) && !is.null(rv$morph_result)
    })
    outputOptions(output, "processing_completed", suspendWhenHidden = FALSE)
    
    # Preview split results
    output$split_preview_first <- renderPlot({
      req(rv$split_paths$first)
      req(file.exists(rv$split_paths$first))
      
      tryCatch({
        img <- magick::image_read(rv$split_paths$first)
        plot(img)
      }, error = function(e) {
        plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, paste("Error:", e$message))
      })
    })
    
    output$split_preview_second <- renderPlot({
      req(rv$split_paths$second)
      req(file.exists(rv$split_paths$second))
      
      tryCatch({
        img <- magick::image_read(rv$split_paths$second)
        plot(img)
      }, error = function(e) {
        plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, paste("Error:", e$message))
      })
    })
    
    # Display processing summary
    output$process_summary <- renderText({
      req(rv$split_result, rv$morph_result)
      
      summary <- rv$morph_result$processing_summary
      paste0(
        "Processing Summary\n",
        "==================\n\n",
        "Split Configuration:\n",
        "  Direction: ", input$split_direction, "\n",
        "  Position: ", input$split_position, "\n\n",
        "Morphing Summary:\n",
        "  Total morphed images: ", summary$total_morphed_images, "\n",
        "  Morphing method: Distance Transform\n",
        "  Steps: ", summary$n_steps_per_pair, "\n",
        "  Output format: JPG\n\n",
        "Processing Options:\n",
        "  Threshold: ", summary$processing_options$threshold
      )
    })
    
    # Update morph step slider when morphing completes
    observe({
      req(rv$morph_result)
      req(!is.null(input$n_steps))
      
      updateSliderInput(
        session,
        "morph_preview_step",
        max = input$n_steps
      )
    })
    
    # Preview morphed result
    output$morph_preview <- renderPlot({
      req(rv$morph_result)
      req(input$morph_preview_step)
      
      step <- input$morph_preview_step
      morphed_images <- rv$morph_result$morphed_images
      
      if (step > length(morphed_images)) {
        step <- length(morphed_images)
      }
      
      tryCatch({
        if (rv$imager_ready) {
          img <- imager::load.image(morphed_images[step])
          plot(img, axes = FALSE, main = paste("Morphing Step", step))
        } else {
          # Fallback to magick
          img <- magick::image_read(morphed_images[step])
          plot(img)
          title(paste("Morphing Step", step))
        }
      }, error = function(e) {
        plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, paste("Error displaying morphed image:", e$message))
      })
    })
    
    # Return reactive with results
    reactive({
      list(
        split_result = rv$split_result,
        morph_result = rv$morph_result,
        split_paths = rv$split_paths
      )
    })
  })
}
