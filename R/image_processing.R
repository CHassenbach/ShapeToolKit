#' Image Processing Module (PNG -> JPG/BMP)
#'
#' UI and server logic to convert PNG images to JPG/BMP using
#' `convert_png_to_image()` with user-configurable options.
#'
#' @param id Module id
#' @return Invisibly returns a reactive containing the last results table
#' @name image_processing
NULL

#' @rdname image_processing
#' @export
image_processing_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        box(
          title = "Image Processing: PNG to JPG/BMP",
          status = "primary",
          solidHeader = TRUE,
          width = 12,

          uiOutput(ns("folder_chooser_ui")),

          uiOutput(ns("output_dir_ui")),
          selectInput(ns("format"), "Output format", choices = c("jpg", "bmp"), selected = "jpg"),
          radioButtons(
            ns("dim_mode"), "Resize by",
            choices = c("Width" = "width", "Height" = "height"),
            selected = "width", inline = TRUE
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'width'", ns("dim_mode")),
            numericInput(ns("width"), "Target width (px)", value = 800, min = 1, step = 10)
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'height'", ns("dim_mode")),
            numericInput(ns("height"), "Target height (px)", value = 600, min = 1, step = 10)
          ),
          sliderInput(ns("padding"), "Padding (px)", min = 0, max = 100, value = 10, step = 1),
          sliderInput(ns("quality"), "JPEG quality", min = 10, max = 100, value = 95, step = 1),
          textInput(ns("background"), "Background color for padding", value = "white"),
          checkboxInput(ns("overwrite"), "Overwrite existing outputs", value = FALSE),

          actionButton(ns("convert"), "Convert images", class = "btn-success")
        )
      )
    )
  )
}

#' @rdname image_processing
#' @export
image_processing_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

  # compact mode: no results table/log retained
  selected_folder <- reactiveVal("")
  selected_output_dir <- reactiveVal(file.path(getwd(), "converted"))
  # last_output_dir, results log removed in compact mode
  shinyfiles_ready <- reactiveVal(FALSE)
  magick_ready <- reactiveVal(FALSE)

    # Attempt silent install of shinyFiles if missing (run once in a reactive context)
    observe({
      ready <- requireNamespace("shinyFiles", quietly = TRUE)
      if (!isTRUE(ready)) {
        try({
          install.packages("shinyFiles", repos = "https://cran.r-project.org", quiet = TRUE)
        }, silent = TRUE)
        ready <- requireNamespace("shinyFiles", quietly = TRUE)
        if (isTRUE(ready)) {
          showNotification("Installed 'shinyFiles'. Folder chooser enabled.", type = "message")
        }
      }
      shinyfiles_ready(isTRUE(ready))
    })

    # Attempt silent install of magick (run once in a reactive context)
    observe({
      ready_m <- requireNamespace("magick", quietly = TRUE)
      if (!isTRUE(ready_m)) {
        try({
          install.packages("magick", repos = "https://cran.r-project.org", quiet = TRUE)
        }, silent = TRUE)
        ready_m <- requireNamespace("magick", quietly = TRUE)
        if (isTRUE(ready_m)) {
          showNotification("Installed 'magick'. Image conversion enabled.", type = "message")
        }
      }
      magick_ready(isTRUE(ready_m))
    })

    # Render folder chooser UI dynamically based on availability
    output$folder_chooser_ui <- renderUI({
      if (isTRUE(shinyfiles_ready())) {
        tagList(
          shinyFiles::shinyDirButton(ns("folder_btn"), label = "Choose folder", title = "Select a folder with PNG files"),
          br(), strong("Selected folder: "), textOutput(ns("folder_selected"), inline = TRUE)
        )
      } else {
        tagList(
          div(class = "text-warning", "Folder chooser not available. Enter a folder path manually."),
          br(),
          div("Enter a folder path manually:"),
          textInput(ns("folder_path_fallback"), "Folder path with PNG files", value = "")
        )
      }
    })

    # Render output directory chooser UI dynamically based on availability
    output$output_dir_ui <- renderUI({
      if (isTRUE(shinyfiles_ready())) {
        tagList(
          shinyFiles::shinyDirButton(ns("outdir_btn"), label = "Choose output directory", title = "Select an output directory"),
          br(), strong("Output directory: "), textOutput(ns("outdir_selected"), inline = TRUE)
        )
      } else {
        textInput(ns("output_dir_fallback"), "Output directory", value = selected_output_dir())
      }
    })

    # Setup folder chooser when shinyFiles becomes available
    observeEvent(shinyfiles_ready(), ignoreInit = FALSE, handlerExpr = {
      if (!isTRUE(shinyfiles_ready())) return()
      vols <- try(shinyFiles::getVolumes()(), silent = TRUE)
      if (inherits(vols, "try-error") || is.null(vols) || length(vols) == 0) {
        vols <- c(Home = normalizePath("~"))
      }
      shinyFiles::shinyDirChoose(input, id = "folder_btn", roots = vols, session = session)
      shinyFiles::shinyDirChoose(input, id = "outdir_btn", roots = vols, session = session)
    })

    observeEvent(input$folder_btn, {
      if (!isTRUE(shinyfiles_ready())) return()
      vols <- try(shinyFiles::getVolumes()(), silent = TRUE)
      if (inherits(vols, "try-error") || is.null(vols) || length(vols) == 0) {
        vols <- c(Home = normalizePath("~"))
      }
      sel <- try(shinyFiles::parseDirPath(vols, input$folder_btn), silent = TRUE)
      if (!inherits(sel, "try-error") && length(sel) == 1) {
        selected_folder(as.character(sel))
      }
    })

    observeEvent(input$outdir_btn, {
      if (!isTRUE(shinyfiles_ready())) return()
      vols <- try(shinyFiles::getVolumes()(), silent = TRUE)
      if (inherits(vols, "try-error") || is.null(vols) || length(vols) == 0) {
        vols <- c(Home = normalizePath("~"))
      }
      sel <- try(shinyFiles::parseDirPath(vols, input$outdir_btn), silent = TRUE)
      if (!inherits(sel, "try-error") && length(sel) == 1) {
        selected_output_dir(as.character(sel))
      }
    })

    output$folder_selected <- renderText({
      pf <- selected_folder()
      if (!is.null(pf) && nzchar(pf)) pf else "(none)"
    })

    output$outdir_selected <- renderText({
      pf <- selected_output_dir()
      if (!is.null(pf) && nzchar(pf)) pf else "(none)"
    })

    observeEvent(input$convert, {
      # basic validation (folder-only workflow)
      if (isTRUE(shinyfiles_ready())) {
        if (!nzchar(selected_folder())) {
          showNotification("Please choose a folder.", type = "warning")
          return(invisible(NULL))
        }
      } else {
        if (is.null(input$folder_path_fallback) || trimws(input$folder_path_fallback) == "") {
          showNotification("Please enter a valid folder path.", type = "warning")
          return(invisible(NULL))
        }
      }

      # Ensure magick is available (attempt on-demand install if needed)
      if (!isTRUE(magick_ready())) {
        try({
          install.packages("magick", repos = "https://cran.r-project.org", quiet = TRUE)
        }, silent = TRUE)
        if (!requireNamespace("magick", quietly = TRUE)) {
          showNotification("Package 'magick' is required and could not be installed automatically. Please install it: install.packages('magick')", type = "error", duration = 10)
          return(invisible(NULL))
        } else {
          magick_ready(TRUE)
          showNotification("'magick' installed. Proceeding with conversion.", type = "message", duration = 5)
        }
      }

      # build dimensions from user selection
      dims <- list()
      if (input$dim_mode == "width") {
        dims$width <- as.numeric(input$width)
      } else {
        dims$height <- as.numeric(input$height)
      }

      # resolve and validate output directory
      out_dir <- if (isTRUE(shinyfiles_ready())) selected_output_dir() else input$output_dir_fallback
      if (is.null(out_dir) || !nzchar(out_dir)) {
        out_dir <- file.path(getwd(), "converted")
      }
      out_dir <- normalizePath(out_dir, winslash = "/", mustWork = FALSE)
      if (!dir.exists(out_dir)) {
        ok_create <- try(dir.create(out_dir, recursive = TRUE, showWarnings = FALSE), silent = TRUE)
        if (inherits(ok_create, "try-error") || !dir.exists(out_dir)) {
          showNotification(sprintf("Cannot create output directory: %s", out_dir), type = "error")
          return(invisible(NULL))
        }
      }
  # store selected output dir internally if needed (not shown in compact UI)

      withProgress(message = "Converting images...", value = 0, {
        res <- NULL
        # folder mode - batch processing per file
        folder_path <- if (isTRUE(shinyfiles_ready())) selected_folder() else input$folder_path_fallback
        folder_path <- normalizePath(folder_path, winslash = "/", mustWork = FALSE)
        if (!dir.exists(folder_path)) {
          showNotification(sprintf("Input folder does not exist: %s", folder_path), type = "error")
          return(invisible(NULL))
        }
        files <- list.files(folder_path, pattern = "\\.png$", full.names = TRUE, ignore.case = TRUE, recursive = TRUE)
        if (length(files) == 0) {
          showNotification(sprintf("No PNG files found in folder: %s", folder_path), type = "warning", duration = 6)
          res <- data.frame(
            input_file = character(0), output_file = character(0),
            original_size = character(0), processed_size = character(0),
            success = logical(0), message = character(0), stringsAsFactors = FALSE
          )
        } else {
          n <- length(files)
          out_list <- vector("list", n)
          for (i in seq_along(files)) {
            incProgress(1 / max(1, n), detail = basename(files[i]))
            out_list[[i]] <- tryCatch({
              convert_png_to_image(
                input_path = files[i],
                output_dir = out_dir,
                dimensions = dims,
                padding = input$padding,
                format = input$format,
                quality = input$quality,
                background = input$background,
                batch_processing = FALSE,
                overwrite = isTRUE(input$overwrite),
                verbose = FALSE
              )
            }, error = function(e) {
              data.frame(
                input_file = files[i], output_file = NA_character_,
                original_size = NA_character_, processed_size = NA_character_,
                success = FALSE, message = conditionMessage(e), stringsAsFactors = FALSE
              )
            })
          }
          res <- do.call(rbind, out_list)
        }
      })
      if (!is.null(res) && nrow(res) > 0) {
        ok <- sum(res$success, na.rm = TRUE)
        showNotification(sprintf("Conversion finished: %d/%d successful.", ok, nrow(res)), type = if (ok > 0) "message" else "warning")
      }
    })

    # compact mode: no additional outputs

    invisible(NULL)
  })
}
