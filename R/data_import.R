#' Data Import Module (Excel)
#'
#' UI and server logic to upload an Excel file (.xlsx/.xls), select a sheet,
#' and preview the data in a table.
#'
#' @param id Module id
#' @return Server returns a list with a reactive `data` containing the imported dataset.
#' @examples
#' # In UI: data_import_ui("import")
#' # In server: imported <- data_import_server("import")
#' @name data_import
NULL

#' @rdname data_import
#' @export
data_import_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        box(
          title = "Upload Spreadsheet",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          fileInput(
            inputId = ns("file"),
            label = "Choose a spreadsheet (.xlsx, .xls, or .ods)",
            accept = c(".xlsx", ".xls", ".ods"),
            buttonLabel = "Browse...",
            placeholder = "No file selected"
          ),
          uiOutput(ns("sheet_ui")),
          sliderInput(
            inputId = ns("n_rows"),
            label = "Preview rows",
            min = 5, max = 100, value = 20, step = 5
          )
        ),
        box(
          title = "Subset Data",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          helpText("Filter rows based on column values. Multiple filters are combined with AND logic."),
          fluidRow(
            column(3, selectInput(ns("filter_col"), "Column", choices = NULL)),
            column(3, selectInput(ns("filter_op"), "Operator", 
                                 choices = c("==" = "eq", "!=" = "ne", ">" = "gt", 
                                           "<" = "lt", ">=" = "ge", "<=" = "le",
                                           "contains" = "contains", "starts with" = "starts",
                                           "ends with" = "ends", "is NA" = "is_na", 
                                           "is not NA" = "not_na"),
                                 selected = "eq")),
            column(3, uiOutput(ns("filter_value_ui"))),
            column(3, div(style = "margin-top: 24px;", 
                         actionButton(ns("filter_add"), "Add filter", class = "btn-primary")))
          ),
          uiOutput(ns("active_filters_ui")),
          fluidRow(
            column(6, div(style = "margin-top: 8px;",
                         actionButton(ns("filter_apply"), "Apply filters", class = "btn-success"))),
            column(6, div(style = "margin-top: 8px;",
                         actionButton(ns("filter_reset"), "Reset to original", class = "btn-warning")))
          ),
          br(),
          textOutput(ns("subset_status"))
        ),
        box(
          title = "Map shapes to data",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          uiOutput(ns("id_col_ui")),
          uiOutput(ns("shape_dir_ui")),
          textOutput(ns("shape_dir_txt")),
          div(style = "margin: 6px 0;",
              actionButton(ns("map_shapes_btn"), "Map shapes", class = "btn-primary")
          ),
          br(),
          verbatimTextOutput(ns("mapping_summary"))
        ),
        box(
          title = "Group Builder",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          helpText("Create or modify grouping columns by defining rules based on ID ranges or lists."),
          fluidRow(
            column(4, textInput(ns("group_target_col"), "Target column (new or existing)", value = "group")),
            column(4, selectInput(ns("group_id_col"), "ID column for grouping", choices = NULL)),
            column(4, radioButtons(ns("group_method"), "Method", choices = c("Ranges (inclusive)" = "range", "List of IDs" = "list"), inline = TRUE))
          ),
          fluidRow(
            column(3, checkboxInput(ns("group_sort_ids"), "Sort IDs ascending", value = TRUE)),
            column(3, textInput(ns("group_default"), "Default fill (optional)", value = "")),
            column(3, div(style = "margin-top: 24px;", actionButton(ns("group_add_rule"), "Add rule"))),
            column(3, div(style = "margin-top: 24px;", actionButton(ns("group_remove_rule"), "Remove last")))
          ),
          uiOutput(ns("group_rules_ui")),
          fluidRow(
            column(3, div(style = "margin-top: 8px;", actionButton(ns("group_apply"), "Apply rules", class = "btn-success"))),
            column(9, div(style = "margin-top: 14px;", textOutput(ns("group_status"))))
          )
        ),
        box(
          title = "Data Preview",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          helpText("To overwrite the original Excel file, pick its location and confirm overwrite."),
          fluidRow(
            column(6,
              uiOutput(ns("save_excel_ui"))
            ),
            column(6, div(style = "margin-top: 28px;",
              textOutput(ns("save_status"))
            ))
          ),
          br(),
          DT::DTOutput(ns("preview")),
          br()
        )
      )
    )
  )
}

#' @rdname data_import
#' @export
data_import_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Helper: null-or-empty coalesce
    `%||%` <- function(a, b) {
      if (is.null(a)) return(b)
      if (is.character(a) && identical(length(a), 1L) && !nzchar(a)) return(b)
      a
    }

    # Working copy of data (editable)
    working_data <- reactiveVal(NULL)
    
    # Store original unfiltered data
    original_data <- reactiveVal(NULL)
    
    # Store active filters
    active_filters <- reactiveVal(list())

    # Track selected sheet options
    sheet_names <- reactiveVal(character())

    # When a file is uploaded, list its sheets
    observeEvent(input$file, {
      req(input$file)
      path <- input$file$datapath

      # Validate file extension
      ext <- tolower(tools::file_ext(input$file$name))
      validate(need(ext %in% c("xlsx", "xls", "ods"), "Please upload a .xlsx, .xls, or .ods file."))

      # Get sheet names safely
      sheets <- tryCatch({
        if (identical(ext, "ods")) {
          # Ensure readODS is available
          if (!requireNamespace("readODS", quietly = TRUE)) {
            try(install.packages("readODS", repos = "https://cran.r-project.org", quiet = TRUE), silent = TRUE)
          }
          validate(need(requireNamespace("readODS", quietly = TRUE), "Package 'readODS' is required to read .ods files."))
          # List sheet names from ODS
          readODS::ods_sheets(path)
        } else {
          # Default to openxlsx for Excel files
          openxlsx::getSheetNames(path)
        }
      }, error = function(e) {
        warning(e)
        character()
      })

      sheet_names(sheets)

      # Render a sheet selector if multiple sheets exist
      output$sheet_ui <- renderUI({
        if (length(sheets) == 0) {
          div(class = "text-danger", "No sheets found in the uploaded file.")
        } else if (length(sheets) == 1) {
          # Only one sheet: show its name; no input needed because we infer it
          tagList(p(strong("Sheet:"), sheets[[1]]))
        } else {
          selectInput(ns("sheet"), "Select sheet", choices = sheets, selected = sheets[[1]])
        }
      })
    }, ignoreInit = TRUE)

    # Reactive: read the data when file or sheet changes
    data_reactive <- reactive({
      req(input$file)
      sheets <- sheet_names()
      sheet_to_read <- if (length(sheets) == 1) sheets[[1]] else input$sheet
      req(sheet_to_read)
      ext <- tolower(tools::file_ext(input$file$name))

      # Read using openxlsx; detect dates and keep as data.frame
      df <- tryCatch({
        if (identical(ext, "ods")) {
          # Ensure readODS is available
          if (!requireNamespace("readODS", quietly = TRUE)) {
            try(install.packages("readODS", repos = "https://cran.r-project.org", quiet = TRUE), silent = TRUE)
          }
          validate(need(requireNamespace("readODS", quietly = TRUE), "Package 'readODS' is required to read .ods files."))
          # Read ODS; sheet can be name or index
          readODS::read_ods(
            path = input$file$datapath,
            sheet = sheet_to_read
          )
        } else {
          openxlsx::read.xlsx(
            xlsxFile = input$file$datapath,
            sheet = sheet_to_read,
            detectDates = TRUE
          )
        }
      }, error = function(e) {
        validate(need(FALSE, paste("Failed to read sheet:", conditionMessage(e))))
        NULL
      })

      # Ensure it's a data.frame
      if (!is.null(df)) df <- as.data.frame(df, stringsAsFactors = FALSE)
      df
    })

    # Keep working_data synced with source data or mapping results
    observeEvent(data_reactive(), {
      df <- data_reactive()
      working_data(df)
      original_data(df)
      active_filters(list())  # Reset filters when new data loaded
    })

    # Subsetting logic ----
    
    # Update filter column choices
    observe({
      df <- original_data()
      req(df)
      cols <- names(df)
      updateSelectInput(session, "filter_col", choices = cols, 
                       selected = if (!is.null(input$filter_col) && input$filter_col %in% cols) input$filter_col else cols[1])
    })
    
    # Dynamic filter value input (hide for NA checks)
    output$filter_value_ui <- renderUI({
      req(input$filter_op)
      if (input$filter_op %in% c("is_na", "not_na")) {
        return(NULL)
      }
      textInput(ns("filter_value"), "Value", value = "")
    })
    
    # Add filter to list
    observeEvent(input$filter_add, {
      req(input$filter_col, input$filter_op)
      
      # Get value (not needed for NA checks)
      val <- if (input$filter_op %in% c("is_na", "not_na")) {
        NULL
      } else {
        req(input$filter_value)
        input$filter_value
      }
      
      # Create filter object
      new_filter <- list(
        col = input$filter_col,
        op = input$filter_op,
        value = val,
        id = length(active_filters()) + 1
      )
      
      # Add to active filters
      filters <- active_filters()
      filters[[length(filters) + 1]] <- new_filter
      active_filters(filters)
      
      # Clear value input
      updateTextInput(session, "filter_value", value = "")
    })
    
    # Display active filters
    output$active_filters_ui <- renderUI({
      filters <- active_filters()
      if (length(filters) == 0) {
        return(helpText("No filters added yet"))
      }
      
      # Create human-readable filter descriptions with remove buttons
      filter_rows <- lapply(seq_along(filters), function(i) {
        f <- filters[[i]]
        op_text <- switch(f$op,
          "eq" = "==", "ne" = "!=", "gt" = ">", "lt" = "<", 
          "ge" = ">=", "le" = "<=", "contains" = "contains",
          "starts" = "starts with", "ends" = "ends with",
          "is_na" = "is NA", "not_na" = "is not NA"
        )
        desc <- if (is.null(f$value)) {
          sprintf("%s %s", f$col, op_text)
        } else {
          sprintf("%s %s '%s'", f$col, op_text, f$value)
        }
        fluidRow(
          column(9, tags$code(desc)),
          column(3, actionButton(ns(paste0("remove_filter_", i)), "Remove", 
                                class = "btn-sm btn-danger"))
        )
      })
      
      tagList(
        tags$hr(),
        tags$strong("Active filters:"),
        tags$div(style = "margin-top: 8px;", filter_rows)
      )
    })
    
    # Remove individual filters
    observe({
      filters <- active_filters()
      if (length(filters) == 0) return()
      
      lapply(seq_along(filters), function(i) {
        observeEvent(input[[paste0("remove_filter_", i)]], {
          current_filters <- active_filters()
          if (i <= length(current_filters)) {
            current_filters[[i]] <- NULL
            active_filters(current_filters)
          }
        }, ignoreInit = TRUE)
      })
    })
    
    # Apply filters
    observeEvent(input$filter_apply, {
      df <- original_data()
      req(df)
      
      filters <- active_filters()
      if (length(filters) == 0) {
        showNotification("No filters to apply", type = "warning")
        return()
      }
      
      # Apply each filter
      filtered <- df
      for (f in filters) {
        col_data <- filtered[[f$col]]
        
        # Apply operator
        keep_rows <- switch(f$op,
          "eq" = col_data == f$value,
          "ne" = col_data != f$value,
          "gt" = as.numeric(col_data) > as.numeric(f$value),
          "lt" = as.numeric(col_data) < as.numeric(f$value),
          "ge" = as.numeric(col_data) >= as.numeric(f$value),
          "le" = as.numeric(col_data) <= as.numeric(f$value),
          "contains" = grepl(f$value, as.character(col_data), fixed = TRUE),
          "starts" = grepl(paste0("^", f$value), as.character(col_data)),
          "ends" = grepl(paste0(f$value, "$"), as.character(col_data)),
          "is_na" = is.na(col_data),
          "not_na" = !is.na(col_data),
          rep(TRUE, nrow(filtered))
        )
        
        # Handle NA in comparisons
        keep_rows[is.na(keep_rows)] <- FALSE
        
        filtered <- filtered[keep_rows, , drop = FALSE]
      }
      
      working_data(filtered)
      showNotification(
        sprintf("Filtered: %d rows remaining (from %d)", nrow(filtered), nrow(df)),
        type = "message"
      )
    })
    
    # Reset to original data
    observeEvent(input$filter_reset, {
      df <- original_data()
      req(df)
      working_data(df)
      active_filters(list())
      showNotification("Data reset to original", type = "message")
    })
    
    # Subset status display
    output$subset_status <- renderText({
      orig <- original_data()
      current <- working_data()
      if (is.null(orig) || is.null(current)) return("")
      
      if (nrow(current) == nrow(orig)) {
        sprintf("Showing all %d rows", nrow(orig))
      } else {
        sprintf("Showing %d of %d rows (%.1f%% filtered)", 
                nrow(current), nrow(orig), 100 * nrow(current) / nrow(orig))
      }
    })

    # Update group ID column choices for the Group builder
    observe({
      df <- working_data(); if (is.null(df)) df <- data_reactive(); req(df)
      cols <- names(df)
      updateSelectInput(session, "group_id_col", choices = cols, selected = if (!is.null(input$group_id_col) && input$group_id_col %in% cols) input$group_id_col else cols[1])
    })

    # Dynamic ID column selector once data is available
    output$id_col_ui <- renderUI({
      df <- working_data(); if (is.null(df)) df <- data_reactive(); req(df)
      selectInput(ns("id_col"), "ID column for shape filenames", choices = names(df), selected = names(df)[1])
    })

    # shinyFiles directory picker for shape folder
    shinyfiles_ready <- reactiveVal(FALSE)
    observe({
      ready <- requireNamespace("shinyFiles", quietly = TRUE)
      if (!isTRUE(ready)) {
        try(install.packages("shinyFiles", repos = "https://cran.r-project.org", quiet = TRUE), silent = TRUE)
        ready <- requireNamespace("shinyFiles", quietly = TRUE)
      }
      shinyfiles_ready(isTRUE(ready))
    })
    output$shape_dir_ui <- renderUI({
      if (!isTRUE(shinyfiles_ready())) return(helpText("Directory picker unavailable"))
      shinyFiles::shinyDirButton(ns("shape_dir"), label = "Choose shape folder", title = "Select shape folder")
    })
    shape_dir_path <- reactiveVal("")
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
      shinyFiles::shinyDirChoose(input, id = "shape_dir", roots = roots, session = session)
    })
    observeEvent(input$shape_dir, {
      req(shinyfiles_ready())
      roots <- try(shinyFiles::getVolumes()(), silent = TRUE)
      if (inherits(roots, "try-error") || is.null(roots) || length(roots) == 0) {
        roots <- c()
      }
      if (.Platform$OS.type == "windows" && dir.exists("C:/")) {
        roots <- c(`C:` = "C:/", roots)
      }
      roots <- c(roots, Home = normalizePath("~"), `Working Dir` = normalizePath(getwd()))
      sel <- try(shinyFiles::parseDirPath(roots, input$shape_dir), silent = TRUE)
      if (!inherits(sel, "try-error") && length(sel) > 0) shape_dir_path(as.character(sel))
    })
    output$shape_dir_txt <- renderText({
      p <- shape_dir_path()
      if (is.null(p) || !nzchar(p)) "No folder selected" else p
    })

    # Save edited data back to Excel (user chooses target path)
    output$save_excel_ui <- renderUI({
      if (!isTRUE(shinyfiles_ready())) return(helpText("Save picker unavailable"))
      shinyFiles::shinySaveButton(ns("save_excel_btn"), label = "Save edited Excel", title = "Choose or overwrite .xlsx", filetype = list(xlsx = "xlsx"))
    })
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
      # Configure save dialog (omit defaultFile for compatibility with older shinyFiles)
      shinyFiles::shinyFileSave(input, id = "save_excel_btn", roots = roots, session = session)
    })
    observeEvent(input$save_excel_btn, {
      if (!isTRUE(shinyfiles_ready())) return()
      roots <- try(shinyFiles::getVolumes()(), silent = TRUE)
      if (inherits(roots, "try-error") || is.null(roots) || length(roots) == 0) {
        roots <- c()
      }
      if (.Platform$OS.type == "windows" && dir.exists("C:/")) {
        roots <- c(`C:` = "C:/", roots)
      }
      roots <- c(roots, Home = normalizePath("~"), `Working Dir` = normalizePath(getwd()))
      save_path_df <- try(shinyFiles::parseSavePath(roots, input$save_excel_btn), silent = TRUE)
      if (inherits(save_path_df, "try-error") || is.null(save_path_df) || nrow(save_path_df) == 0) {
        output$save_status <- renderText("Save cancelled or invalid path."); return()
      }
      path <- as.character(save_path_df$datapath[1])
      if (!grepl("\\.xlsx$", path, ignore.case = TRUE)) path <- paste0(path, ".xlsx")

      # Prepare data for Excel (drop list columns like 'shape')
      df <- working_data(); if (is.null(df)) df <- effective_data(); req(df)
      out <- df
      if ("shape" %in% names(out) && is.list(out$shape)) out$shape <- NULL

      # Ensure openxlsx is available
      if (!requireNamespace("openxlsx", quietly = TRUE)) {
        output$save_status <- renderText("Package 'openxlsx' is required. Please install it."); return()
      }
      # Attempt overwrite
      ok <- try({
        if (file.exists(path)) {
          # Best-effort overwrite handling
          try(unlink(path), silent = TRUE)
        }
        openxlsx::write.xlsx(out, path)
      }, silent = TRUE)
      if (inherits(ok, "try-error")) {
        output$save_status <- renderText(paste0("Failed to save: ", conditionMessage(attr(ok, "condition"))))
      } else {
        output$save_status <- renderText(paste0("Saved: ", path))
      }
    })

    # Hold mapped data and summary
    mapped_data <- reactiveVal(NULL)
    mapping_summary <- reactiveVal(NULL)

    observeEvent(input$map_shapes_btn, {
      df <- working_data(); if (is.null(df)) df <- data_reactive(); req(df)
      id_col <- input$id_col; req(id_col)
      shp_dir <- shape_dir_path(); req(nzchar(shp_dir))

      # Fixed options (simple defaults); can be adjusted here if needed
      opts <- list(
        recursive = FALSE,
        case_sensitive = FALSE,
        fail_on_missing = FALSE,
        validate_shapes = TRUE
      )

      result <- tryCatch({
        map_shapes_to_data(
          data = df,
          id_col = id_col,
          shape_folder = shp_dir,
          shape_col = "shape",
          options = opts,
          verbose = TRUE
        )
      }, error = function(e) {
        showNotification(paste("Shape mapping failed:", conditionMessage(e)), type = "error")
        NULL
      })

      if (!is.null(result)) {
        mapped_data(result)
        mapping_summary(attr(result, "mapping_summary"))
        # Update working_data with mapped result (includes 'shape' column)
        working_data(result)
        showNotification("Shape mapping completed", type = "message")
      }
    })

    output$mapping_summary <- renderText({
      s <- mapping_summary(); if (is.null(s)) return("")
      paste0(
        "Rows: ", s$total_rows,
        " | Files found: ", s$files_found,
        " | Successful: ", s$successful_mappings,
        " | Failed imports: ", s$failed_imports,
        " | Missing: ", s$missing_shapes,
        " | Rate: ", s$mapping_rate, "%"
      )
    })

    # Effective data to use downstream: prefer mapped data when available
    effective_data <- reactive({
      df <- working_data()
      if (!is.null(df)) return(df)
      md <- mapped_data()
      if (!is.null(md)) return(md)
      data_reactive()
    })

    # DataTable proxy for efficient updates after edits
    proxy <- DT::dataTableProxy("preview", session = session)

    # Render preview table
    output$preview <- DT::renderDT({
      df <- working_data(); if (is.null(df)) df <- effective_data(); req(nrow(df) > 0)
      # Display-friendly copy for list columns like 'shape'
      display_df <- df
      if ("shape" %in% names(display_df) && is.list(display_df[["shape"]])) {
        display_df$shape <- vapply(display_df$shape, function(x) if (is.null(x)) "NULL" else "Out", character(1))
      }

      page_len <- if (!is.null(input$n_rows)) input$n_rows else 20
      DT::datatable(
        display_df,
        options = list(pageLength = min(50, page_len), scrollX = TRUE, ordering = TRUE),
        rownames = FALSE,
        filter = "top",
        selection = "none",
        editable = FALSE
      )
    })

    # Group builder rules handling ------------------------------------------
    group_rules_n <- reactiveVal(1L)
    observeEvent(input$group_add_rule, { group_rules_n(group_rules_n() + 1L) })
    observeEvent(input$group_remove_rule, { group_rules_n(max(1L, group_rules_n() - 1L)) })

    output$group_rules_ui <- renderUI({
      df <- working_data(); if (is.null(df)) df <- data_reactive(); req(df)
      id_col <- input$group_id_col
      choices <- if (!is.null(id_col) && id_col %in% names(df)) unique(df[[id_col]]) else character()
      # Sort choices if requested
      if (isTRUE(input$group_sort_ids) && length(choices)) {
        choices <- tryCatch({
          if (inherits(choices, c("Date","POSIXct","POSIXt")) || is.numeric(choices)) sort(choices) else sort(as.character(choices))
        }, error = function(...) sort(as.character(choices)))
      }
      n <- group_rules_n()
      method <- input$group_method %||% "range"
      ui_list <- vector("list", n)
      for (i in seq_len(n)) {
        if (identical(method, "list")) {
          ui_list[[i]] <- wellPanel(
            fluidRow(
              column(4, textInput(ns(paste0("g_label_", i)), paste0("Rule ", i, ": Group label"), value = "")),
              column(8, selectizeInput(ns(paste0("g_ids_", i)), paste0("IDs for group ", i), choices = choices, multiple = TRUE))
            )
          )
        } else {
          ui_list[[i]] <- wellPanel(
            fluidRow(
              column(4, textInput(ns(paste0("g_label_", i)), paste0("Rule ", i, ": Group label"), value = "")),
              column(4, selectInput(ns(paste0("g_start_", i)), paste0("Start ID (", i, ")"), choices = choices)),
              column(4, selectInput(ns(paste0("g_end_", i)), paste0("End ID (", i, ")"), choices = choices))
            )
          )
        }
      }
      do.call(tagList, ui_list)
    })

    observeEvent(input$group_apply, {
      df <- working_data(); if (is.null(df)) df <- data_reactive(); req(df)
      id_col <- input$group_id_col
      if (is.null(id_col) || !nzchar(id_col) || !id_col %in% names(df)) {
        output$group_status <- renderText("Please select a valid ID column."); return()
      }
      col_name <- input$group_target_col %||% "group"
      method <- input$group_method %||% "range"
      # Prepare target column with default fill
      default_val <- input$group_default
      df[[col_name]] <- rep_len(if (is.null(default_val)) NA_character_ else as.character(default_val), nrow(df))

      # Build ID ordering for ranges
      uniq_vals <- unique(df[[id_col]])
      if (isTRUE(input$group_sort_ids)) {
        uniq_vals <- tryCatch({
          if (inherits(uniq_vals, c("Date","POSIXct","POSIXt")) || is.numeric(uniq_vals)) sort(uniq_vals) else sort(as.character(uniq_vals))
        }, error = function(...) sort(as.character(uniq_vals)))
      }
      # Map value to order index
      ord_map <- seq_along(uniq_vals); names(ord_map) <- as.character(uniq_vals)

      applied <- 0L
      n <- group_rules_n()
      for (i in seq_len(n)) {
        label <- input[[paste0("g_label_", i)]]
        if (is.null(label) || !nzchar(label)) next
        if (identical(method, "list")) {
          ids <- input[[paste0("g_ids_", i)]]
          if (is.null(ids) || length(ids) == 0) next
          idx <- which(as.character(df[[id_col]]) %in% as.character(ids))
          if (length(idx)) {
            df[[col_name]][idx] <- as.character(label)
            applied <- applied + length(idx)
          }
        } else {
          s <- input[[paste0("g_start_", i)]]; e <- input[[paste0("g_end_", i)]]
          if (is.null(s) || is.null(e) || !nzchar(s) || !nzchar(e)) next
          ps <- suppressWarnings(as.integer(ord_map[as.character(s)]))
          pe <- suppressWarnings(as.integer(ord_map[as.character(e)]))
          if (is.na(ps) || is.na(pe)) next
          lo <- min(ps, pe); hi <- max(ps, pe)
          range_vals <- uniq_vals[lo:hi]
          idx <- which(as.character(df[[id_col]]) %in% as.character(range_vals))
          if (length(idx)) {
            df[[col_name]][idx] <- as.character(label)
            applied <- applied + length(idx)
          }
        }
      }
      working_data(df)
      # Refresh table via proxy to avoid jump
      display_df <- df
      if ("shape" %in% names(display_df) && is.list(display_df$shape)) {
        display_df$shape <- vapply(display_df$shape, function(x) if (is.null(x)) "NULL" else "Out", character(1))
      }
      DT::replaceData(proxy, display_df, resetPaging = FALSE, rownames = FALSE)
      output$group_status <- renderText(paste0("Applied labels to ", applied, " rows in column '", col_name, "'."))
    })


    # Export removed: edits apply in-place to working_data and propagate downstream

    # Return a list of reactives for downstream use
    return(list(
      data = effective_data
    ))
  })
}
