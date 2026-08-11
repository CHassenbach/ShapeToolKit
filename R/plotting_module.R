# Suppress R CMD check notes for ggplot2 aes() column names
utils::globalVariables(c("x", "y", "certainty", "group", "elevation", "z"))

#' Plotting Module (shape_plot)
#'
#' UI and server to configure and render plots using `shape_plot()`.
#' Exposes the main parameters and consumes data from the Data Import module.
#'
#' @param id Module id
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
          checkboxInput(ns("mode_3d"), "3D Mode", value = FALSE),
          conditionalPanel(
            condition = sprintf("input['%s'] == true", ns("mode_3d")),
            selectInput(ns("z_col"), "Z column", choices = c("(none)" = ""))
          ),
          selectInput(ns("group_col"), "Group column (optional)", choices = c("(none)" = "")),
          uiOutput(ns("group_vals_ui")),
          selectInput(ns("gradient_col"), "Gradient color column (optional)", choices = c("(none)" = "")),
          shiny::tags$small(shiny::tags$em("Color points by a continuous numeric column using a gradient. Overrides group coloring when selected."))
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
          conditionalPanel(
            condition = sprintf("input['%s'] !== ''", ns("gradient_col")),
            shiny::tags$hr(),
            shiny::tags$strong("Gradient Coloring"),
            shiny::tags$br(), shiny::tags$br(),
            colourpicker::colourInput(ns("gradient_low"), "Gradient low color", value = "#440154"),
            checkboxInput(ns("gradient_use_mid"), "Use midpoint color", value = FALSE),
            conditionalPanel(
              condition = sprintf("input['%s'] == true", ns("gradient_use_mid")),
              colourpicker::colourInput(ns("gradient_mid"), "Gradient mid color", value = "#21908C")
            ),
            colourpicker::colourInput(ns("gradient_high"), "Gradient high color", value = "#FDE725"),
            textInput(ns("gradient_legend_title"), "Gradient legend title", value = "", placeholder = "Default: column name")
          ),
          numericInput(ns("title_size"), "Title size", value = 24, min = 6, step = 1),
          numericInput(ns("label_size"), "Axis label size", value = 20, min = 6, step = 1),
          numericInput(ns("tick_size"), "Tick label size", value = 15, min = 6, step = 1),
          numericInput(ns("legend_size"), "Legend font size", value = 13, min = 6, step = 1),
          checkboxInput(ns("show_legend"), "Show legend", value = TRUE),
          numericInput(ns("legend_offset_h"), "Legend horizontal spacing (lines)", value = 0, min = 0, step = 0.5),
          shiny::sliderInput(ns("legend_offset_v"), "Legend vertical position", min = 0, max = 1, value = 0.5, step = 0.05),
          shiny::tags$small(shiny::tags$em("Vertical: 0 = bottom, 0.5 = center, 1 = top.")),
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
          title = "Features - 3D Hull",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          conditionalPanel(
            condition = sprintf("input['%s'] == true", ns("mode_3d")),
            checkboxInput(ns("hull_3d_show"), "Show 3D hull", value = FALSE),
            conditionalPanel(
              condition = sprintf("input['%s'] == true", ns("hull_3d_show")),
              uiOutput(ns("hull_3d_groups_ui")),
              selectInput(
                ns("hull_3d_type"),
                "Hull type",
                choices  = c("Convex hull" = "convex", "Alpha hull" = "alpha"),
                selected = "convex"
              ),
              conditionalPanel(
                condition = sprintf("input['%s'] == 'alpha'", ns("hull_3d_type")),
                numericInput(
                  ns("hull_3d_alpha"),
                  "Alpha radius",
                  value = 1.0,
                  min   = 0.001,
                  step  = 0.1
                ),
                helpText(HTML(paste0(
                  "<small>Smaller &alpha; &rarr; tighter surface; larger &alpha; &rarr; ",
                  "approaches convex hull. Tune to your PC axis scale. ",
                  "Requires the <code>alphashape3d</code> package.</small>"
                )))
              ),
              checkboxInput(ns("hull_3d_fill"), "Fill hull faces", value = TRUE),
              checkboxInput(ns("hull_3d_wire"), "Show wireframe edges", value = FALSE),
              shiny::sliderInput(
                ns("hull_3d_opacity"),
                "Hull face opacity",
                min = 0, max = 1, value = 0.3, step = 0.05
              ),
              uiOutput(ns("hull_3d_group_color_pickers"))
            )
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == false", ns("mode_3d")),
            helpText("Enable 3D Mode in Data Mapping to configure 3D hull options.")
          )
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
          checkboxInput(ns("shapes_combined_hull"), "Use combined hull across all selected groups", value = FALSE),
          shiny::tags$small(shiny::tags$em("When enabled, computes one hull across all selected groups and places shapes only at those boundary points. Shift direction uses the combined centroid.")),
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
                "Both" = "both",
                "Topographic Map" = "topographic",
                "3D Surface" = "surface_3d"
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
            # Heatmap / Both controls
            conditionalPanel(
              condition = sprintf("input['%s'] !== 'topographic'", ns("gap_display_mode")),
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
              )
            ),
            # Topographic map controls
            conditionalPanel(
              condition = sprintf("input['%s'] === 'topographic'", ns("gap_display_mode")),
              selectInput(
                ns("topo_palette"),
                "Topographic Palette",
                choices = c(
                  "Terrain" = "terrain",
                  "Viridis" = "viridis",
                  "Plasma" = "plasma",
                  "Grayscale" = "gray"
                ),
                selected = "terrain"
              ),
              checkboxInput(
                ns("topo_show_contours"),
                "Show contour lines",
                value = TRUE
              ),
              numericInput(
                ns("topo_n_breaks"),
                "Number of contour levels",
                value = 8,
                min = 2,
                max = 30,
                step = 1
              ),
              colourpicker::colourInput(
                ns("topo_contour_color"),
                "Contour line color",
                value = "#333333"
              ),
              numericInput(
                ns("topo_contour_width"),
                "Contour line width",
                value = 0.4,
                min = 0.1,
                max = 3,
                step = 0.1
              )
            ),
            uiOutput(ns("gap_status_ui"))
          )
        ),
        box(
          title = "Features - Gap Comparison",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          checkboxInput(ns("gap_compare_show"), "Compare two gap analyses", value = FALSE),
          helpText("Load two gap analysis RDS files to overlay and highlight morphospace differences between groups."),
          conditionalPanel(
            condition = sprintf("input['%s'] == true", ns("gap_compare_show")),
            uiOutput(ns("gap_compare_files_ui")),
            hr(),
            selectInput(
              ns("gap_compare_mode"),
              "Comparison Mode",
              choices = c(
                "Difference Heatmap (2 files: A \u2212 B)" = "difference",
                "Inverted Overlay (warm = A gaps, cool = B gaps)" = "overlay"
              ),
              selected = "difference"
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] === 'difference'", ns("gap_compare_mode")),
              helpText(HTML("<small>Difference = File A certainty \u2212 File B certainty. Shared gaps cancel out to white.</small>")),
              colourpicker::colourInput(ns("gap_compare_diff_high_a"), "Group A unique gaps color (high end)", value = "#D6604D"),
              colourpicker::colourInput(ns("gap_compare_diff_mid"),    "No difference color (midpoint)",        value = "#FFFFFF"),
              colourpicker::colourInput(ns("gap_compare_diff_high_b"), "Group B unique gaps color (high end)", value = "#2166AC")
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] === 'overlay'", ns("gap_compare_mode")),
              helpText(HTML("<small>Group A uses these colors. Group B uses their pixel-inverse (like Ctrl+I in Photoshop). Shared gaps cancel toward gray; unique gaps keep their color.</small>")),
              colourpicker::colourInput(ns("gap_compare_ovl_low"),  "Low certainty color",  value = "#FFFFFF"),
              colourpicker::colourInput(ns("gap_compare_ovl_mid"),  "Mid certainty color",  value = "#FFFF00"),
              colourpicker::colourInput(ns("gap_compare_ovl_high"), "High certainty color", value = "#FF0000")
            ),
            checkboxInput(
              ns("gap_compare_auto_pc"),
              "Auto-detect PC pair from plot axes",
              value = TRUE
            ),
            conditionalPanel(
              condition = sprintf("!input['%s']", ns("gap_compare_auto_pc")),
              uiOutput(ns("gap_compare_pc_pair_ui"))
            ),
            numericInput(
              ns("gap_compare_alpha"),
              "Overlay Transparency",
              value = 0.6,
              min   = 0,
              max   = 1,
              step  = 0.1
            ),
            uiOutput(ns("gap_compare_status_ui"))
          )
        ),
        box(
          title = "Features - 3D Gap Surfaces",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          conditionalPanel(
            condition = sprintf("input['%s'] == true", ns("mode_3d")),
            checkboxInput(ns("gap_3d_show"), "Show gap surfaces on background planes", value = FALSE),
            helpText("Projects gap certainty from loaded gap results onto the three background planes of the 3D plot. Uses the same gap results file as the 2D Gap Overlay."),
            conditionalPanel(
              condition = sprintf("input['%s'] == true", ns("gap_3d_show")),
              uiOutput(ns("gap_3d_threshold_selector")),
              shiny::sliderInput(
                ns("gap_3d_alpha"),
                "Surface opacity",
                min = 0, max = 1, value = 0.5, step = 0.05
              ),
              colourpicker::colourInput(
                ns("gap_3d_low_color"),
                "Low Certainty Color",
                value = "#FFFFFF"
              ),
              colourpicker::colourInput(
                ns("gap_3d_mid_color"),
                "Mid Certainty Color",
                value = "#FFFF00"
              ),
              colourpicker::colourInput(
                ns("gap_3d_high_color"),
                "High Certainty Color",
                value = "#FF0000"
              ),
              checkboxInput(
                ns("gap_3d_mask_below"),
                "Hide regions below threshold",
                value = TRUE
              ),
              uiOutput(ns("gap_3d_status_ui"))
            )
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == false", ns("mode_3d")),
            helpText("Enable 3D Mode in Data Mapping to configure 3D gap surface options.")
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
          shiny::tags$small(shiny::tags$em("Use \\n to split the label onto two lines (e.g. PC1\\n(50%))")),
          textInput(ns("y_label"), "Y axis label", value = NULL, placeholder = "Default: Y column name"),
          shiny::tags$small(shiny::tags$em("Use \\n to split the label onto two lines (e.g. PC2\\n(30%))")),
          textInput(ns("x_adjust"), "X label adjust (x,y)", value = "0,0"),
          textInput(ns("y_adjust"), "Y label adjust (x,y)", value = "0,0"),
          numericInput(ns("x_size"), "X label size", value = 5, min = 1, step = 0.5),
          numericInput(ns("y_size"), "Y label size", value = 5, min = 1, step = 0.5),
          checkboxInput(ns("rotate_y"), "Rotate Y label", value = FALSE),
          checkboxInput(ns("show_borders"), "Show borders", value = TRUE),
          numericInput(ns("plot_margin_top"), "Plot top padding (lines)", value = 1.5, min = 0, step = 0.5),
          numericInput(ns("preview_height"), "Preview height (px)", value = 600, min = 200, step = 50)
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
              tags$li("In the Plots pane, click", tags$strong("Export"), "-> choose your format"),
              tags$li("Available formats: SVG (editable!), PNG, TIFF, PDF, EPS")
            ),
            tags$p(style = "margin-top: 8px; margin-bottom: 0; font-style: italic;",
                  "Tip: SVG exports from RStudio preserve individual plot elements for editing in vector graphics software!")
          ),
          downloadButton(ns("download_plot"), "Download ggplot (.rds)", class = "btn-primary")
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == true", ns("mode_3d")),
          box(
            title = "3D Rotation Video",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            helpText("Rotate the 3D plot through a full 360° and export as an MP4 video. Requires the 'av' package and plotly's kaleido/orca renderer."),
            selectInput(
              ns("rotation_axis"),
              "Rotation axis",
              choices = c("Azimuth (horizontal)"           = "azimuth",
                          "Elevation (vertical)"            = "elevation",
                          "Both (azimuth then elevation)"   = "both"),
              selected = "azimuth"
            ),
            numericInput(ns("rotation_frames"), "Frames per axis", value = 72, min = 12, max = 360, step = 12),
            helpText("In 'Both' mode the total frame count is doubled (one full rotation per axis)."),
            numericInput(ns("rotation_fps"),    "Frames per second", value = 24, min  =  6, max = 60,  step = 1),
            numericInput(ns("rotation_width"),  "Frame width (px)",  value = 800, min = 400, max = 3840, step = 100),
            numericInput(ns("rotation_height"), "Frame height (px)", value = 600, min = 300, max = 2160, step = 100),
            numericInput(
              ns("rotation_eye_r"),
              "Camera distance (eye radius)",
              value = 2.5, min = 0.5, max = 10, step = 0.1
            ),
            textInput(
              ns("rotation_filename"),
              "Output filename (without extension)",
              value = "rotation_video",
              placeholder = "rotation_video"
            ),
            shinyFiles::shinyDirButton(
              ns("rotation_out_dir_btn"),
              label = "Choose output folder",
              title = "Select folder for frames and video"
            ),
            br(), br(),
            strong("Output folder: "),
            textOutput(ns("rotation_out_dir_display"), inline = TRUE),
            helpText("Frames are saved to a sub-folder inside the output folder. The video is saved alongside it."),
            tags$br(),
            actionButton(ns("generate_rotation_video"), "Generate Video", class = "btn-warning", icon = icon("film")),
            tags$br(), tags$br(),
            uiOutput(ns("rotation_video_status"))
          )
        ),
        box(
          title = "Interactive Mode",
          status = "warning",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          checkboxInput(ns("interactive_mode"), "Enable Interactive Mode", value = FALSE),
          helpText("Interactive mode requires plotly. Click on points to see IDs, click on the morphospace to see reconstructed shapes."),
          conditionalPanel(
            condition = sprintf("input['%s'] == true", ns("interactive_mode")),
            helpText("Tip: PCA model will be auto-loaded if reconstruction CSV files are found alongside your data."),
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
            helpText("Click on the plot to see shapes. Points show actual data, clicking empty space shows reconstructed hypothetical shapes."),
            plotOutput(ns("shape_preview"), height = "auto"),
            verbatimTextOutput(ns("hover_info"))
          )
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == false && input['%s'] !== 'surface_3d' && input['%s'] == false", ns("interactive_mode"), ns("gap_display_mode"), ns("mode_3d")),
          box(
            title = "Plot",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            uiOutput(ns("plot_ui")),
            br(),
            verbatimTextOutput(ns("messages"))
          )
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == false && input['%s'] == true", ns("interactive_mode"), ns("mode_3d")),
          box(
            title = "3D Scatter Plot",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            plotly::plotlyOutput(ns("plot_3d"), height = "600px"),
            br(),
            verbatimTextOutput(ns("messages"))
          )
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == false && input['%s'] === 'surface_3d'", ns("interactive_mode"), ns("gap_display_mode")),
          box(
            title = "Topographic 3D Surface",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            helpText("3D surface of gap elevation (mountains = occupied morphospace, valleys = gaps)."),
            plotly::plotlyOutput(ns("topo_3d_plot"), height = "600px"),
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

#' Plotting Module Server
#'
#' Server counterpart for the Plotting Module.
#'
#' @param id Module id
#' @param data_reactive A reactive function returning a data.frame from Data Import
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

    # Reactive value for gap comparison (fixed at 2 files)
    gap_compare_results_list <- reactiveVal(list())
    
    # Auto-install av, webshot2 and chromote for rotation video export
    rotation_deps_ready <- reactiveVal(FALSE)
    observe({
      pkgs <- c("av", "webshot2", "chromote", "htmlwidgets")
      missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
      if (length(missing) > 0) {
        try(install.packages(missing, repos = "https://cran.r-project.org", quiet = TRUE), silent = TRUE)
      }
      rotation_deps_ready(all(vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)))
    })

    rotation_out_dir <- reactiveVal(normalizePath(getwd(), winslash = "/"))

    observeEvent(shinyfiles_ready(), {
      if (!isTRUE(shinyfiles_ready())) return()
      roots <- try(shinyFiles::getVolumes()(), silent = TRUE)
      if (inherits(roots, "try-error") || is.null(roots) || length(roots) == 0) roots <- c()
      if (.Platform$OS.type == "windows" && dir.exists("C:/")) roots <- c(`C:` = "C:/", roots)
      roots <- c(roots, Home = normalizePath("~"), `Working Dir` = normalizePath(getwd()))
      shinyFiles::shinyDirChoose(input, id = "rotation_out_dir_btn", roots = roots, session = session)
    })

    observeEvent(input$rotation_out_dir_btn, {
      req(shinyfiles_ready())
      roots <- try(shinyFiles::getVolumes()(), silent = TRUE)
      if (inherits(roots, "try-error") || is.null(roots) || length(roots) == 0) roots <- c()
      if (.Platform$OS.type == "windows" && dir.exists("C:/")) roots <- c(`C:` = "C:/", roots)
      roots <- c(roots, Home = normalizePath("~"), `Working Dir` = normalizePath(getwd()))
      sel <- try(shinyFiles::parseDirPath(roots, input$rotation_out_dir_btn), silent = TRUE)
      if (!inherits(sel, "try-error") && length(sel) > 0 && nzchar(sel)) {
        rotation_out_dir(normalizePath(as.character(sel), winslash = "/"))
      }
    })

    output$rotation_out_dir_display <- renderText({
      rotation_out_dir()
    })

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
          tags$strong("Warning: No PCA model loaded"),
          tags$p("Reconstruction CSV files not found. Interactive mode will show IDs only.", style = "margin: 5px 0 0 0;")
        )
      } else {
        tags$div(
          style = "padding: 10px; background-color: #d4edda; border: 1px solid #28a745; border-radius: 4px;",
          tags$strong("PCA model loaded"),
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
          tags$strong("Warning: No gap results loaded"),
          tags$p("Please select a gap detection results file (.rds)", style = "margin: 5px 0 0 0;")
        )
      } else {
        tags$div(
          style = "padding: 10px; background-color: #d4edda; border: 1px solid #28a745; border-radius: 4px; margin-top: 10px;",
          tags$strong("Gap results loaded"),
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

    # Gap 3D threshold selector
    output$gap_3d_threshold_selector <- renderUI({
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
        ns("gap_3d_threshold"),
        threshold_label,
        choices = thresholds,
        selected = thresholds[length(thresholds)]
      )
    })

    # Gap 3D status UI
    output$gap_3d_status_ui <- renderUI({
      results <- gap_results()

      if (is.null(results)) {
        tags$div(
          style = "padding: 10px; background-color: #fff3cd; border: 1px solid #ffc107; border-radius: 4px; margin-top: 10px;",
          tags$strong("No gap results loaded"),
          tags$p("Load a gap results file using the Gap Overlay box above.", style = "margin: 5px 0 0 0;")
        )
      } else {
        x_col <- input$x_col %||% ""
        y_col <- input$y_col %||% ""
        z_col <- input$z_col %||% ""
        available_pairs <- names(results$results)

        matched <- character(0)
        if (grepl("^PC[0-9]+$", x_col) && grepl("^PC[0-9]+$", y_col) && grepl("^PC[0-9]+$", z_col)) {
          xn <- as.integer(gsub("PC", "", x_col))
          yn <- as.integer(gsub("PC", "", y_col))
          zn <- as.integer(gsub("PC", "", z_col))
          for (pair in list(c(xn, yn), c(xn, zn), c(yn, zn))) {
            k1 <- sprintf("PC%d-PC%d", pair[1], pair[2])
            k2 <- sprintf("PC%d-PC%d", pair[2], pair[1])
            key <- if (k1 %in% available_pairs) k1 else if (k2 %in% available_pairs) k2 else NULL
            if (!is.null(key)) matched <- c(matched, key)
          }
        }

        if (length(matched) > 0) {
          tags$div(
            style = "padding: 10px; background-color: #d4edda; border: 1px solid #28a745; border-radius: 4px; margin-top: 10px;",
            tags$strong("Planes to be projected:"),
            tags$ul(style = "margin: 5px 0 0 0;",
              lapply(matched, tags$li)
            )
          )
        } else {
          tags$div(
            style = "padding: 10px; background-color: #fff3cd; border: 1px solid #ffc107; border-radius: 4px; margin-top: 10px;",
            tags$strong("No matching PC pairs"),
            tags$p(paste("Available in gap results:", paste(available_pairs, collapse = ", ")),
                   style = "margin: 5px 0 0 0;")
          )
        }
      }
    })

    # ── Gap Comparison: two fixed file slots ────────────────────────────────────
    output$gap_compare_files_ui <- renderUI({
      tagList(
        fluidRow(
          column(width = 4, textInput(ns("gap_compare_label_1"), "Label A", value = "Group A")),
          column(width = 8, fileInput(ns("gap_compare_file_1"), "Gap Results File A (.rds)", accept = ".rds"))
        ),
        fluidRow(
          column(width = 4, textInput(ns("gap_compare_label_2"), "Label B", value = "Group B")),
          column(width = 8, fileInput(ns("gap_compare_file_2"), "Gap Results File B (.rds)", accept = ".rds"))
        )
      )
    })

    # Load each comparison file slot (slots 1-2)
    for (.cmp_i in 1:2) {
      local({
        ii <- .cmp_i
        observeEvent(input[[paste0("gap_compare_file_", ii)]], {
          req(input[[paste0("gap_compare_file_", ii)]])
          file_path <- input[[paste0("gap_compare_file_", ii)]]$datapath
          tryCatch({
            res <- readRDS(file_path)
            if (!inherits(res, "morphospace_gaps")) {
              showNotification(
                paste0("File ", ii, ": Not a valid gap results object (.rds)."),
                type = "error", duration = 5
              )
              return()
            }
            lbl <- input[[paste0("gap_compare_label_", ii)]] %||% paste0("Group ", LETTERS[ii])
            current <- gap_compare_results_list()
            current[[as.character(ii)]] <- list(results = res, label = lbl, slot = ii)
            gap_compare_results_list(current)
            showNotification(
              sprintf("Loaded '%s': %d PC pair(s), %d gap region(s).",
                      lbl, length(res$results), nrow(res$summary_table)),
              type = "message", duration = 5
            )
          }, error = function(e) {
            showNotification(
              paste0("Error loading file ", ii, ": ", conditionMessage(e)),
              type = "error", duration = 8
            )
          })
        })
      })
    }
    rm(.cmp_i)

    # Gap comparison PC pair selector (manual mode)
    output$gap_compare_pc_pair_ui <- renderUI({
      lst <- gap_compare_results_list()
      if (length(lst) == 0) return(helpText("Load at least one gap file first."))
      all_pairs <- unique(unlist(lapply(lst, function(x) names(x$results$results))))
      selectInput(ns("gap_compare_pc_pair"), "PC Pair to Compare", choices = all_pairs)
    })

    # Gap comparison status panel
    output$gap_compare_status_ui <- renderUI({
      lst <- gap_compare_results_list()
      n   <- length(lst)
      if (n == 0) {
        tags$div(
          style = "padding:8px; background-color:#fff3cd; border:1px solid #ffc107; border-radius:4px; margin-top:8px;",
          tags$strong("No files loaded yet. Select gap results files above.")
        )
      } else {
        items <- lapply(lst, function(x) {
          tags$li(sprintf("'%s': %d PC pair(s)", x$label, length(x$results$results)))
        })
        tags$div(
          style = "padding:8px; background-color:#d4edda; border:1px solid #28a745; border-radius:4px; margin-top:8px;",
          tags$strong(sprintf("%d / 2 file(s) loaded:", n)),
          do.call(tags$ul, c(list(style = "margin:4px 0 0 0;"), items)),
          if (n < 2) tags$p(style = "color:#856404; margin:4px 0 0;",
            icon("exclamation-triangle"), " Load both files to enable comparison.") else NULL
        )
      }
    })

    output$gap_3d_status_ui <- renderUI({
      results <- gap_results()

      if (is.null(results)) {
        tags$div(
          style = "padding: 10px; background-color: #fff3cd; border: 1px solid #ffc107; border-radius: 4px; margin-top: 10px;",
          tags$strong("No gap results loaded"),
          tags$p("Load a gap results file using the Gap Overlay box above.", style = "margin: 5px 0 0 0;")
        )
      } else {
        x_col <- input$x_col %||% ""
        y_col <- input$y_col %||% ""
        z_col <- input$z_col %||% ""
        available_pairs <- names(results$results)

        matched <- character(0)
        if (grepl("^PC[0-9]+$", x_col) && grepl("^PC[0-9]+$", y_col) && grepl("^PC[0-9]+$", z_col)) {
          xn <- as.integer(gsub("PC", "", x_col))
          yn <- as.integer(gsub("PC", "", y_col))
          zn <- as.integer(gsub("PC", "", z_col))
          for (pair in list(c(xn, yn), c(xn, zn), c(yn, zn))) {
            k1 <- sprintf("PC%d-PC%d", pair[1], pair[2])
            k2 <- sprintf("PC%d-PC%d", pair[2], pair[1])
            key <- if (k1 %in% available_pairs) k1 else if (k2 %in% available_pairs) k2 else NULL
            if (!is.null(key)) matched <- c(matched, key)
          }
        }

        if (length(matched) > 0) {
          tags$div(
            style = "padding: 10px; background-color: #d4edda; border: 1px solid #28a745; border-radius: 4px; margin-top: 10px;",
            tags$strong("Planes to be projected:"),
            tags$ul(style = "margin: 5px 0 0 0;",
              lapply(matched, tags$li)
            )
          )
        } else {
          tags$div(
            style = "padding: 10px; background-color: #fff3cd; border: 1px solid #ffc107; border-radius: 4px; margin-top: 10px;",
            tags$strong("No matching PC pairs"),
            tags$p(paste("Available in gap results:", paste(available_pairs, collapse = ", ")),
                   style = "margin: 5px 0 0 0;")
          )
        }
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
    output$hull_3d_groups_ui <- renderUI({
      if (!isTRUE(input$mode_3d)) return(NULL)
      if (!isTRUE(input$hull_3d_show)) return(NULL)
      df <- data_reactive(); req(df)
      gcol <- input$group_col
      if (is.null(gcol) || gcol == "" || !gcol %in% names(df)) return(NULL)
      vals <- if (!is.null(input$group_vals) && length(input$group_vals)) input$group_vals else unique(df[[gcol]])
      selectInput(ns("hull_3d_groups"), "Hull groups", choices = vals, selected = vals, multiple = TRUE)
    })

    output$hull_3d_group_color_pickers <- renderUI({
      if (!isTRUE(colourpicker_ready())) return(NULL)
      if (!isTRUE(input$mode_3d)) return(NULL)
      if (!isTRUE(input$hull_3d_show)) return(NULL)
      df <- data_reactive(); req(df)
      gcol <- input$group_col
      if (is.null(gcol) || gcol == "" || !gcol %in% names(df)) return(NULL)
      groups <- input$hull_3d_groups
      if (is.null(groups) || length(groups) == 0) return(NULL)
      # Default palette mirrors the point color palette
      pal <- tryCatch({
        if (requireNamespace("scales", quietly = TRUE)) scales::hue_pal()(length(groups)) else rep("#1f77b4", length(groups))
      }, error = function(...) rep("#1f77b4", length(groups)))
      safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", as.character(x))
      picker_list <- mapply(function(g, default_col) {
        colourpicker::colourInput(
          ns(paste0("hull_3d_color_", safe_id(g))),
          paste0("Hull color: ", g),
          value = default_col
        )
      }, groups, pal, SIMPLIFY = FALSE)
      do.call(tagList, picker_list)
    })

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
      updateSelectInput(session, "z_col", choices = c("(none)" = "", cols),
        selected = if (!is.null(input$z_col) && input$z_col %in% cols) input$z_col else cols[min(3, length(cols))])
      updateSelectInput(session, "group_col", choices = c("(none)" = "", cols), selected = if (!is.null(input$group_col) && input$group_col %in% cols) input$group_col else "")
      updateSelectInput(session, "gradient_col", choices = c("(none)" = "", cols), selected = if (!is.null(input$gradient_col) && input$gradient_col %in% cols) input$gradient_col else "")
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

      # Resolve Z column for 3D mode
      z_col_val <- if (isTRUE(input$mode_3d)) {
        zc <- input$z_col
        if (!is.null(zc) && nzchar(zc) && zc %in% names(df)) zc else NULL
      } else NULL
      if (isTRUE(input$mode_3d) && is.null(z_col_val)) {
        showNotification("3D Mode is on but no valid Z column selected.", type = "warning")
        return()
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
          tick_size = input$tick_size,
          legend_size = input$legend_size
        ),
        show_legend = isTRUE(input$show_legend),
        legend_offset_h = input$legend_offset_h %||% 0,
        legend_offset_v = input$legend_offset_v %||% 0.5,
        plot_margin_top = input$plot_margin_top %||% 1.5,
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
        hulls_3d = {
          # Collect per-group colors for 3D hull
          hull_3d_color_by_group <- NULL
          if (isTRUE(colourpicker_ready()) && !is.null(gcol) && nzchar(gcol)) {
            h3d_groups <- if (!is.null(gvals) && length(gvals)) gvals else unique(df[[gcol]])
            safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", as.character(x))
            cols3d <- vapply(h3d_groups, function(g) {
              val <- input[[paste0("hull_3d_color_", safe_id(g))]]
              if (is.null(val) || !nzchar(val)) NA_character_ else val
            }, character(1))
            names(cols3d) <- as.character(h3d_groups)
            if (any(!is.na(cols3d))) hull_3d_color_by_group <- cols3d[!is.na(cols3d)]
          }
          list(
            show        = isTRUE(input$hull_3d_show),
            groups      = input$hull_3d_groups,
            fill        = if (is.null(input$hull_3d_fill)) TRUE else isTRUE(input$hull_3d_fill),
            wireframe   = isTRUE(input$hull_3d_wire),
            opacity     = if (is.null(input$hull_3d_opacity)) 0.3 else as.numeric(input$hull_3d_opacity),
            colors      = hull_3d_color_by_group,
            hull_type   = if (!is.null(input$hull_3d_type)) input$hull_3d_type else "convex",
            alpha_value = if (!is.null(input$hull_3d_alpha)) as.numeric(input$hull_3d_alpha) else 1.0
          )
        },
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
          combined_hull = isTRUE(input$shapes_combined_hull),
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
        x_label = if (nzchar(input$x_label)) gsub("\\\\n", "\n", input$x_label) else x_col,
        y_label = if (nzchar(input$y_label)) gsub("\\\\n", "\n", input$y_label) else y_col,
        x_adjust = x_adj,
        y_adjust = y_adj,
        x_size = input$x_size,
        y_size = input$y_size,
        rotate_y = isTRUE(input$rotate_y),
        show_borders = isTRUE(input$show_borders)
      )

      # Collect gradient coloring inputs
      grad_col <- input$gradient_col
      gradient_params <- NULL
      if (!is.null(grad_col) && nzchar(grad_col) && grad_col %in% names(df)) {
        gradient_params <- list(
          col = grad_col,
          low  = input$gradient_low  %||% "#440154",
          mid  = if (isTRUE(input$gradient_use_mid)) input$gradient_mid %||% "#21908C" else NULL,
          high = input$gradient_high %||% "#FDE725",
          legend_title = if (nzchar(input$gradient_legend_title %||% "")) input$gradient_legend_title else grad_col
        )
      }

      # Call shape_plot (disable internal export, we use downloadHandler)
      messages("")
      p <- tryCatch({
        shape_plot(
          data = df,
          x_col = x_col,
          y_col = y_col,
          z_col = z_col_val,
          group_col = if (nzchar(gcol)) gcol else NULL,
          group_vals = gvals,
          styling = styling,
          features = features,
          labels = labels,
          export_options = list(export = FALSE),
          interactive = isTRUE(input$interactive_mode),
          pca_model = if (isTRUE(input$interactive_mode)) pca_model() else NULL,
          gradient = gradient_params,
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
                polygon_width = input$gap_polygon_width,
                topo_palette = input$topo_palette,
                topo_show_contours = isTRUE(input$topo_show_contours),
                topo_n_breaks = input$topo_n_breaks,
                topo_contour_color = input$topo_contour_color,
                topo_contour_width = input$topo_contour_width
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

      # Add gap comparison overlay if enabled
      if (!is.null(p) && isTRUE(input$gap_compare_show)) {
        lst <- gap_compare_results_list()
        if (length(lst) >= 2) {
          # Resolve PC pair
          cmp_pair <- NULL
          if (isTRUE(input$gap_compare_auto_pc)) {
            if (grepl("^PC[0-9]+$", x_col) && grepl("^PC[0-9]+$", y_col)) {
              xn <- as.integer(gsub("PC", "", x_col))
              yn <- as.integer(gsub("PC", "", y_col))
              all_pairs <- unique(unlist(lapply(lst, function(x) names(x$results$results))))
              cands <- c(sprintf("PC%d-PC%d", xn, yn), sprintf("PC%d-PC%d", yn, xn))
              cmp_pair <- cands[cands %in% all_pairs][1]
            }
          } else {
            cmp_pair <- input$gap_compare_pc_pair
          }

          if (!is.null(cmp_pair) && !is.na(cmp_pair)) {
            tryCatch({
              p <- .add_gap_comparison_overlay(
                plot             = p,
                gap_results_list = lapply(lst, `[[`, "results"),
                labels           = vapply(lst, `[[`, "label", FUN.VALUE = character(1)),
                pc_pair          = cmp_pair,
                display_mode     = input$gap_compare_mode %||% "difference",
                alpha            = input$gap_compare_alpha %||% 0.6,
                diff_high_a      = input$gap_compare_diff_high_a %||% "#D6604D",
                diff_mid         = input$gap_compare_diff_mid     %||% "#FFFFFF",
                diff_high_b      = input$gap_compare_diff_high_b  %||% "#2166AC",
                ovl_low          = input$gap_compare_ovl_low      %||% "#FFFFFF",
                ovl_mid          = input$gap_compare_ovl_mid      %||% "#FFFF00",
                ovl_high         = input$gap_compare_ovl_high     %||% "#FF0000"
              )
            }, error = function(e) {
              messages(paste0("Gap comparison error: ", conditionMessage(e)))
            })
          } else {
            messages(sprintf(
              "Gap Comparison: No data found for %s vs %s. Check that both files contain this PC pair.",
              x_col, y_col
            ))
          }
        } else if (isTRUE(input$gap_compare_show)) {
          messages("Gap Comparison: Load at least 2 gap files to compare.")
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

    output$plot_ui <- renderUI({
      h <- input$preview_height %||% 600
      plotOutput(ns("plot"), height = h)
    })

    output$plot <- renderPlot({
      p <- plot_obj(); req(p)
      req(!inherits(p, "plotly"))
      print(p)
    })

    # Render 3D scatter plot
    output$plot_3d <- plotly::renderPlotly({
      req(isTRUE(input$mode_3d))
      p <- plot_obj(); req(p)
      req(inherits(p, "plotly"))

      # Inject 3D gap surfaces if enabled
      if (isTRUE(input$gap_3d_show)) {
        results <- gap_results()
        df      <- data_reactive()
        x_col   <- input$x_col %||% ""
        y_col   <- input$y_col %||% ""
        z_col   <- input$z_col %||% ""

        if (!is.null(results) && !is.null(df) &&
            nzchar(x_col) && nzchar(y_col) && nzchar(z_col)) {
          threshold <- suppressWarnings(as.numeric(input$gap_3d_threshold))
          if (is.na(threshold)) threshold <- 0

          p <- tryCatch(
            .add_3d_gap_surfaces(
              p           = p,
              gap_results = results,
              x_col       = x_col,
              y_col       = y_col,
              z_col       = z_col,
              df          = df,
              threshold   = threshold,
              alpha       = input$gap_3d_alpha %||% 0.5,
              low_color   = input$gap_3d_low_color  %||% "#FFFFFF",
              mid_color   = input$gap_3d_mid_color  %||% "#FFFF00",
              high_color  = input$gap_3d_high_color %||% "#FF0000",
              mask_below  = isTRUE(input$gap_3d_mask_below)
            ),
            error = function(e) {
              messages(paste0("3D gap surface error: ", e$message))
              p
            }
          )
        }
      }

      p
    })

    # Render 3D topographic surface
    output$topo_3d_plot <- plotly::renderPlotly({
      req(isTRUE(input$gaps_show))
      req(identical(input$gap_display_mode, "surface_3d"))

      results <- gap_results()
      req(results)

      # Resolve PC pair (same auto-detect logic as main overlay)
      x_col <- input$x_col %||% ""
      y_col <- input$y_col %||% ""
      selected_pc_pair <- NULL

      if (isTRUE(input$gap_auto_detect_pair) &&
          grepl("^PC[0-9]+$", x_col) && grepl("^PC[0-9]+$", y_col)) {
        x_pc <- as.integer(gsub("PC", "", x_col))
        y_pc <- as.integer(gsub("PC", "", y_col))
        p1 <- sprintf("PC%d-PC%d", x_pc, y_pc)
        p2 <- sprintf("PC%d-PC%d", y_pc, x_pc)
        if (p1 %in% names(results$results)) selected_pc_pair <- p1
        else if (p2 %in% names(results$results)) selected_pc_pair <- p2
      }

      if (is.null(selected_pc_pair)) {
        selected_pc_pair <- input$gap_pc_pair %||% names(results$results)[[1]]
      }

      pair_result <- results$results[[selected_pc_pair]]
      req(pair_result)

      grid_x        <- pair_result$grid_x
      grid_y        <- pair_result$grid_y
      gap_certainty <- pair_result$gap_certainty

      # elevation = 1 - gap_certainty; matrix layout: rows = x, cols = y
      elev_matrix <- 1 - gap_certainty
      # plotly surface expects z[row, col] where row ~ y and col ~ x
      elev_matrix_t <- t(elev_matrix)

      # Build colorscale
      topo_pal <- input$topo_palette %||% "terrain"
      colorscale <- switch(
        topo_pal,
        terrain = list(
          list(0,   "#1a57a5"),   # valley - deep blue
          list(0.2, "#4a9ee8"),   # blue
          list(0.4, "#79c585"),   # lowland green
          list(0.6, "#c8b560"),   # upland tan
          list(0.8, "#a07840"),   # brown
          list(1,   "#f0f0f0")    # peak - snow white
        ),
        viridis = list(
          list(0, "#440154"), list(0.25, "#31688e"),
          list(0.5, "#35b779"), list(0.75, "#b4de2c"), list(1, "#fde725")
        ),
        plasma = list(
          list(0, "#0d0887"), list(0.25, "#7e03a8"),
          list(0.5, "#cc4778"), list(0.75, "#f89441"), list(1, "#f0f921")
        ),
        gray = list(list(0, "#111111"), list(1, "#eeeeee")),
        # default to terrain
        list(
          list(0,   "#1a57a5"), list(0.2, "#4a9ee8"),
          list(0.4, "#79c585"), list(0.6, "#c8b560"),
          list(0.8, "#a07840"), list(1,   "#f0f0f0")
        )
      )

      pcs   <- strsplit(selected_pc_pair, "-")[[1]]
      x_lab <- if (length(pcs) >= 1) pcs[[1]] else "PC x"
      y_lab <- if (length(pcs) >= 2) pcs[[2]] else "PC y"

      plotly::plot_ly(
        x = grid_x,
        y = grid_y,
        z = elev_matrix_t,
        type       = "surface",
        colorscale = colorscale,
        colorbar   = list(title = "Elevation<br>(1\u2212gap)"),
        contours   = if (isTRUE(input$topo_show_contours)) {
          n_brks <- max(2L, as.integer(input$topo_n_breaks %||% 8L))
          list(
            z = list(
              show      = TRUE,
              usecolormap = TRUE,
              highlightcolor = input$topo_contour_color %||% "#333333",
              project   = list(z = TRUE),
              size      = 1 / n_brks
            )
          )
        } else {
          list()
        }
      ) |>
        plotly::layout(
          scene = list(
            xaxis = list(title = x_lab),
            yaxis = list(title = y_lab),
            zaxis = list(title = "Elevation"),
            camera = list(eye = list(x = 1.5, y = -1.8, z = 1.2))
          ),
          margin = list(l = 0, r = 0, t = 30, b = 0)
        )
    })

    # Render interactive plotly plot
    output$interactive_plot <- plotly::renderPlotly({
      p <- plot_obj()
      req(p)
      req(inherits(p, "plotly"))
      p
    })
    
    # Handle click events for shape reconstruction (2D interactive mode)
    observeEvent(plotly::event_data("plotly_click", source = "morphospace"), {
      hover_data <- plotly::event_data("plotly_click", source = "morphospace")
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

            # Highlight the selected point on the plot (trace index 1 = selection marker)
            plotly::plotlyProxy("interactive_plot", session) |>
              plotly::plotlyProxyInvoke("restyle", list(x = list(pc1), y = list(pc2)), list(1L))
            
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

    # Handle click events for shape reconstruction in 3D interactive mode ----
    # The invisible 3D grid (trace 0, curveNumber == 0) provides click coordinates
    # anywhere in the morphospace volume.
    observeEvent(plotly::event_data("plotly_click", source = "morphospace_3d"), {
      hover_data <- plotly::event_data("plotly_click", source = "morphospace_3d")
      req(hover_data)

      pc1  <- hover_data$x
      pc2  <- hover_data$y
      pc3  <- hover_data$z
      curve_number <- hover_data$curveNumber

      df    <- data_reactive()
      req(df)
      x_col <- input$x_col
      y_col <- input$y_col
      z_col <- input$z_col

      # curveNumber == 0 is the invisible 3D grid; anything higher is a data trace
      is_grid <- !is.null(curve_number) && curve_number == 0

      if (!is_grid) {
        # Clicked directly on a data point - show specimen info
        point_number <- hover_data$pointNumber
        is_data_point <- FALSE
        point_idx <- NULL

        if (!is.null(point_number) && !is.null(pc1) && !is.null(pc2) && !is.null(pc3)) {
          tolerance <- 0.001
          matches <- which(
            abs(df[[x_col]] - pc1) < tolerance &
            abs(df[[y_col]] - pc2) < tolerance &
            abs(df[[z_col]] - pc3) < tolerance
          )
          if (length(matches) > 0) {
            is_data_point <- TRUE
            point_idx <- matches[1]
          }
        }

        if (is_data_point && !is.null(point_idx)) {
          point_id <- if ("ID" %in% names(df)) df$ID[point_idx] else paste("Point", point_idx)
          point_info <- list(
            type  = "data_point",
            id    = point_id,
            pc1   = df[[x_col]][point_idx],
            pc2   = df[[y_col]][point_idx],
            pc3   = df[[z_col]][point_idx],
            x_col = x_col,
            y_col = y_col,
            z_col = z_col
          )
          hover_point_info(point_info)
          hover_shape_coords(NULL)
        }

      } else {
        # Clicked on the invisible 3D grid - reconstruct hypothetical shape
        model <- pca_model()

        if (!is.null(model) && !is.null(pc1) && !is.null(pc2) && !is.null(pc3)) {
          tryCatch({
            x_pc_index <- as.integer(gsub("PC", "", x_col))
            y_pc_index <- as.integer(gsub("PC", "", y_col))
            z_pc_index <- as.integer(gsub("PC", "", z_col))

            coords <- .reconstruct_shape_from_hover(
              model,
              x_value    = pc1,
              y_value    = pc2,
              x_pc_index = x_pc_index,
              y_pc_index = y_pc_index,
              other_pcs  = setNames(pc3, paste0("PC", z_pc_index)),
              nb_pts     = 120
            )

            hover_point_info(list(
              type  = "reconstructed_3d",
              pc1   = pc1,
              pc2   = pc2,
              pc3   = pc3,
              x_col = x_col,
              y_col = y_col,
              z_col = z_col,
              shape_source = "reconstruction"
            ))
            hover_shape_coords(coords)

            # Highlight the selected point in the 3D plot (trace index 1 = selection marker)
            plotly::plotlyProxy("interactive_plot", session) |>
              plotly::plotlyProxyInvoke("restyle",
                list(x = list(pc1), y = list(pc2), z = list(pc3)),
                list(1L)
              )

          }, error = function(e) {
            message("3D reconstruction error: ", e$message)
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
        z_label <- if (!is.null(info$z_col)) info$z_col else NULL
        paste0(
          "Data Point\n",
          "ID: ", info$id, "\n",
          x_label, ": ", round(info$pc1, 3), "\n",
          y_label, ": ", round(info$pc2, 3), "\n",
          if (!is.null(z_label) && !is.null(info$pc3)) paste0(z_label, ": ", round(info$pc3, 3), "\n") else "",
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
      } else if (info$type == "reconstructed_3d") {
        x_label <- if (!is.null(info$x_col)) info$x_col else "PC1"
        y_label <- if (!is.null(info$y_col)) info$y_col else "PC2"
        z_label <- if (!is.null(info$z_col)) info$z_col else "PC3"
        paste0(
          "Hypothetical Shape (Reconstructed \u2014 3D)\n",
          x_label, ": ", round(info$pc1, 3), "\n",
          y_label, ": ", round(info$pc2, 3), "\n",
          z_label, ": ", round(info$pc3, 3), "\n",
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
        z_label <- if (!is.null(info$z_col)) info$z_col else NULL
        paste0(
          "Data Point\n",
          "ID: ", info$id, "\n",
          x_label, ": ", round(info$pc1, 3), "\n",
          y_label, ": ", round(info$pc2, 3), "\n",
          if (!is.null(z_label) && !is.null(info$pc3)) paste0(z_label, ": ", round(info$pc3, 3), "\n") else "",
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
      } else if (info$type == "reconstructed_3d") {
        x_label <- if (!is.null(info$x_col)) info$x_col else "PC1"
        y_label <- if (!is.null(info$y_col)) info$y_col else "PC2"
        z_label <- if (!is.null(info$z_col)) info$z_col else "PC3"
        paste0(
          "Hypothetical Shape (Reconstructed \u2014 3D)\n",
          x_label, ": ", round(info$pc1, 3), "\n",
          y_label, ": ", round(info$pc2, 3), "\n",
          z_label, ": ", round(info$pc3, 3), "\n",
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

    # Rotation video status UI
    output$rotation_video_status <- renderUI({
      p <- plot_obj()
      if (is.null(p) || !inherits(p, "plotly")) {
        helpText("Render a 3D plot first before generating the rotation video.")
      } else if (!isTRUE(rotation_deps_ready())) {
        tags$span(style = "color:orange;", "Installing required packages (av, webshot2, chromote) — please wait…")
      } else {
        tags$span(style = "color:green;", "Ready to generate video.")
      }
    })

    # Generate 3D rotation video and save to folder
    observeEvent(input$generate_rotation_video, {
      p <- plot_obj()
      if (is.null(p) || !inherits(p, "plotly")) {
        showNotification("No 3D plot rendered. Render a 3D plot first.", type = "error"); return()
      }
      if (!isTRUE(rotation_deps_ready())) {
        showNotification("Required packages are still installing. Please wait and try again.", type = "warning"); return()
      }

      stem    <- input$rotation_filename %||% "rotation_video"
      if (!nzchar(stem)) stem <- "rotation_video"
      out_dir <- rotation_out_dir()

      if (!dir.exists(out_dir)) {
        showNotification(paste("Output folder does not exist:", out_dir), type = "error"); return()
      }

      frames_dir <- file.path(out_dir, paste0(stem, "_frames"))
      out_file   <- file.path(out_dir, paste0(stem, ".mp4"))

      n_frames <- max(12L, as.integer(input$rotation_frames %||% 72L))
      fps      <- max(1L,  as.integer(input$rotation_fps    %||% 24L))
      w        <- max(100L, as.integer(input$rotation_width  %||% 800L))
      h        <- max(100L, as.integer(input$rotation_height %||% 600L))
      eye_r    <- max(0.1, as.numeric(input$rotation_eye_r  %||% 2.5))
      axis     <- input$rotation_axis %||% "azimuth"

      showNotification(paste0("Generating ", n_frames, " frames…"), id = "rot_progress", duration = NULL, type = "message")

      tryCatch({
        withProgress(message = "Rendering rotation video", value = 0, {
          .create_3d_rotation_video(
            plot        = p,
            out_file    = out_file,
            frames_dir  = frames_dir,
            n_frames    = n_frames,
            fps         = fps,
            width       = w,
            height      = h,
            eye_r       = eye_r,
            axis        = axis,
            progress_cb = function(i, n) {
              incProgress(1 / n, detail = sprintf("Frame %d / %d", i, n))
            }
          )
        })
        removeNotification("rot_progress")
        showNotification(
          paste0("Video saved to: ", out_file, "\nFrames in: ", frames_dir),
          type = "message", duration = 8
        )
      }, error = function(e) {
        removeNotification("rot_progress")
        showNotification(paste("Video generation failed:", e$message), type = "error", duration = 10)
      })
    })

    # Removed: hull specimens modal button and observer (now shown inline in the legend)

    invisible(list(plot = plot_obj))
  })
}

# ── Rotation video helpers ────────────────────────────────────────────────────

#' Detect a Chromium-based browser and set CHROMOTE_CHROME if needed
#' @keywords internal
.ensure_chromium <- function() {
  if (nzchar(Sys.getenv("CHROMOTE_CHROME"))) return(invisible(TRUE))

  candidates <- if (.Platform$OS.type == "windows") {
    c(
      file.path(Sys.getenv("ProgramFiles"),       "Microsoft/Edge/Application/msedge.exe"),
      file.path(Sys.getenv("ProgramFiles(x86)"),  "Microsoft/Edge/Application/msedge.exe"),
      file.path(Sys.getenv("ProgramFiles"),       "Google/Chrome/Application/chrome.exe"),
      file.path(Sys.getenv("ProgramFiles(x86)"),  "Google/Chrome/Application/chrome.exe"),
      file.path(Sys.getenv("LOCALAPPDATA"),        "Microsoft/Edge/Application/msedge.exe"),
      file.path(Sys.getenv("ProgramFiles"),       "BraveSoftware/Brave-Browser/Application/brave.exe")
    )
  } else if (Sys.info()[["sysname"]] == "Darwin") {
    c(
      "/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge",
      "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
      "/Applications/Chromium.app/Contents/MacOS/Chromium",
      "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser"
    )
  } else {
    # Linux: check PATH
    unlist(lapply(c("google-chrome", "google-chrome-stable", "chromium-browser", "chromium", "microsoft-edge", "brave-browser"),
                  function(b) tryCatch(trimws(system(paste("which", b), intern = TRUE, ignore.stderr = TRUE)), error = function(e) "")))
  }

  found <- Filter(nzchar, candidates[file.exists(candidates)])
  if (length(found) == 0) {
    stop("No Chromium-based browser found. Install Google Chrome or Microsoft Edge, or set the CHROMOTE_CHROME environment variable.")
  }
  Sys.setenv(CHROMOTE_CHROME = found[[1]])
  invisible(TRUE)
}

#' Create a 3D Rotation Video from a plotly scatter3d Object
#'
#' Renders \code{n_frames} static PNG snapshots of \code{plot} while rotating
#' the camera, then stitches them into an MP4 with \pkg{av}.
#' Frame capture uses \pkg{webshot2} (headless Chrome) — no Python required.
#'
#' @param plot      A plotly scatter3d object.
#' @param out_file  Destination file path (should end in \code{.mp4}).
#' @param n_frames  Number of frames (full 360° rotation).
#' @param fps       Frames per second in the output video.
#' @param width     Frame width in pixels.
#' @param height    Frame height in pixels.
#' @param eye_r     Camera eye radius (distance from centre).
#' @param axis      One of \code{"azimuth"} (horizontal spin) or
#'   \code{"elevation"} (vertical tilt).
#'
#' @keywords internal
.create_3d_rotation_video <- function(plot, out_file, frames_dir = NULL,
                                      n_frames = 72L, fps = 24L,
                                      width = 800L, height = 600L,
                                      eye_r = 2.5, axis = "azimuth",
                                      progress_cb = NULL) {
  use_tmp <- is.null(frames_dir)
  if (use_tmp) {
    frames_dir <- tempfile("rotation_frames_")
    on.exit(unlink(frames_dir, recursive = TRUE), add = TRUE)
  }
  dir.create(frames_dir, recursive = TRUE, showWarnings = FALSE)

  .ensure_chromium()

  angles <- seq(0, 2 * pi * (1 - 1 / n_frames), length.out = n_frames)

  # For "both" mode, build azimuth sequence followed by elevation sequence
  if (axis == "both") {
    frame_specs <- c(
      lapply(angles, function(th) list(axis = "azimuth",   theta = th)),
      lapply(angles, function(th) list(axis = "elevation", theta = th))
    )
  } else {
    frame_specs <- lapply(angles, function(th) list(axis = axis, theta = th))
  }
  total_frames <- length(frame_specs)
  frame_paths  <- character(total_frames)

  # Reuse a single chromote session for speed
  session <- chromote::ChromoteSession$new(width = width, height = height)
  on.exit(session$close(), add = TRUE)

  for (i in seq_along(frame_specs)) {
    spec  <- frame_specs[[i]]
    theta <- spec$theta

    if (spec$axis == "azimuth") {
      eye <- list(x = eye_r * cos(theta), y = eye_r * sin(theta), z = eye_r * 0.5)
    } else {
      # elevation: rotate in the XZ plane; phi=0 starts from the side
      phi <- theta - pi / 2
      eye <- list(x = eye_r * cos(phi), y = eye_r * 0.5, z = eye_r * sin(phi))
    }

    frame_plot <- plotly::layout(
      plot,
      scene = list(camera = list(eye = eye))
    )

    html_path  <- file.path(frames_dir, sprintf("frame_%04d.html", i))
    frame_path <- file.path(frames_dir, sprintf("frame_%04d.png",  i))

    htmlwidgets::saveWidget(frame_plot, html_path, selfcontained = TRUE)
    webshot2::webshot(
      url    = html_path,
      file   = frame_path,
      vwidth = width,
      vheight = height,
      delay  = 1.5  # allow WebGL to finish rendering
    )
    frame_paths[[i]] <- frame_path
    if (is.function(progress_cb)) progress_cb(i, total_frames)
  }

  av::av_encode_video(
    input     = frame_paths,
    output    = out_file,
    framerate = fps
  )

  invisible(out_file)
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
#' @param topo_palette Character: "terrain", "viridis", "plasma", or "gray"
#' @param topo_show_contours Logical: draw isoline contour lines
#' @param topo_n_breaks Integer: number of contour break levels
#' @param topo_contour_color Color for contour lines
#' @param topo_contour_width Line width for contour lines
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
                                     polygon_width = 1.2,
                                     topo_palette = "terrain",
                                     topo_show_contours = TRUE,
                                     topo_n_breaks = 8L,
                                     topo_contour_color = "#333333",
                                     topo_contour_width = 0.4) {
  # Collect overlay layers and prepend them once so they render behind existing layers.
  background_layers <- list()
  
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
    
    # Queue heatmap layer for background rendering
    background_layers[[length(background_layers) + 1L]] <- ggplot2::geom_raster(
      data = gap_df,
      ggplot2::aes(x = x, y = y, fill = certainty),
      alpha = alpha,
      inherit.aes = FALSE
    )

    plot <- plot +
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
        
        # Queue polygon outlines for background rendering (above heatmap, below data points)
        background_layers[[length(background_layers) + 1L]] <- ggplot2::geom_polygon(
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
  
  if (display_mode == "topographic") {
    gap_certainty <- pair_result$gap_certainty
    grid_x <- pair_result$grid_x
    grid_y <- pair_result$grid_y
    
    # Elevation = 1 - gap_certainty: mountains = occupied, valleys = gaps
    topo_df <- expand.grid(x = grid_x, y = grid_y)
    topo_df$elevation <- 1 - as.vector(gap_certainty)
    topo_df <- topo_df[!is.na(topo_df$elevation), ]
    
    # Build terrain colour palette
    topo_colors <- switch(
      topo_palette %||% "terrain",
      terrain  = grDevices::terrain.colors(256),
      viridis  = if (requireNamespace("viridisLite", quietly = TRUE)) {
        viridisLite::viridis(256, direction = -1)
      } else {
        grDevices::terrain.colors(256)
      },
      plasma   = if (requireNamespace("viridisLite", quietly = TRUE)) {
        viridisLite::plasma(256, direction = -1)
      } else {
        grDevices::terrain.colors(256)
      },
      gray     = grDevices::gray.colors(256, start = 0.05, end = 0.95),
      grDevices::terrain.colors(256)
    )
    
    background_layers[[length(background_layers) + 1L]] <- ggplot2::geom_raster(
      data = topo_df,
      ggplot2::aes(x = x, y = y, fill = elevation),
      alpha = alpha,
      inherit.aes = FALSE
    )

    plot <- plot +
      ggplot2::scale_fill_gradientn(
        colors = topo_colors,
        limits = c(0, 1),
        name   = "Elevation\n(1\u2212gap)"
      )
    
    if (isTRUE(topo_show_contours)) {
      n_breaks <- max(2L, as.integer(topo_n_breaks %||% 8L))
      brks <- seq(0, 1, length.out = n_breaks)
      
      # geom_contour needs all three x/y/z in the same data frame on a complete grid;
      # interpolate NA cells back to a full grid via a wide matrix for contouring
      elev_matrix <- matrix(
        topo_df$elevation[
          match(as.character(interaction(topo_df$x, topo_df$y)),
                as.character(interaction(
                  rep(grid_x, times = length(grid_y)),
                  rep(grid_y, each  = length(grid_x))
                )))
        ],
        nrow = length(grid_x),
        ncol = length(grid_y)
      )
      contour_df <- expand.grid(x = grid_x, y = grid_y)
      contour_df$elevation <- as.vector(elev_matrix)
      
      background_layers[[length(background_layers) + 1L]] <- ggplot2::geom_contour(
        data = contour_df,
        ggplot2::aes(x = x, y = y, z = elevation),
        breaks     = brks,
        color      = topo_contour_color %||% "#333333",
        linewidth  = topo_contour_width %||% 0.4,
        inherit.aes = FALSE
      )
    }
  }

  if (length(background_layers) > 0) {
    plot$layers <- c(background_layers, plot$layers)
  }
  
  return(plot)
}

#' Add 3D Gap Surfaces to a Plotly 3D Scatter Plot
#'
#' Projects gap certainty heatmaps as semi-transparent surfaces onto the three
#' background planes (XY, XZ, YZ) of a plotly scatter3d object.
#'
#' @param p A plotly scatter3d object.
#' @param gap_results A morphospace_gaps object.
#' @param x_col,y_col,z_col Column names mapping to the three plot axes (e.g. "PC1").
#' @param df Data frame used in the plot (to compute axis ranges for wall positions).
#' @param threshold Numeric certainty threshold; cells below this value are masked to NA.
#' @param alpha Numeric surface opacity (0-1).
#' @param low_color Color for low gap certainty (matches 2D overlay low color).
#' @param mid_color Color for mid gap certainty (matches 2D overlay mid color).
#' @param high_color Color for high gap certainty (matches 2D overlay high color).
#' @param mask_below Logical; if TRUE, cells with certainty < threshold are set to NA.
#'
#' @keywords internal
.add_3d_gap_surfaces <- function(p, gap_results, x_col, y_col, z_col,
                                  df, threshold, alpha = 0.5,
                                  low_color = "#FFFFFF", mid_color = "#FFFF00",
                                  high_color = "#FF0000", mask_below = TRUE) {

  # Parse PC indices from column names
  if (!grepl("^PC[0-9]+$", x_col) ||
      !grepl("^PC[0-9]+$", y_col) ||
      !grepl("^PC[0-9]+$", z_col)) {
    return(p)
  }

  xn <- as.integer(gsub("PC", "", x_col))
  yn <- as.integer(gsub("PC", "", y_col))
  zn <- as.integer(gsub("PC", "", z_col))

  available_pairs <- names(gap_results$results)

  # Helper: find a PC pair key in either ordering
  find_pair <- function(a, b) {
    k1 <- sprintf("PC%d-PC%d", a, b)
    k2 <- sprintf("PC%d-PC%d", b, a)
    if (k1 %in% available_pairs) return(k1)
    if (k2 %in% available_pairs) return(k2)
    NULL
  }

  # Compute data range for each axis to place wall at min - 10% of range
  safe_range <- function(col) {
    vals <- df[[col]]
    vals <- vals[is.finite(vals)]
    if (length(vals) == 0) return(c(-1, 1))
    r <- range(vals)
    if (diff(r) == 0) r <- r + c(-0.5, 0.5)
    r
  }

  x_rng <- safe_range(x_col)
  y_rng <- safe_range(y_col)
  z_rng <- safe_range(z_col)

  x_wall <- x_rng[1] - 0.10 * diff(x_rng)
  y_wall <- y_rng[1] - 0.10 * diff(y_rng)
  z_wall <- z_rng[1] - 0.10 * diff(z_rng)

  # Build colorscale list matching the 2D scale_fill_gradient2 (low -> mid -> high)
  cs_list <- list(
    list(0,    low_color  %||% "#FFFFFF"),
    list(0.5,  mid_color  %||% "#FFFF00"),
    list(1,    high_color %||% "#FF0000")
  )

  # Helper: prepare surfacecolor matrix from a pair_result.
  # gap_certainty is stored as [grid_x rows x grid_y cols]; plotly surface
  # expects surfacecolor[ny rows x nx cols] (i.e. transposed).
  # When the pair was stored in reverse order (flipped=TRUE), swap x/y roles.
  prepare_surface <- function(pair_result, flip = FALSE) {
    gc <- pair_result$gap_certainty
    gx <- pair_result$grid_x
    gy <- pair_result$grid_y

    if (flip) {
      # Pair stored as PCb-PCa; swap so first axis = x, second = y
      gc <- t(gc)
      tmp <- gx; gx <- gy; gy <- tmp
    }

    sc <- t(gc)  # now [ny rows x nx cols] for plotly
    if (isTRUE(mask_below) && !is.na(threshold)) {
      sc[sc < threshold] <- NA_real_
    }

    list(surfacecolor = sc, x_vec = gx, y_vec = gy)
  }

  show_scale <- TRUE  # show colorbar on the first surface only

  # ---- XY plane (z = z_wall) ----
  pair_xy <- find_pair(xn, yn)
  if (!is.null(pair_xy)) {
    tryCatch({
      pr <- gap_results$results[[pair_xy]]
      flipped <- !startsWith(pair_xy, sprintf("PC%d-", xn))
      surf <- prepare_surface(pr, flip = flipped)
      nx <- length(surf$x_vec); ny <- length(surf$y_vec)

      p <- plotly::add_trace(
        p,
        type         = "surface",
        x            = surf$x_vec,
        y            = surf$y_vec,
        z            = matrix(z_wall, nrow = ny, ncol = nx),
        surfacecolor = surf$surfacecolor,
        colorscale   = cs_list,
        cmin         = 0, cmax = 1,
        opacity      = alpha,
        showscale    = show_scale,
        colorbar     = list(title = list(text = "Gap\nCertainty", side = "right"), len = 0.5),
        showlegend   = FALSE,
        name         = sprintf("Gap: %s (XY)", pair_xy),
        hovertemplate = paste0(x_col, ": %{x:.3f}<br>", y_col,
                               ": %{y:.3f}<br>Gap certainty: %{surfacecolor:.2f}<extra></extra>")
      )
      show_scale <- FALSE
    }, error = function(e) {
      warning(sprintf("3D gap surface failed for XY plane (%s): %s", pair_xy, e$message))
    })
  }

  # ---- XZ plane (y = y_wall) ----
  pair_xz <- find_pair(xn, zn)
  if (!is.null(pair_xz)) {
    tryCatch({
      pr <- gap_results$results[[pair_xz]]
      flipped <- !startsWith(pair_xz, sprintf("PC%d-", xn))
      surf <- prepare_surface(pr, flip = flipped)
      # surf$x_vec = PC_x values, surf$y_vec = PC_z values
      nx <- length(surf$x_vec); nz <- length(surf$y_vec)

      # Parametric surface on y = y_wall:
      #   [row i, col j] -> 3D point (x_vec[j], y_wall, y_vec[i])
      x_mat <- matrix(rep(surf$x_vec, each = nz), nrow = nz, ncol = nx)
      y_mat <- matrix(y_wall, nrow = nz, ncol = nx)
      z_mat <- matrix(rep(surf$y_vec, times = nx), nrow = nz, ncol = nx)

      p <- plotly::add_trace(
        p,
        type         = "surface",
        x            = x_mat,
        y            = y_mat,
        z            = z_mat,
        surfacecolor = surf$surfacecolor,
        colorscale   = cs_list,
        cmin         = 0, cmax = 1,
        opacity      = alpha,
        showscale    = show_scale,
        showlegend   = FALSE,
        name         = sprintf("Gap: %s (XZ)", pair_xz),
        hovertemplate = paste0(x_col, ": %{x:.3f}<br>", z_col,
                               ": %{z:.3f}<br>Gap certainty: %{surfacecolor:.2f}<extra></extra>")
      )
      show_scale <- FALSE
    }, error = function(e) {
      warning(sprintf("3D gap surface failed for XZ plane (%s): %s", pair_xz, e$message))
    })
  }

  # ---- YZ plane (x = x_wall) ----
  pair_yz <- find_pair(yn, zn)
  if (!is.null(pair_yz)) {
    tryCatch({
      pr <- gap_results$results[[pair_yz]]
      flipped <- !startsWith(pair_yz, sprintf("PC%d-", yn))
      surf <- prepare_surface(pr, flip = flipped)
      # surf$x_vec = PC_y values, surf$y_vec = PC_z values
      ny2 <- length(surf$x_vec); nz <- length(surf$y_vec)

      # Parametric surface on x = x_wall:
      #   [row i, col j] -> 3D point (x_wall, x_vec[j], y_vec[i])
      x_mat <- matrix(x_wall, nrow = nz, ncol = ny2)
      y_mat <- matrix(rep(surf$x_vec, each = nz), nrow = nz, ncol = ny2)
      z_mat <- matrix(rep(surf$y_vec, times = ny2), nrow = nz, ncol = ny2)

      p <- plotly::add_trace(
        p,
        type         = "surface",
        x            = x_mat,
        y            = y_mat,
        z            = z_mat,
        surfacecolor = surf$surfacecolor,
        colorscale   = cs_list,
        cmin         = 0, cmax = 1,
        opacity      = alpha,
        showscale    = show_scale,
        showlegend   = FALSE,
        name         = sprintf("Gap: %s (YZ)", pair_yz),
        hovertemplate = paste0(y_col, ": %{y:.3f}<br>", z_col,
                               ": %{z:.3f}<br>Gap certainty: %{surfacecolor:.2f}<extra></extra>")
      )
    }, error = function(e) {
      warning(sprintf("3D gap surface failed for YZ plane (%s): %s", pair_yz, e$message))
    })
  }

  p
}

#' Compare Multiple Gap Analysis Results as a Plot Overlay
#'
#' Renders a difference heatmap or inverted-color overlay from two or more
#' \code{morphospace_gaps} objects onto an existing ggplot.
#'
#' In \strong{difference} mode (2 files), the heatmap shows
#' \code{certainty_A - certainty_B}: red cells are gaps unique to Group A,
#' blue cells are gaps unique to Group B, and white indicates no difference.
#'
#' In \strong{overlay} mode, each file is rendered as a separate semi-transparent
#' raster. File 1 uses warm colors (white \eqn{\to} red); subsequent files use
#' cool, inverted colors (white \eqn{\to} blue, green, \ldots), so regions where
#' the groups diverge remain visible.
#'
#' @param plot A \code{ggplot2} object to add the overlay to.
#' @param gap_results_list A named or unnamed list of \code{morphospace_gaps} objects.
#' @param labels Character vector of group labels (one per element of
#'   \code{gap_results_list}).  Defaults to \code{"Group A"}, \code{"Group B"}, etc.
#' @param pc_pair Character. PC pair key present in the results, e.g. \code{"PC1-PC2"}.
#' @param display_mode Either \code{"difference"} (diverging heatmap, 2 files) or
#'   \code{"overlay"} (per-file inverted color layers).
#' @param alpha Numeric (0-1). Overall transparency for the raster overlay.
#'
#' @return The modified \code{ggplot2} object.
#' @keywords internal
.add_gap_comparison_overlay <- function(plot,
                                         gap_results_list,
                                         labels       = NULL,
                                         pc_pair,
                                         display_mode = "difference",
                                         alpha        = 0.6,
                                         diff_high_a  = "#D6604D",
                                         diff_mid     = "#FFFFFF",
                                         diff_high_b  = "#2166AC",
                                         ovl_low      = "#FFFFFF",
                                         ovl_mid      = "#FFFF00",
                                         ovl_high     = "#FF0000") {

  if (inherits(plot, "plotly")) {
    warning("Gap comparison overlay is not supported in interactive plotly mode.")
    return(plot)
  }

  if (is.null(labels)) {
    labels <- paste0("Group ", LETTERS[seq_along(gap_results_list)])
  }

  # Extract the per-pair result for each file; drop files lacking this pair
  pair_results <- lapply(gap_results_list, function(res) res$results[[pc_pair]])
  valid        <- !vapply(pair_results, is.null, logical(1))
  pair_results <- pair_results[valid]
  labels       <- labels[valid]

  if (length(pair_results) == 0) {
    warning(sprintf("No gap data found for PC pair '%s' in any loaded file.", pc_pair))
    return(plot)
  }

  background_layers <- list()

  if (display_mode == "difference" && length(pair_results) >= 2) {

    pr_a   <- pair_results[[1]]
    pr_b   <- pair_results[[2]]
    grid_x <- pr_a$grid_x
    grid_y <- pr_a$grid_y

    cert_a <- pr_a$gap_certainty              # matrix [n_x x n_y]
    cert_b <- .resample_gap_certainty(        # resampled to A's grid
      from_x   = pr_b$grid_x,
      from_y   = pr_b$grid_y,
      from_cert = pr_b$gap_certainty,
      to_x     = grid_x,
      to_y     = grid_y
    )

    diff_mat <- cert_a - cert_b               # positive = A-unique gap
    diff_mat[is.na(cert_a) & is.na(cert_b)] <- NA

    # Diverging color ramp: B-unique (diff_high_b) -> midpoint -> A-unique (diff_high_a)
    diverge_ramp <- grDevices::colorRamp(c(diff_high_b, diff_mid, diff_high_a))

    flat_diff <- as.vector(diff_mat)
    flat_norm <- (flat_diff + 1) / 2          # map [-1, 1] -> [0, 1]
    flat_norm <- pmax(0, pmin(1, flat_norm))

    rgb_mat   <- diverge_ramp(flat_norm)
    alpha_int <- as.integer(round(alpha * 255))
    hex_colors <- ifelse(
      is.na(flat_diff),
      NA_character_,
      sprintf("#%02X%02X%02X%02X",
              as.integer(rgb_mat[, 1]),
              as.integer(rgb_mat[, 2]),
              as.integer(rgb_mat[, 3]),
              alpha_int)
    )

    df_diff        <- expand.grid(x = grid_x, y = grid_y)
    df_diff$fill_c <- hex_colors
    df_diff        <- df_diff[!is.na(df_diff$fill_c), ]

    # Use rasterGrob so the comparison layer does not interfere with fill scales
    n_x    <- length(grid_x)
    n_y    <- length(grid_y)
    clr_m  <- matrix(hex_colors, nrow = n_x, ncol = n_y)
    clr_m  <- t(clr_m)                        # rasterGrob: rows=y, cols=x
    clr_m  <- clr_m[nrow(clr_m):1L, , drop = FALSE]  # flip rows: rasterGrob row 1 = top = highest y

    grob <- grid::rasterGrob(
      clr_m,
      x          = 0.5, y = 0.5,
      width      = 1,   height = 1,
      interpolate = TRUE
    )

    bg_layer <- ggplot2::annotation_custom(
      grob,
      xmin = min(grid_x), xmax = max(grid_x),
      ymin = min(grid_y), ymax = max(grid_y)
    )
    background_layers[[1L]] <- bg_layer

  } else {

    # Overlay mode: pre-blend A and B at the pixel level before compositing.
    # Two separate semi-transparent layers use Porter-Duff compositing, not a true 50/50
    # mix, so complementary colors would not cancel to gray. Pre-blending the RGB values
    # directly ensures color_A + invert(color_B) = (128,128,128) for any matched certainty.
    pr_a <- pair_results[[1]]
    pr_b <- pair_results[[2]]
    gx   <- pr_a$grid_x
    gy   <- pr_a$grid_y
    n_x  <- length(gx)
    n_y  <- length(gy)

    cert_a <- pr_a$gap_certainty
    cert_b <- .resample_gap_certainty(pr_b$grid_x, pr_b$grid_y, pr_b$gap_certainty, gx, gy)

    ramp      <- grDevices::colorRamp(c(ovl_low, ovl_mid, ovl_high))
    flat_a    <- as.vector(cert_a)
    flat_b    <- as.vector(cert_b)
    norm_a    <- pmax(0, pmin(1, ifelse(is.na(flat_a), 0, flat_a)))
    norm_b    <- pmax(0, pmin(1, ifelse(is.na(flat_b), 0, flat_b)))
    rgb_a     <- ramp(norm_a)               # [n x 3], range 0-255
    rgb_b_inv <- 255 - ramp(norm_b)           # pixel inversion: B occupied -> black, B gap -> cyan
    rgb_blend <- (rgb_a + rgb_b_inv) / 2    # true 50/50 pixel mix

    na_cell <- is.na(flat_a) & is.na(flat_b)
    a_int   <- as.integer(round(alpha * 255))
    hexes   <- ifelse(
      na_cell,
      "#FFFFFF00",
      sprintf("#%02X%02X%02X%02X",
              as.integer(rgb_blend[, 1]),
              as.integer(rgb_blend[, 2]),
              as.integer(rgb_blend[, 3]),
              a_int)
    )

    clr_m <- t(matrix(hexes, nrow = n_x, ncol = n_y))
    clr_m <- clr_m[nrow(clr_m):1L, , drop = FALSE]  # flip rows: rasterGrob row 1 = top = highest y
    grob  <- grid::rasterGrob(clr_m, x = 0.5, y = 0.5, width = 1, height = 1, interpolate = TRUE)
    background_layers[[1L]] <- ggplot2::annotation_custom(
      grob, xmin = min(gx), xmax = max(gx), ymin = min(gy), ymax = max(gy)
    )
  }

  if (length(background_layers) > 0) {
    plot$layers <- c(background_layers, plot$layers)
  }

  plot
}

#' Resample a Gap Certainty Matrix via Nearest-Neighbour Lookup
#'
#' @param from_x,from_y Numeric vectors giving the source grid axes.
#' @param from_cert Numeric matrix \code{[length(from_x) x length(from_y)]}.
#' @param to_x,to_y Numeric vectors giving the target grid axes.
#'
#' @return Numeric matrix \code{[length(to_x) x length(to_y)]}.
#' @keywords internal
.resample_gap_certainty <- function(from_x, from_y, from_cert, to_x, to_y) {
  xi <- vapply(to_x, function(v) which.min(abs(from_x - v)), integer(1))
  yi <- vapply(to_y, function(v) which.min(abs(from_y - v)), integer(1))
  result <- matrix(NA_real_, nrow = length(to_x), ncol = length(to_y))
  for (i in seq_along(to_x)) {
    result[i, ] <- from_cert[xi[i], yi]
  }
  result
}