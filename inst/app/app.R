# ShapeToolKit Embedded Shiny App (installed under inst/app)

# Note: This app composes the exported modules from the ShapeToolKit package.
# It is bundled into the installed package so users can launch it via
# ShapeToolKit::run_shape_app().

# Increase max upload file size (default 200MB) to support large RDS results.
# Optional override: set env var HAUGSHAPE_MAX_UPLOAD_MB (integer/float).
.haugshape_max_upload_mb <- suppressWarnings(as.numeric(Sys.getenv("HAUGSHAPE_MAX_UPLOAD_MB", "200")))
if (is.na(.haugshape_max_upload_mb) || .haugshape_max_upload_mb <= 0) .haugshape_max_upload_mb <- 200
options(shiny.maxRequestSize = .haugshape_max_upload_mb * 1024^2)

library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "ShapeToolKit"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("1. Image Processing", tabName = "image_processing", icon = icon("images"),
        menuSubItem("Convert PNG to JPG/BMP", tabName = "image_processing"),
        menuSubItem("Complete Halved Shapes", tabName = "complete_shapes")
      ),
      menuItem("2. Morph Shapes",     tabName = "morph_shapes",     icon = icon("wand-magic-sparkles")),
      menuItem("3. Shape Analysis",   tabName = "shape_analysis",   icon = icon("project-diagram"),
        menuSubItem("Run Analysis", tabName = "shape_analysis"),
        menuSubItem("Reconstruct Shapes", tabName = "shape_reconstruction"),
        menuSubItem("PC Contribution Plots", tabName = "pc_contribution_plots", icon = icon("shapes")),
        menuSubItem("Gap Detection", tabName = "gap_detection"),
        menuSubItem("PCA Saturation Curve", tabName = "pca_saturation", icon = icon("chart-line")),
        menuSubItem("Shape Panel", tabName = "shape_panel", icon = icon("images"))
      ),
      menuItem("4. Data Import",      tabName = "data_import",      icon = icon("table")),
      menuItem("5. Plotting",         tabName = "plotting",         icon = icon("chart-line")),
      menuItem("6. Overview",         tabName = "overview",         icon = icon("th-large"))
    )
  ),
  dashboardBody(
    tabItems(
  tabItem(tabName = "image_processing", ShapeToolKit::image_processing_ui("img")),
  tabItem(tabName = "complete_shapes",  ShapeToolKit::complete_shapes_ui("cs")),
  tabItem(tabName = "morph_shapes",     ShapeToolKit::morph_shapes_ui("ms")),
  tabItem(tabName = "shape_analysis",   ShapeToolKit::shape_analysis_ui("sa")),
  tabItem(tabName = "shape_reconstruction", ShapeToolKit::shape_reconstruction_ui("sr")),
  tabItem(tabName = "pc_contribution_plots", ShapeToolKit::pc_contribution_plots_ui("pcp")),
  tabItem(tabName = "data_import",      ShapeToolKit::data_import_ui("di")),
  tabItem(tabName = "plotting",         ShapeToolKit::plotting_ui("pl")),
  tabItem(tabName = "gap_detection",    ShapeToolKit::gap_detection_ui("gd")),
  tabItem(tabName = "pca_saturation",   ShapeToolKit::pca_saturation_ui("pca_sat")),
  tabItem(tabName = "overview",         ShapeToolKit::overview_ui("ov")),
  tabItem(tabName = "shape_panel",      ShapeToolKit::shape_panel_ui("sp"))
    )
  )
)

server <- function(input, output, session) {
  # Initialize modules
  ShapeToolKit::image_processing_server("img")
  ShapeToolKit::complete_shapes_server("cs")
  ShapeToolKit::morph_shapes_server("ms")
  ShapeToolKit::shape_analysis_server("sa")
  ShapeToolKit::shape_reconstruction_server("sr")
  ShapeToolKit::pc_contribution_plots_server("pcp")

  # Data Import provides data for plotting
  imported <- ShapeToolKit::data_import_server("di")  # list with $data reactive
  ShapeToolKit::plotting_server("pl", data_reactive = imported$data)
  ShapeToolKit::gap_detection_server("gd")
  ShapeToolKit::pca_saturation_server("pca_sat")
  ShapeToolKit::overview_server("ov", data_reactive = imported$data)
  ShapeToolKit::shape_panel_server("sp", data_reactive = imported$data)
}

shinyApp(ui, server)
