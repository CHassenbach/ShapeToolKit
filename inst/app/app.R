# HaugShape v2 Embedded Shiny App (installed under inst/app)

# Note: This app composes the exported modules from the HaugShapeV2 package.
# It is bundled into the installed package so users can launch it via
# HaugShapeV2::run_haug_app().

# Increase max upload file size (default 200MB) to support large RDS results.
# Optional override: set env var HAUGSHAPE_MAX_UPLOAD_MB (integer/float).
.haugshape_max_upload_mb <- suppressWarnings(as.numeric(Sys.getenv("HAUGSHAPE_MAX_UPLOAD_MB", "200")))
if (is.na(.haugshape_max_upload_mb) || .haugshape_max_upload_mb <= 0) .haugshape_max_upload_mb <- 200
options(shiny.maxRequestSize = .haugshape_max_upload_mb * 1024^2)

library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "HaugShape v2"),
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
        menuSubItem("Gap Detection", tabName = "gap_detection"),
        menuSubItem("PCA Saturation Curve", tabName = "pca_saturation", icon = icon("chart-line"))
      ),
      menuItem("4. Data Import",      tabName = "data_import",      icon = icon("table")),
      menuItem("5. Plotting",         tabName = "plotting",         icon = icon("chart-line")),
      menuItem("6. Overview",         tabName = "overview",         icon = icon("th-large"))
    )
  ),
  dashboardBody(
    tabItems(
  tabItem(tabName = "image_processing", HaugShapeV2::image_processing_ui("img")),
  tabItem(tabName = "complete_shapes",  HaugShapeV2::complete_shapes_ui("cs")),
  tabItem(tabName = "morph_shapes",     HaugShapeV2::morph_shapes_ui("ms")),
  tabItem(tabName = "shape_analysis",   HaugShapeV2::shape_analysis_ui("sa")),
  tabItem(tabName = "shape_reconstruction", HaugShapeV2::shape_reconstruction_ui("sr")),
  tabItem(tabName = "data_import",      HaugShapeV2::data_import_ui("di")),
  tabItem(tabName = "plotting",         HaugShapeV2::plotting_ui("pl")),
  tabItem(tabName = "gap_detection",    HaugShapeV2::gap_detection_ui("gd")),
  tabItem(tabName = "pca_saturation",   HaugShapeV2::pca_saturation_ui("pca_sat")),
  tabItem(tabName = "overview",         HaugShapeV2::overview_ui("ov"))
    )
  )
)

server <- function(input, output, session) {
  # Initialize modules
  HaugShapeV2::image_processing_server("img")
  HaugShapeV2::complete_shapes_server("cs")
  HaugShapeV2::morph_shapes_server("ms")
  HaugShapeV2::shape_analysis_server("sa")
  HaugShapeV2::shape_reconstruction_server("sr")

  # Data Import provides data for plotting
  imported <- HaugShapeV2::data_import_server("di")  # list with $data reactive
  HaugShapeV2::plotting_server("pl", data_reactive = imported$data)
  HaugShapeV2::gap_detection_server("gd")
  HaugShapeV2::pca_saturation_server("pca_sat")
  HaugShapeV2::overview_server("ov", data_reactive = imported$data)
}

shinyApp(ui, server)
