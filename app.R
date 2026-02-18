# HaugShape v2 - Minimal Starting Point
# Clean Shiny app with empty Data Import tab

# Increase max upload file size (default 200MB) to support large RDS results.
# Optional override: set env var HAUGSHAPE_MAX_UPLOAD_MB (integer/float).
.haugshape_max_upload_mb <- suppressWarnings(as.numeric(Sys.getenv("HAUGSHAPE_MAX_UPLOAD_MB", "200")))
if (is.na(.haugshape_max_upload_mb) || .haugshape_max_upload_mb <= 0) .haugshape_max_upload_mb <- 200
options(shiny.maxRequestSize = .haugshape_max_upload_mb * 1024^2)

library(shiny)
library(shinydashboard)
library(DT)

# Source module files if running as a standalone app script
module_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
for (f in module_files) try(source(f, local = TRUE), silent = TRUE)

# Define UI
ui <- dashboardPage(
  
  # Header
  dashboardHeader(title = "HaugShape v2 - Morphometric Analysis"),
  
  # Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("1. Image Processing", tabName = "images", icon = icon("image")),
      menuItem("2. Shape Analysis", tabName = "shape", icon = icon("chart-area"),
        menuSubItem("Run Analysis", tabName = "shape"),
        menuSubItem("Reconstruct Shapes", tabName = "reconstruct"),
        menuSubItem("Gap Detection", tabName = "gap_detection")
      ),
      menuItem("3. Data Import", tabName = "import", icon = icon("upload")),
      menuItem("4. Plotting", tabName = "plotting", icon = icon("chart-line"))
    )
  ),
  
  # Body
  dashboardBody(
    tabItems(

      # Image Processing Tab
      tabItem(tabName = "images",
        image_processing_ui("img_proc")
      )
      ,
      # Shape Analysis Tab
      tabItem(tabName = "shape",
        shape_analysis_ui("shape_an")
      ),
      
      # Shape Reconstruction Tab
      tabItem(tabName = "reconstruct",
        shape_reconstruction_ui("shape_recon")
      ),

      # Data Import Tab
      tabItem(tabName = "import",
        data_import_ui("import_excel")
      ),

      # Plotting Tab
      tabItem(tabName = "plotting",
        plotting_ui("plotting")
      ),
      
      # Gap Detection Tab
      tabItem(tabName = "gap_detection",
        gap_detection_ui("gap_det")
      )
      
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Initialize data import module
  imported <- data_import_server("import_excel")

  # Example: observe when data is available
  observeEvent(imported$data(), {
    df <- imported$data()
    if (!is.null(df)) {
      message(sprintf("Imported %d rows and %d columns", nrow(df), ncol(df)))
    }
  })

  # Initialize image processing module
  image_processing_server("img_proc")

  # Initialize shape analysis module
  shape_analysis_server("shape_an")
  
  # Initialize shape reconstruction module
  shape_reconstruction_server("shape_recon")

  # Initialize plotting module (uses data from Data Import)
  plotting_server("plotting", data_reactive = imported$data)
  
  # Initialize gap detection module
  gap_detection_server("gap_det")
}

# Run the application
shinyApp(ui = ui, server = server)