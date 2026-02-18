# Simple HaugShape v2 Launcher
# This script launches the Shiny app directly

# Install required packages if missing
if (!require("shiny", quietly = TRUE)) install.packages("shiny")
if (!require("shinydashboard", quietly = TRUE)) install.packages("shinydashboard")

# Load required libraries
library(shiny)
library(shinydashboard)

cat("Launching HaugShape v2...\n")

# Define UI directly here
ui <- dashboardPage(
  
  # Header
  dashboardHeader(title = "HaugShape v2 - Morphometric Analysis"),
  
  # Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("1. Data Import", tabName = "import", icon = icon("upload"))
    )
  ),
  
  # Body
  dashboardBody(
    tabItems(
      
      # Data Import Tab (empty template)
      tabItem(tabName = "import",
        fluidRow(
          box(
            title = "Data Import", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            
            # This is where you'll add your data import functionality
            h3("Welcome to HaugShape v2"),
            p("This is a clean starting point for your morphometric analysis application."),
            p("Add your data import functionality here.")
            
          )
        )
      )
      
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Add your server logic here
  
}

# Run the application
runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)