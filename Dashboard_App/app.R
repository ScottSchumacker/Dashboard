# Scott Schumacker
# R Shiny dashboard showcasing different visualizations

# Loading libraries
library(shiny)
library(shinydashboard)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Scott Schumacker"),
  dashboardSidebar(),
  dashboardBody()
)

# Server
server <- function(input, output) { }

shinyApp(ui, server)
