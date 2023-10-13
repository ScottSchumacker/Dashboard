# Scott Schumacker
# R Shiny dashboard showcasing different visualizations

# Loading libraries
library(shiny)
library(shinydashboard)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Scott Schumacker"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("World Population", tabName = "worldPop", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "worldPop",
          box(title = "Controls",
              selectInput(inputId = "countryName", "Country", choices = world_pop$country)
              )    
    )
  ))
)

# Server
server <- function(input, output) { }

shinyApp(ui, server)
