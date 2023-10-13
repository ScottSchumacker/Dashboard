# Scott Schumacker
# R Shiny dashboard showcasing different visualizations

# Loading libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Scott Schumacker"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("World Population", tabName = "worldPop", icon = icon("signal")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "worldPop",
          box(title = "Controls",
              selectInput(inputId = "countryName", "Country", choices = long$country)
              ),
          dataTableOutput("popTable")
    )
  ))
)

# Server
server <- function(input, output) {
  
  worldPlotData <- reactive({
    long %>% 
      filter(country == input$countryName)
  })
  
  output$popTable <- renderDataTable({
    worldPlotData()
  })
  
}

shinyApp(ui, server)
