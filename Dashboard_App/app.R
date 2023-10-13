# Scott Schumacker
# R Shiny dashboard showcasing different visualizations

# Loading libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(data.table)

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
  
  # Transforming the population data from wide format to long format
  long <- melt(setDT(world_pop), id.vars = c("country"), variable.name = "year")
  colnames(long) <- c("country", "year", "population")
  
  # Creating a reactive subset of data based on user input
  worldPlotData <- reactive({
    long %>% 
      filter(country == input$countryName)
  })
  
  # Creating the population table
  output$popTable <- renderDataTable({
    worldPlotData()
  })
  
}

shinyApp(ui, server)
