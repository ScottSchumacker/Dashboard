# Scott Schumacker
# R Shiny dashboard showcasing different visualizations

# Loading libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(data.table)
library(plotly)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Scott Schumacker"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("World Population", tabName = "worldPop", icon = icon("signal")),
      menuItem("Netflix Insights", tabName = "netflix", icon = icon("signal")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "worldPop",
        box(title = "Controls",
          selectInput(inputId = "countryName", "Country",
                      choices = long$country)
        ),
        box(dataTableOutput("popTable"), height = "500px")
      ),
      tabItem(tabName = "netflix",
        box(plotlyOutput("top_ten_out"), width = "100%", title = "Top Ten Countr
            ies by Number of Releases (2014-2021)")
      )
    )
  )
)

# Server
server <- function(input, output) {
  # Loading in Netflix Dataset
  # Processing Netflix data for top ten visualization
  country_table <- netflix_titles %>% 
    group_by(country) %>% 
    summarise(n = n()) %>% 
    filter(n >= 106) %>% 
    na.omit()
  
  colnames(country_table) <- c("Country", "Releases")
  country_table$Country <- as.factor(country_table$Country)
  top10 <- ggplot(data = country_table, aes(x = Country, y = Releases)) +
    geom_bar(stat = "identity", fill = "red", color = "black", alpha = 0.7)
  top10
  top10_interactive <- ggplotly(top10)
  output$top_ten_out <- renderPlotly(top10_interactive)
  # Transforming the population data from wide format to long format
  long <- melt(setDT(world_pop), id.vars = c("country"), variable.name = "year")
  colnames(long) <- c("country", "year", "population")
  # Creating a reactive subset of data based on user input
  world_plot_data <- reactive({
    long %>%
      filter(country == input$countryName)
  })
  # Creating the population table
  output$popTable <- renderDataTable({
    world_plot_data()
  }, options = list(
    pageLength = 10
  ))
}

shinyApp(ui, server)
