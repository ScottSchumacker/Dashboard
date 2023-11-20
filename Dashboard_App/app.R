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
  # Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("World Population", tabName = "worldPop", icon = icon("signal")),
      menuItem("Netflix Insights", tabName = "netflix", icon = icon("signal")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  # Body
  dashboardBody(
    tabItems(
      tabItem(tabName = "netflix",
        box(plotlyOutput("topTenOut"), width = "100%", title = "Top Ten Countr
            ies by Number of Releases (2008-2021)"),
        box(plotOutput("releaseTime"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  # Loading in Netflix Data set (add code)
  netflixTitles <- netflix_titles3
  # Extracting only the year
  netflixTitles$dateadded <- substr(netflixTitles$date_added, 1, 4)
  # Cleaning Netflix data set (add code)
  # Processing Netflix data for top ten visualization
  countryTable <- netflixTitles %>% 
    group_by(country) %>% 
    summarise(n = n()) %>% 
    filter(n >= 106) %>% 
    na.omit()
  
  colnames(countryTable) <- c("Country", "Releases")
  countryTable$Country <- as.factor(countryTable$Country)
  
  top10 <- ggplot(data = countryTable, aes(x = Country, y = Releases)) +
    geom_bar(stat = "identity", fill = "red", color = "black", alpha = 0.7)
  top10
  top10Interactive <- ggplotly(top10)
  output$topTenOut <- renderPlotly(top10Interactive)
  
  testDF <- netflixTitles %>% 
    select(type, country, date_added) %>% 
    filter(country == "United States")
  
  testDF2 <- testDF %>% 
    group_by(date_added) %>% 
    summarise(n = n()) %>% 
    na.omit()
  
  p2 <- ggplot(testDF2, aes(x = date_added, y = n)) +
    geom_point(size = 4, alpha = 0.6) + xlab("Year") +
    ylab("Number of Releases") + geom_smooth(method = "lm")
  
  output$releaseTime <- renderPlot(p2)

  # Creating the population table
  output$popTable <- renderDataTable({
    world_plot_data()
  }, options = list(
    pageLength = 10
  ))
}

shinyApp(ui, server)
