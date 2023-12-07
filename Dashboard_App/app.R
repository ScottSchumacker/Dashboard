# Scott Schumacker
# R Shiny dashboard showcasing different visualizations

# Loading libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(renv)
library(shinyalert)

# UI
ui <- dashboardPage(

  dashboardHeader(title = "NETFLIX INSIGHTS"),
  
  # Sidebar
  dashboardSidebar(
    sidebarMenu(style = "position: fixed; width: 210px;",
      # Selector for countries UI
      uiOutput("countryChoose"),
      actionButton("aboutButton", "About", width = "180px")
    )
  ),
  # Body
  dashboardBody(
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    # Top ten countries bar plot UI  
    box(plotlyOutput("topTenOut"), width = "100%", title = 
          "Top Ten Countries by Number of Releases (2008-2021)"),
      
    fluidRow(
             # Interactive point plot UI
             box(plotlyOutput("releaseTime"), 
                 title = "Number of Releases by Year"),
          
             # Interactive donut plot UI
             box(plotOutput("donutUI"), title = "Type of release")),
    
    fluidRow(
      # Interactive data table 
      box(dataTableOutput("tableUI"), title = "Table of releases",
          width = "100%")
    )
  )
)

# Server
server <- function(input, output) {
  
  # Creating a modal pop up at the start
  shinyalert("Welcome!", "Welcome to my shiny dashboard! - Scott",
             type = "info", closeOnClickOutside = TRUE)
  
  # Loading in Netflix Data set (add code)
  netflixTitles <- netflix_titles3
  # Extracting only the year
  netflixTitles$dateadded <- substr(netflixTitles$date_added, 1, 4)
  # Processing Netflix data for top ten visualization
  countryTable <- netflixTitles %>% 
    group_by(country) %>% 
    summarise(n = n()) %>% 
    filter(n >= 106) %>% 
    na.omit()
  
  # Renaming columns and tidying DF
  colnames(countryTable) <- c("Country", "Releases")
  countryTable$Country <- as.factor(countryTable$Country)
  
  # Creating output for UI selector
  output$countryChoose <- renderUI({
    selectInput(
      "countrySelect",
      "Country",
      countryTable$Country,
      selected = "United States"
    )
  })
  
  # Creating plot output for top ten bar plot
  top10 <- ggplot(data = countryTable, aes(x = Country, y = Releases)) +
    geom_bar(stat = "identity", fill = "red", color = "black", alpha = 0.8)
  top10
  top10Interactive <- ggplotly(top10)
  output$topTenOut <- renderPlotly(top10Interactive)
  
  # Creating DF for interactive point plot
  pointDF <- netflixTitles %>% 
    select(type, country, date_added)
  
  # Tidying DF for interactive point plot
  pointDF$date_added <- as.Date(pointDF$date_added)
  pointDF$date_added <- format(as.Date(pointDF$date_added, 
                                       format="%d/%m/%Y"), "%Y")
  
  # Creating DF for interactive donut chart
  donutDF <- netflixTitles %>% 
    select(type, country, rating)
  
  output$donutUI <- renderPlot({
    donutDF2 <- donutDF %>% 
      filter(country == input$countrySelect) %>% 
      group_by(type) %>% 
      summarise(total = n()) %>% 
      na.omit()
    
    donutDF2$fraction <- donutDF2$total / sum(donutDF2$total)
    donutDF2$ymax <- cumsum(donutDF2$fraction)
    donutDF2$ymin <- c(0, head(donutDF2$ymax, n=-1))
    donutDF2$labelPosition <- (donutDF2$ymax + donutDF2$ymin) / 2
    donutDF2$label <- paste0(donutDF2$type, "\n frequency: ", 
                             round(donutDF2$fraction, 2))
    donutP <- ggplot(donutDF2, aes(ymax = ymax, ymin = ymin, xmax = 4, 
                                   xmin = 3, fill = type)) +
      geom_rect() +
      geom_label(x=3.5, aes(y=labelPosition, label = label), size=6) +
      scale_fill_brewer(palette = "RdGy") +
      coord_polar(theta = "y") +
      xlim(c(2, 4)) +
      theme_void() +
      theme(legend.position = "none") +
      scale_fill_manual(values = c("red", "#f9f9f9"))
    donutP
  })
  
  output$releaseTime <- renderPlotly({
    
    pointDF2 <- pointDF %>% 
      filter(country == input$countrySelect) %>% 
      group_by(date_added) %>% 
      summarise(n = n()) %>% 
      na.omit()
    
    # Creating plot output for interactive point plot
    p2 <- ggplot(pointDF2, aes(x = date_added, y = n)) +
      geom_point(size = 5, alpha = 0.8, color = "red") + xlab("Year") +
      ylab("Number of Releases") + geom_smooth(method = "lm")
    ggplotly(p2)
  })
  
  # Creating copy of netlifxDF for data table
  newTableDF <- netflix_titles3 %>% 
    select(country, type, title, date_added, rating, director)

  output$tableUI <- renderDataTable({
    finalTableDF <- newTableDF %>% 
      filter(country == input$countrySelect)
    finalTableDF
  })
  
  # Creating observent event for about button
  observeEvent(input$aboutButton, {
    shinyalert("About", "This application was created with the following
               primary dependencies: shiny, shinydashboard, ggplot2, dplyr",
               type = "info")
  })
  
}

shinyApp(ui, server)
