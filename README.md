# Kansas-Final
---
title: "Final Project"
author: "Lex Whitson"
date: "2025-12-01"
output: html_document
runtime: shiny
---

```{r}
setwd("C:\\Users\\Lex_l\\OneDrive\\DATA 824")
library(shiny)
library(plotly)
library(dplyr)
library(shinythemes)

county_data <- read.csv("KansasPopulation.csv")

ui <- fluidPage(
    theme = shinytheme("cosmo"),
    titlePanel("Kansas County Population Explorer"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("county", "Select County(s):",
                        choices = unique(county_data$County),
                        selected = "Kansas",
                        multiple = TRUE), 
            selectInput("year", "Select Year for Comparison:",
                        choices = unique(county_data$Year),
                        selected = 2018),
            sliderInput("yearRange", "Select Year Range for Trend:",
                        min = min(county_data$Year),
                        max = max(county_data$Year),
                        value = c(2010, 2020),
                        step = 1),
            actionButton("update", "Update")
                        
        ),
       mainPanel(
      h3("Population Comparison for Selected Year"),
      plotlyOutput("popPlot"),
      br(),
      
      h3("Population Trend Over Years"),
      plotlyOutput("scatterPlot"),
      br(),
      
      h3("Filtered Data"),
      tableOutput("dataTable")
        )
    )
)
      
server <- function(input, output, session) {
    barData <- eventReactive(input$update, {
        county_data %>%
            filter(County %>% input$county, Year == input$year)
    })
    scatterData <- reactive({
        county_data %>% 
            filter(County %in% input$county,
                   Year >= input$yearRange[1],
                   Year <= input$yearRange[2])
    }) 
    output$popPlot <- renderPlotly({
        data <- barData()
        req(nrow(data) > 0)
        
        plot_ly(
            data,
            x = ~County,
            y = ~Population,
            type = "bar",
            color = ~county_data
        ) %>%
            layout(
                title = paste("Population of Selected Counties in", input$year),
                yaxis = list(title = "Population")
            )
    })
    output$scatterPlot <- renderPlotly({
        data <- scatterData()
        req(nrow(data) > 0)
        
        plot_ly(
            data,
            x = ~Year,
            y = ~Population,
            color = ~County,
            type = 'scatter',
            mode = 'lines+markers'
        ) %>%
        layout(
            title = "Population Trend Over Years",
            xaxis = list(title = "Year"),
            yaxis = list(title = "Population",
                         tickformat = "d")
        )
    })
    output$dataTable <- renderTable({
        scatterData() %>% arrange(County, Year)
    })
    
}


shinyApp(ui, server)


```
