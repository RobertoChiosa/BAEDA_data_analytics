#################################################################################
###############            Copyright © BAEDA Lab 2021             ###############
###############                     -------                       ###############
###############                  Roberto Chiosa                   ###############
###############             roberto.chiosa@polito.it              ###############
#################################################################################

library(shiny)
library(plotly)
library(lubridate)
library(magrittr)
library(dplyr)
library(shinycssloaders)

# this first UI is composed of a plot button and accepts the input variables
# requires a dataframe as input
lineplotInput <- function(id, dataframe){
  ns <- NS(id)
  width_percent <- "100%"
  # function to find coldates column find datetime row
  date_max = max( dataframe[, "Timestamp"] )
  date_min = min( dataframe[, "Timestamp"] )
  
  tagList(
    actionButton(ns("plot_button"), "Plot", class = "btn-success", icon = icon("chart-bar"), width = width_percent),
    dateRangeInput("daterange", "Date range:",
                   start  = if_else(date_max-date_min > lubridate::years(1), date_max- lubridate::days(365), date_min),
                   end    = date_max,
                   min    = date_min,
                   max    = date_max,
                   format = "yyyy-mm-dd",
                   separator = " - ", 
                   width = width_percent),
    selectInput(ns("variableY"), label = "Variable Y:", width = width_percent, 
                choices = colnames(dplyr::select_if( dataframe, is.numeric))), # chose numerical variable
    selectInput(ns("colorvariable"), label = "Color Variable:", width = width_percent,
                choices = c("None", colnames(dplyr::select_if( dataframe , is.factor))) ),
  )
}

# this second UI plots the output
# needs no input, calls the server output
lineplotOutput <- function(id){
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("plot"))
  )
}

# this server function 
# needs datafrae input treated as reactive
# need to find column timestamp

lineplotServer <- function(input, output, session, dataframe){
  
  output$plot <- renderPlotly({ 
    req(input$plot_button)  # requires the plot button to be pressed
    isolate({               # avoid reactivity of parameters
      data_plot <- dataframe() # turns reactive to static
      plot <- ggplot(data =  data_plot,
                     mapping =  aes(x = as.POSIXct(Timestamp, "%Y-%m-%d %H:%M:%S", tz = "Europe/Rome"),
                                    y =  data_plot[,input$variableY],
                                    text = paste(
                                      # ' Time:', format(Date_Time, "%H:%M:%S"), 
                                      #            '<br> Date: ', Date, '<br>',
                                      input$variableY, ':', data_plot[,input$variableY]
                                    ), 
                                    group = 1, # solve ggplotly problem
                                    color = if (input$colorvariable == "None") {NULL} else { data_plot[,input$colorvariable]} 
                     )
      ) + 
        geom_line(na.rm = TRUE)+
        theme_bw() +
        labs( x = "Date Time", y = input$variableY)
      
      
      plot <- ggplotly(plot, tooltip = c("text"), dynamicTicks = TRUE) 
      
      plot %>%
        layout(hovermode = "x unified",
               showlegend = T,
               autosize = T,
               # legend = list(orientation = 'h',
               #               bgcolor = "#E2E2E2",
               #               bordercolor = "#FFFFFF",
               #               borderwidth = 2),
               yaxis = list( autorange = TRUE),
               xaxis = list( autorange = TRUE,
                             rangeselector =  list(
                               buttons = list( 
                                 list(count = 1, label = 'All', step = 'all'),
                                 list(count = 1, label = 'Year', step = 'year', stepmode = 'backward'),
                                 list(count = 6, label = 'Semester', step = 'month', stepmode = 'backward'),
                                 list(count = 1, label = 'Month', step = 'month', stepmode = 'backward'),
                                 list(count = 7, label = 'Week', step = 'day', stepmode = 'backward'),
                                 list(count = 6, label = '6 Hours', step = 'hour', stepmode = 'backward')
                               ))
               )
        )
    }) # end isolate
  })
}

## this function permits to test the module outside the main app
testApp <- function(){
  ui <- fluidPage(
    column(width = 3, uiOutput("UI1") %>% withSpinner()) ,
    column(width = 9, uiOutput("UI2") %>% withSpinner())
  )
  
  server <- function(input, output, session){
    df <- reactiveValues()
    df$data <- readRDS("./modules/data.rds")
    
    output$UI1 <- renderUI(lineplotInput("input", df$data ) )
    output$UI2 <- renderUI(lineplotOutput("input") )
    
    callModule(lineplotServer, "input", reactive({df$data}))
  }
  
  shinyApp(ui,server)
}

testApp()
