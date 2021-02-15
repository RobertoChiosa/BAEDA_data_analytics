library(shiny)
library(dplyr)

dataframeInput <- function(id, label = "Select columns", dataframe){ 
  width_percent <- "100%"
  ns <- NS(id)
  tagList(
    selectInput(ns("columns"), label, width = width_percent, 
                choices = colnames(dplyr::select_if( dataframe, is.numeric)), multiple = T, selected = 1),
  )
} 

dataframeOutput<- function(id){ 
  ns <- NS(id)
  tableOutput(ns("table"))
}

dataframeServer <- function(input, output, session, dataframe){  
  
  output$table <- renderTable({
    dataframe[,input$columns]
  })
  
  # return
  return( reactive({ dataframe[,input$columns] }) )
}


# APP <- function(){
  
  ui <- fluidPage(
    column(width = 4,  uiOutput("UIINPUT"), textOutput("cols")
    ),
    column(width = 4, dataframeOutput("iris") )
    
  )
  server <- function(input, output, session) {
    
    # global reactive values
    data <- reactiveValues(df = iris)
    
    # output UI to filter
    output$UIINPUT <- renderUI({  dataframeInput("iris", label = "Colonne", data$df) })
    
    data$result <- callModule(dataframeServer, "iris", data$df)
    
    output$cols <- renderText({
      dim(data$result)[1]
    })
    
  } 
  shinyApp(ui, server)
# }
# 
# APP()
