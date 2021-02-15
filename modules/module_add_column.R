#' Do some stuff
#' @param a A nice description
#' @param b A great description
#' @return Final stuff
#' 
#' 
library(shiny)
library(shinyWidgets)
library(shinyalert)
library(datasets)
library(dplyr)

add_column_Input <- function(id){ 
  ns <- NS(id)
  tagList(
    textInput(ns("expression"), "IF ELSE expression", 
              placeholder = "if_else(CONDITION, TRUE, FALSE)",
              width = "100%"),
    actionButton(ns("add_columnName"), label = "Add")
  )
  
} 

add_column_Server <- function(input, output, session, dataframe){

  input$add_columnName
 dataframe
  
}


# APP <- function(){
#   
ui <- fluidPage(
  column(width = 4,   add_column_Input("one")   ),
  column(width = 4,   dataTableOutput("table")  )
)
server <- function(input, output, session) {
  
  df <- reactive({mtcars})
  
  df <- callModule(add_column_Server, "one", df) 
  
  
  output$table <- renderDataTable({   df()   })
  
} 
shinyApp(ui, server)

# }
# 
# APP()
