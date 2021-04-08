#' manage_addColumn UI Function
#'
#' @description This module permits to modify the column name by adding units of measure as well
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @import shinydashboard
#' @import dplyr magrittr
#' @importFrom shinyFeedback feedbackWarning hideFeedback
mod_manage_addColumn_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # sets color of plus in button
    shiny::tags$style(".fa-refresh {color:white}"),
    # ui in a collapsible box
    box(
      h5("Please compose the expression"),
      column(width = 5, style = "padding-left:0px; padding-right:10px;",
             selectInput(label = NULL, ns("condition_LHS"), choices = NULL )
      ),
      column(width = 2, style = "padding-left:0px; padding-right:10px;",
             selectInput(label = NULL, ns("condition_operator"), choices = c(">",">=","<", "<=","==") )
      ),
      column(width = 5, style = "padding-left:0px; padding-right:0px;",
             textInput(label = NULL, ns("condition_RHS"), placeholder = "(e.g., 1200)", value = NULL)
      ),
      column(width = 6, style = "padding-left:0px; padding-right:10px;",
             textInput(label = NULL, ns("condition_true"), placeholder = "IF TRUE", value = NULL)
      ),
      column(width = 6, style = "padding-left:0px; padding-right:10px;",
             textInput(label = NULL, ns("condition_false"), placeholder = "ELSE (FALSE)", value = NULL)
      ),
      verbatimTextOutput(ns("expression")),
      column(
        width = 12,
        style = "padding-left:0px; padding-right:0px;",
        shiny::splitLayout(
          cellWidths = c("80%", "20%"),
          shiny::textInput(
            ns("add_columnName"),
            label = NULL,
            value = "",
            placeholder = "Column name...",
            width = "100%"
          ),
          shiny::actionButton(
            ns("add_submit"),
            label = NULL,
            icon = icon("refresh"),
            class = "btn-success",
            width = "100%"
          )
        )
      ),
      solidHeader = T, collapsible = T, collapsed = TRUE, width = 12,
      title = "Add Column", status = "primary"
    )
  )
}

#' manage_addColumn Server Functions
#'
#' @noRd
mod_manage_addColumn_server <- function(id, rvs_dataset) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Update selectInput according to dataset
    observe({
      choices <- colnames(rvs_dataset)
      updateSelectInput(session, "condition_LHS", choices = choices)
    })
    
    # creates the expression
    observe(
      output$expression <- renderText(
        paste(
          "if_else(",
          input$condition_LHS, input$condition_operator, input$condition_RHS, ",",
          input$condition_true,",", input$condition_false, ")"
        )
        
      )
    )
    
    # Define the ReactiveValue to return : "toReturn"
    # with slots "rvs_dataset" & "trigger"
    toReturn <- reactiveValues(dataset = NULL,  trigger = 0)
    
    # (Re)load button
    observeEvent(input$add_submit, {
      
      validated <- TRUE                          # validated is TRUE if the value is acceptable FALSE if not acceptable
      
      # hide previous feedbacks
      shinyFeedback::hideFeedback("condition_false")
      shinyFeedback::hideFeedback("condition_true")
      shinyFeedback::hideFeedback("condition_RHS")
      shinyFeedback::hideFeedback("add_columnName")
      
      if (input$condition_false == "") { shinyFeedback::feedbackWarning("condition_false", TRUE, "Please fill")
        validated = FALSE} 
      if (input$condition_true == "") { shinyFeedback::feedbackWarning("condition_true", TRUE, "Please fill")
        validated = FALSE} 
      if (input$condition_RHS== "") { shinyFeedback::feedbackWarning("condition_RHS", TRUE, "Please fill")
        validated = FALSE} 
      if (input$add_columnName == "") { shinyFeedback::feedbackWarning("add_columnName", TRUE, "Please fill")
        validated = FALSE} 
      
      req(validated) #if validation passed do
      
      expression_toeval <- paste(
        "if_else(",
        input$condition_LHS, input$condition_operator, input$condition_RHS, ",",
        input$condition_true,",", input$condition_false, ")"
      )
      
      toReturn$dataset    <- dplyr::mutate( rvs_dataset, 
                                             !!input$add_columnName := as.factor( eval(parse(text = expression_toeval))) )
      toReturn$trigger    <- toReturn$trigger + 1
    })
    
    return(toReturn)
    
  })
}


# test module
library(shiny)
library(shinydashboard)
library(shinyFeedback)
library(magrittr)
library(dplyr)
ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    column(width = 4,
           mod_manage_addColumn_ui("manage_addColumn_ui_1")),
    column(width = 8,
           DT::DTOutput("table"))
  )
)
server <- function(input, output, session) {

  data_rv <- reactiveValues( df_tot = eDASH::data[,c(1:4)])                 # reactive value to store the loaded dataframes

  output$table <- DT::renderDT({
    data_rv$df_tot
  })

  data_add <-  mod_manage_addColumn_server("manage_addColumn_ui_1", rvs_dataset = data_rv$df_tot)
  # When applied function (data_mod2$trigger change) :
  #   - Update rv$variable with module output "variable"
  #   - Update rv$fun_history with module output "fun"
  observeEvent(data_add$trigger, {
    req(data_add$trigger > 0)
    data_rv$df_tot    <- data_add$dataset
  })
}

shinyApp(ui, server)

## To be copied in the UI
# mod_manage_addColumn_ui("manage_addColumn_ui_1")

## To be copied in the server
# mod_manage_addColumn_server("manage_addColumn_ui_1", rvs_dataset = reactiveValues())
