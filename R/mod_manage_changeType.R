#' manage_manage_changeType UI Function
#'
#' @description This module permits to modify the column name by adding units of measure as well
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @import shinydashboard
#' @importFrom shinyalert shinyalert
#' @importFrom rlang is_empty
mod_manage_manage_changeType_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # sets color of plus in button
    shiny::tags$style(".fa-refresh {color:white}"),
    # ui in a collapsible box
    box(
      column(
        width = 6,
        style = "padding-left:0px; padding-right:5px;",
        shiny::selectInput(
          ns("actual_name"),
          label = "Select column to modify:",
          choices = NULL,
          selected = NULL,
          width = "100%"
        )
      ),
      column(
        width = 6,
        style = " padding-left:5px;padding-right:0px;",
        shiny::selectizeInput(
          ns("type"),
          label = "Type:",
          choices = c("as.factor", 
                      "as.numeric", 
                      "as.integer",
                      "as.character",
                      "as.Date",
                      "as.POSIXct"
                      ),
          # all admitted unit # implement with groups
          selected = NULL,
          multiple = FALSE,
          options = list(maxOptions = 5),
          width = "100%"
        )
      ), 
      column(
        width = 12,
        style = "padding-left:0px; padding-right:0px;",
        shiny::splitLayout(
          cellWidths = c("80%", "20%"),
          shiny::textInput(
            ns("new_name"),
            label = NULL,
            value = "",
            placeholder = "New name...",
            width = "100%"
          ),
          shiny::actionButton(
            ns("new_name_submit"),
            label = NULL,
            icon = icon("refresh"),
            class = "btn-success",
            width = "100%"
          )
        )
      ),
      solidHeader = T, collapsible = T, collapsed = TRUE, width = 12,
      title = "Change Column Type", status = "primary"
    )
  )
}

#' manage_manage_changeType Server Functions
#'
#' @noRd
mod_manage_manage_changeType_server <- function(id, rvs_dataset) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Update selectInput according to dataset
    observe({
      choices <- colnames(rvs_dataset)
      updateSelectInput(session, "actual_name", choices = choices)
    })
    
    # Define the ReactiveValue to return : "toReturn"
    # with slots "rvs_dataset" & "trigger"
    toReturn <- reactiveValues(dataset = NULL,  trigger = 0)
    
    # (Re)load button
    observeEvent(input$new_name_submit, {
      
      toReturn$dataset <- rvs_dataset %>%
        dplyr::mutate_at(.vars = vars(input$actual_name), .funs = input$type)
      
      toReturn$trigger  <- toReturn$trigger + 1
    })
    
    return(toReturn)
    
  })
}


# test module
library(shiny)
library(shinydashboard)
ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    column(width = 4,
           mod_manage_manage_changeType_ui("manage_manage_changeType_ui_1")),
    column(width = 8,
           DT::DTOutput("table"))
  )
)
server <- function(input, output, session) {
  
  data_rv <- reactiveValues( df_tot = eDASH::data)                 # reactive value to store the loaded dataframes
  
  output$table <- DT::renderDT({
    data_rv$df_tot
  })
  
  data_rename <-  mod_manage_manage_changeType_server("manage_manage_changeType_ui_1", rvs_dataset = data_rv$df_tot)
  # When applied function (data_mod2$trigger change) :
  #   - Update rv$variable with module output "variable"
  #   - Update rv$fun_history with module output "fun"
  observeEvent(data_rename$trigger, {
    req(data_rename$trigger > 0)
    data_rv$df_tot    <- data_rename$dataset
  })
  
  
}

shinyApp(ui, server)

## To be copied in the UI
# mod_manage_manage_changeType_ui("manage_manage_changeType_ui_1")

## To be copied in the server
# mod_manage_manage_changeType_server("manage_manage_changeType_ui_1", rvs_dataset = reactiveValues())

