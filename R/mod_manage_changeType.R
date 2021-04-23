#' manage_manage_changeType UI Function
#'
#' @description This module permits to modify the column name by adding units of measure as well
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @import magrittr
#' @import shinydashboard
#' @importFrom shinyalert shinyalert
#' @importFrom rlang is_empty
#' @importFrom dplyr mutate_at vars rename
mod_manage_manage_changeType_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # sets color of plus in button
    shiny::tags$style(".fa-refresh {color:white}"),
    # ui in a collapsible box
    box(
      shiny::selectInput(
        ns("actual_name"),
        label = "Select column:",
        choices = NULL,
        selected = NULL,
        width = "100%"
      ),
      shiny::selectizeInput(
        ns("type"),
        label = "Select type for conversion:",
        choices = c("as_integer" = "as_integer",
                    "as_numeric" = "as_numeric",
                    "as_factor" = "as_factor",
                    "as_character" = "as_character",
                    "month" = "month",
                    "wday" = "wday",
                    "as_mdy" = "as_mdy",
                    "as_dmy" = "as_dmy",
                    "as_ymd" = "as_ymd",
                    "as_ymd_hms" = "as_ymd_hms",
                    "as_ymd_hm" = "as_ymd_hm",
                    "as_mdy_hms" = "as_mdy_hms",
                    "as_mdy_hm" = "as_mdy_hm",
                    "as_dmy_hms" = "as_dmy_hms",
                    "as_dmy_hm" = "as_dmy_hm",
                    "as_hms" = "as_hms",
                    "as_hm" = "as_hm"
        ),
        # all admitted unit # implement with groups
        selected = NULL,
        multiple = FALSE,
        width = "100%"
      ),
      
      shiny::htmlOutput(  ns("type_preview_actual_title") ),
      shiny::verbatimTextOutput(  ns("type_preview_actual") ),
      
      shiny::htmlOutput(  ns("type_preview_future_title") ),
      shiny::verbatimTextOutput(  ns("type_preview_future") ),
      
      column(
        width = 12,
        style = "padding-left:0px; padding-right:0px;",
        shiny::splitLayout(
          cellWidths = c("80%", "20%"),
          shiny::textInput(
            ns("new_name"),
            label = NULL,
            value = "",
            placeholder = "(Optional) New name...",
            width = "100%"
          ),
          uiOutput(ns("button_js"))
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
mod_manage_manage_changeType_server <- function(id,infile = NULL,  rvs_dataset) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # apply button
    output$button_js <- renderUI({
      if (is.null(infile())) {
        shinyjs::disabled(
          shiny::actionButton(
            ns("new_name_submit"),
            label = NULL,
            icon = icon("refresh"),
            class = "btn-success",
            width = "100%"
          )
        )
      } else {
        shiny::actionButton(
          ns("new_name_submit"),
          label = NULL,
          icon = icon("refresh"),
          class = "btn-success",
          width = "100%"
        )
      }
    })
    
    
    # reactive value to evaluate the string name
    # it is true if some special values are found
    # observe this reactive value and show warning live
    name_val <- reactive({ grepl('[^[:alnum:]]', input$new_name) })
    observe( {
      # validate new name
      shinyFeedback::feedbackWarning("new_name", name_val(), "Please don't use special characters")
    })
    
    # Update selectInput according to dataset
    observe({
      req( !is.null(infile())  )
      
      # gets rvs_dataset as reactive value to solve update inputs
      # choices <- variable_list_with_class(rvs_dataset()) 
      choices <- colnames(rvs_dataset())
      updateSelectInput(session, "actual_name", choices = choices)
      
      # preview of type conversion
      output$type_preview_actual_title <- renderText({
        paste("Summary of <code>", input$actual_name,  "</code> as is (Actual)")
      })
      output$type_preview_actual <- renderPrint({
        
        variable <- as.data.frame(rvs_dataset()[, input$actual_name])
        summary( variable )
      })
      
      output$type_preview_future_title <- renderText({
        paste("Summary of <code>", input$actual_name, " </code> when converted (Future)")
      })
      output$type_preview_future <- renderPrint({
        variable <- as.data.frame( dplyr::mutate_at(rvs_dataset(), .vars = dplyr::vars(input$actual_name), .funs = input$type)[, input$actual_name]  ) 
        summary( variable)
      })
      
    })
    
    # Define the ReactiveValue to return : "toReturn"
    # with slots "rvs_dataset" & "trigger"
    toReturn <- reactiveValues(dataset = NULL,  trigger = 0)
    
    # (Re)load button
    observeEvent(input$new_name_submit, {
      
      
      toReturn$dataset <-  rvs_dataset() %>%
        dplyr::mutate_at( .vars = dplyr::vars(input$actual_name), .funs = input$type) 
      
      if (input$new_name != "") {
        toReturn$dataset <-  rvs_dataset() %>%
          dplyr::rename( !!input$new_name := !!input$actual_name )
      } 
  
      toReturn$trigger  <- toReturn$trigger + 1
    })
    
    return(toReturn)
    
  })
}
# 
# # test module
# library(shiny)
# library(shinydashboard)
# library(magrittr)
# library(lubridate)
# 
# source("./mod_manage_changeType_utils.R")
# ui <- dashboardPage(
#   dashboardHeader(disable = TRUE),
#   dashboardSidebar(disable = TRUE),
#   dashboardBody(
#     column(width = 4,
#            mod_manage_manage_changeType_ui("manage_manage_changeType_ui_1")),
#     column(width = 8,
#            DT::DTOutput("table"))
#   )
# )
# server <- function(input, output, session) {
#   
#   data_rv <- reactiveValues( df_tot = readRDS("/Users/robi/Desktop/dashboard-student-old/data/df_cooling_1.rds"))                 # reactive value to store the loaded dataframes
#   
#   
#   
#   output$table <- DT::renderDT({
#     data_rv$df_tot
#   })
#   
#   data_rename <-  mod_manage_manage_changeType_server("manage_manage_changeType_ui_1",
#                                                       rvs_dataset = reactive({data_rv$df_tot}),
#                                                       infile = reactive({TRUE}))
#   # When applied function (data_mod2$trigger change) :
#   #   - Update rv$variable with module output "variable"
#   #   - Update rv$fun_history with module output "fun"
#   observeEvent(data_rename$trigger, {
#     req(data_rename$trigger > 0)
#     data_rv$df_tot    <- data_rename$dataset
#   })
#   
#   
# }
# 
# shinyApp(ui, server)

## To be copied in the UI
# mod_manage_manage_changeType_ui("manage_manage_changeType_ui_1")

## To be copied in the server
# mod_manage_manage_changeType_server("manage_manage_changeType_ui_1", rvs_dataset = reactiveValues())



