#' manage_renameColumn UI Function
#'
#' @description This module permits to modify the column name by adding units of measure as well
#' It is composed of a select input for variables, units of measure choiche and input text for new name.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @import shinydashboard
#' @importFrom shinyalert shinyalert
#' @importFrom rlang is_empty
#' @importFrom shinyFeedback feedbackWarning hideFeedback
#' @importFrom shinyjs disabled
mod_manage_renameColumn_ui <- function(id) {
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
          label = "Select column:",
          choices = NULL,
          selected = NULL,
          width = "100%"
        )
      ),
      column(
        width = 6,
        style = " padding-left:5px;padding-right:0px;",
        shiny::selectizeInput(
          ns("units"),
          label = "Units:",
          choices = list("NULL", "Energy" = list("kWh"), "Power" = list("kW") ), # improve
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
          uiOutput(ns("button_js"))
        )
      ),
      solidHeader = T, collapsible = T, collapsed = TRUE, width = 12,
      title = "Rename Column", status = "primary"
    )
  )
}

#' manage_renameColumn Server Function
#' @noRd
mod_manage_renameColumn_server <- function(id, infile = NULL, rvs_dataset) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # apply button
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
      choices <- colnames(rvs_dataset())
      # choices <- variable_list_with_class(rvs_dataset()) 
      updateSelectInput(session, "actual_name", choices = choices)
    })
    
    # Define the ReactiveValue to return : "toReturn"
    # with slots "dataset" & "trigger"
    toReturn <- reactiveValues(dataset = NULL,  trigger = 0)
    
    # (Re)load button
    observeEvent(input$new_name_submit, {
      validated <- TRUE 
      
      if (input$new_name == "") { shinyFeedback::feedbackWarning("new_name", TRUE, "Please choose  name")
        validated = FALSE} 
      
      # requires no special character in string and the name to be filled
      req(validated, !name_val(),  !is.null(infile())  ) #if validation passed do
    
      # add unit of measures if units selected
      unitsString <- if (input$units == "NULL") {NULL} else { gsub(" ", "", paste("[", input$units, "]")) } 
      
      # change column name if new name in input
      nameString <- if (input$new_name == "") {NULL} else {input$new_name}
      
      # combine strings in format *NAME* [*UNIT*]
      new_name_string <- paste(nameString, unitsString)
      
      # update toReturn reactiveValues
      toReturn$dataset   <- dplyr::rename(rvs_dataset(), !!new_name_string := !!input$actual_name )
      toReturn$trigger  <- toReturn$trigger + 1
    })
    return(toReturn)
    
  })
}

# this is the function that looks for classes and gets only first class
# used for selec tinputs
list_function <- function(x){
  a <- class(x)
  if (length(class(x)) == 1) {
    a
  } else {
    a[1]
  }
}


# # test module
# library(shiny)
# library(shinydashboard)
# ui <- dashboardPage(
#   dashboardHeader(disable = TRUE),
#   dashboardSidebar(disable = TRUE),
#   dashboardBody(
#     shinyjs::useShinyjs(),
#     shinyFeedback::useShinyFeedback(),
#     column(width = 4,
#            mod_manage_renameColumn_ui("manage_renameColumn_ui_1")),
#     column(width = 8,
#            DT::DTOutput("table"))
#   )
# )
# server <- function(input, output, session) {
# 
#   data_rv <- reactiveValues( df_tot = eDASH::data[,c(1:5)])                 # reactive value to store the loaded dataframes
# 
#   output$table <- DT::renderDT({
#     data_rv$df_tot
#   })
# 
#   data_rename <-  mod_manage_renameColumn_server("manage_renameColumn_ui_1",
#                                                  infile = reactive({TRUE}),
#                                                  rvs_dataset = reactive({ data_rv$df_tot })
#   )
#   # When applied function (data_mod2$trigger change) :
#   #   - Update rv$variable with module output "variable"
#   #   - Update rv$fun_history with module output "fun"
#   observeEvent(data_rename$trigger, {
#     req(data_rename$trigger > 0)
#     data_rv$df_tot <- data_rename$dataset                 # reactive value to store the loaded dataframes
#     a <- 2
#   })
# 
# 
# }
# 
# shinyApp(ui, server)

## To be copied in the UI
# mod_manage_renameColumn_ui("manage_renameColumn_ui_1")

## To be copied in the server
# mod_manage_renameColumn_server("manage_renameColumn_ui_1", rvs_dataset = reactive({ rvs }))
