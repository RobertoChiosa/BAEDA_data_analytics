#' manage_renameColumn UI Function
#'
#' @description This module permits to modify the column name by adding units of measure as well
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom shinyalert shinyalert
#' @importFrom rlang is_empty
mod_manage_renameColumn_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::checkboxInput(ns("modifyColumns_checkbox"), "Rename column", value = FALSE),
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == true", ns('modifyColumns_checkbox')),
      column(
        width = 6,
        style = "padding-left:0px; padding-right:5px;",
        shiny::selectInput(
          ns("columnName"),
          label = "Select column to modify:",
          choices = c(''),
          selected = NULL,
          width = "100%"
        )
      ),
      column(
        width = 6,
        style = " padding-left:5px;padding-right:0px;",
        shiny::selectizeInput(
          ns("units"),
          label = "Units of measurements:",
          choices = c("", "kW"),
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
            ns("new_columnName"),
            label = NULL,
            value = "",
            placeholder = "New column name...",
            width = "100%"
          ),
          shiny::actionButton(
            ns("new_columnName_submit"),
            label = NULL,
            icon = icon("refresh"),
            width = "100%"
          )
        )
      )
    ),
  )
}

#' manage_renameColumn Server Functions
#'
#' @noRd
mod_manage_renameColumn_server <- function(id, rvs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observe({
      updateSelectInput(session, inputId = 'columnName', choices = colnames(rvs))
    })
    
    observeEvent(input$new_columnName_submit, {
      unitsString <-
        if (input$units == "") {
          NULL
        } else {
          paste("[", input$units, "]")
        } # add unit of measures if units selected
      nameString <-
        if (input$new_columnName == "") {
          NULL
        } else {
          input$new_columnName
        } # change column name if new name in input
      columnString <-
        gsub(" ", "", paste(nameString, unitsString)) # combine strings in format *NAME* [*UNIT*]
      # change column name
      #if (!rlang::is_empty(columnString)) {
      # colnames(rvs)[colnames(rvs) == input$columnName] <- columnString
      colnames(rvs)[colnames(rvs) == input$columnName] <- nameString
      #}
      shinyalert::shinyalert(
        title = "Column successfully renamed",
        text = paste(
          "Name <b>",
          input$new_columnName,
          "</b> assigned to <b>",
          input$columnName,
          "</b>"
        ),
        type = "success",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = TRUE
      )
      rvs
    })
    
    
  })
}


# # test module
# ui <- fluidPage(
#   column(width = 4,
#          mod_manage_renameColumn_ui("manage_renameColumn_ui_1")),
#   column(width = 8,
#          dataTableOutput("table"))
# )
# 
# server <- function(input, output, session) {
#   
#   data_rv <- reactiveValues()                 # reactive value to store the loaded dataframes
#   data_rv$df_tot <- data
#   
#   output$table <- renderDataTable(data_rv$df_tot)
#   mod_manage_renameColumn_server("manage_renameColumn_ui_1", data_rv$df_tot)
# }
# 
# shinyApp(ui, server)

## To be copied in the UI
# mod_manage_renameColumn_ui("manage_renameColumn_ui_1")

## To be copied in the server
# mod_manage_renameColumn_server("manage_renameColumn_ui_1", rvs)
