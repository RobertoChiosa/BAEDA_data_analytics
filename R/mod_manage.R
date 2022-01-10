#' manage UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom shinyWidgets searchInput pickerInput updatePickerInput
mod_manage_ui_input <- function(id) {
  ns <- NS(id)
  tagList(box(
    width = 12,
    title = shiny::HTML(
      "Data Wrangling options
       <a
         id=\"button_wrangling\"
         data-toggle=\"tooltip\"
         title=\" Data Handling options.\"
         class=\"dropdown\">
         <i class=\"fa fa-info-circle\"></i>
       </a>"
    ),
    p(
      "This section permits to perform data wrangling on the loaded dataset."
    ),
    # mod_manage_ui_input("manage_ui_1"),
    # shinyWidgets::pickerInput(
    #   ns("keepColumnName"),
    #   label = "Select column to keep:",
    #   choices = NULL,
    #   # all available columns in the original dataframe
    #   selected = NULL,
    #   # by default all selected
    #   options = list(`actions-box` = TRUE),
    #   multiple = T
    # )
  ))
  
}

#' manage UI output
#'
#' @noRd
mod_manage_ui_output <- function(id) {
  ns <- NS(id)
  
  # 2.5) Display datatable ----------------------------------------------------------------------
  # displays through datatable function the actual selected dataframe
  tagList(# fluidRow(
    #   # 2.3.1) number of rows in the current dataframe value box
    #   # valueBoxOutput(ns("valueBox_rows"), width = 6),
    #   # 2.3.2) number of columns in the current dataframe value box
    #   # valueBoxOutput(ns("valueBox_columns"), width = 6)
    # ),
    
    wellPanel(DT::DTOutput(outputId = ns("table"))))
}


#' manage Server Functions
#'
#' @noRd
mod_manage_server <- function(id,  infile = NULL, rvs_dataset) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # # Update selectInput according to dataset
    # observe({
    #   req( !is.null(infile())  )
    #   # gets rvs_dataset_dataset as reactive value to solve update inputs
    #   choices <- colnames(rvs_dataset())
    #   # choices <- variable_list_with_class(rvs_dataset_dataset())
    #   updatePickerInput(session, "keepColumnName", choices = choices, selected = choices)
    # })
    
    
    # # 2.3) Value boxes ----------------------------------------------------------------------
    # # 2.3.1) number of rows in the current dataframe value box
    # output$valueBox_rows <- renderValueBox({
    #   req( !is.null(infile())  )
    #   valueBox(
    #     length(input$table_rows_all),
    #     "Number of rows",
    #     icon = icon("arrows-alt-v"),
    #     color = "orange"
    #   )
    # })
    #
    # # 2.3.2) number of columns in the current dataframe value box
    # output$valueBox_columns <- renderValueBox({
    #   req( !is.null(infile())  )
    #
    #   valueBox(
    #     length(input$keepColumnName),
    #     "Number of columns",
    #     icon = icon("arrows-alt-h"),
    #     color = "blue"
    #   )
    # })
    
    # 2.5) Display datatable ----------------------------------------------------------------------
    # displays through datatable function the actual selected dataframe
    output$table <- DT::renderDT({
      req(!is.null(infile()))
      DT::datatable(
        # rvs_dataset()[,input$keepColumnName],
        rvs_dataset(),
        selection = "none",
        rownames = FALSE,
        style = "bootstrap",
        filter = "top",
        #fbox
        escape = FALSE,
        ## must use fillContainer = FALSE to address
        ## see https://github.com/rstudio/DT/issues/367
        ## https://github.com/rstudio/DT/issues/379
        fillContainer = FALSE,
        ## works with client-side processing
        extensions = "KeyTable",
        options = list(
          # autoWidth = TRUE, # permits to adapt the columns to the width of the box
          # scrollX = 500, # permits to scroll along x
          # deferRender = TRUE,
          # scroller = TRUE,
          # dom = 'lBfrtip',
          # fixedColumns = list(leftColumns = 2),
          # fixedHeader = TRUE
          keys = TRUE,
          autoWidth = FALSE,
          # permits to adapt the columns to the width of the box
          scrollX = 500,
          # permits to scroll along x
          search = list(regex = TRUE),
          columnDefs = list(
            list(
              orderSequence = c("desc", "asc"),
              targets = "_all"
            ),
            list(className = "dt-center", targets = "_all")
          ),
          processing = FALSE,
          pageLength = 10,
          lengthMenu = list(c(5, 10, 25, 50, -1), c("5", "10", "25", "50", "All"))
        )
      )
      
      # isInt <- sapply(rvs_dataset(), is.integer)
      # isDbl <- sapply(rvs_dataset(), is.double)
      #
      # ## rounding as needed
      # if (sum(isDbl) > 0)
      #   dt_table <- DT::formatRound(dt_table, colnames(rvs_dataset())[isDbl], digits = 2)
      # if (sum(isInt) > 0)
      #   dt_table <- DT::formatRound(dt_table, colnames(rvs_dataset())[isInt], digits = 0)
      
    })
    
  })
}
#
# # test module
# library(shiny)
# library(shinydashboard)
# library(shinyWidgets)
# library(magrittr)
# library(dplyr)
#
# ui <- dashboardPage(
#   dashboardHeader(disable = TRUE),
#   dashboardSidebar(disable = TRUE),
#   dashboardBody(
#     column(width = 4,
#            mod_manage_ui_input("mm")),
#     column(width = 8,
#            mod_manage_ui_output("mm"))
#   )
# )
# server <- function(input, output, session) {
#
#   data_rv <- reactiveValues( df_tot = eDASH::data[,c(1:4)])                 # reactive value to store the loaded dataframes
#
#   mod_manage_server("mm",
#                     infile = reactive({TRUE}),
#                     rvs_dataset = reactive({ data_rv$df_tot })
#   )
#   # # When applied function (data_mod2$trigger change) :
#   # #   - Update rv$variable with module output "variable"
#   # #   - Update rv$fun_history with module output "fun"
#   # observeEvent(data_add$trigger, {
#   #   req(data_add$trigger > 0)
#   #   data_rv$df_tot    <- data_add$dataset
#   # })
# }
#
# shinyApp(ui, server)

## To be copied in the UI
# mod_manage_ui("manage_ui_1")

## To be copied in the server
# mod_manage_server("manage_ui_1", reactive({ rvs_dataset}) )
