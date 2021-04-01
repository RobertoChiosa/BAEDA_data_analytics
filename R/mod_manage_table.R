#' manage_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @importFrom DT datatable DTOutput renderDT
#' @importFrom shiny NS tagList
mod_manage_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    DT::DTOutput(ns("table"))
  )
}

#' manage_table Server Functions
#'
#' @noRd
mod_manage_table_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$table <- DT::renderDT({
      DT::datatable(
        data,
        selection = "none",
        rownames = FALSE,
        style = "bootstrap",
        filter = fbox,
        escape = FALSE,
        ## must use fillContainer = FALSE to address
        ## see https://github.com/rstudio/DT/issues/367
        ## https://github.com/rstudio/DT/issues/379
        # fillContainer = FALSE,
        ## works with client-side processing
        extensions = "KeyTable",
        options = list(
          keys = TRUE,
          search = list(regex = TRUE),
          columnDefs = list(
            list(
              orderSequence = c("desc", "asc"),
              targets = "_all"
            ),
            list(className = "dt-center", targets = "_all")
          ),
          autoWidth = TRUE,
          processing = FALSE,
          pageLength = 10,
          lengthMenu = list(c(5, 10, 25, 50,-1), c("5", "10", "25", "50", "All"))
        )
      )
    })
    
    
    
  })
}

## To be copied in the UI
# mod_manage_table_ui("manage_table_ui_1")

## To be copied in the server
# mod_manage_table_server("manage_table_ui_1")
