#' manage UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import shiny
#' @importFrom shinyWidgets searchInput pickerInput 

mod_manage_ui_input <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("keepColumns")),
    
    # 2.3) Value boxes ----------------------------------------------------------------------
    # 2.3.1) number of rows in the current dataframe value box
    valueBoxOutput(ns("valueBox_rows"), width = 12),
    # 2.3.2) number of columns in the current dataframe value box
    valueBoxOutput(ns("valueBox_columns"), width = 12)
  )
  
}

#' manage UI output
#'
#' @noRd 
mod_manage_ui_output <- function(id) {
  ns <- NS(id)
  # 2.5) Display datatable ----------------------------------------------------------------------
  # displays through datatable function the actual selected dataframe
  tagList(wellPanel(DT::DTOutput(ns("table"))))
}


#' manage Server Functions
#'
#' @noRd 
mod_manage_server <- function(id, rvs){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # 2.3) Value boxes ----------------------------------------------------------------------
    # 2.3.1) number of rows in the current dataframe value box
    output$valueBox_rows <- renderValueBox({
      #req(input$file) # requires that a file is loaded
      valueBox(
        length(input$table_rows_all),
        "Number of rows",
        icon = icon("arrows-alt-v"),
        color = "orange"
      )
    })
    # 2.3.2) number of columns in the current dataframe value box
    output$valueBox_columns <- renderValueBox({
      #req(input$file) # requires that a file is loaded
      valueBox(
        length(input$keepColumnName),
        "Number of columns",
        icon = icon("arrows-alt-h"),
        color = "blue"
      )
    })
    # 2.5) Display datatable ----------------------------------------------------------------------
    # displays through datatable function the actual selected dataframe
    output$table <- DT::renderDT({
      
      dt_table <- DT::datatable(
        #rvs[,input$keepColumnName],
        rvs,
        selection = "none",
        rownames = FALSE,
        style = "bootstrap",
        filter = "top", #fbox
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
          autoWidth = FALSE, # permits to adapt the columns to the width of the box
          scrollX = 500, # permits to scroll along x
          search = list(regex = TRUE),
          columnDefs = list(
            list(
              orderSequence = c("desc", "asc"),
              targets = "_all"
            ),
            list(className = "dt-center", targets = "_all")
          ),
          processing = FALSE,
          pageLength = 25,
          lengthMenu = list(c(5, 10, 25, 50,-1), c("5", "10", "25", "50", "All"))
        )
      )
      
      # isInt <- sapply(rvs, is.integer)
      # isDbl <- sapply(rvs, is.double)
      # 
      # ## rounding as needed
      # if (sum(isDbl) > 0)
      #   dt_table <- DT::formatRound(dt_table, colnames(rvs)[isDbl], digits = 2)
      # if (sum(isInt) > 0)
      #   dt_table <- DT::formatRound(dt_table, colnames(rvs)[isInt], digits = 0)
      
      dt_table
      
    })
    # 2.6) Keep column ----------------------------------------------------------------------
    # selection of all the available columns and possibility to exclude some
    output$keepColumns <- renderUI({
      # req(input$file) # requires that a file is loaded
      tagList(
        shinyWidgets::pickerInput(
          "keepColumnName",
          label = "Select column to keep:",
          choices = colnames(rvs),
          # all available columns in the original dataframe
          selected = colnames(rvs),
          # by default all selected
          options = list(`actions-box` = TRUE),
          multiple = T
        )
      )
    })
    
    
    
  })
}

## To be copied in the UI
# mod_manage_ui("manage_ui_1")

## To be copied in the server
# mod_manage_server("manage_ui_1", rvs)
