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
    # 2.7) Rename column ----------------------------------------------------------------------
    checkboxInput(ns("modifyColumns_chackbox"), "Rename column", value = FALSE),
    conditionalPanel(condition = sprintf("input['%s'] == true", ns('modifyColumns_chackbox')), # if we want to rename 
           selectInput(ns("ergs"), "Label", choices = "A")          
    ),
    # 2.9) summariza table ----------------------------------------------------------------------
    checkboxInput(ns("summarizeVar_chackbox"), "Summarize column", value = FALSE),
    conditionalPanel(condition = sprintf("input['%s'] == true", ns('summarizeVar_chackbox')), # if we want to rename 
                     
    ),
    # 2.9) transform table ----------------------------------------------------------------------
    checkboxInput(ns("transformVar_chackbox"), "Transform column", value = FALSE),
    conditionalPanel(condition = sprintf("input['%s'] == true", ns('transformVar_chackbox')), # if we want to rename 
                     
    ),
    # 2.8) Add column ----------------------------------------------------------------------
    checkboxInput(ns("addColumns_chackbox"), "Add column", value = FALSE),
    conditionalPanel(condition = sprintf("input['%s'] == true", ns('addColumns_chackbox')), # if we want to rename 
                     
    ),
    # 2.9) Pivot table ----------------------------------------------------------------------
    checkboxInput(ns("pivotTable_chackbox"), "Pivot table", value = FALSE),
    conditionalPanel(condition = sprintf("input['%s'] == true", ns('pivotTable_chackbox')), # if we want to rename 
                     
    ),
    # 2.4) Rename dataframe ----------------------------------------------------------------------
    shinyWidgets::searchInput(inputId = ns("new_dataframe_name"), label = "Save current dataframe", 
                              placeholder = "New name..", 
                              value = NULL, # initial value
                              btnSearch = icon("plus"), btnReset = icon("backspace"), # icons
                              width = "100%"),
    # 2.10) Download filtered dataframe ----------------------------------------------------------------------
    downloadButton("download_filtered", "Download Filtered Dataframe (as csv)", style = "width:100%;"),   
  )
}

#' manage UI output
#'
#' @noRd 
mod_manage_ui_output <- function(id){
  ns <- NS(id)
  tagList(
    
  )
}


#' manage Server Functions
#'
#' @noRd 
mod_manage_server <- function(id, rvs){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$keepColumns <- renderUI({
      tagList(
        shinyWidgets::pickerInput(ns("keepColumnName"), label = "Select column to keep:",
                                  choices = colnames( rvs  ), # all available columns in the original dataframe
                                  selected = colnames( rvs ), # by default all selected
                                  options = list(`actions-box` = TRUE), multiple = T) 
      )
    })
    
    
  })
}

## To be copied in the UI
# mod_manage_ui("manage_ui_1")

## To be copied in the server
# mod_manage_server("manage_ui_1", rvs)
