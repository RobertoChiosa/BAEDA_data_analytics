#' load_ext_file UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList selectInput actionButton tags
mod_load_ext_file_ui_sidebar <- function(id){
  ns <- shiny::NS(id)

  
}
    
#' load_ext_file Server Functions
#'
#' @noRd 
mod_load_ext_file_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_load_ext_file_ui_sidebar("load_ext_file_ui_1")
    
## To be copied in the server
# mod_load_ext_file_server("load_ext_file_ui_1")
