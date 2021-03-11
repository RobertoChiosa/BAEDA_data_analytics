#' modulo_prova UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_modulo_prova_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' modulo_prova Server Functions
#'
#' @noRd 
mod_modulo_prova_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_modulo_prova_ui("modulo_prova_ui_1")
    
## To be copied in the server
# mod_modulo_prova_server("modulo_prova_ui_1")
