#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {

  data_rv <- reactiveValues()
  data_rv$df_tot <- data
  # plot modules
  mod_histogram_server("histogram_ui_1")
  # modules advanced
  mod_cart_server("cart_ui_1",data_rv$df_tot)
  
  mod_load_ext_file_server("load_ext_file_ui_1")
}
