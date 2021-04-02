#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinyBS addPopover
#' @noRd
app_server <- function( input, output, session ) {
  
  ###### 2) "MANAGE" TAB ----------------------------------------------------------------------
  shinyBS::addPopover(session, 
                      id =  "classification_box", 
                      title = "Objective", 
                      content = HTML('<h4> description </h4>'),
                      placement = "right",
                      trigger = "hover"
  )
  
  
  ###### 2) "MANAGE" TAB ----------------------------------------------------------------------
  # global environment and global options
  data_rv <- reactiveValues()                 # reactive value to store the loaded dataframes
  data_rv_results <- reactiveValues()             # reactive value where we will store all the loaded dataframes
  options(shiny.maxRequestSize = 100*1024^2)  # this option permits to read larger files than shiny default
  
  
  
  data_rv$df_tot <- data
  
  mod_manage_renameColumn_server("manage_renameColumn_ui_1", data_rv$df_tot)
  
  
  # plot modules
  mod_histogram_server("histogram_ui_1")
  # modules advanced
  mod_cart_server("cart_ui_1",data_rv$df_tot)
  
  # modules manage
  mod_manage_server("manage_ui_1", data_rv$df_tot)
  
  # server to load external file
  mod_load_ext_file_server("load_ext_file_ui_1", reactive({ input$upload }), data_rv, data_rv_results)
  
  # module clustering
  mod_clustering_server("clustering_ui_1",data_rv$df_tot)
  
  # 2.2) Dataframe dropdown creation ----------------------------------------------------------------------
  # create a reactive list of loaded dataframes. When new file loaded the list is updated
  reactive_list <- reactive({ 
    names(data_rv)
  })
  
  # when the list changes the sidebar change the inputs as well see selection_dataframe() function
  observeEvent(reactive_list(),{
    updateSelectInput(session, "dataframe",                               # when new file loaded and new name given the select is updated
                      choices = reactive_list(),                          # update choices with all loaded data
                      selected = reactive_list()[length(reactive_list())] # selected the last loaded file
    )
  })
  
  
  
}
