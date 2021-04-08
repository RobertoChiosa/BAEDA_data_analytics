#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinyBS addPopover
#' @noRd
app_server <- function( input, output, session ) {
  ###### 1) DATA STRUCTURE ----------------------------------------------------------------------
  # global environment and global options
  options(shiny.maxRequestSize = 100*1024^2)  # this option permits to read larger files than shiny default
  data_rv <- reactiveValues()                 # reactive value to store the loaded dataframes
  data_rv_results <- reactiveValues()         # reactive value where we will store all the loaded dataframes
  data_rv$df_tot <- data                      # upload as default a well known dataset
  
  ######  2) DATA LOADING ----------------------------------------------------------------------
  # # server to load external file
  mod_load_ext_file_server(id = "load_ext_file_ui_1", 
                           toggle_button_input = reactive({ input$upload }), 
                           data_rv             = data_rv, 
                           data_rv_results     = data_rv_results)
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
  
  ###### 3) "MANAGE" TAB ----------------------------------------------------------------------
  # modules manage
  mod_manage_server("manage_ui_1", reactive({data_rv$df_tot})  )
  
  ###### 3.1) change column name/variable name
  data_rename <-   mod_manage_renameColumn_server(id = "manage_renameColumn_ui_1", 
                                                  rvs_dataset = reactive({data_rv$df_tot})  
  )
  # When applied function (data_rename$trigger change) :
  #   - Update data_rv$df_tot with module output "dataset"
  observeEvent(data_rename$trigger, {
    req(data_rename$trigger > 0) # requires a trigger
    data_rv$df_tot <- data_rename$dataset
  })
  
  ###### 3.2) change column type
  data_type <-   mod_manage_manage_changeType_server(id = "manage_manage_changeType_ui_1", 
                                                     rvs_dataset = reactive({data_rv$df_tot})  
  )
  # When applied function (data_rename$trigger change) :
  #   - Update data_rv$df_tot with module output "dataset"
  observeEvent(data_type$trigger, {
    req(data_type$trigger > 0) # requires a trigger
    data_rv$df_tot <- data_type$dataset
  })
  
  ###### 3.2) change column type
  data_add <-   mod_manage_addColumn_server(id = "manage_addColumn_ui_1", 
                                            rvs_dataset = reactive({data_rv$df_tot})  
  )
  # When applied function (data_rename$trigger change) :
  #   - Update data_rv$df_tot with module output "dataset"
  observeEvent(data_add$trigger, {
    req(data_add$trigger > 0) # requires a trigger
    data_rv$df_tot <- data_add$dataset
  })
  
  ###### 4) "VISUALIZE" TAB ----------------------------------------------------------------------
  # plot modules
  mod_histogram_server(id = "histogram_ui_1")
  
  ###### 2) "CLASSIFICATION" TAB ----------------------------------------------------------------------
  mod_cart_server(id = "cart_ui_1",data_rv$df_tot)
  
  ###### 2) "CLUSTERING" TAB ----------------------------------------------------------------------
  mod_clustering_server(id = "clustering_ui_1",data_rv$df_tot)
  
}
