#' load_ext_file UI Function
#'
#' @description A shiny Module.
#' This module permits to load external files into the app
#' The ui consists in a modal that opens when the session starts and when the user clicks on button
#' 
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import shiny
#' @importFrom shinyBS bsModal toggleModal
#' @importFrom shinyFeedback feedbackWarning hideFeedback feedbackDanger feedbackSuccess
#' @importFrom utils read.csv

mod_load_ext_file_ui_modal <- function(id){
  ns <- shiny::NS(id)
  tagList()
}

#' load_ext_file Server Functions
#'
#' The inputs are 
#' - the reactive input$button clicked by the user
#' - the reactive values datataframe reactiveValues()
#' - the reactive values result
#' 
#' @noRd 
mod_load_ext_file_server <- function(id, toggle_button_input, data_rv, data_rv_results){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
  
    # this is the modal shown when the session starts and when the button is clcked
    modal_custom <- function(failed = FALSE){
      modalDialog(
        fluidRow(
          shiny::column(width = 12, align = 'center',
                        # type of files that can be loaded
                        shiny::selectInput(ns("type"), "Chose the type of file:",
                                           c("", 
                                             "Comma-separated values (.csv)" = "csv",
                                             "R object (.rds)"="rds"
                                           )
                        ),
                        # the user wants to load a csv file
                        shiny::conditionalPanel(sprintf("input['%s'] == 'csv'", ns('type')),
                                                column(width = 6, shiny::selectInput(   ns("separator"),    "Separator:",  c("Comma (,)" = ",", "Semicolon (;)" = ";")) ),
                                                column(width = 6, shiny::selectInput(   ns("decimal"),      "Decimal:",    c("Point (.)" = ".","Comma (,)" = ",")) ),
                                                column(width = 4, shiny::checkboxInput( ns("header"),       "Header?",     value = TRUE) ),
                                                column(width = 4, shiny::checkboxInput( ns("timestamp_csv"),"Timestamp column?", value = TRUE) ),
                                                column(width = 4, shiny::checkboxInput( ns("strAsFact"),    "String as Factor?", value = TRUE) ),
                                                shiny::conditionalPanel(sprintf("input['%s'] == true", ns('timestamp_csv')),
                                                                        shiny::selectInput(ns("timezone_csv"), "Timezone:", choices = OlsonNames(), selected = "Europe/Rome"),
                                                ),
                        ),
                        # the user wants to load a rds file
                        shiny::conditionalPanel(sprintf("input['%s'] == 'rds'", ns('type')),
                                                shiny::checkboxInput(ns("timestamp_rds"), "Timestamp column?", value = TRUE),
                                                shiny::conditionalPanel(sprintf("input['%s'] == true", ns('timestamp_rds')),
                                                                        shiny::selectInput(ns("timezone_rds"), "Timezone:",
                                                                                           choices = OlsonNames(),
                                                                                           selected = "Europe/Rome"
                                                                        ),
                                                ),
                        ),
                        shiny::conditionalPanel(sprintf("input['%s'] != ''", ns('type')),
                                                shiny::fileInput(ns("file"), paste("Upload file:") )
                        )
          )
          
        ),
        size = 'm',
        easyClose = TRUE,
        fade = TRUE,
        shiny::tags$head(shiny::tags$style("#startupModal .modal-footer{ display:none}")), # removes footer default
        title = shiny::HTML('
                        <p align="center">
                          <a href="https://www.researchgate.net/lab/Building-Automation-and-Energy-Data-Analytics-Lab-Alfonso-Capozzoli">
                            <img src="www/BAEDA-logo-dashboard.png" alt="Logo" height="80">
                          </a>
                          <h3 align="center"> <i> Student Version </i> </h3>
                          <p align="center">
                            Now you can perform advanced data analytics tasks on your energy data.
                          </p>
                        </p>')
      )
    }
    
    
    # 1) When the session starts show the modal ----------------------------------------------------------------------
    #observeEvent(session, {
     # showModal( modal_custom() )
    #})
    
    # 2) Reopens when requested by the user ----------------------------------------------------------------------
    shiny::observeEvent(toggle_button_input(),{
      shinyFeedback::hideFeedback("file")                             # hides previous file feedback
      showModal( modal_custom() )  # reopens modal
    })
    
    # 3) Load external file ----------------------------------------------------------------------
    shiny::observeEvent(input$file,{
      inFile <- input$file   # input file loaded
      nome <- inFile$name    # input file name
      
      # validate that the file is in the right format
      admitted <- base::gsub( " ", "", paste("\\.", input$type, "$" ))  # this is the only accepted file given the one chosen
      validated <- base::grepl(admitted, nome)                          # validated is TRUE if the value is acceptable FALSE if not acceptable
      
      # gives error feedback if the file is not in the format required/selected and STOPS the execution
      shinyFeedback::hideFeedback("file")
      if (validated == TRUE) { shinyFeedback::feedbackSuccess("file", TRUE, "Format accepted")}
      else { shinyFeedback::feedbackDanger("file", TRUE, "Format not accepted") }
      
      # the execution CONTINUES only if the file is accepted
      req(validated, cancelOutput = TRUE)
      
      # notification that the file is being loaded
      id <- shiny::showNotification("Reading data...", duration = NULL, closeButton = FALSE, type = "message")
      on.exit( shinyBS::toggleModal(session, "startupModal", toggle = "close"), add = TRUE)
      on.exit( shiny::removeNotification(id), add = TRUE)
      
      ## Return values
      # reads the input file and assigns it to the reactive value data
      data_rv[[nome]] <- switch(input$type, # condition on the file type
                                csv = read.csv(file = inFile$datapath, header = input$header, sep = input$separator,
                                               dec = input$decomal, stringsAsFactors = input$strAsFact, check.names = FALSE),
                                rds = readRDS(file = inFile$datapath)
                                # xls = read_excel(path = inFile$datapath)
      )
      # saves the selected timezone and timestamp column in the global environment
      data_rv_results[["timestamp"]] <- base::gsub(" ", "", paste("timestamp_", input$type))
      data_rv_results[["timezone"]] <- base::gsub(" ", "", paste("timezone_", input$type))
      data_rv_results$infile <- TRUE # signal that one file has been loaded
      removeModal()  # remove modal
    })
  })
}

# library(shiny)
# 
# ui<-fluidPage(
#   mod_load_ext_file_ui_modal("load_ext_file_ui_1")
# )
# 
# server<-function(input,output,session) {
#   mod_load_ext_file_server("load_ext_file_ui_1")
# }
# 
# shinyApp(ui, server)

## To be copied in the UI
# mod_load_ext_file_ui_modal("load_ext_file_ui_1")

## To be copied in the server
# mod_load_ext_file_server("load_ext_file_ui_1", reactive({ toggle_button_input }), data_rv, data_rv_results)
