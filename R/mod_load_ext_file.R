#' load_ext_file UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import shiny
#' @importFrom shinyBS bsModal
#' 

mod_load_ext_file_ui_modal <- function(id){
  ns <- shiny::NS(id)
  # this is the modal shown when the session starts and when the button is clcked
  shiny::tagList(
    shinyBS::bsModal(id = ns('startupModal'),
                     trigger = ns('open'),
                     size = 'medium',
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
                        </p>
                        <br /> '),
                     shiny::column(width = 12, align = 'center',
                                   # type of files that can be loaded
                                   shiny::selectInput(ns("type"), "Chose the type of file:",
                                                      c("", "Comma-separated values (.csv)" = "csv",
                                                        "R object (.rds)"="rds"
                                                      ), selected = NULL
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
                                   shiny::conditionalPanel(sprintf("input['%s'] == ''", ns('type')),
                                                           shiny::fileInput(ns("file"), paste("Upload file:") )
                                   )
                     ),
                     
                     # dont' know why but i have to create a footer with a transparent action button
                     footer = shiny::tagList(column(12, align = "center",  modalButton("Cancel")),
                                             shiny::actionButton(ns("upload"), "", style = "color: #ffffff; background-color: #ffffff; border-color: #ffffff")
                     )
    )
  )
}

#' load_ext_file Server Functions
#'
#' @noRd 
mod_load_ext_file_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # when he session starts show the modal
    shinyBS::toggleModal(session, "startupModal", toggle = ns("open"))
    
    # 1.1) Upload new dataframe ----------------------------------------------------------------------
    # # when clicked a the upload modal is sown again
    # observeEvent(input$upload,{ 
    #   shinyFeedback::hideFeedback("file") # hides previous file feedback
    #   toggleModal(session, "startupModal", toggle = "open") # reopens modal
    # })
    
  })
}

library(shiny)
library(shinyBS)

ui<-fluidPage(mod_load_ext_file_ui_modal("load_ext_file_ui_1"))

server<-function(input,output,session) {
  mod_load_ext_file_server("load_ext_file_ui_1")
}

shinyApp(ui, server)




## To be copied in the UI
# mod_load_ext_file_ui_modal("load_ext_file_ui_1")

## To be copied in the server
# mod_load_ext_file_server("load_ext_file_ui_1")
