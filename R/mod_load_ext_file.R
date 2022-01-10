#' load_ext_file UI Function
#'
#' @description A shiny Module.
#' This module permits to load external files (.csv, .RData) into the app
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

mod_load_ext_file_ui_modal <- function(id) {
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

mod_load_ext_file_server <-
  function(id,
    toggle_button_input,
    data_rv,
    data_rv_results) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      
      # this is the modal shown when the session starts and when the button is clcked
      modal_custom <- function(failed = FALSE) {
        modalDialog(
          fluidRow(
            shiny::column(
              width = 12,
              align = 'center',
              
              # type of files that can be loaded
              shiny::selectInput(
                inputId = ns("type"),
                label = "Chose the type of file:",
                choices = c(
                  "",
                  "Comma-separated values (.csv)" = "csv",
                  "R object (.RData)" = "RData"
                )
              ),
              
              # CSV ------------------------------------
              shiny::conditionalPanel(
                condition = sprintf("input['%s'] == 'csv'", ns('type')),
                
                # select the separator, default to comma
                column(
                  width = 6,
                  shiny::selectInput(
                    inputId = ns("separator"),
                    label = "Separator:",
                    choices = c("Comma (,)" = ",", "Semicolon (;)" = ";")
                  )
                ),
                # select the decimal point separator, default to point
                column(
                  width = 6,
                  shiny::selectInput(
                    inputId = ns("decimal"),
                    label = "Decimal:",
                    choices = c("Point (.)" = ".", "Comma (,)" = ",")
                  )
                ),
                # choose if there is a header or not in data, default to true
                column(
                  width = 4,
                  shiny::checkboxInput(
                    inputId = ns("header"),
                    label = "Header?",
                    value = TRUE
                  )
                ),
                # choose if there is a timestamp column or not in data, default to true
                column(
                  width = 4,
                  shiny::checkboxInput(
                    inputId = ns("timestamp_csv"),
                    label = "Timestamp column?",
                    value = TRUE
                  )
                ),
                # choose wether to convert string to factors, default to true
                column(
                  width = 4,
                  shiny::checkboxInput(
                    inputId = ns("strAsFact"),
                    label = "String as Factor?",
                    value = TRUE
                  )
                ),
                # if there is a timestamp column display available time zones
                shiny::conditionalPanel(
                  condition = sprintf("input['%s'] == true", ns('timestamp_csv')),
                  
                  shiny::selectInput(
                    inputId = ns("timezone_csv"),
                    label = "Timezone:",
                    choices = OlsonNames(),
                    selected = "Europe/Rome"
                  ),
                ),
              ),
              
              # RData ------------------------------------
              # this part of code could be optimize by using only one timestamp column check box
              # the user wants to load a RData file
              shiny::conditionalPanel(
                condition = sprintf("input['%s'] == 'RData'", ns('type')),
                # choose if there is a timestamp column or not in data, default to true
                shiny::checkboxInput(
                  inputId = ns("timestamp_RData"),
                  label = "Timestamp column?",
                  value = TRUE
                ),
                # if there is a timestamp column display available time zones
                shiny::conditionalPanel(
                  condition = sprintf("input['%s'] == true", ns('timestamp_RData')),
                  shiny::selectInput(
                    inputId = ns("timezone_RData"),
                    label = "Timezone:",
                    choices = OlsonNames(),
                    selected = "Europe/Rome"
                  ),
                ),
              ),
              shiny::conditionalPanel(
                condition = sprintf("input['%s'] != ''", ns('type')),
                shiny::fileInput(
                  inputId = ns("file"),
                  label = paste("Upload file:"))
              )
            )
          ),
          
          size = 'm',              # size of the modal, medium
          easyClose = TRUE,        # close if click outside
          fade = TRUE,             # fade when clicked
          # removes footer default
          shiny::tags$head(
            shiny::tags$style("#startupModal .modal-footer{ display:none}")
          ),
          # add title in html style
          title = shiny::HTML(
            '
                        <p align="center">
                          <a href="https://www.researchgate.net/lab/Building-Automation-and-Energy-Data-Analytics-Lab-Alfonso-Capozzoli">
                            <img src="www/BAEDA-logo-dashboard.png" alt="Logo" height="80">
                          </a>
                          <h3 align="center"> <i> Student Version </i> </h3>
                          <p align="center">
                            Now you can perform advanced data analytics tasks on your energy data.
                          </p>
                        </p>'
          )
        )
      }
      
      
      # 1) When the session starts show the modal ------------------------------------
      observeEvent(session, {
        showModal(modal_custom())
      })
      
      # 2) Reopens when requested by the user ------------------------------------
      shiny::observeEvent(toggle_button_input(), {
        shinyFeedback::hideFeedback("file")     # hides previous file feedback
        showModal(modal_custom())               # reopens modal
      })
      
      # 3) Load external file ------------------------------------
      shiny::observeEvent(input$file, {
        inFile <- input$file   # input file loaded
        nome <- inFile$name    # input file name
        
        # validate that the file is in the right format
        admitted <-
          base::gsub(" ", "", paste("\\.", input$type, "$"))
        
        # validated is TRUE if the value is acceptable FALSE if not acceptable
        validated <-
          base::grepl(admitted, nome)                         
        
        # removes previous feedback messages
        shinyFeedback::hideFeedback("file")
        
        # gives error feedback if the file is not in the format required/selected and STOPS the execution
        if (validated == TRUE) {
          shinyFeedback::feedbackSuccess("file", TRUE, "Format accepted")
        }
        else {
          shinyFeedback::feedbackDanger("file", TRUE, "Format not accepted")
        }
        
        # the execution CONTINUES only if the file is accepted
        req(validated, cancelOutput = TRUE)
        
        # notification that the file is being loaded
        id <-
          shiny::showNotification(
            "Reading data...",
            duration = NULL,
            closeButton = FALSE,
            type = "message"
          )
        # when the process finishes close modal and notification
        on.exit(
          shinyBS::toggleModal(
            session = session,
            modalId = "startupModal",
            toggle = "close"
          ),
          add = TRUE
        )
        on.exit(shiny::removeNotification(id), add = TRUE)
        
        ## Return values
        # reads the input file and assigns it to the reactive value data
        data_rv[[nome]] <-
          switch(
            input$type,
            # condition on the file type
            csv = read.csv(
              file = inFile$datapath,
              header = input$header,
              sep = input$separator,
              dec = input$decomal,
              stringsAsFactors = input$strAsFact,
              check.names = FALSE
            ),
            # see this strategy to load RData
            # https://riptutorial.com/shiny/example/24961/upload--rdata-files-to-shiny-with-fileinput--
            RData = {
              e = new.env()
              name <- load(file = inFile$datapath, envir = e)
              e[[name]]
            }
            # xls = read_excel(path = inFile$datapath)
          )
        # saves the selected timezone and timestamp column in the global environment
        data_rv_results[["timestamp"]] <-
          base::gsub(" ", "", paste("timestamp_", input$type))
        
        data_rv_results[["timezone"]] <-
          base::gsub(" ", "", paste("timezone_", input$type))
        
        # signal that one file has been loaded
        data_rv_results$infile <-
          TRUE
        
        removeModal() 
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
