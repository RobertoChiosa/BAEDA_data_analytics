#' @name manage_transform
#' @aliases mod_manage_transform_ui
#' @aliases mod_manage_transform_server
#' 
#' @title Column  Transformation Module
#' 
#' @description 
#' This module permits to transform a column of the input dataframe 
#' 
#' @param id,input,output,session Internal parameters for {shiny}.
#' 
#' @examples \dontrun{
#' 
#' # To be copied in the UI
#' mod_manage_transform_ui(id = "manage_transform_ui_1")
#' 
#' # To be copied in the server
#' mod_manage_transform_server(id = "manage_transform_ui_1",
#'                             infile = reactive({ infile }),
#'                             rvs_dataset = reactive({ rvs$data })
#' )
#' }
#' 
#' @import shiny
#' @import shinydashboard
#' @import dplyr magrittr
#' @importFrom shinyFeedback feedbackWarning hideFeedback
#' 


#' @rdname manage_transform
#' 
#' @export
mod_manage_transform_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # sets color of plus in button
    shiny::tags$style(".fa-refresh {color:white}"),
    # ui in a collapsible box
    box(
      shiny::selectInput(
        ns("actual_name"),
        label = "Select variable (column):",
        choices = NULL,
        selected = NULL,
        width = "100%"
      ),
      shiny::selectizeInput(
        ns("type"),
        label = "Select conversion type:",
        choices = c("bin",
                    "normalize",
                    "recode",
                    "remove/reorder levels", 
                    "rename",
                    "summarize",
                    "pivot"
                    ),
        # all admitted unit # implement with groups
        selected = NULL,
        multiple = FALSE,
        width = "100%"
      ),
      
      shiny::htmlOutput(  ns("type_preview_actual_title") ),
      shiny::verbatimTextOutput(  ns("type_preview_actual") ),
      
      shiny::htmlOutput(  ns("type_preview_future_title") ),
      shiny::verbatimTextOutput(  ns("type_preview_future") ),
      
      column(
        width = 12,
        style = "padding-left:0px; padding-right:0px;",
        shiny::splitLayout(
          cellWidths = c("80%", "20%"),
          shiny::textInput(
            ns("new_name"),
            label = NULL,
            value = "",
            placeholder = "(Optional) New name...",
            width = "100%"
          ),
          uiOutput(ns("button_js"))
        )
      ),
      solidHeader = T, collapsible = T, collapsed = TRUE, width = 12,
      title = "Transform", status = "primary"
    )
  )
}

#' @rdname manage_transform
#' 
#' @param infile A reactive boolean used to understand if a dataset has been loaded on client side. It is used to disable buttons and avoids incorrect user inputs. Pass as \code{reactive({...})}.
#' @param rvs_dataset A reactive values dataset created from \code{reactiveValues()} and passed to the module from the external environment. Pass as \code{reactive({...})}.
#' 
#' @export
mod_manage_transform_server <- function(id, infile = NULL, rvs_dataset) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # apply button
    output$button_js <- renderUI({
      if (is.null(infile())) {
        shinyjs::disabled(
          shiny::actionButton(
            ns("new_name_submit"),
            label = NULL,
            icon = icon("refresh"),
            class = "btn-success",
            width = "100%"
          )
        )
      } else {
        shiny::actionButton(
          ns("new_name_submit"),
          label = NULL,
          icon = icon("refresh"),
          class = "btn-success",
          width = "100%"
        )
      }
    })
    
    
    # reactive value to evaluate the string name
    # it is true if some special values are found
    # observe this reactive value and show warning live
    name_val <- reactive({ grepl('[^[:alnum:]]', input$new_name) })
    observe( {
      # validate new name
      shinyFeedback::feedbackWarning("new_name", name_val(), "Please don't use special characters")
    })
    
    # Update selectInput according to dataset
    observe({
      req( !is.null(infile())  )
      
      # gets rvs_dataset as reactive value to solve update inputs
      # choices <- variable_list_with_class(rvs_dataset()) 
      choices <- colnames(rvs_dataset())
      updateSelectInput(session, "actual_name", choices = choices)
    })
    
    observeEvent(input$actual_name, {
      req( !is.null(infile())  )
      # preview of type conversion
      output$type_preview_actual_title <- renderText({
        paste("Summary of <code>", input$actual_name,  "</code> as is (Actual)")
      })
      output$type_preview_actual <- renderPrint({
        
        variable <- as.data.frame(rvs_dataset()[, input$actual_name])
        summary( variable )
      })
      
      output$type_preview_future_title <- renderText({
        paste("Summary of <code>", input$actual_name, " </code> when converted (Future)")
      })
      output$type_preview_future <- renderPrint({
        variable <- as.data.frame( dplyr::mutate_at(rvs_dataset(), .vars = dplyr::vars(input$actual_name), .funs = input$type)[, input$actual_name]  ) 
        summary( variable)
      })
      
    })
    
    
    
    # Define the ReactiveValue to return : "toReturn"
    # with slots "rvs_dataset" & "trigger"
    toReturn <- reactiveValues(dataset = NULL,  trigger = 0)
    
    # (Re)load button
    observeEvent(input$new_name_submit, {
      
      
      toReturn$dataset <-  rvs_dataset() %>%
        dplyr::mutate_at( .vars = dplyr::vars(input$actual_name), .funs = input$type) 
      
      if (input$new_name != "") {
        toReturn$dataset <-  rvs_dataset() %>%
          dplyr::rename( !!input$new_name := !!input$actual_name )
      } 
      
      toReturn$trigger  <- toReturn$trigger + 1
    })
    
    return(toReturn)
    
  })
}



#' Shiny app snippet to offline test the functionality of the modules.
#' Comment and uncomment when necesssary.
#' devtools::document() to render roxygen comments an preview with ?mod_manage_transform
#' @noRd

# library(shiny)
# library(shinydashboard)
# library(shinyFeedback)
# library(magrittr)
# library(dplyr)
# ui <- dashboardPage(
#   dashboardHeader(disable = TRUE),
#   dashboardSidebar(disable = TRUE),
#   dashboardBody(
#     column(width = 4,
#            mod_manage_transform_ui("manage_transform_ui_1")),
#     column(width = 8,
#            DT::DTOutput("table"))
#   )
# )
# server <- function(input, output, session) {
#   
#   data_rv <- reactiveValues( df_tot = eDASH::data[,c(1:4)])                 # reactive value to store the loaded dataframes
#   
#   output$table <- DT::renderDT({
#     data_rv$df_tot
#   })
#   
#   data_add <-  mod_manage_transform_server("manage_transform_ui_1",
#                                            infile = reactive({TRUE}),
#                                            rvs_dataset = reactive({data_rv$df_tot}))
#   # When applied function (data_mod2$trigger change) :
#   #   - Update data_rv$df_tot with module output "variable"
#   observeEvent(data_add$trigger, {
#     req(data_add$trigger > 0)
#     data_rv$df_tot    <- data_add$dataset
#   })
# }
# 
# shinyApp(ui, server)