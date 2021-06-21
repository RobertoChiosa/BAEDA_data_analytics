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
      # select the column to modify
      shiny::selectInput(
        ns("actual_name"),
        label = "Select variable (column):",
        choices = NULL,
        selected = NULL,
        width = "100%"
      ),
      # select the type of conversion
      shiny::selectInput(
        ns("transformation"),
        label = "Select transformation type:",
        choices = c(
          "Change Type",
          "Bin",
          "Normalize",
          "Remove/reorder levels", 
          "Rename",
          "Summarize",
          "Transform"
        ),
        # all admitted unit # implement with groups
        selected = NULL,
        multiple = FALSE,
        width = "100%"
      ),
      
      ###### CHANGE TYPE ----------------------------------------------------------------------
      # permits to change the column class 
      shiny::conditionalPanel( condition = sprintf("input['%s'] == 'Change Type' ", ns('transformation') ),
                               shiny::selectizeInput(
                                 inputId = ns("type"),
                                 label = "Select conversion type:",
                                 choices = c("as_integer"  = "as_integer",
                                             "as_numeric"  = "as_numeric",
                                             "as_factor"   = "as_factor",
                                             "as_character"= "as_character",
                                             "as_mdy"      = "as_mdy",
                                             "as_dmy"      = "as_dmy",
                                             "as_ymd"      = "as_ymd",
                                             "as_ymd_hms"  = "as_ymd_hms",
                                             "as_ymd_hm"   = "as_ymd_hm",
                                             "as_mdy_hms"  = "as_mdy_hms",
                                             "as_mdy_hm"   = "as_mdy_hm",
                                             "as_dmy_hms"  = "as_dmy_hms",
                                             "as_dmy_hm"   = "as_dmy_hm",
                                             "as_hms"      = "as_hms",
                                             "as_hm"       = "as_hm"
                                 ),
                                 # all admitted unit # implement with groups
                                 selected = NULL,
                                 multiple = FALSE,
                                 width = "100%"
                               )
      ),
      ###### BIN ----------------------------------------------------------------------
      shiny::conditionalPanel( condition = sprintf("input['%s'] == 'Bin' ", ns('transformation') ),
                               shiny::numericInput(
                                 inputId = ns("number_bins"),
                                 label = "Chose number of bins",
                                 value = 10,
                                 min = 1,
                                 max = 1000,
                                 step = 1
                               )  
      ),
      ###### NORMALIZE ----------------------------------------------------------------------
      # permits to normalize by using different kind of normalization
      # - MAX MIN
      # - ZSCORE
      # - MAX
      shiny::conditionalPanel( condition = sprintf("input['%s'] == 'Normalize' ", ns('transformation') ),
                               shiny::selectInput(
                                 inputId = ns("normalization_type"),
                                 label = "Chose normalization type",
                                 choices = c("Z-Score" = "zscore")
                               )
      ),
      ###### REMOVE/REORDER LEVELS ----------------------------------------------------------------------
      shiny::conditionalPanel( condition = sprintf("input['%s'] == 'Remove/reorder levels' ", ns('transformation') ),
                               shiny::selectInput(
                                 inputId = ns("levels"),
                                 label = "Remove/reorder levels:",
                                 choices = NULL, # updated accordind to the variable selected
                                 selected = NULL,
                                 multiple = TRUE,
                                 width = "100%"
                               ),
      ),
      ###### RENAME ----------------------------------------------------------------------
      shiny::conditionalPanel( condition = sprintf("input['%s'] == 'Rename' ", ns('transformation') ),
                               
      ),
      ###### SUMMARIZE ----------------------------------------------------------------------
      shiny::conditionalPanel( condition = sprintf("input['%s'] == 'Summarize' ", ns('transformation') ),
      ),
      ###### TRANSFORM ----------------------------------------------------------------------
      shiny::conditionalPanel( condition = sprintf("input['%s'] == 'Transform' ", ns('transformation') ),
                               
      ),
      
      
      
      ###### PREVIEW ----------------------------------------------------------------------
      shiny::verbatimTextOutput(  ns("type_preview_actual") ),
      shiny::verbatimTextOutput(  ns("type_preview_future") ),
      
      column(
        width = 12,
        style = "padding-left:0px; padding-right:0px;",
        h5("Chose a name for the transformed variable (optional)"),
        shiny::splitLayout(
         
          cellWidths = c("80%", "20%"),
          shiny::textInput(
            inputId = ns("new_name"),
            label = NULL,
            value = "",
            placeholder = "(Optional) New name...",
            width = "100%"
          ),
          shinyjs::disabled(
            shiny::actionButton(
              inputId = ns("new_name_submit"),
              label = NULL,
              icon = icon("refresh"),
              class = "btn-success",
              width = "100%"
            )
          )
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
      # enambles the button submit
      shinyjs::enable("new_name_submit")
      
# updates choiches according to the loaded dataset
      choices <- colnames(rvs_dataset())
      updateSelectInput(session, "actual_name", choices = choices)
     
    })
    
    
    # update the preview accordincìg to the type of transformation
    
    observeEvent(input$actual_name, {
      req( !is.null(infile())  )
      
      updateTextInput(session, "new_name", value = input$actual_name)
      
      # preview of type conversion ANTE
      # output$type_preview_actual_title <- renderText({
      #   paste("Summary of <code>", input$actual_name,  "</code>as is (Actual)")
      # })
      output$type_preview_actual <- renderPrint({
        variable <- as.data.frame(rvs_dataset()[, input$actual_name])
        variable_print <- data.frame(x = variable)
        colnames(variable_print)[1] <- paste("Summary of", input$actual_name,  "as is (ACTUAL)")
        summary( variable_print )
      })
      
      #preview of the POST
      output$type_preview_future_title <- renderText({
        paste("Summary of <code>", input$actual_name, " </code>when converted (POST)")
      })
      output$type_preview_future <- renderPrint({
        
        switch (input$transformation,
                "Change Type"           = variable <- as.data.frame( dplyr::mutate_at(rvs_dataset(), .vars = dplyr::vars(input$actual_name), .funs = input$type)[, input$actual_name]  ), 
                "Bin"                   = variable <- as.data.frame( dplyr::mutate_at(rvs_dataset(), .vars = dplyr::vars(input$actual_name), .funs = input$type)[, input$actual_name]  ), 
                "Normalize"             = variable <- as.data.frame( dplyr::mutate_at(rvs_dataset(), .vars = dplyr::vars(input$actual_name), .funs = input$normalization_type)[, input$actual_name]  ), 
                "Remove/reorder levels" = variable <- as.data.frame( dplyr::mutate_at(rvs_dataset(), .vars = dplyr::vars(input$actual_name), .funs = input$type)[, input$actual_name]  ), 
                "Rename"                = variable <- as.data.frame( dplyr::mutate_at(rvs_dataset(), .vars = dplyr::vars(input$actual_name), .funs = input$type)[, input$actual_name]  ), 
                "Summarize"             = variable <- as.data.frame( dplyr::mutate_at(rvs_dataset(), .vars = dplyr::vars(input$actual_name), .funs = input$type)[, input$actual_name]  ), 
                "Transform"             = variable <- as.data.frame( dplyr::mutate_at(rvs_dataset(), .vars = dplyr::vars(input$actual_name), .funs = input$type)[, input$actual_name]  ), 
        )
      
        variable_print <- data.frame(x = variable)
        colnames(variable_print)[1] <-  paste("Summary of", input$actual_name, "when converted (Future)")
        summary(variable_print)
      })
      
    })
    
    
    
    # Define the ReactiveValue to return : "toReturn"
    # with slots "rvs_dataset" & "trigger"
    toReturn <- reactiveValues(dataset = NULL,  trigger = 0)
    
    # (Re)load button
    observeEvent(input$new_name_submit, {
      
      switch (input$transformation,
              "Change Type"           = toReturn$dataset <- dplyr::mutate_at(rvs_dataset(), .vars = dplyr::vars(input$actual_name), .funs = input$type),
              "Bin"                   = toReturn$dataset <- dplyr::mutate_at(rvs_dataset(), .vars = dplyr::vars(input$actual_name), .funs = input$type),
              "Normalize"             = toReturn$dataset <- dplyr::mutate_at(rvs_dataset(), .vars = dplyr::vars(input$actual_name), .funs = input$normalization_type),
              "Remove/reorder levels" = toReturn$dataset <- dplyr::mutate_at(rvs_dataset(), .vars = dplyr::vars(input$actual_name), .funs = input$type),
              "Rename"                = toReturn$dataset <- dplyr::mutate_at(rvs_dataset(), .vars = dplyr::vars(input$actual_name), .funs = input$type), 
              "Summarize"             = toReturn$dataset <- dplyr::mutate_at(rvs_dataset(), .vars = dplyr::vars(input$actual_name), .funs = input$type), 
              "Transform"             = toReturn$dataset <- dplyr::mutate_at(rvs_dataset(), .vars = dplyr::vars(input$actual_name), .funs = input$type), 
      )
      
      
      if (input$new_name != input$actual_name) {
        toReturn$dataset <-  toReturn$dataset %>%
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

library(shiny)
library(shinydashboard)
library(shinyFeedback)
library(magrittr)
library(dplyr)
#source("./R/utils_change_type.R")
#source("./R/utils_normalize.R")

ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    column(width = 4,
           mod_manage_transform_ui("manage_transform_ui_1")),
    column(width = 8,
           DT::DTOutput("table"))
  )
)
server <- function(input, output, session) {
  
  data_rv <- reactiveValues( df_tot = eDASH::data[,c(1:4)])                 # reactive value to store the loaded dataframes
  
  output$table <- DT::renderDT({
    data_rv$df_tot
  })
  
  data_add <-  mod_manage_transform_server("manage_transform_ui_1",
                                           infile = reactive({TRUE}),
                                           rvs_dataset = reactive({data_rv$df_tot}))
  # When applied function (data_mod2$trigger change) :
  #   - Update data_rv$df_tot with module output "variable"
  observeEvent(data_add$trigger, {
    req(data_add$trigger > 0)
    data_rv$df_tot    <- data_add$dataset
  })
}

shinyApp(ui, server)