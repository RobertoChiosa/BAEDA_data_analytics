#' visualization_histogram UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import shiny
#' @import ggplot2
mod_visualization_histogram_ui_input <- function(id){
  ns <- NS(id)
  tagList(
    
    
    selectInput(ns("variable"), label = NULL, choices = NULL),
    
    sliderInput(
      ns("bins"),
      label = "Number of bins:",
      min = 1,
      max = 100,
      value = 30
    ),
    # sliderInput(inputId = "hours_slider_hist", label = "Hours:", min = 0, max = 24, value = c(0, 24) ),
    # sliderTextInput(
    #   inputId = "month_slider_hist",
    #   label = "Month:",
    #   choices = base::sort(unique(data[[input$dataframe]][,"Month"])),
    #   selected = base::sort(unique(data[[input$dataframe]][,"Month"]))[c(1, length(base::sort(unique(data[[input$dataframe]][,"Month"]))))]
    # ),
    column(
      width = 6,
      style = "padding-left:0px; padding-right:5px;",
      selectInput(ns("fillvariable"), label = "Fill Variable:", choices = NULL), 
    ),
    column(
      width = 6,
      style = "padding-left:5px; padding-right:0px;",
      selectInput(ns("facetvariable"), label = "Facet Variable:", choices = NULL),
    ), 
    conditionalPanel(condition = sprintf("input['%s'] != 'None'", ns('facetvariable')),
                     numericInput(
                       ns("nrowvariable"),
                       "Number of facet rows",
                       value = 3,
                       min = 1
                     )
    ),
    hr(),
    column(
      "Style",
      width = 6,
      checkboxInput(ns("checkbox_flip"), label = "Flip", value = FALSE),
      checkboxInput(ns("checkbox_density"), label = "Density", value = FALSE)
    ),
    column(
      "Scale",
      width = 6,
      checkboxInput(ns("checkbox_logx"), label = "Log-X", value = FALSE),
      checkboxInput(ns("checkbox_logy"), label = "Log-Y", value = FALSE)
    )
  )
}

mod_visualization_histogram_ui_output <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns("plot"))
  )
}

#' visualization_histogram Server Functions
#'
#' @noRd 
mod_visualization_histogram_server <- function(id, infile = NULL, rvs_dataset){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Update selectInput according to dataset
    observe({
      req( !is.null(infile())  )
      # gets rvs_dataset as reactive value to solve update inputs
      choices <- colnames(rvs_dataset())
      # choices <- variable_list_with_class(rvs_dataset()) 
      updateSelectInput(session, "variable", choices = choices)
      
      choices_factor <-  c("None", colnames(rvs_dataset()) )
      # choices <- variable_list_with_class(rvs_dataset()) 
      updateSelectInput(session, "fillvariable",   choices = c("None", choices_factor))
      updateSelectInput(session, "facetvariable",  choices = c("None", choices_factor))
    })
    
    # trigger <- reactiveValues( i=0 )
    # 
    # observeEvent(input$add_filter, {
    #   # maximum number of filters less than dataframe dimensions
    #   req( trigger$i < dim(rvs_dataset())[2]  )
    #   trigger$i <- trigger$i+1
    #   insertUI(
    #     selector = gsub(" ", "", paste('#', ns("filter_column") )),
    #     where = "afterBegin",
    #     ui = tagList(
    #       column(
    #         h6(paste("Filter variable", trigger$i)),
    #         width = 12,
    #        # style = "padding-left:0px; padding-right:0px;",
    #         selectInput(ns(paste("filter_variable", trigger$i)), label =  NULL, choices = colnames(rvs_dataset()) ),
    #         sliderInput(ns(paste("filter_slider", trigger$i)), label = NULL, min = 0, max = 24, value = c(0, 24) )
    #       )
    #     )
    #   )
    # })
    # 
    # observeEvent(input$remove_filter, {
    #   removeUI(
    #     selector =  gsub(" ", "", paste('#', ns("filter_column") ))
    #   )
    # })
    
    
    # output box - plot - HISTOGRAM
    output$plot <- renderPlot({
      req(rvs_dataset(), input$variable)
      
      plot <- ggplot2::ggplot(rvs_dataset()) + 
        
        # fill variable
        if (input$fillvariable == "None") {
          geom_histogram( aes_string(x = input$variable), bins = input$bins, na.rm = TRUE)
        } else {
          geom_histogram( aes_string(x = input$variable, fill = input$fillvariable), bins = input$bins, na.rm = TRUE)
        }
      
      
      # flip
      if (input$checkbox_flip == TRUE) {
        plot <- plot + coord_flip()
      }
      # add wrap
      if (input$facetvariable == "None") {
        NULL
      } else {
        plot <- plot + facet_wrap( ~  rvs_dataset()[, input$facetvariable], nrow = input$nrowvariable)
      }
      # log
      if (input$checkbox_logx == TRUE) {
        plot <- plot + ggplot2::scale_x_continuous(trans = 'log10')
      }
      if (input$checkbox_logy == TRUE) {
        plot <- plot + ggplot2::scale_y_continuous(trans = 'log10')
      }
      
      # tema
      plot <- plot + labs( x = input$variable, fill = input$fillvariable ) + theme_bw() 
      
      plot
      #hist(iris$Sepal.Length)
      #   req(input$plot_button)  # requires the plot button to be pressed
      #   isolate({               # avoid reactivity of parameters
      #     
      #     rvs_dataset() <- data[[input$dataframe]] %>%
      #       dplyr::filter( min_dec >= input$hours_slider_hist[1], 
      #                      min_dec <= input$hours_slider_hist[2],
      #                      Month >= input$month_slider_hist[1], 
      #                      Month <= input$month_slider_hist[2]
      #       )
      #     
      #     plot <- ggplot(data =  rvs_dataset(),
      #                    mapping =  aes(x =  rvs_dataset()[,input$variable],
      #                                   fill = if (input$fillvariable == "None") {NULL} else { rvs_dataset()[,input$fillvariable]},
      #                    ),
      #     ) + theme_bw() 
      #     
      #     # densità
      #     if (input$checkbox_density == TRUE) {
      #       plot <- plot + geom_density(aes(y = ..density..,
      #                                       fill = if (input$fillvariable == "None") {NULL} else { rvs_dataset()[,input$fillvariable]}),
      #                                   alpha = 0.4,
      #                                   bw = input$bins/5,
      #                                   na.rm = TRUE)  # aggiungo distribuzione
      #     } else{plot <- plot + geom_histogram(bins = input$bins, na.rm = TRUE)} #
      #     # flip
      #     if (input$checkbox_flip == TRUE) {plot <- plot + coord_flip()}
      #     # tema
      #     plot <- plot + labs( x = input$variable, fill = input$fillvariable )
      #     # add wrap
      #     if (input$facetvariable == "None") {NULL} else {plot <- plot + facet_wrap(~  rvs_dataset()[,input$facetvariable], nrow = input$nrowvariable)}
      #     # log
      #     # flip
      #     if (input$checkbox_logx == TRUE) {plot <- plot + scale_x_continuous(trans = 'log10') }
      #     if (input$checkbox_logy == TRUE) {plot <- plot + scale_y_continuous(trans = 'log10') }
      #     
      #     plot <- ggplotly(plot, tooltip = c("y"))
      #     resetLoadingButton("plot_button")         # reset the loading button
      #     plot
      #     
      #   }) # end isolate
      #   
    })
    
  })
}

# 
# # test module
# library(shiny)
# library(ggplot2)
# library(shinydashboard)
# library(shinyFeedback)
# library(magrittr)
# library(dplyr)
# ui <- dashboardPage(
#   dashboardHeader(disable = TRUE),
#   dashboardSidebar(disable = TRUE),
#   dashboardBody(
#     column(width = 4,
#            box(
#              mod_visualization_histogram_ui_input("visualization_histogram_ui_1"),
#              solidHeader = T, collapsible = T, collapsed = TRUE, width = 12,
#              title = "Plot Parameters", status = "primary"
#            )
#     ),
#     column(width = 8,
#            mod_visualization_histogram_ui_output("visualization_histogram_ui_1")),
#   )
# )
# server <- function(input, output, session) {
#   
#   data_rv <- reactiveValues( df_tot = eDASH::data[,c(1:4)])                 # reactive value to store the loaded dataframes
#   
#   data_hist <-  mod_visualization_histogram_server("visualization_histogram_ui_1",
#                                                    infile = reactive({TRUE}),
#                                                    rvs_dataset = reactive({data_rv$df_tot}))
#   # When applied function (data_mod2$trigger change) :
#   #   - Update rv$variable with module output "variable"
#   #   - Update rv$fun_history with module output "fun"
#   # observeEvent(data_hist$trigger, {
#   #   req(data_hist$trigger > 0)
#   #   data_rv$df_tot    <- data_hist$dataset
#   # })
# }
# 
# shinyApp(ui, server)

## To be copied in the UI
# mod_visualization_histogram_ui("visualization_histogram_ui_1")

## To be copied in the server
# mod_visualization_histogram_server("visualization_histogram_ui_1")
