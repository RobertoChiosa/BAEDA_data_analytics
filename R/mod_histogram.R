#' histogram UI Function
#'
#' @description A shiny Module.
#' This module contains the information to build histogram
#' 
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import ggplot2
#' @importFrom dplyr select_if filter
#' @importFrom shinyWidgets sliderTextInput tooltipOptions
#' @import shiny
#' @importFrom shinyjs disabled

mod_histogram_ui_input <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
   
      shiny::selectInput(  ns("variable"),          
                           label = "Variable:",       
                           choices = colnames(dplyr::select_if( data, is.numeric))
      ),
      shiny::sliderInput(  ns("bins"),              
                           label = "Number of bins:", 
                           min = 1, max = 100, value = 30
      ),
      # shiny::sliderInput(  ns("hours_slider_hist"), 
      #                      label = "Hours:",          
      #                      min = 0, max = 24, value = c(0, 24) 
      # ),
      # shinyWidgets::sliderTextInput(    ns("month_slider_hist"), 
      #                                   label = "Month:",          
      #                                   choices = sort(unique(data[,"Month"])),  
      #                                   selected = sort(unique(data[,"Month"]))[c(1, length(base::sort(unique(data[,"Month"]))))]
      # ),
      shiny::column(width = 6,  style = "padding-left:0px; padding-right:5px;",
                    shiny::selectInput( ns("fillvariable"), label = "Fill Variable:", 
                                        choices = c("None", colnames(dplyr::select_if( data, is.factor))) ),
      ),
      shiny::column(width = 6,  style = "padding-left:5px; padding-right:0px;",
                    shiny::selectInput( ns("facetvariable"), label = "Facet Variable:", 
                                        choices = c("None", colnames(dplyr::select_if( data, is.factor))) ),
      ),
      shiny::conditionalPanel(condition = sprintf("input['%s'] != 'None'", ns('facetvariable')),
                              shiny::numericInput( ns("nrowvariable"),   label = "Number of facet rows", value = 3, min = 1)), 
      tags$hr(),
      shiny::column("Style", width = 6,
                    shiny::checkboxInput( ns("checkbox_flip"),    label = "Flip",    value = FALSE),
                    shiny::checkboxInput( ns("checkbox_density"), label = "Density", value = FALSE)
      ),
      shiny::column("Scale", width = 6,
                    shiny::checkboxInput( ns("checkbox_logx"),    label = "Log-X",   value = FALSE),
                    shiny::checkboxInput( ns("checkbox_logy"),    label = "Log-Y",   value = FALSE)
      ),
      #shinyjs::disabled(
        shiny::actionButton(ns("plot_button"), "Plot", 
                            class = "btn-success", 
                            icon = icon("chart-bar"), width = "100%")
        #),
  )
}

#' histogram UI Functions
#' output plot function
#' @noRd 
mod_histogram_ui_output <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyWidgets::dropdownButton( size = "sm",
                                  tags$h3("Graphical parameters"),
                                  shiny::numericInput(ns('width'), label = 'height', value = 400, step = 100),
                                  circle = TRUE, status = "primary", icon = icon("gear"), width = "400px",
                                  tooltip = shinyWidgets::tooltipOptions(title = "Click to modify plot")
    ),
    plotOutput(ns("histogram"), height = "500px")
  )
}
#' histogram Server Functions
#'
#' @noRd 
mod_histogram_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    data_plot <-reactive({ data }) 
    
    output$histogram <- renderPlot({
      
      req(input$plot_button)  # requires the plot button to be pressed
      
      
      # data_plot <- data %>%
      #   dplyr::filter( min_dec >= input$hours_slider_hist[1], 
      #                  min_dec <= input$hours_slider_hist[2],
      #                  Month >= input$month_slider_hist[1], 
      #                  Month <= input$month_slider_hist[2]
      #   )
      
      
      # fill variable or not?
      if (input$fillvariable == "None") {
        plot <- ggplot(data =  data_plot(),  mapping =  aes_string(x =  input$variable))
      } else {
        plot <- ggplot(data =  data_plot(),  mapping =  aes_string(x =  input$variable, fill = input$fillvariable ))
      }
      
      # geom add density or hist
      if (input$checkbox_density == TRUE) {
        plot <- plot + geom_density(aes(y = ..density..),
                                    alpha = 0.4,
                                    bw = input$bins/5,
                                    na.rm = TRUE)  # aggiungo distribuzione
      } else {
        plot <- plot + geom_histogram(bins = input$bins, na.rm = TRUE)
      } 
      
      # tema
      plot <- plot + labs( x = input$variable, fill = input$fillvariable ) + theme_bw() 
      
      # add wrap
      if (input$facetvariable != "None") {plot <- plot + facet_wrap(~  data_plot()[,input$facetvariable], nrow = input$nrowvariable)}
      # log transformation
      if (input$checkbox_logx == TRUE) {plot <- plot + scale_x_continuous(trans = 'log10') }
      if (input$checkbox_logy == TRUE) {plot <- plot + scale_y_continuous(trans = 'log10') }
      
      # flip axes
      if (input$checkbox_flip == TRUE) {plot <- plot + coord_flip()}
      
      # plot <- ggplotly(plot, tooltip = c("y"))
      plot
      
    })
    
  })
}

## To be copied in the UI
# mod_histogram_ui_input("histogram_ui_1")
# mod_histogram_ui_output("histogram_ui_1")

## To be copied in the server
# mod_histogram_server("histogram_ui_1")
