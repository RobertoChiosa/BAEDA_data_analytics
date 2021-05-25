#' @name mod_preprocessing_outliers
#' @aliases mod_preprocessing_outliers_ui_input
#' @aliases mod_preprocessing_outliers_ui_output
#' @aliases mod_preprocessing_outliers_server
#' 
#' @title Preprocessing Module for outliers identification
#' 
#' @description 
#' This module permits to identify outliers through different techniques.
#' 
#' @param id,input,output,session Internal parameters for {shiny}.
#' 
#' @examples \dontrun{
#' 
#' # To be copied in the UI
#' mod_preprocessing_outliers_ui_input(id = "preprocessing_outliers_ui_1")
#' mod_preprocessing_outliers_ui_output(id = "preprocessing_outliers_ui_1")
#' 
# To be copied in the server
#'mod_preprocessing_outliers_server(id = "preprocessing_outliers_ui_1",
#'                                  infile = reactive({infile}), 
#'                                  rvs_dataset = reactive({data_rv})  
#')
#' }
#'
#' @import shiny dplyr ggplot2
#' @importFrom magrittr `%>%`
#' @importFrom shinyFeedback showFeedbackWarning hideFeedback
#' @importFrom stats na.omit quantile IQR
#' @importFrom lubridate is.POSIXct
#' @importFrom shinyWidgets dropdownButton
#' @importFrom shinyBS bsTooltip
#' @importFrom shinycssloaders withSpinner
#' @importFrom stats median
#' @importFrom utils write.csv
#' @importFrom shinyjs disable enable runjs
#' 
#' @rdname mod_preprocessing_outliers
#' 
#' @export
mod_preprocessing_outliers_ui_input <- function(id) {
  ns <- NS(id)
  
  shiny::tagList(
    box(
      solidHeader = T, collapsible = T, collapsed = FALSE, width = 12,
      title = "Outliers identification", status = "primary",
      shiny::selectInput(
        ns("outldet_method"),
        'Select outlier detection method',
        choices = c("Boxplot"
                    # "Method2",
                    # "Method3",
                    # "Majority Voting 2/3"
        )
      ),
      shiny::selectInput(ns("variable"), 'Select variable (numeric)', choices = NULL),
      
      # parameters for boxplot
      shiny::conditionalPanel(
        condition = sprintf("input['%s']=='Boxplot'", ns('outldet_method')),
        
        shiny::selectInput(ns("facetwrap"), 'Select the facet variable (factor)', choices = NULL),
        
        shiny::selectInput(
          ns("outl_type"),
          'Select the outlier type to show in "Outliers Table"',
          choices = c(
            "Upper bound outliers" = "upper",
            "Lower bound outliers" = "lower",
            "All outliers (upper+lower)" = "all"
          ),
          selected = "all"
        ), 
        shiny::sliderInput(
          ns("k_iqr_range"),
          'Select the interquartile range multiplier (k*IQR) for outliers definition:',
          value = 1.5,
          min = 1,
          max = 10,
          step = 0.1
        )
      ),
      
      # parameters for Method2
      #simple outlier detection parameters, to replace with the parameters needed for the selected outliers detection function, such as iqr range for boxplots
      shiny::conditionalPanel(
        condition = sprintf("input['%s']=='Method2'", ns('outldet_method')),
        shiny::selectInput(ns("graphx2"), 'Select x variable for the scatter plot', choices = NULL),
        shiny::sliderInput(
          ns("limit_cond_2"),
          'Select the threshold for outlier detection:',
          value = 550,
          min = 0,
          max = 1000,
          step = 0.5
        )
      ),
      
      # parameters for Method3
      shiny::conditionalPanel(
        condition = sprintf("input['%s']=='Method3'", ns('outldet_method')),
        shiny::selectInput(ns("graphx3"), 'Select x variable for the scatter plot', choices = NULL),
        shiny::sliderInput(
          ns("limit_cond_3"),
          'Select the threshold for outlier detection:',
          value = 600,
          min = 0,
          max = 1000,
          step = 0.5
        )
      ), 
      
      # parameters for majority voting
      shiny::conditionalPanel(
        condition = sprintf("input['%s']=='Majority Voting 2/3'", ns('outldet_method')),
        shiny::selectInput(
          ns("facetwrap4"),
          'Select the variable to facet by (boxplot)',
          choices = NULL
        ),
        shiny::selectInput(
          ns("outl_type4"),
          'Select the outlier type to show in "Outliers Table" (boxplot)',
          choices = c("upper", "lower", "all"),
          selected = "all"
        ),
        shiny::sliderInput(
          ns("k_iqr_range4"),
          'Select the interquartile range multiplier for outliers detection (boxplot):',
          value = 1.5,
          min = 1,
          max = 10,
          step = 0.1
        ),
        shiny::sliderInput(
          ns("limit_cond_4_2"),
          'Select the threshold for outlier detection (Method2):',
          value = 550,
          min = 0,
          max = 1000,
          step = 0.5
        ),
        shiny::sliderInput(
          ns("limit_cond_4_3"),
          'Select the threshold for outlier detection (Method3):',
          value = 600,
          min = 0,
          max = 1000,
          step = 0.5
        )
      ),
      shinyjs::disabled(
        shiny::actionButton(
          ns("replace_outliers_button"),
          label = "Replace outliers with NAs",
          icon = icon("eraser"),
          class = "btn-warning",
          width = "100%"
        )
      ),
      br(),
      br(),
      div(style = "display:center-align;float:center ;width:100%;text-align: center;", shiny::downloadButton(ns("dataset_download"), "Download dataset"))
    )
  )
}

#' @rdname mod_preprocessing_outliers
#' 
#' @export
mod_preprocessing_outliers_ui_output <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::conditionalPanel( condition = sprintf("input['%s']!='Majority Voting 2/3'", ns('outldet_method')),
                             shinyWidgets::dropdownButton( size = "sm",
                                                           tags$h4("Graphical parameters"),
                                                           shiny::checkboxInput(ns("n_obs"),   label = 'Show boxplot stats', value = FALSE),
                                                           numericInput(ns('plot_fontsize'),   label = 'Font size:', value = 12, step = 1),
                                                           numericInput(ns('plot_dl_width'),   label = 'Width of plot to be downloaded (px):', value = 700, step = 10),
                                                           numericInput(ns('plot_dl_height'),  label = 'Height of plot to be downloaded (px):', value = 500, step = 10),
                                                           div(style="display:left-align;float:left ;width:100%;text-align: left;", shiny::downloadButton(ns("plot_download"),"Download plot")),
                                                           circle = TRUE, status = "primary", icon = icon("gear"), width = "400px",
                                                           tooltip = shinyWidgets::tooltipOptions(title = "Click to modify or download plot")
                             ),
                             shinycssloaders::withSpinner(shiny::plotOutput(ns("plot1")))
    ),
    tabsetPanel(
      tabPanel("Main dataset",   br(), shinycssloaders::withSpinner( DT::dataTableOutput(ns("main_dataset")))  ),
      tabPanel("Outliers table", br(), shinycssloaders::withSpinner(     shiny::htmlOutput( ns("outl_percent")) ),  br(), shinycssloaders::withSpinner(DT::dataTableOutput(ns("outliers_table"))))
    )
  )
}

#' @rdname mod_preprocessing_outliers
#' 
#' @param infile A reactive boolean used to understand if a dataset has been loaded on client side. It is used to disable buttons and avoids incorrect user inputs. Pass as \code{reactive({...})}.
#' @param rvs_dataset A reactive values dataset created from \code{reactiveValues()} and passed to the module from the external environment. Pass as \code{reactive({...})}.
#' 
#' @export
mod_preprocessing_outliers_server <- function(id, infile = NULL, rvs_dataset){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # dataset download function
    output$dataset_download <- shiny::downloadHandler(
      filename = "download_dataset.csv",
      content = function(file) {
        utils::write.csv(rvs_dataset(), file)
      }
    )
    
    ###### UPDATE INPUTS ----------------------------------------------------------------------
    
    # Update selectInput according to dataset in input
    observe({
      req( !is.null(infile())  )
      
      
      # updates dataset variables selection depending on the current dataset
      variable_choices <- colnames(rvs_dataset())[sapply(rvs_dataset(), is.numeric)]
      facet_choices    <- c("NULL", colnames(rvs_dataset())[sapply(rvs_dataset(), is.factor)])
      graphx_choices   <- colnames(rvs_dataset())[sapply(rvs_dataset(), lubridate::is.POSIXct)]
      
      updateSelectInput(session, "variable",   choices = variable_choices)
      updateSelectInput(session, "facetwrap",  choices = facet_choices)
      updateSelectInput(session, "facetwrap4", choices = facet_choices)
      updateSelectInput(session, "graphx2",    choices = graphx_choices)
      updateSelectInput(session, "graphx3",    choices = graphx_choices)
      
      
      # only if breview has been clicked we enable cleaning
      req(input$variable)
      shinyjs::enable("replace_outliers_button")
    })
    
    ###### WARNINGS ----------------------------------------------------------------------
    
    observeEvent(input$k_iqr_range, {
      # give feedback depending on the k_iqr_range
      if (input$k_iqr_range < 1.5) {
        shinyFeedback::showFeedbackWarning("k_iqr_range",
                                           "Very low IQR multipliers may result in extremely high numbers of outliers detected!")
      }
      else if (input$k_iqr_range > 3) {
        shinyFeedback::showFeedbackWarning("k_iqr_range",
                                           "Very high IQR multpliers may result in extremely low numbers of outliers detected!")
      } else {
        # hide previous feedback if any
        shinyFeedback::hideFeedback("k_iqr_range")
      }
    })
    
    observeEvent(input$k_iqr_range4, {
      # give feedback depending on the k_iqr_range
      if (input$k_iqr_range4 < 1.5) {
        shinyFeedback::showFeedbackWarning("k_iqr_range4",
                                           "Very low IQR multipliers may result in extremely high numbers of outliers detected!")
      }
      else if (input$k_iqr_range4 > 3) {
        shinyFeedback::showFeedbackWarning("k_iqr_range4",
                                           "Very high IQR multipliers may result in extremely low numbers of outliers detected!")
      } else {
        # hide previous feedback if any
        shinyFeedback::hideFeedback("k_iqr_range4")
      }
    })
    
    ###### INTERNAL DATASET DEFINITIONS----------------------------------------------------------------------
    
    # input dataset grouped by facet wrap and with row nmber
    data_set_filtrato_iniziale <- reactive({
      rvs_dataset() %>%
        dplyr::mutate(rownumber=row_number()) %>%
        dplyr::group_by_(input$facetwrap) 
    })
    
    data_set_filtrato_intermedio <- reactive({
      switch (input$outldet_method,
              "Boxplot" = data_set_filtrato_iniziale()[input$main_dataset_rows_all,] %>%
                dplyr::mutate(outlier = is_outlier1(.data[[input$variable]], input$k_iqr_range, input$outl_type)),
              "Method2" =  data_set_filtrato_iniziale()[input$main_dataset_rows_all,] %>%
                dplyr::mutate(outlier = is_outlier2(.data[[input$variable]], input$limit_cond_2)),
              "Method3" = data_set_filtrato_iniziale()[input$main_dataset_rows_all,] %>%
                dplyr::mutate(outlier = is_outlier3(.data[[input$variable]], input$limit_cond_3)),
              "Majority Voting 2/3" = data_set_filtrato_iniziale()[input$main_dataset_rows_all,] %>%
                dplyr::mutate(outlier1 = is_outlier1(.data[[input$variable]], input$k_iqr_range4, input$outl_type4)) %>%
                dplyr::mutate(outlier2 = is_outlier2(.data[[input$variable]], input$limit_cond_4_2)) %>%
                dplyr::mutate(outlier3 = is_outlier3(.data[[input$variable]], input$limit_cond_4_3))
      )
    })
    
    data_set_filtrato_intermedio_norownumbers <- reactive({
      data_set_filtrato_intermedio() %>%
        select(-rownumber)
    })
    
    percent <- reactive({
      if(input$outldet_method!="Majority Voting 2/3"){
        res <- nrow(data_set_filtrato_intermedio()[data_set_filtrato_intermedio()$outlier == TRUE & !is.na(data_set_filtrato_intermedio()$outlier),])/nrow(data_set_filtrato_intermedio())*100
      }
      else{
        res <- (nrow(data_set_filtrato_intermedio()[data_set_filtrato_intermedio()$outlier1 == TRUE & !is.na(data_set_filtrato_intermedio()$outlier1) & data_set_filtrato_intermedio()$outlier2 == TRUE & !is.na(data_set_filtrato_intermedio()$outlier2),])+
                  nrow(data_set_filtrato_intermedio()[data_set_filtrato_intermedio()$outlier2 == TRUE & !is.na(data_set_filtrato_intermedio()$outlier2) & data_set_filtrato_intermedio()$outlier3 == TRUE & !is.na(data_set_filtrato_intermedio()$outlier3),])+
                  nrow(data_set_filtrato_intermedio()[data_set_filtrato_intermedio()$outlier1 == TRUE & !is.na(data_set_filtrato_intermedio()$outlier1) & data_set_filtrato_intermedio()$outlier3 == TRUE & !is.na(data_set_filtrato_intermedio()$outlier3),])-
                  2* nrow(data_set_filtrato_intermedio()[data_set_filtrato_intermedio()$outlier1 == TRUE & !is.na(data_set_filtrato_intermedio()$outlier1) & data_set_filtrato_intermedio()$outlier2 == TRUE & !is.na(data_set_filtrato_intermedio()$outlier2) & data_set_filtrato_intermedio()$outlier3 == TRUE & !is.na(data_set_filtrato_intermedio()$outlier3),]))/nrow(data_set_filtrato_intermedio())*100
      }
      
      round(res,2)
    })
    
    ###### OUTPUTS ----------------------------------------------------------------------
    
    
    output$main_dataset <- DT::renderDT({
      validate(need(input$variable, "Please provide a valid dataset."))
      req(input$variable)
      DT::datatable(rvs_dataset(),
                    selection = "none",
                    rownames = FALSE,
                    style = "bootstrap",
                    filter = "top", #fbox
                    escape = FALSE,
                    ## must use fillContainer = FALSE to address
                    ## see https://github.com/rstudio/DT/issues/367
                    ## https://github.com/rstudio/DT/issues/379
                    fillContainer = FALSE,
                    ## works with client-side processing
                    extensions = "KeyTable",
                    options = list(
                      keys = TRUE,
                      autoWidth = FALSE, # permits to adapt the columns to the width of the box
                      scrollX = 500, # permits to scroll along x
                      search = list(regex = TRUE),
                      columnDefs = list(
                        list(
                          orderSequence = c("desc", "asc"),
                          targets = "_all"
                        ),
                        list(className = "dt-center", targets = "_all")
                      ),
                      processing = FALSE,
                      pageLength = 10,
                      lengthMenu = list(c(5, 10, 25, 50,-1), c("5", "10", "25", "50", "All"))
                    )
      )
    })
    
    output$outl_percent <- renderText({
      validate(need(input$variable, "Please provide a valid dataset."))
      req(input$variable)
      
      paste("The percentage of outliers in the selected portion of the dataset is <code>",  percent(),  "%</code>")
    })
    
    output$outliers_table <- DT::renderDT({
      
      req(input$variable)
      DT::datatable(
        if(input$outldet_method!="Majority Voting 2/3"){
          dplyr::filter(data_set_filtrato_intermedio_norownumbers(), outlier == TRUE)}
        else{
          dplyr::filter(data_set_filtrato_intermedio_norownumbers(), (outlier1 == TRUE & outlier2 == TRUE) | (outlier2 == TRUE & outlier3 == TRUE) | (outlier1 == TRUE & outlier3 == TRUE))
        },
        selection = "none",
        rownames = FALSE,
        style = "bootstrap",
        # filter = "top", #fbox
        escape = FALSE,
        ## must use fillContainer = FALSE to address
        ## see https://github.com/rstudio/DT/issues/367
        ## https://github.com/rstudio/DT/issues/379
        fillContainer = FALSE,
        ## works with client-side processing
        extensions = "KeyTable",
        options = list(
          keys = TRUE,
          autoWidth = FALSE, # permits to adapt the columns to the width of the box
          scrollX = 500, # permits to scroll along x
          search = list(regex = TRUE),
          columnDefs = list(
            list(
              orderSequence = c("desc", "asc"),
              targets = "_all"
            ),
            list(className = "dt-center", targets = "_all")
          ),
          processing = FALSE,
          pageLength = 10,
          lengthMenu = list(c(5, 10, 25, 50,-1), c("5", "10", "25", "50", "All"))
        )
      )
    })
    
    # plot boxplot or scatter plot
    
    output$plot1 <- renderPlot({
      # requires a facet variable to plot
      validate(need(input$variable, "Please provide a valid dataset."))
      req(input$variable)
      
      if (input$outldet_method == "Boxplot") {
        if(input$facetwrap != "NULL"){
          plot <- ggplot2::ggplot(
            data = data_set_filtrato_intermedio(), 
            ggplot2::aes_string(
              x = input$facetwrap,
              y = input$variable,
              group = input$facetwrap,
              fill = input$facetwrap
            )
          ) +
            # adds error bars
            stat_boxplot(geom ='errorbar', 
                         coef = input$k_iqr_range,
                         width = 0.6,
                         na.rm = TRUE ) + 
            # adds box plot
            ggplot2::geom_boxplot(
              coef = input$k_iqr_range,
              width = 0.6,
              na.rm = TRUE # fixes Warning: Removed N rows containing non-finite values (stat_boxplot).
            ) +
            { # adds summary
              if ( input$n_obs==TRUE){
                ggplot2::stat_summary(fun.data = n_fun1, geom = "text", na.rm = TRUE, hjust=0.5, vjust=1)
              }
            }+
            {
              if ( input$n_obs==TRUE){
                ggplot2::stat_summary(fun.data = n_fun2, geom = "text", na.rm = TRUE, hjust=0.5, vjust=-0.5)
              }
            }+
            ggplot2::theme_bw() +
            theme(legend.position = "none",
                  panel.grid = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.title  = element_text(size = input$plot_fontsize),
                  axis.text.x = element_text(size = input$plot_fontsize, margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
                  axis.text.y = element_text(size = input$plot_fontsize, margin = unit(c(0.3,0.3,0.3,0.3), "cm")))
        }
        else{
          plot <- ggplot2::ggplot(
            data = data_set_filtrato_intermedio(),
            ggplot2::aes_string(
              y = input$variable
            )
          ) +
            stat_boxplot(geom ='errorbar', 
                         coef = input$k_iqr_range,
                         width = 0.6,
                         na.rm = TRUE ) + # adds error bars
            ggplot2::geom_boxplot(
              coef = input$k_iqr_range,
              width = 0.6,
              aes_string(y=input$variable),
              fill = "skyblue2", 
              na.rm = TRUE # fixes Warning: Removed N rows containing non-finite values (stat_boxplot).
            ) +
            {
              if (input$n_obs == TRUE){
                ggplot2::stat_summary(fun.data = n_fun1, geom = "text", na.rm = TRUE, hjust=0.5, vjust=1, mapping = aes(x = 0))
              }
            }+
            {
              if (input$n_obs == TRUE){
                ggplot2::stat_summary(fun.data = n_fun2, geom = "text", na.rm = TRUE, hjust=0.5, vjust=-0.5, mapping = aes(x = 0))
              }
            }+
            ggplot2::theme_bw() +
            theme(legend.position = "none",
                  panel.grid = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.title=element_text(size=input$plot_fontsize),
                  axis.text.x = element_text(size=input$plot_fontsize, margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
                  axis.text.y = element_text(size=input$plot_fontsize, margin = unit(c(0.3,0.3,0.3,0.3), "cm")))
        }
        
      } else  if (input$outldet_method == "Method2") {
        plot <- ggplot2::ggplot(data_set_filtrato_intermedio(), 
                                ggplot2::aes_string(
                                  x = input$graphx2,
                                  y = input$variable
                                )
        ) +
          ggplot2::geom_point(na.rm = TRUE)+
          ggplot2::geom_point(data = data_set_filtrato_intermedio()[data_set_filtrato_intermedio()$outlier == TRUE,], color = 'red', na.rm = TRUE)
        
      } else  if (input$outldet_method == "Method3") {
        plot <- ggplot2::ggplot(data_set_filtrato_intermedio(), 
                                ggplot2::aes_string(
                                  x = input$graphx3,
                                  y = input$variable
                                )
        ) +
          ggplot2::geom_point(na.rm = TRUE)+
          ggplot2::geom_point(data = data_set_filtrato_intermedio()[data_set_filtrato_intermedio()$outlier == TRUE,], color = 'red', na.rm = TRUE)
      }
      

      #plot download function
      output$plot_download <- shiny::downloadHandler(
        filename = "plot.png",
        content = function(file) {
          ggplot2::ggsave(
            file,
            device = png,
            plot = plot,
            width = input$plot_dl_width,
            height = input$plot_dl_height,
            limitsize = FALSE
          )
        }
      )
      
      plot
      
    })
    
  
    
    ###### MODAL and PROCESS ----------------------------------------------------------------------
    # function for dialog box when clicking on replace with NAs button
    modal_confirm <- function(){
      ns <- session$ns
      shiny::modalDialog(
        size = 'm',
        easyClose = TRUE,
        fade = TRUE,
        title = "Replace outliers with NAs",
        shiny::HTML("<strong style='color:red;' >WARNING: </strong> after replacing outliers with NAs, the current dataset will be overwritten. Continue?"),
        footer = tagList(
          actionButton(ns("no"), "Dismiss"),
          actionButton(ns("yes"),"Confirm", class = "btn btn-danger")
        )
      )
    }
    
    observeEvent(input$replace_outliers_button,{
      # ask users if they want to replace outliers
      shiny::showModal(modal_confirm())
    })
    
    observeEvent(input$no,{
      shiny::removeModal()
    })
    
    # Define the ReactiveValue to return : "toReturn"
    # with slots "rvs_dataset" & "trigger"
    toReturn <- reactiveValues(dataset = NULL,  trigger = 0)
    
    
    observeEvent(input$yes,{
      
      # notification of process
      id <- showNotification("Performing cleaning...", duration = NULL, closeButton = FALSE, type = "message")
      on.exit(removeNotification(id), add = TRUE)
      
      shiny::removeModal()
      
      # find in the intermediate dataframe the rows corresponding to outliers
      rows_outliers <- 
        if(input$outldet_method!="Majority Voting 2/3"){
          data_set_filtrato_intermedio() %>%
            dplyr::filter(outlier == TRUE)
        }
      else{
        data_set_filtrato_intermedio() %>%
          dplyr::filter((outlier1 == TRUE & outlier2 == TRUE) | (outlier2 == TRUE & outlier3 == TRUE) | (outlier1 == TRUE & outlier3 == TRUE))
      }
      
      # return the main dataset with NAs in the column where outliers are tagged as TRUE
      toReturn$dataset <- rvs_dataset() %>%
        dplyr::mutate(rownumber = row_number()) %>%
        dplyr::mutate(!!input$variable := ifelse(rownumber %in%  rows_outliers$rownumber, NA, .data[[input$variable]])) %>%
        dplyr::select(-rownumber)
      # update trigger for the external environment
      toReturn$trigger <- toReturn$trigger +1
      
    })
    # return to the external environment the reactivevalues list
    return(toReturn)
  })
}

#' Shiny app snippet to offline test the functionality of the modules.
#' Comment and uncomment when necessary.
#' devtools::document() to render roxygen comments an preview with ?mod_manage_addColumn
#' @noRd
# 
# library(shiny)
# library(shinydashboard)
# library(shiny)
# library(ggplot2)
# library(magrittr)
# library(shinyBS)
# library(shinyWidgets)
# library(dplyr)
# library(rpart)
# library(stats)
# library(shinyFeedback)
# library(partykit)
# library(MLmetrics)
# library(grid)
# library(shinyjs)
# 
# ui <- dashboardPage(
#   dashboardHeader(disable = TRUE),
#   dashboardSidebar(disable = TRUE),
#   dashboardBody(
#     shinyjs::useShinyjs(),
#     shinyFeedback::useShinyFeedback(),
#     column(width = 4,
#            box(width = 12,
#                mod_preprocessing_cleaning_ui_input("preprocessing_cleaning_ui_1")
#            )
#     ),
#     column(width = 8,
#            box(width = 12,
#                mod_preprocessing_cleaning_ui_output("preprocessing_cleaning_ui_1")
#            )
#     )
#   )
# )
# server <- function(input, output, session) {
# 
#   data_rv <- reactiveValues( df_tot = eDASH::data)                 # reactive value to store the loaded dataframes
#   data_rv_results <- reactiveValues(infile = TRUE)  # NULL to simulate no dataset added, TRUE to simulate dataset added
# 
#   data_cart <-  mod_preprocessing_cleaning_server("preprocessing_cleaning_ui_1",
#                                                   infile = reactive({data_rv_results$infile}),
#                                                   rvs_dataset = reactive({data_rv$df_tot}) )
#   # When applied function (data_mod2$trigger change) :
#   #   - Update rv$variable with module output "variable"
#   #   - Update rv$fun_history with module output "fun"
#   # observeEvent(data_rename$trigger, {
#   #   req(data_rename$trigger > 0)
#   #   data_rv$df_tot    <- data_rename$dataset
#   # })
# 
# 
# }
# 
# shinyApp(ui, server)
