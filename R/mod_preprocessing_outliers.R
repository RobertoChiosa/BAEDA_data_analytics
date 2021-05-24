#' preprocessing_outliers UI Function
#'
#' @description A shiny Module. I divided the module into
#' - input UI where the inputs parameters are
#' - output UI where the output plots/tables are
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import shiny
#' @import dplyr
#' @import magrittr
#' @import ggplot2
#' @importFrom shinyFeedback showFeedbackWarning hideFeedback
#' @importFrom stats na.omit quantile IQR
#' @importFrom shinyWidgets dropdownButton
#' @importFrom shinyBS bsTooltip
#' @importFrom stats median
#' @importFrom utils write.csv
#' @importFrom shinyjs disable runjs
mod_preprocessing_outliers_ui_input <- function(id) {
  ns <- NS(id)

  shiny::tagList(
    box(
      shiny::selectInput(ns("outldet_method"), 'Select outlier detection method', 
                         choices = c("Boxplot",
                                     "Method2",
                                     "Method3",
                                     "Majority Voting 2/3")
      ), 
      shiny::selectInput(ns("variable"), 'Select variable', choices = NULL),
      shiny::conditionalPanel( condition = sprintf("input['%s']=='Boxplot'", ns('outldet_method')),
                               shiny::selectInput(ns("facetwrap"), 'Select the variable to facet by', choices = NULL),
                               shiny::selectInput(
                                 ns("outl_type"),
                                 'Select the outlier type to show in "Outliers Table"',
                                 choices = c("upper", "lower", "all"),
                                 selected = "all"
                               ),
                               shiny::checkboxInput(ns("n_obs"), 'Show boxplot stats', value = FALSE), 
                               shiny::sliderInput(
                                 ns("k_iqr_range"),
                                 'Select the interquartile range multiplier for outliers detection:',
                                 value = 1.5,
                                 min = 1,
                                 max = 10,
                                 step = 0.1
                               )),
      #simple outlier detection parameters, to replace with the parameters needed for the selected outliers detection function, such as iqr range for boxplots
      shiny::conditionalPanel(condition = sprintf("input['%s']=='Method2'", ns('outldet_method')),
                              shiny::selectInput(ns("graphx2"),'Select x variable for the scatter plot', choices = NULL),
                              shiny::sliderInput(
                                ns("limit_cond_2"),
                                'Select the threshold for outlier detection:',
                                value = 550,
                                min = 0,
                                max = 1000,
                                step = 0.5
                              )
      ),
      shiny::conditionalPanel(condition = sprintf("input['%s']=='Method3'", ns('outldet_method')),
                              shiny::selectInput(ns("graphx3"),'Select x variable for the scatter plot', choices = NULL),
                              shiny::sliderInput(
                                ns("limit_cond_3"),
                                'Select the threshold for outlier detection:',
                                value = 600,
                                min = 0,
                                max = 1000,
                                step = 0.5
                              )
      ),
      shiny::conditionalPanel(condition = sprintf("input['%s']=='Majority Voting 2/3'", ns('outldet_method')),
        shiny::selectInput(ns("facetwrap4"), 'Select the variable to facet by (boxplot)', choices = NULL),
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
      shiny::actionButton(
        ns("replace_outliers_button"),
        label = "Replace outliers with NAs",
        icon = icon("eraser"),
        class = "btn-warning",
        width = "100%"
      ),
      br(),
      br(),
      div(style="display:center-align;float:center ;width:100%;text-align: center;", shiny::downloadButton(ns("dataset_download"),"Download dataset")),
      solidHeader = T, collapsible = T, collapsed = FALSE, width = 12,
      title = "Outliers identification", status = "primary"
    )
  )
}

mod_preprocessing_outliers_ui_output <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::conditionalPanel( condition = sprintf("input['%s']!='Majority Voting 2/3'", ns('outldet_method')),
    shinyWidgets::dropdownButton( size = "sm",
                                  tags$h4("Graphical parameters"),
                                  numericInput(ns('plot_fontsize'), label = 'Font size:', value = 11, step = 1),
                                  numericInput(ns('plot_dl_width'), label = 'Width of plot to be downloaded (px):', value = 1500, step = 10),
                                  numericInput(ns('plot_dl_height'), label = 'Height of plot to be downloaded (px):', value = 500, step = 10),
                                  div(style="display:left-align;float:left ;width:100%;text-align: left;", shiny::downloadButton(ns("plot_download"),"Download plot")),
                                  circle = TRUE, status = "primary", icon = icon("gear"), width = "400px",
                                  tooltip = shinyWidgets::tooltipOptions(title = "Click to modify or download plot")
    ),
    shinycssloaders::withSpinner(shiny::plotOutput(ns("plot1")))
    ),
    tabsetPanel(
      tabPanel("Main dataset", br(), shinycssloaders::withSpinner(DT::dataTableOutput(ns("main_dataset")))),
      tabPanel("Outliers table"  , br(), shinycssloaders::withSpinner(shiny::verbatimTextOutput(ns("outl_percent"))),  br(), shinycssloaders::withSpinner(DT::dataTableOutput(ns("outliers_table"))))
    )
  )
}

#' preprocessing_outliers Server Functions  
#' @noRd 
mod_preprocessing_outliers_server <- function(id, data_set){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # dataset download function
    output$dataset_download <- shiny::downloadHandler(
      filename = "download_dataset.csv",
      content = function(file) {
        utils::write.csv(data_set(), file)
      }
    )
    
    # Update selectInput according to dataset in input
    observe({
      # updates dataset variables selection depending on the current dataset
      variable_choices <- colnames(data_set())[sapply(data_set(), is.numeric)]
      facet_choices    <- c("NULL", colnames(data_set())[sapply(data_set(), is.factor)])
      graphx_choices   <- colnames(data_set())[sapply(data_set(), lubridate::is.POSIXct)]
      
      updateSelectInput(session, "variable",   choices = variable_choices)
      updateSelectInput(session, "facetwrap",  choices = facet_choices)
      updateSelectInput(session, "facetwrap4", choices = facet_choices)
      updateSelectInput(session, "graphx2",    choices = graphx_choices)
      updateSelectInput(session, "graphx3",    choices = graphx_choices)
    })
    
    # give feedback about inputs
    observeEvent(input$k_iqr_range, {
      # hide previous feedback if any
      shinyFeedback::hideFeedback("k_iqr_range")
      # give feedback depending on the k_iqr_range
      if (input$k_iqr_range < 1.5) {
        shinyFeedback::showFeedbackWarning("k_iqr_range",
                                           "Very low IQR multipliers may result in extremely high numbers of outliers detected!")
      }
      else if (input$k_iqr_range > 3) {
        shinyFeedback::showFeedbackWarning("k_iqr_range",
                                           "Very high IQR multpliers may result in extremely low numbers of outliers detected!")
      }
    })
    
    
    observeEvent(input$k_iqr_range4, {
      # hide previous feedback if any
      shinyFeedback::hideFeedback("k_iqr_range4")
      # give feedback depending on the k_iqr_range
      if (input$k_iqr_range4 < 1.5) {
        shinyFeedback::showFeedbackWarning("k_iqr_range4",
                                           "Very low IQR multipliers may result in extremely high numbers of outliers detected!")
      }
      else if (input$k_iqr_range4 > 3) {
        shinyFeedback::showFeedbackWarning("k_iqr_range4",
                                           "Very high IQR multipliers may result in extremely low numbers of outliers detected!")
      }
    })
    
    ## consider to move out from modules and to define global functions
    # function to detect outliers 
    # input:
    # x = a vector [numeric]
    # k_iqr_range = a numeric range [numeric]
    # return a logic vector
    is_outlier1 <-  function(x, k_iqr_range, type) {
      if (type == "all"){
        return(x < stats::quantile(x, 0.25, na.rm=TRUE) - k_iqr_range * stats::IQR(x, na.rm=TRUE) |
                 x > stats::quantile(x, 0.75, na.rm=TRUE) + k_iqr_range * stats::IQR(x, na.rm=TRUE))
      }
      if(type == "upper"){
        return(x > stats::quantile(x, 0.75, na.rm=TRUE) + k_iqr_range * stats::IQR(x, na.rm=TRUE))
      }
      if(type == "lower"){
        return(x < stats::quantile(x, 0.25, na.rm=TRUE) - k_iqr_range * stats::IQR(x, na.rm=TRUE))
      }
    }
    
    #simple functions to define outliers, in order to test full functionality of the structure
    is_outlier2 <- function(x, limit){
      return(x > limit)
    }
    
    is_outlier3 <- function(x, limit){
      return(x > limit)
    }
    
    
    # functions to define where to put the stats for each boxplot
    n_fun1<-function(x){
      return(data.frame(y=Inf, label= paste('count =', length(x))
      ))
    }
    
    n_fun2<-function(x){
      return(data.frame(y=-Inf, label= paste('median =', stats::median(x))
      ))
    }
    
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
    
    
    
    # this is defined internally
    data_set_filtrato_iniziale <- reactive(
      data_set() %>%
        dplyr::mutate(rownumber=row_number()) %>%
        dplyr::group_by_(input$facetwrap) 
    )
    
    
    output$main_dataset <- DT::renderDT({
      req(input$facetwrap)
      DT::datatable(data_set(),
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
    
    
  
    data_set_filtrato_intermedio <- reactive(
      if(input$outldet_method=="Boxplot"){
        data_set_filtrato_iniziale()[input$main_dataset_rows_all,] %>%
          dplyr::mutate(outlier = is_outlier1(.data[[input$variable]], input$k_iqr_range, input$outl_type))
      }
      else{
        if(input$outldet_method=="Method2"){
          data_set_filtrato_iniziale()[input$main_dataset_rows_all,] %>%
            dplyr::mutate(outlier = is_outlier2(.data[[input$variable]], input$limit_cond_2))
        }
        else{
          if(input$outldet_method=="Method3"){
            data_set_filtrato_iniziale()[input$main_dataset_rows_all,] %>%
              dplyr::mutate(outlier = is_outlier3(.data[[input$variable]], input$limit_cond_3))
          }
          else{
            if(input$outldet_method=="Majority Voting 2/3"){
              data_set_filtrato_iniziale()[input$main_dataset_rows_all,] %>%
                dplyr::mutate(outlier1 = is_outlier1(.data[[input$variable]], input$k_iqr_range4, input$outl_type4)) %>%
                dplyr::mutate(outlier2 = is_outlier2(.data[[input$variable]], input$limit_cond_4_2)) %>%
                dplyr::mutate(outlier3 = is_outlier3(.data[[input$variable]], input$limit_cond_4_3))
            }
          }}}
    )
    
    
    data_set_filtrato_intermedio_norownumbers <- reactive(
      data_set_filtrato_intermedio() %>%
      select(-rownumber)
    )
    
    
    percent <- reactive(
      if(input$outldet_method!="Majority Voting 2/3"){
      nrow(data_set_filtrato_intermedio()[data_set_filtrato_intermedio()$outlier == TRUE & !is.na(data_set_filtrato_intermedio()$outlier),])/nrow(data_set_filtrato_intermedio())*100
      }
      else{
        (nrow(data_set_filtrato_intermedio()[data_set_filtrato_intermedio()$outlier1 == TRUE & !is.na(data_set_filtrato_intermedio()$outlier1) & data_set_filtrato_intermedio()$outlier2 == TRUE & !is.na(data_set_filtrato_intermedio()$outlier2),])+
        nrow(data_set_filtrato_intermedio()[data_set_filtrato_intermedio()$outlier2 == TRUE & !is.na(data_set_filtrato_intermedio()$outlier2) & data_set_filtrato_intermedio()$outlier3 == TRUE & !is.na(data_set_filtrato_intermedio()$outlier3),])+
        nrow(data_set_filtrato_intermedio()[data_set_filtrato_intermedio()$outlier1 == TRUE & !is.na(data_set_filtrato_intermedio()$outlier1) & data_set_filtrato_intermedio()$outlier3 == TRUE & !is.na(data_set_filtrato_intermedio()$outlier3),])-
          2* nrow(data_set_filtrato_intermedio()[data_set_filtrato_intermedio()$outlier1 == TRUE & !is.na(data_set_filtrato_intermedio()$outlier1) & data_set_filtrato_intermedio()$outlier2 == TRUE & !is.na(data_set_filtrato_intermedio()$outlier2) & data_set_filtrato_intermedio()$outlier3 == TRUE & !is.na(data_set_filtrato_intermedio()$outlier3),]))/nrow(data_set_filtrato_intermedio())*100
      }
     )
    
    
    
      
    output$outl_percent <- renderPrint({
      sprintf("The percentage of outliers in the selected portion of the dataset is %f%%", percent())
    }
    )
    
    
    output$outliers_table <- DT::renderDT({
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
    # reactive in order to permit download
    plotdata <- reactive(
      if(input$outldet_method == "Boxplot"){
        if(input$facetwrap != "NULL"){
        ggplot2::ggplot(
          data = data_set_filtrato_intermedio(), 
          ggplot2::aes_string(
            x = input$facetwrap,
            y = input$variable,
            group = input$facetwrap,
            fill = input$facetwrap
          )
        ) +
          stat_boxplot(geom ='errorbar', 
                       coef = input$k_iqr_range,
                       width = 0.6,
                       na.rm = TRUE ) + # adds error bars
          ggplot2::geom_boxplot(
            coef = input$k_iqr_range,
            width = 0.6,
            na.rm = TRUE # fixes Warning: Removed N rows containing non-finite values (stat_boxplot).
          ) +
          {
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
                axis.title=element_text(size=input$plot_fontsize),
                axis.text.x = element_text(size=input$plot_fontsize, margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
                axis.text.y = element_text(size=input$plot_fontsize, margin = unit(c(0.3,0.3,0.3,0.3), "cm")))
        }
        else{
          ggplot2::ggplot(
            data = data_set_filtrato_intermedio(),
            ggplot2::aes_string(
              y = input$variable
            )
          )+
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
      }
      else{
        if(input$outldet_method == "Method2"){
          ggplot2::ggplot(data_set_filtrato_intermedio(), 
                          ggplot2::aes_string(
                            x = input$graphx2,
                            y = input$variable
                          )
          ) +
            ggplot2::geom_point(na.rm = TRUE)+
            ggplot2::geom_point(data = data_set_filtrato_intermedio()[data_set_filtrato_intermedio()$outlier == TRUE,], color = 'red', na.rm = TRUE)
        }
        else{
          if(input$outldet_method == "Method3"){
            ggplot2::ggplot(data_set_filtrato_intermedio(), 
                            ggplot2::aes_string(
                              x = input$graphx3,
                              y = input$variable
                            )
            ) +
              ggplot2::geom_point(na.rm = TRUE)+
              ggplot2::geom_point(data = data_set_filtrato_intermedio()[data_set_filtrato_intermedio()$outlier == TRUE,], color = 'red', na.rm = TRUE)
          }
        }
      }
    )
    
    output$plot1 <- renderPlot({
      req(input$facetwrap)
      plotdata()
    })
    
    
    #plot download function
    output$plot_download <- shiny::downloadHandler(
      filename = "plot.png",
      content = function(file){
        ggplot2::ggsave(file, device = png, plot = plotdata(), width=input$plot_dl_width, height=input$plot_dl_height, limitsize=FALSE)
      }
    )
    
    observeEvent(input$yes,{
      
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
      toReturn$dataset <- data_set() %>%
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

# #the following code is used for debug
# library(shiny)
# library(dplyr)
# library(magrittr)
# library(ggplot2)
# library(viridis)
# library(shinyFeedback)
# library(stats)
# library(prob)
# library(shinydashboard)
# ui <- dashboardPage(
#   dashboardHeader(disable = TRUE),
#   dashboardSidebar(disable = TRUE),
#   dashboardBody(
#     fluidRow(
#       box(width = 4,
#           title = "Preprocessing degli outliers puntuali",
#           mod_preprocessing_outliers_ui_input("preprocessing_outliers_ui_1")
#       ),
#       box(width = 8,
#           mod_preprocessing_outliers_ui_output("preprocessing_outliers_ui_1")
#       )
#     )
#   )
# )
# 
# server <- function(input, output, session) {
#   data_rv <- reactiveValues( df_tot = df_15min )                 # reactive value to store the loaded dataframes
# 
#   data_rv <- reactiveValues( df_tot = df_15min )                 # reactive value to store the loaded dataframes
# 
#   ### preprocessing outliers detection
#   data_preprocessing <-  mod_preprocessing_outliers_server("preprocessing_outliers_ui_1",
#                                                            data_set = reactive({data_rv$df_tot}) )
#   # DONT DELETE I need for further integration
#   # When applied function (data_mod2$trigger change) :
#   #   - Update rv$variable with module output "variable"
#   #   - Update rv$fun_history with module output "fun"
#   observeEvent(data_preprocessing$trigger, {
#     req(data_preprocessing$trigger > 0)
#     data_rv$df_tot    <- data_preprocessing$dataset
#   })
# 
# }
# 
# shinyApp(ui, server)

## To be copied in the UI
# mod_preprocessing_outliers_ui_input("preprocessing_outliers_ui_1")
# mod_preprocessing_outliers_ui_output("preprocessing_outliers_ui_1")

## To be copied in the server
# mod_preprocessing_outliers_server("preprocessing_outliers_ui_1", data_set = reactive({data_rv$df_tot}))
