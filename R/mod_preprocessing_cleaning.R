#' @name mod_preprocessing_cleaning
#' @aliases mod_preprocessing_cleaning_ui_input
#' @aliases mod_preprocessing_cleaning_ui_output
#' @aliases mod_preprocessing_cleaning_server
#' 
#' @title Preprocessing Module
#' 
#' @description 
#' This module permits to preprocess datasets through different methods.
#' 
#' @param id,input,output,session Internal parameters for {shiny}.
#' 
#' @examples \dontrun{
#' 
#' # To be copied in the UI
#' mod_preprocessing_cleaning_ui_input(id = "preprocessing_cleaning_ui_1")
#' mod_preprocessing_cleaning_ui_output(id = "preprocessing_cleaning_ui_1")
#' 
#' # To be copied in the server
#' mod_preprocessing_cleaning_server(id = "preprocessing_cleaning_ui_1",
#'                                  infile = reactive({data_rv_results$infile}), 
#'                                  rvs_dataset = reactive({data_rv[[input$dataframe]]})  
#')
#' }
#' 
#' @import shiny dplyr ggplot2 imputeTS htmltools
#' @importFrom shinyWidgets dropdownButton tooltipOptions
#' @importFrom shinycssloaders withSpinner
#' @importFrom magrittr `%>%`
#' @importFrom shinyFeedback feedbackWarning hideFeedback showFeedbackDanger
#' @importFrom plyr ddply
#' @importFrom stats reformulate
#' @importFrom simputation impute_cart
#' @importFrom lubridate is.POSIXct
#' @importFrom shinyjs disable enable
#' 
#' @rdname mod_preprocessing_cleaning
#' 
#' @export
mod_preprocessing_cleaning_ui_input <- function(id){
  ns <- NS(id)
  tagList(
    box(
      solidHeader = T,
      collapsible = T,
      collapsed = TRUE,
      width = 12,
      title = "NA replacement",
      status = "primary",
      
      
      shiny::selectInput(ns("variable"), 'Select variable', choices = NULL),
      column(
        width = 10,
        style = "padding-left:0px; padding-right:5px;",
        shiny::selectInput(
          ns("graphx"),
          'Select x variable for the plot (POSIXct required)',
          choices = NULL
        )
      ),
      
      column( width = 2,
              style = "padding-left:0px; padding-right:0px;",
              shinyjs::disabled(
                shiny::actionButton(
                  ns("plot_preview"),
                  label = "Plot",
                  style = "margin-top: 25px;",
                  icon = icon("refresh"),
                  class = "btn-success",
                  width = "100%"
                )
              )
      ),
      ###### METHOD SELECTION----------------------------------------------------------------------
      # this selector permits to chose among different methods of na imputation
      # - "Interpolation",
      # - "Weighted moving average",
      # - "Global constant",
      # - "Forward/Backward observation carrying",
      # - "Seasonal decomposition",
      # - "Seasonal splitting",
      # - "Lookup table",
      # - "Regression tree"
      # 
      shiny::selectInput(
        ns("imputmethod"),
        'Select the NA imputation method',
        choices = c(
          "Interpolation",
          "Weighted moving average",
          "Global constant",
          "Forward/Backward observation carrying",
          "Seasonal decomposition",
          "Seasonal splitting",
          "Lookup table",
          "Regression tree"
        )
      ),
      shiny::checkboxInput(ns("preview_check"),
                           'Show graphical preview of imputation results',
                           value = FALSE),
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] != 'Lookup table' & input['%s'] !='Regression tree'",ns('imputmethod'), ns('imputmethod') ),
        
        ###### INTERPOLATION ----------------------------------------------------------------------
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'Interpolation'", ns('imputmethod')),
          shiny::selectInput(
            inputId = ns("intmethod"),
            label = 'Select the interpolation algorithm',
            choices = c(
              "Linear interpolation"             = "linear",
              "Spline interpolation"             = "spline",
              "Stineman algorithm interpolation" = "stine"
            )
          ),
        ),
        ###### WEIGHTED MOOVING AVERAGE ----------------------------------------------------------------------
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'Weighted moving average'", ns('imputmethod')),
          shiny::selectInput(
            inputId = ns("weighting"),
            label = 'Select the weighting to be used',
            choices = c(
              "Exponential Weighted Moving Average (EWMA)" = "exponential",
              "Simple Moving Average (SMA)" = "simple",
              "Linear Weighted Moving Average (LWMA)" = "linear"
            )
          ),
          shiny::numericInput(
            inputId = ns("window_width"),
            label = 'Width of the moving average window',
            value = 2,
            min = 1,
            max = 10000,
            step = 1,
            width = '100%'
          ),
        ), 
        ###### GLOBAL CONSTANT ----------------------------------------------------------------------
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'Global constant'", ns('imputmethod')),
          shiny::selectInput(
            inputId = ns("global_function"),
            label = 'Select the function to be used for replacement',
            choices = c(
              "Mean"   = "mean",
              "Median" = "median",
              "Mode"   = "mode"
            )
          ),
        ), 
        ###### FORWARD/BACKWARD OBSERVATION CARRIED ----------------------------------------------------------------------
        shiny::conditionalPanel(
          condition = sprintf( "input['%s'] == 'Forward/Backward observation carrying'", ns('imputmethod')),
          shiny::selectInput(
            inputId = ns("option"),
            label = 'Select the algorithm to be used',
            choices = c(
              "Last Observation Carried Forward (LOCF)" = "locf",
              "Next Observation Carried Backward (NOCB)" = "nocb"
            )
          ),
          shiny::selectInput(
            inputId = ns("na_remaining"),
            label = 'Select the method to be used for remaining NAs',
            choices = c(
              "Perform LOCF/NOCB from the reverse direction" = "rev",
              "Return the series with NAs"                   = "keep",
              "Replace remaining NAs by overall mean"        = "mean"
            )
          ),
        ), 
        ###### SEASONAL DECOMPOSITION ----------------------------------------------------------------------
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'Seasonal decomposition'", ns('imputmethod')),
          shiny::selectInput(
            inputId = ns("dec_algorithm"),
            label = 'Select the algorithm to be used after decomposition',
            choices = c(
              "Interpolation" = "interpolation",
              "Forward/Backward observation carrying" = "locf",
              "Global constant" = "mean",
              "Weighted moving average" = "ma"
            )
          ),
          
          # SEASONAL DECOMPOSITION -> INTERPOLATION
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'interpolation'", ns('dec_algorithm')),
            shiny::selectInput(
              inputId = ns("intmethod5"),
              label = 'Select the interpolation algorithm',
              choices = c(
                "Linear interpolation"             = "linear",
                "Spline interpolation"             = "spline",
                "Stineman algorithm interpolation" = "stine"
              )
            ),
          ), 
          # SEASONAL DECOMPOSITION -> LOCF
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'locf'", ns('dec_algorithm')),
            shiny::selectInput(
              inputId = ns("option5"),
              label = 'Select the algorithm to be used',
              choices = c(
                "Last Observation Carried Forward (LOCF)"  = "locf",
                "Next Observation Carried Backward (NOCB)" = "nocb"
              )
            ),
            shiny::selectInput(
              inputId = ns("na_remaining5"),
              label = 'Select the method to be used for remaining NAs',
              choices = c(
                "Perform LOCF/NOCB from the reverse direction" = "rev",
                "Return the series with NAs"                   = "keep",
                "Replace remaining NAs by overall mean"        = "mean"
              )
            ),
          ),
          # SEASONAL DECOMPOSITION -> MEAN
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'mean'", ns('dec_algorithm')),
            shiny::selectInput(
              inputId = ns("global_function5"),
              label = 'Select the function to be used for replacement',
              choices = c(
                "Mean" = "mean",
                "Median" = "median",
                "Mode" = "mode"
              )
            ),
          ),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'ma'", ns('dec_algorithm')),
            shiny::selectInput(
              inputId = ns("weighting5"),
              label = 'Select the weighting to be used',
              choices = c(
                "Exponential Weighted Moving Average (EWMA)" = "exponential",
                "Simple Moving Average (SMA)"                = "simple",
                "Linear Weighted Moving Average (LWMA)"      = "linear"
              )
            ),
            shiny::numericInput(
              inputId = ns("window_width5"),
              label = 'Select the width of the moving average window',
              value = 2,
              min = 1,
              max = 10000,
              step = 1,
              width = '100%'
            ),
          ),
        ), 
        ###### SEASONAL SPLITTING ----------------------------------------------------------------------
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'Seasonal splitting'", ns('imputmethod')),
          shiny::selectInput(
            inputId = ns("split_algorithm"),
            label = 'Select the algorithm to be used after decomposition',
            choices = c(
              "Interpolation" = "interpolation",
              "Forward/Backward observation carrying" = "locf",
              "Global constant" = "mean",
              "Weighted moving average" = "ma"
            )
          ),
          # SEASONAL SPLITTING -> INTERPOLATION
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'interpolation'", ns('split_algorithm')),
            shiny::selectInput(
              inputId = ns("intmethod6"),
              'Select the interpolation algorithm',
              choices = c(
                "Linear interpolation" = "linear",
                "Spline interpolation" = "spline",
                "Stineman algorithm interpolation" = "stine"
              )
            ),
          ),
          # SEASONAL SPLITTING -> LOCF
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'locf'", ns('split_algorithm')),
            shiny::selectInput(
              inputId = ns("option6"),
              'Select the algorithm to be used',
              choices = c(
                "Last Observation Carried Forward (LOCF)" = "locf",
                "Next Observation Carried Backward (NOCB)" = "nocb"
              )
            ),
            shiny::selectInput(
              inputId = ns("na_remaining6"),
              'Select the method to be used for remaining NAs',
              choices = c(
                "Perform LOCF/NOCB from the reverse direction" = "rev",
                "Return the series with NAs" = "keep",
                "Replace remaining NAs by overall mean" = "mean"
              )
            ),
          ),
          # SEASONAL SPLITTING -> MEAN
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'mean'", ns('split_algorithm')),
            shiny::selectInput(
              inputId = ns("global_function6"),
              'Select the function to be used for replacement',
              choices = c(
                "Mean" = "mean",
                "Median" = "median",
                "Mode" = "mode"
              )
            ),
          ),
          # SEASONAL SPLITTING -> MA
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'ma'", ns('split_algorithm')),
            shiny::selectInput(
              inputId = ns("weighting6"),
              'Select the weighting to be used',
              choices = c(
                "Exponential Weighted Moving Average (EWMA)" = "exponential",
                "Simple Moving Average (SMA)" = "simple",
                "Linear Weighted Moving Average (LWMA)" = "linear"
              )
            ),
            shiny::numericInput(
              inputId = ns("window_width6"),
              'Select the width of the moving average window',
              value = 2,
              min = 1,
              max = 10000,
              step = 1,
              width = '100%'
            ),
          ),
        ),
        shiny::numericInput(
          inputId = ns("maxgap"),
          'Select the maximum number of successive NAs to perform imputation on',
          value = 3,
          min = 1,
          max = 10000,
          step = 1,
          width = '100%'
        ),
      ), 
      ###### LOOKUP TABLE ----------------------------------------------------------------------
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'Lookup table'", ns('imputmethod')),
        shiny::selectInput(
          inputId = ns("lookupmethod"),
          'Select the function to apply to construct the lookup table',
          choices = c("Mean", "Median")
        ),
        shiny::selectInput(
          inputId = ns("factor"),
          'Select the grouping factor(s)',
          multiple = TRUE,
          choices = NULL
        )
      ), 
      ###### REGRESSION TREEE ----------------------------------------------------------------------
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'Regression tree'", ns('imputmethod')),
        shiny::sliderInput(
          inputId = ns("maxdepth"),
          'Select the maximum tree depth',
          value = 10,
          min = 1,
          max = 30,
          step = 1
        ),
        shiny::sliderInput(
          inputId = ns("minsplit"),
          'Select the minimum number of observations in a node for further splitting',
          value = 2,
          min = 1,
          max = 50,
          step = 1
        ),
        shiny::sliderInput(
          inputId = ns("minbucket"),
          'Select the minimum number of observations for terminal nodes',
          value = 2,
          min = 1,
          max = 50,
          step = 1
        ),
        shiny::sliderInput(
          inputId = ns("complpar"),
          'Select the complexity parameter value',
          value = 0.01,
          min = 0,
          max = 0.1,
          step = 0.001
        ),
        shiny::selectInput(
          inputId = ns("predict"),
          'Select the predictor variable(s)',
          multiple = TRUE,
          choices = NULL
        )
      ),
      shinyjs::disabled(
        shiny::actionButton(
          inputId = ns("replacement_button"),
          label = "Perform NA replacement",
          icon = icon("pencil-alt"),
          class = "btn-success",
          width = "100%"
        )
      ))
  )
}

#' @rdname mod_preprocessing_cleaning
#' 
#' @export
mod_preprocessing_cleaning_ui_output <- function(id) {
  ns <- NS(id)
  tagList(
    # this button gives the possibility to modify plot parameters like
    # - fontsize
    # - width height of downloaded plot
    shinyWidgets::dropdownButton( size = "sm",
                                  tags$h4("Graphical parameters"),
                                  numericInput(ns('plot_fontsize'),  label = 'Font size:', value = 11, step = 1),
                                  numericInput(ns('plot_dl_width'),  label = 'Width of plot to be downloaded (px):', value = 1500, step = 10),
                                  numericInput(ns('plot_dl_height'), label = 'Height of plot to be downloaded (px):', value = 500, step = 10),
                                  div(style="display:left-align;float:left ;width:100%;text-align: left;", shiny::downloadButton(ns("plot_download"),"Download plot")),
                                  circle = TRUE, status = "primary", icon = icon("gear"), width = "400px",
                                  tooltip = shinyWidgets::tooltipOptions(title = "Click to modify plot")
    ),
    # plot of the whole timeseries
    shinycssloaders::withSpinner(shiny::plotOutput(ns("plot1"))),
    tabsetPanel(
      id = ns("tabs"),
      tabPanel( title = "Main dataset", # table representation of the original dataset
                br(),
                shinycssloaders::withSpinner(DT::dataTableOutput(ns("main_dataset")))
      ),
      tabPanel(title = "NAs Table",   # table representing the NA identified
               br(),
               shiny::verbatimTextOutput(ns("na_percent")),
               br(),
               shinycssloaders::withSpinner(DT::dataTableOutput(ns("na_table")) )
      )
    )
  )
}


#' @rdname mod_preprocessing_cleaning
#' 
#' @param infile A reactive boolean used to understand if a dataset has been loaded on client side. It is used to disable buttons and avoids incorrect user inputs. Pass as \code{reactive({...})}.
#' @param rvs_dataset A reactive values dataset created from \code{reactiveValues()} and passed to the module from the external environment. Pass as \code{reactive({...})}.
#' 
#' @export
mod_preprocessing_cleaning_server <- function(id, infile = NULL, rvs_dataset){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    ###### UPDATE INPUTS ----------------------------------------------------------------------
    observe({
      req( !is.null(infile())  )
      # activates buttons
      shinyjs::enable("replacement_button")
      shinyjs::enable("plot_preview")
      
      # defines inputs
      variable_choices <- colnames(rvs_dataset())[sapply(rvs_dataset(), is.numeric)]
      graphx_choices   <- colnames(rvs_dataset())[sapply(rvs_dataset(), lubridate::is.POSIXct)]
      grouping_choices <- colnames(rvs_dataset())[sapply(rvs_dataset(), is.factor)]
      
      updateSelectInput(session, inputId = "variable", choices = variable_choices)
      updateSelectInput(session, inputId = "graphx",   choices = graphx_choices)
      updateSelectInput(session, inputId = "factor",   choices = grouping_choices)
      updateSelectInput(session, inputId = "predict",  choices = grouping_choices)
    })
    
    
    ###### WARNINGS ----------------------------------------------------------------------
    # warning messages on number of successive NAs
    # I put 'suppressWarnings' to fix the issue with evaluation of max consecutive NAs when the selected selected has no NAs
    # without it, an error message is shown in the console: 'Warning in max(lengths[values]) : no non-missing arguments to max; returning -Inf'
    observeEvent({input$maxgap 
      input$variable
      input$main_dataset_rows_all
      input$imputmethod
      input$dec_algorithm
      input$split_algorithm}, {
        req(input$maxgap)
        req(input$main_dataset_rows_all)
        req(input$variable)
        req(input$imputmethod)
        
        shinyFeedback::hideFeedback("maxgap")
        if (input$maxgap > 3 & (input$imputmethod == "Interpolation" | (input$imputmethod == "Seasonal decomposition" & input$dec_algorithm == "interpolation") | (input$imputmethod == "Seasonal splitting" & input$split_algorithm == "interpolation")) & (input$maxgap >= suppressWarnings(longestNAstretch(rvs_dataset()[input$main_dataset_rows_all,input$variable])))) {
          shinyFeedback::showFeedbackWarning(
            "maxgap",
            "Interpolation becomes less reliable when performed on many successive NAs!"
          )
        }
        else{
          if (input$maxgap < suppressWarnings(longestNAstretch(rvs_dataset()[input$main_dataset_rows_all,input$variable]))){
            shinyFeedback::showFeedbackDanger(
              "maxgap",
              sprintf("There are %i successive NAs in the selected data. You must set 'maxgap' to be at least equal to that in order for the imputation method to be applied to all NAs!", longestNAstretch(rvs_dataset()[input$main_dataset_rows_all,input$variable]))
            )
          }
        }
        
      })
    
    
    ###### INTERNAL DATASET DEFINITIONS----------------------------------------------------------------------
    
    # show lookup table: if method selected a new tab appears
    observeEvent(input$imputmethod, {
      if (input$imputmethod == "Lookup table") {
        shiny::appendTab(inputId = "tabs",
                         tabPanel( title = "Lookup table",
                                   DT::dataTableOutput(ns("lookuptable")) %>% shinycssloaders::withSpinner()
                         ))
      }
      else{
        shiny::removeTab(inputId = "tabs",
                         target = "Lookup table")
      }
    })
    
    #function for counting longest stretch of consecutive NAs
    longestNAstretch <- function(x){
      with(rle(is.na(x)), max(lengths[values]))
    }
    
    # this is defined internally
    data_set_numerato <- reactive(
      rvs_dataset() %>%
        dplyr::mutate(rownumber=row_number())
    )
    
    data_set_intermedio <- reactive(
      
      if(input$imputmethod == "Interpolation"){
        rvs_dataset() %>%
          dplyr::mutate(!!input$variable := imputeTS::na_interpolation(.data[[input$variable]], option = input$intmethod, maxgap = input$maxgap))
      }
      else{
        if(input$imputmethod == "Weighted moving average"){
          req(input$window_width)
          rvs_dataset() %>%
            dplyr::mutate(!!input$variable := imputeTS::na_ma(.data[[input$variable]], k = input$window_width, maxgap = input$maxgap, weighting = input$weighting))
        }
        else{
          if(input$imputmethod == "Global constant"){
            rvs_dataset() %>%
              dplyr::mutate(!!input$variable := imputeTS::na_mean(.data[[input$variable]], option = input$global_function, maxgap = input$maxgap))
          }
          else{
            if(input$imputmethod == "Forward/Backward observation carrying"){
              rvs_dataset()%>%
                dplyr::mutate(!!input$variable := imputeTS::na_locf(.data[[input$variable]], option = input$option, maxgap = input$maxgap, na_remaining = input$na_remaining))
            }
            else{
              if(input$imputmethod == "Seasonal decomposition" & input$dec_algorithm == "interpolation"){
                rvs_dataset()%>%
                  dplyr::mutate(!!input$variable := imputeTS::na_seadec(.data[[input$variable]], algorithm = input$dec_algorithm, find_frequency = TRUE, maxgap = input$maxgap, option = input$intmethod5))
              }
              else{
                if(input$imputmethod == "Seasonal decomposition" & input$dec_algorithm == "locf"){
                  rvs_dataset()%>%
                    dplyr::mutate(!!input$variable := imputeTS::na_seadec(.data[[input$variable]], algorithm = input$dec_algorithm, find_frequency = TRUE, maxgap = input$maxgap, option = input$option5, na_remaining = input$na_remaining5))
                }
                else{
                  if(input$imputmethod == "Seasonal decomposition" & input$dec_algorithm == "mean"){
                    rvs_dataset()%>%
                      dplyr::mutate(!!input$variable := imputeTS::na_seadec(.data[[input$variable]], algorithm = input$dec_algorithm, find_frequency = TRUE, maxgap = input$maxgap, option = input$global_function5))
                  }
                  else{
                    if(input$imputmethod == "Seasonal decomposition" & input$dec_algorithm == "ma"){
                      req(input$window_width5)
                      rvs_dataset()%>%
                        dplyr::mutate(!!input$variable := imputeTS::na_seadec(.data[[input$variable]], algorithm = input$dec_algorithm, find_frequency = TRUE, maxgap = input$maxgap, k = input$window_width5, weighting = input$weighting5))
                    }
                    else{
                      if(input$imputmethod == "Seasonal splitting" & input$split_algorithm == "interpolation"){
                        rvs_dataset()%>%
                          dplyr::mutate(!!input$variable := imputeTS::na_seasplit(.data[[input$variable]], algorithm = input$split_algorithm, find_frequency = TRUE, maxgap = input$maxgap, option = input$intmethod6))
                      }
                      else{
                        if(input$imputmethod == "Seasonal splitting" & input$split_algorithm == "locf"){
                          rvs_dataset()%>%
                            dplyr::mutate(!!input$variable := imputeTS::na_seasplit(.data[[input$variable]], algorithm = input$split_algorithm, find_frequency = TRUE, maxgap = input$maxgap, option = input$option6, na_remaining = input$na_remaining6))
                        }
                        else{
                          if(input$imputmethod == "Seasonal splitting" & input$split_algorithm == "mean"){
                            rvs_dataset()%>%
                              dplyr::mutate(!!input$variable := imputeTS::na_seasplit(.data[[input$variable]], algorithm = input$split_algorithm, find_frequency = TRUE, maxgap = input$maxgap, option = input$global_function6))
                          }
                          else{
                            if(input$imputmethod == "Seasonal splitting" & input$split_algorithm == "ma"){
                              req(input$window_width6)
                              rvs_dataset()%>%
                                dplyr::mutate(!!input$variable := imputeTS::na_seasplit(.data[[input$variable]], algorithm = input$split_algorithm, find_frequency = TRUE, maxgap = input$maxgap, k = input$window_width6, weighting = input$weighting6))
                            }
                            else{
                              if(input$imputmethod == "Lookup table" & input$lookupmethod == "Mean"){
                                rvs_dataset() %>%
                                  dplyr::group_by_at(input$factor) %>%
                                  dplyr::mutate(LookupPower = mean(.data[[input$variable]], na.rm = TRUE)) %>%
                                  dplyr::mutate(!!input$variable := ifelse(is.na(.data[[input$variable]]), LookupPower, .data[[input$variable]]))
                              }
                              else{
                                if(input$imputmethod == "Lookup table" & input$lookupmethod == "Median"){
                                  rvs_dataset() %>%
                                    dplyr::group_by_at(input$factor) %>%
                                    dplyr::mutate(LookupPower = median(.data[[input$variable]], na.rm = TRUE)) %>%
                                    dplyr::mutate(!!input$variable := ifelse(is.na(.data[[input$variable]]), LookupPower, .data[[input$variable]]))
                                }
                                else{
                                  if(input$imputmethod == "Regression tree"){
                                    simputation::impute_cart(
                                      dat = data.frame(rvs_dataset()),
                                      formula = stats::reformulate(response = sprintf("%s", input$variable),
                                                                   termlabels = sprintf("%s", input$predict)),
                                      maxdepth = input$maxdepth,
                                      minsplit = input$minsplit,
                                      minbucket = input$minbucket,
                                      cp = input$complpar
                                    )
                                  }
                                }
                              }
                            }
                          }
                        } 
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    )
    
    ###### OUTPUTS ----------------------------------------------------------------------
    
    # table with external DATASET can be rendered immediately
    output$main_dataset <- DT::renderDT({
      validate(need(!is.null(infile()) , "Please load a dataset"))
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
    
    # table with NA in external DATASET can be rendered immediately
    percent <- reactive(nrow(rvs_dataset()[input$main_dataset_rows_all,][is.na(rvs_dataset()[input$main_dataset_rows_all,][[input$variable]]),])/nrow(rvs_dataset()[input$main_dataset_rows_all,])*100)
    
    output$na_percent <- renderPrint({
      req(!is.null(infile()) )
      sprintf("The percentage of NAs in the selected portion of the dataset is %f%%", percent())
    })
    
    output$na_table <- DT::renderDT({
      validate(need(!is.null(infile()) , "Please load a dataset"))
      DT::datatable(
        dplyr::filter(rvs_dataset()[input$main_dataset_rows_all,], is.na(.data[[input$variable]])),
        selection = "none",
        rownames = FALSE,
        style = "bootstrap",
        #filter = "top", #fbox
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
    
    # plot distribution of missing values in the main dataset
    # reactive in order to permit download
    plotdata <- reactive({
      if(input$preview_check == TRUE){
        if(sum(is.na(rvs_dataset()[input$main_dataset_rows_all,][[input$variable]]))>0){
          imputeTS::ggplot_na_imputations(rvs_dataset()[input$main_dataset_rows_all,][[input$variable]], data_set_intermedio()[input$main_dataset_rows_all,][[input$variable]],
                                          x_axis_labels = rvs_dataset()[input$main_dataset_rows_all,][[input$graphx]],
                                          subtitle = 'Missing values replacements highlighted in red',
                                          theme = theme_bw())+
            ggplot2::aes_string(
              x = input$graphx,
              y = input$variable)+
            theme(legend.position = "none",
                  panel.grid = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.title = element_text(size=input$plot_fontsize),
                  axis.text.x = element_text(size=input$plot_fontsize, margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
                  axis.text.y = element_text(size=input$plot_fontsize, margin = unit(c(0.3,0.3,0.3,0.3), "cm")))
        }
        else{
          imputeTS::ggplot_na_distribution(rvs_dataset()[input$main_dataset_rows_all,][[input$variable]],
                                           x_axis_labels = rvs_dataset()[input$main_dataset_rows_all,][[input$graphx]],
                                           color_missing = 'red',
                                           color_missing_border = 'red',
                                           subtitle = 'The selected portion of the time series contains no missing values!' ,
                                           theme = theme_bw()
          )+
            ggplot2::aes_string(
              x = input$graphx,
              y = input$variable)+
            theme(legend.position = "none",
                  panel.grid = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.title = element_text(size=input$plot_fontsize),
                  axis.text.x = element_text(size=input$plot_fontsize, margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
                  axis.text.y = element_text(size=input$plot_fontsize, margin = unit(c(0.3,0.3,0.3,0.3), "cm")))
          
        }
      }
      else{
        imputeTS::ggplot_na_distribution(rvs_dataset()[input$main_dataset_rows_all,][[input$variable]],
                                         x_axis_labels = rvs_dataset()[input$main_dataset_rows_all,][[input$graphx]],
                                         color_points = 'black',
                                         color_lines  = 'black',
                                         color_missing = 'red',
                                         color_missing_border = 'red',
                                         subtitle = 'Time series with missing regions highlighted in red',
                                         theme = theme_bw()
        )+
          ggplot2::aes_string(
            x = input$graphx,
            y = input$variable)+
          theme(legend.position = "none",
                panel.grid = element_blank(),
                axis.ticks.x = element_blank(),
                axis.title = element_text(size=input$plot_fontsize),
                axis.text.x = element_text(size=input$plot_fontsize, margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
                axis.text.y = element_text(size=input$plot_fontsize, margin = unit(c(0.3,0.3,0.3,0.3), "cm")))
        
      }
    })
    
    output$plot1 <- renderPlot({
      validate(need(!is.null(infile()) , "Please load a dataset"))
      validate(need(input$plot_preview, "Please press plot button to display the lineplot"))
      req(input$plot_preview, input$main_dataset_rows_all)
      plotdata()
    })
    
    # plot download function
    output$plot_download <- shiny::downloadHandler(
      filename = "plot.png",
      content = function(file){
        ggplot2::ggsave(file, device = png, plot = plotdata(), width=input$plot_dl_width, height = input$plot_dl_height, limitsize = FALSE)
      }
    )
    
    # dataset storing the lookup table
    data_set_lookup <- reactive(if (input$lookupmethod == "Mean") {
      plyr::ddply(rvs_dataset(),
                  input$factor,
                  dplyr::summarise,
                  Lookup_Power = mean(.data[[input$variable]], na.rm = TRUE))
    }
    else{
      if (input$lookupmethod == "Median") {
        plyr::ddply(rvs_dataset(),
                    input$factor,
                    dplyr::summarise,
                    Lookup_Power = median(.data[[input$variable]], na.rm = TRUE))
      }
    })
    
    
    output$lookuptable <- DT::renderDT({
      DT::datatable(data_set_lookup(),
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
    
    
    ###### MODAL and PROCESS ----------------------------------------------------------------------
    
    # function for dialog box when clicking on replace NAs button
    modal_confirm <- function(){
      ns <- session$ns
      shiny::modalDialog(
        size = 'm',
        easyClose = TRUE,
        fade = TRUE,
        title = "Replace outliers with NAs",
        shiny::HTML("<strong style='color:red;' >WARNING: </strong> after replacing NAs, the current dataset will be overwritten. Continue?"),
        footer = tagList(
          actionButton(ns("no"), "Dismiss"),
          actionButton(ns("yes"),"Confirm", class = "btn btn-danger")
        )
      )
    }
    
    observeEvent(input$replacement_button,{
      # ask users if they want to replace NAs
      shiny::showModal(modal_confirm())
    })
    
    observeEvent(input$no,{
      shiny::removeModal()
    })
    
    # Define the ReactiveValue to return : "toReturn"
    # with slots "dataset" & "trigger"
    toReturn <- reactiveValues(dataset = NULL,  trigger = 0)
    
    observeEvent(input$yes,{
      
      # notification of process
      id <- showNotification("Performing cleaning...", duration = NULL, closeButton = FALSE, type = "message")
      on.exit(removeNotification(id), add = TRUE)
      
      
      shiny::removeModal()
      
      rows_nas <- data_set_numerato()[input$main_dataset_rows_all,] %>%
        dplyr::filter(is.na(.data[[input$variable]]))
      
      data_set_temporaneo <- 
        if(input$imputmethod == "Regression tree"){
          simputation::impute_cart(
            dat = data.frame(data_set_numerato()),
            formula = stats::reformulate(response = sprintf("%s", input$variable),
                                         termlabels = sprintf("%s", input$predict),
            ),
            maxdepth = input$maxdepth,
            minsplit = input$minsplit,
            minbucket = input$minbucket,
            cp = input$complpar
          )
        } else {
          data_set_numerato()
        }
      
      
      # return the main dataset with NAs replaced
      toReturn$dataset <-      
        if(input$imputmethod == "Interpolation"){
          data_set_numerato() %>%
            dplyr::mutate(!!input$variable := ifelse(rownumber %in%  rows_nas$rownumber, imputeTS::na_interpolation(.data[[input$variable]], option = input$intmethod, maxgap = input$maxgap), .data[[input$variable]])) %>%
            dplyr::select(-rownumber)
        }
      else{
        if(input$imputmethod == "Weighted moving average"){
          req(input$window_width)
          data_set_numerato() %>%
            dplyr::mutate(!!input$variable := ifelse(rownumber %in%  rows_nas$rownumber, imputeTS::na_ma(.data[[input$variable]], k = input$window_width, maxgap = input$maxgap, weighting = input$weighting), .data[[input$variable]])) %>%
            dplyr::select(-rownumber)
        }
        else{
          if(input$imputmethod == "Global constant"){
            data_set_numerato() %>%
              dplyr::mutate(!!input$variable := ifelse(rownumber %in%  rows_nas$rownumber, imputeTS::na_mean(.data[[input$variable]], option = input$global_function, maxgap = input$maxgap), .data[[input$variable]])) %>%
              dplyr::select(-rownumber)
          }
          else{
            if(input$imputmethod == "Forward/Backward observation carrying"){
              data_set_numerato() %>%
                dplyr::mutate(!!input$variable := ifelse(rownumber %in%  rows_nas$rownumber, imputeTS::na_locf(.data[[input$variable]], option = input$option, maxgap = input$maxgap, na_remaining = input$na_remaining), .data[[input$variable]])) %>%
                dplyr::select(-rownumber)
            }
            else{
              if(input$imputmethod == "Seasonal decomposition" & input$dec_algorithm == "interpolation"){
                data_set_numerato() %>%
                  dplyr::mutate(!!input$variable := ifelse(rownumber %in%  rows_nas$rownumber, imputeTS::na_seadec(.data[[input$variable]], algorithm = input$dec_algorithm, find_frequency = TRUE, maxgap = input$maxgap, option = input$intmethod5), .data[[input$variable]])) %>%
                  dplyr::select(-rownumber)
              }
              else{
                if(input$imputmethod == "Seasonal decomposition" & input$dec_algorithm == "locf"){
                  data_set_numerato() %>%
                    dplyr::mutate(!!input$variable := ifelse(rownumber %in%  rows_nas$rownumber, imputeTS::na_seadec(.data[[input$variable]], algorithm = input$dec_algorithm, find_frequency = TRUE, maxgap = input$maxgap, option = input$option5, na_remaining = input$na_remaining5), .data[[input$variable]])) %>%
                    dplyr::select(-rownumber)
                }
                else{
                  if(input$imputmethod == "Seasonal decomposition" & input$dec_algorithm == "mean"){
                    data_set_numerato() %>%
                      dplyr::mutate(!!input$variable := ifelse(rownumber %in%  rows_nas$rownumber, imputeTS::na_seadec(.data[[input$variable]], algorithm = input$dec_algorithm, find_frequency = TRUE, maxgap = input$maxgap, option = input$global_function5), .data[[input$variable]])) %>%
                      dplyr::select(-rownumber)
                  }
                  else{
                    if(input$imputmethod == "Seasonal decomposition" & input$dec_algorithm == "ma"){
                      req(input$window_width5)
                      data_set_numerato() %>%
                        dplyr::mutate(!!input$variable := ifelse(rownumber %in%  rows_nas$rownumber, imputeTS::na_seadec(.data[[input$variable]], algorithm = input$dec_algorithm, find_frequency = TRUE, maxgap = input$maxgap, k = input$window_width5, weighting = input$weighting5), .data[[input$variable]])) %>%
                        dplyr::select(-rownumber)
                    }
                    else{
                      if(input$imputmethod == "Seasonal splitting" & input$split_algorithm == "interpolation"){
                        data_set_numerato() %>%
                          dplyr::mutate(!!input$variable := ifelse(rownumber %in%  rows_nas$rownumber, imputeTS::na_seadec(.data[[input$variable]], algorithm = input$split_algorithm, find_frequency = TRUE, maxgap = input$maxgap, option = input$intmethod6), .data[[input$variable]])) %>%
                          dplyr::select(-rownumber)
                      }
                      else{
                        if(input$imputmethod == "Seasonal splitting" & input$split_algorithm == "locf"){
                          data_set_numerato() %>%
                            dplyr::mutate(!!input$variable := ifelse(rownumber %in%  rows_nas$rownumber, imputeTS::na_seadec(.data[[input$variable]], algorithm = input$split_algorithm, find_frequency = TRUE, maxgap = input$maxgap, option = input$option6, na_remaining = input$na_remaining6), .data[[input$variable]])) %>%
                            dplyr::select(-rownumber)
                        }
                        else{
                          if(input$imputmethod == "Seasonal splitting" & input$split_algorithm == "mean"){
                            data_set_numerato() %>%
                              dplyr::mutate(!!input$variable := ifelse(rownumber %in%  rows_nas$rownumber, imputeTS::na_seadec(.data[[input$variable]], algorithm = input$split_algorithm, find_frequency = TRUE, maxgap = input$maxgap, option = input$global_function6), .data[[input$variable]])) %>%
                              dplyr::select(-rownumber)
                          }
                          else{
                            if(input$imputmethod == "Seasonal splitting" & input$split_algorithm == "ma"){
                              req(input$window_width6)
                              data_set_numerato() %>%
                                dplyr::mutate(!!input$variable := ifelse(rownumber %in%  rows_nas$rownumber, imputeTS::na_seadec(.data[[input$variable]], algorithm = input$split_algorithm, find_frequency = TRUE, maxgap = input$maxgap, k = input$window_width6, weighting = input$weighting6), .data[[input$variable]])) %>%
                                dplyr::select(-rownumber)
                            }
                            else{
                              if(input$imputmethod == "Lookup table" & input$lookupmethod == "Mean"){
                                data.frame(
                                  data_set_numerato() %>%
                                    dplyr::group_by_at(input$factor) %>%
                                    dplyr::mutate(LookupPower = mean(.data[[input$variable]], na.rm = TRUE)) %>%
                                    dplyr::mutate(!!input$variable := ifelse(rownumber %in%  rows_nas$rownumber, LookupPower, .data[[input$variable]])) %>%
                                    dplyr::ungroup() %>%
                                    dplyr::select(-rownumber)
                                )
                              }
                              else{
                                if(input$imputmethod == "Lookup table" & input$lookupmethod == "Median"){
                                  data.frame(
                                    data_set_numerato() %>%
                                      dplyr::group_by_at(input$factor) %>%
                                      dplyr::mutate(LookupPower = median(.data[[input$variable]], na.rm = TRUE)) %>%
                                      dplyr::mutate(!!input$variable := ifelse(rownumber %in%  rows_nas$rownumber, LookupPower, .data[[input$variable]])) %>%
                                      dplyr::ungroup() %>%
                                      dplyr::select(-rownumber)
                                  )
                                }
                                else{
                                  if(input$imputmethod == "Regression tree"){
                                    data_set_numerato() %>%
                                      dplyr::mutate(!!input$variable := ifelse(rownumber %in% rows_nas$rownumber, data_set_temporaneo[[input$variable]], .data[[input$variable]])) %>%
                                      dplyr::select(-rownumber)
                                  }
                                }
                              }
                            } 
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      
      
      # update trigger for the external environment
      toReturn$trigger <- toReturn$trigger +1
      
    })
    
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
