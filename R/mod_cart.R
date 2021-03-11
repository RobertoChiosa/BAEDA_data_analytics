#' cart UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import shiny
#' @import ggplot2
#' @import magrittr
#' @importFrom shinyBS bsTooltip
#' @importFrom shinyWidgets radioGroupButtons dropdownButton tooltipOptions
#' @importFrom dplyr select_if select mutate_if mutate_at
#' @importFrom rpart rpart rpart.control plotcp
#' @importFrom stats predict reformulate
#' @importFrom shinyFeedback feedbackWarning hideFeedback
#' @importFrom partykit predict.party as.party
#' @importFrom MLmetrics Accuracy Recall ConfusionMatrix
#' @importFrom grid gpar

mod_cart_ui_input <- function(id){
  ns <- NS(id)
  
  # define padding style for column
  style_LCol <- "padding-left:0px; padding-right:10px;"
  style_RCol <- "padding-left:0px; padding-right:0px;"
  
  tagList(
    column(width = 6,  style = style_LCol,
           selectInput(ns('algorithm'), 'Algorithm:', choices = c("rpart") ), # add evtree
           shinyBS::bsTooltip(ns("algorithm"), title = "Algorithm to perform cart", placement = "right", options = list(container = "body"), trigger = "hover")
    ),
    column(width = 6,  style = style_RCol,
           selectInput(ns('objective'), 'Objective:', choices = c("Descriptive", "Predictive") ),
    ),
    
    # # if predictive constructed i build a test and train sample
    conditionalPanel(condition = sprintf("input['%s'] == 'Predictive'", ns('objective')),
                     column(width = 6,  style = style_LCol,
                            numericInput(ns("train_size"), "Train Size [%]:", 70, min = 0, max = 100, step = 1), # train size percentage
                     ),
                     column(width = 6,  style = style_RCol,
                            numericInput(ns("seed_predictive"), label = "Sample Seed:", 1234) # seed sul sample
                     ),
                     # 70 30 continua oppure random sample sbilanciato rispetto variabile categorica
                     # modello su train e prediction confusion matrix
                     # predict per testarlo confusion matrix
                     
                     ## descriptive tutto
                     # campionamento random test train
    ),
    # # if descriptive selected i only describe the whole dataset
    conditionalPanel(condition = sprintf("input['%s'] == 'rpart'", ns('algorithm')),
                     column(width = 6,  style = style_LCol,
                            selectInput(ns('target_var_rt'), 'Target variable:', choices = c("") )
                     ),
                     column(width = 6,  style = style_RCol,
                            selectInput(ns('target_var_rt_class'), 'Coerce to class:', choices = c("numeric", "factor", "ordered") )
                     ),
                     h5("Select split variables (coerce to)"),
                     column(width = 4,  style = style_LCol,
                            selectInput(ns('split_var_num_rt'), NULL, 
                                        choices = c("as.numeric()"="", colnames(dplyr::select_if( data, is.numeric))) , multiple = TRUE),
                     ),
                     column(width = 4,  style = style_RCol,
                            selectInput(ns('split_var_ord_rt'), NULL, 
                                        choices = c("as.ordered()"="", colnames(dplyr::select_if( data, function(col) is.factor(col) | is.integer(col) )) ) , multiple = TRUE),
                     ),
                     column(width = 4,  style = style_RCol,
                            selectInput(ns('split_var_fact_rt'), NULL, 
                                        choices = c("as.factor()"="", colnames(dplyr::select_if( data, function(col) is.factor(col) | is.integer(col) )) ) , multiple = TRUE),
                     ),
                     selectInput(ns('index_rt'), 'Splitting index:', choices = c("gini", "information")),
                     sliderInput(ns("maxdepth_rt"), label = "Max depth:", min = 1, max = 20, value = 4),
                     sliderInput(ns("cp_rt"), label = "Complexity parameter:", min = 0, max = 1e-1, value = 0, step = 1e-5),
                     column(width = 4,  style = style_LCol,
                            numericInput( ns("minsplit_rt"), label = "Min split:", min = 1, max = 10, value = 0),
                     ),
                     column(width = 4,  style = style_LCol,
                            numericInput(ns("minbucket_rt"), label = "Min bucket:", min = 1, max = 100, value = 30), # input numerico default e suggestions info
                     ),
                     column(width = 4,  style = style_RCol,
                            numericInput(ns("xval_rt"), label = "Cross validation:", min = 0,  value = 10), # input numerico default e suggestions info
                     )
    ),
    # conditionalPanel(condition = sprintf("input['%s'] == 'evtree'", ns('algorithm')), 
    #                  selectInput(ns('target_var_ev'), 'Select target variable (categorical):', choices = colnames(dplyr::select_if( data, is.factor)) ),
    #                  selectInput(ns('split_var_ev'), 'Select categorical split variables:', choices = colnames(dplyr::select_if( data, is.factor)) , multiple = TRUE),
    #                  numericInput(ns("minsplit_ev"), label = "Min split:", min = 1, max = 100, value = 30), # numeric
    #                  numericInput(ns("minbucket_ev"), label = "Min bucket:", min = 1, max = 100, value = 30), # numeric
    #                  sliderInput(ns("maxdepth_ev"), label = "Max depth:", min = 1, max = 100, value = 30),
    #                  textInput(ns("seed_ev"), label = "Seed:", placeholder = "(ex.) 1234")
    # ),
    actionButton(ns("cart_button"), "Perform CART", class = "btn-success", icon = icon("chart-bar"), width = "100%"),
  )
}

mod_cart_ui_output<- function(id, type){
  ns <- NS(id)
  
  if (type == "tree") {
    tagList(
      shinyWidgets::dropdownButton( size = "sm",
                                    tags$h3("Graphical parameters"),
                                    numericInput(ns('out_cart_tree_fontsize'), label = 'Fontsize:', value = 11, step = 1),
                                    numericInput(ns('out_cart_tree_tnex'), label = 'Terminal nodes extension:', value = 2.5, step = 0.5),
                                    circle = TRUE, status = "primary", icon = icon("gear"), width = "400px",
                                    tooltip = shinyWidgets::tooltipOptions(title = "Click to modify plot")
      ),
      plotOutput(ns("out_cart_tree"), height = "400px") # %>% withSpinner(color = loadingGifColor)
    )
  } else if (type == "cp") {
    tagList(
      shinyWidgets::dropdownButton( size = "sm",
                                    tags$h3("Graphical parameters"),
                                    selectInput(ns("out_cart_cp_color"), "Line color",choices = c("red", "green", "blue")),
                                    selectInput(ns("out_cart_cp_upper"), "Upper",choices = c("size", "splits", "none")),
                                    numericInput(ns('out_cart_cp_lty'), label = 'Line Type:', value = 2),
                                    circle = TRUE, status = "primary", icon = icon("gear"), width = "400px",
                                    tooltip = shinyWidgets::tooltipOptions(title = "Click to modify plot")
      ),
      plotOutput(ns("out_cart_cp"), height = "400px"),   #%>% withSpinner(color = loadingGifColor),
      verbatimTextOutput(ns("out_cart_cptable"))         #%>% withSpinner(color = loadingGifColor)
    )
  } else if (type == "CM") {
    tagList(
      plotOutput(ns("out_cart_cm"))
    )
  }
  
}


#' cart Server Functions
#'
#' @noRd 
mod_cart_server <- function(id, rvs){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # updates ui do for all
    observe({
      updateSelectInput(session, inputId = 'target_var_rt', choices = colnames(rvs))
    })
    
    
    cart_results <- reactiveValues()
    
    observeEvent(input$cart_button,{
      
      # remove feedback if any
      shinyFeedback::hideFeedback("target_var_rt") 
      shinyFeedback::hideFeedback("split_var_num_rt") 
      shinyFeedback::hideFeedback("split_var_ord_rt") 
      shinyFeedback::hideFeedback("split_var_fact_rt") 
      
      # validation part
      validation_split_var <- is.null(input$split_var_num_rt) & is.null(input$split_var_ord_rt) & is.null(input$split_var_fact_rt)
      
      if ( validation_split_var == TRUE ) { # incompatible condition
        shinyFeedback::feedbackWarning("split_var_num_rt", TRUE, "Empty") 
        shinyFeedback::feedbackWarning("split_var_ord_rt", TRUE, "Empty") 
        shinyFeedback::feedbackWarning("split_var_fact_rt", TRUE, "Empty") 
      } 
      
      validation_duplicate_var <- input$target_var_rt %in% c(input$split_var_num_rt, input$split_var_ord_rt, input$split_var_fact_rt)
      
      if ( validation_duplicate_var == TRUE ) { # incompatible condition
        shinyFeedback::feedbackWarning("target_var_rt", TRUE, "Conflicting") 
      } 
      
      # condition to continue if validation conditions admitted
      req( !validation_split_var, !validation_duplicate_var)
      
      #   # validation
      #   req(input$file)                                                   # require a uploaded file                                                # stops execution if no datetime found
      #   
      #   # notification of process
      #   id <- showNotification("Performing CART...", duration = NULL, closeButton = FALSE, type = "message")
      #   on.exit(removeNotification(id), add = TRUE)
      
      
      # select the dataframe to perform CART on
      dfct <- rvs %>%
        dplyr::select(c(input$target_var_rt, input$split_var_num_rt, input$split_var_fact_rt, input$split_var_ord_rt) ) %>% # keep only selected variables
        dplyr::mutate_at(input$split_var_fact_rt, ~factor(., order = F)) %>% # remove order for those variables for which I DON'T WANT ORDER
        dplyr::mutate_at(input$split_var_ord_rt, ~factor(., order = T)) # remove order for those variables for which I WANT ORDER
      
      # switch (input$target_var_rt_class,
      #         numeric = dfct <- dplyr::mutate_at(dfct, input$target_var_rt, ~numeric()),
      #         factor =  dfct <- dplyr::mutate_at(dfct, input$target_var_rt, ~factor(., order = F)),
      #         ordered = dfct <- dplyr::mutate_at(dfct, input$target_var_rt, ~factor(., order = T))
      # )
      
      
      if (input$objective == "Descriptive") {
        
        if (input$algorithm == "rpart") {
          ct.rpart <- rpart::rpart(
            stats::reformulate(response = input$target_var_rt , termlabels = c(input$split_var_num_rt, input$split_var_fact_rt, input$split_var_ord_rt)),                                                  # target attribute based on training attributes
            data = dfct ,                                                               # data to be used
            parms = list(split = input$index_rt),
            #method = input$method_rt,
            control = rpart::rpart.control(minbucket = input$minbucket_rt,  # 120 min 15 minutes sampling*number of days
                                           cp = input$cp_rt ,                                          # nessun vincolo sul cp permette lo svoluppo completo dell'albero
                                           # xval = (dim(dfct)[1] - 1 ),                        # k-fold leave one out LOOCV
                                           xval = input$xval_rt,
                                           maxdepth = input$maxdepth_rt
            ))
          # save to local environment
          cart_results[["cart_model"]] <- ct.rpart
          cart_results[["cart_objective"]] <- "Descriptive"
          cart_results[["cart_train_size"]] <- 100
        }
        
        
      } else { # predictive
        
        smp_size <- floor(input$train_size/100 * nrow(dfct)) # sample size
        set.seed(as.numeric(input$seed_predictive)) # set the seed to make your partition reproducible
        
        train_ind <- sample(seq_len(nrow(dfct)), size = smp_size, replace = F) # train index
        
        # definisco train set
        dfct_train <- dfct[train_ind, ]
        # definisco test set
        dfct_test <- dfct[-train_ind, ]
        
        if (input$algorithm == "rpart") {
          ct.rpart <- rpart::rpart(
            stats::reformulate(response = input$target_var_rt , termlabels = c(input$split_var_num_rt, input$split_var_fact_rt, input$split_var_ord_rt)),                                                  # target attribute based on training attributes
            data = dfct_train ,                                                               # data to be used
            parms = list(split = input$index_rt),
            #method = input$method_rt,
            control = rpart::rpart.control(minbucket = input$minbucket_rt,  # 120 min 15 minutes sampling*number of days
                                           cp = input$cp_rt ,                                          # nessun vincolo sul cp permette lo svoluppo completo dell'albero
                                           # xval = (dim(dfct)[1] - 1 ),                        # k-fold leave one out LOOCV
                                           xval = input$xval_rt,
                                           maxdepth = input$maxdepth_rt
            ))
          # save to global environment
          cart_results[["cart_model"]] <- ct.rpart
          cart_results[["cart_objective"]] <- "Predictive"
          cart_results[["cart_train_size"]] <- input$train_size
        }
        
        # calculate accuracy parameters
        
        if (class( dfct_train[[input$target_var_rt]]) == "factor" ) {
          dfct_train$pred.rpart <- stats::predict(object = ct.rpart, dfct_train, type = "class")
        } else {
          dfct_train$pred.rpart <- stats::predict(object = ct.rpart, dfct_train)
        }
        dfct_train$node.rpart <- partykit::predict.party(object = partykit::as.party(ct.rpart), dfct_train , type = "node")
        
        if (class( dfct_test[[input$target_var_rt]]) == "factor" ) {
          dfct_test$pred.rpart <- stats::predict(object = ct.rpart, dfct_test, type = "class")
        } else {
          dfct_test$pred.rpart <- stats::predict(object = ct.rpart, dfct_test)
        }
        dfct_test$node.rpart <- partykit::predict.party(object = partykit::as.party(ct.rpart), dfct_test , type = "node")
        
        # evaluation metrics
        if (class( dfct_train[[input$target_var_rt]]) == "factor" ) {
          cart_results[["cart_cm"]]   <- MLmetrics::ConfusionMatrix(  dfct_test$pred.rpart, dfct_test[[input$target_var_rt]]  )
        }
        
        cart_results[["accuracy"]]  <- MLmetrics::Accuracy(         dfct_test$pred.rpart, dfct_test[[input$target_var_rt]]  )*100
        cart_results[["recall"]]    <- MLmetrics::Recall(           dfct_test$pred.rpart, dfct_test[[input$target_var_rt]]  )*100
        #cart_results[["precision"]] <- Precision(        dfct_test$pred.rpart, dfct_test[[input$target_var_rt]])*100
      }
      
    })
    
    output$out_cart_tree <- renderPlot({
      req(input[["cart_button"]], cart_results[["cart_model"]])
      # cols <- as.vector(cart_results[["cart_df_color"]]$Color)
      ct_party <- partykit::as.party(cart_results[["cart_model"]])
      #names(ct_party$data) <- c(input$target_var_rt, input$split_var_rt) # change labels to plot
      plot(ct_party,
           main = paste(cart_results[["cart_objective"]], "\nTrain size", cart_results[["cart_train_size"]], "%"),
           tnex = input$out_cart_tree_tnex,  gp = grid::gpar(fontsize = input$out_cart_tree_fontsize))
      
    })
    
    # plot complexity parameter
    output$out_cart_cp <- renderPlot({
      req(input[["cart_button"]], cart_results[["cart_model"]])
      rpart::plotcp(cart_results[["cart_model"]],
                    lty = input$out_cart_cp_lty,
                    col = input$out_cart_cp_color,
                    upper = input$out_cart_cp_upper)
    })
    
    # print complexity parameter table
    output$out_cart_cptable <- renderPrint({
      req(input[["cart_button"]], cart_results[["cart_model"]])
      cart_results[["cart_model"]][["cptable"]]
    })
    
    # plot confusion matrix
    output$out_cart_cm <- renderPlot({
      req(input[["cart_button"]], cart_results[["cart_cm"]])
      
      CMplot <- ggplot2::ggplot(data =  as.data.frame(cart_results[["cart_cm"]]),
                                mapping = aes(x = y_pred, y = rev(y_true), fill = Freq)
      ) +
        ggplot2::geom_tile(colour = "white") +
        ggplot2::geom_text(aes(label = sprintf("%1.0f",Freq)), vjust = 1) +
        ggplot2::scale_fill_gradient(low = "white", high = "steelblue") +
        ggplot2::labs( title = "Confusion Matrix",
                       subtitle = paste("Accuracy:", cart_results[["accuracy"]], "%\n",
                                        "Recall:", cart_results[["recall"]], "%\n"
                                        
                       ),
                       x = paste("Predicted", input$target_var_rt ),
                       y = paste("Actual", input$target_var_rt), 
                       fill = ""
        ) +
        ggplot2::scale_y_discrete( expand = c(0,0)) +
        ggplot2::scale_x_discrete( expand = c(0,0))
      
      return(CMplot)
    })
  })
}

## To be copied in the UI
# mod_cart_ui_input("cart_ui_1")
# mod_cart_ui_output("cart_ui_1", type = "tree")
# mod_cart_ui_output("cart_ui_1", type = "cp")
# mod_cart_ui_output("cart_ui_1", type = "CM")

## To be copied in the server
# mod_cart_server("cart_ui_1", rvs)
