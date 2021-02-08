cartInput <- function(id, data ) {
  ns <- NS(id)
  
  tagList(
    selectInput(NS(id, 'type'), 'Select algorithm:', choices = c("rpart", "evtree") ),
    radioGroupButtons( NS(id, 'objective'), label = NULL, choices = c("Descriptive", "Predictive"), selected = "Descriptive", justified = TRUE),
    conditionalPanel(condition = sprintf("input['%s'] == 'Predictive'", NS(id, 'objective')),
                       #paste(paste('input.', NS(id, 'objective'), sep = ''), "== 'Predictive'") ,
                     p("additional parameters..."),
                     ## predictive train test
                     # percentuale
                     # 70 30 continua oppure random sample sbilanciato rispetto variabile categorica
                     # seed sul sample
                     # modello su train e prediction confusion matrix 
                     # predict per testarlo confusion matrix
                     # seed per evtree
                     ## descriptive tutto
                     # campionamento random test train
    ),
    conditionalPanel(condition = sprintf("input['%s'] == 'rpart'", NS(id, 'type')),
                     selectInput(NS(id, 'target_rpart') , 'Target variable:', choices = colnames(data) ),
                     h5("Select split variables (as.numeric - as.ordered - as.factor)"),
                     column(width = 4,  style = "padding-left:0px; padding-right:0px;",
                            selectInput(NS(id, 'split_num_rpart'), NULL, choices = colnames(dplyr::select_if( data, is.numeric)) , multiple = TRUE),
                     ),
                     column(width = 4,  style = "padding-left:10px; padding-right:0px;",
                            selectInput(NS(id, 'split_ord_rpart'), NULL, choices = colnames(dplyr::select_if( data, is.factor )) , multiple = TRUE),
                     ),
                     column(width = 4,  style = "padding-left:10px; padding-right:0px;",
                            selectInput(NS(id, 'split_fact_rpart'), NULL, choices = colnames(dplyr::select_if( data, is.factor )) , multiple = TRUE),
                     ),
                     selectInput(NS(id, 'index_rpart'), 'Splitting index:', choices = c("gini", "information")),
                     sliderInput(NS(id, 'maxdepth_rpart'), label = "Max depth:", min = 1, max = 20, value = 4),
                     sliderInput(NS(id, 'cp_rpart'), label = "Complexity parameter:", min = 0, max = 1e-1, value = 0, step = 1e-5),
                     column(width = 4,  style = "padding-left:0px; padding-right:0px;",
                            numericInput(NS(id, 'minsplit_rpart'), label = "Min split:", min = 1, max = 10, value = 0),
                     ),
                     column(width = 4,  style = "padding-left:10px; padding-right:0px;",
                            numericInput(NS(id, 'minbucket_rpart'), label = "Min bucket:", min = 1, max = 100, value = 30), # input numerico default e suggestions info
                     ),
                     column(width = 4,  style = "padding-left:10px; padding-right:0px;",
                            numericInput(NS(id, 'xval_rpart'), label = "Cross validation:", min = 0,  value = 10), # input numerico default e suggestions info
                     )
    ),
    conditionalPanel(condition = sprintf("input['%s'] == 'evtree'", NS(id, 'type')),
                    
    #                  selectInput('target_var_ev', 'Select target variable (categorical):', choices = colnames(dplyr::select_if( data, is.factor)) ),
    #                  selectInput('split_var_ev', 'Select categorical split variables:', choices = colnames(dplyr::select_if( data, is.factor)) , multiple = TRUE),
    #                  numericInput(inputId = "minsplit_ev", label = "Min split:", min = 1, max = 100, value = 30), # numeric
    #                  numericInput(inputId = "minbucket_ev", label = "Min bucket:", min = 1, max = 100, value = 30), # numeric
    #                  sliderInput(inputId = "maxdepth_ev", label = "Max depth:", min = 1, max = 100, value = 30),
    #                  textInput(inputId = "seed_ev", label = "Seed:", placeholder = "(ex.) 1234")
    ),
    actionButton(NS(id, 'button'), "Perform CART", class = "btn-success", icon = icon("chart-bar"), width = "100%"),
  )
}

cartServer <- function(id, data) {
  
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$button,{
      # validation
      #req(input$file)                                                   # require a uploaded file                                                # stops execution if no datetime found
      
      # notification of process
      idnotif <- showNotification("Performing CART...", duration = NULL, closeButton = FALSE, type = "message")
      on.exit(removeNotification(idnotif), add = TRUE)
      
      # select the dataframe to perform CART on
      dfct <- isolate(data) %>%
        select(c(input$target_rpart, input$split_num_rpart, input$split_fact_rpart, input$split_ord_rpart) ) %>% # keep only selected variables
        mutate_at(input$split_fact_rpart, ~factor(., order = F)) %>% # remove order for those variables for which I DON'T WANT ORDER
        mutate_at(input$split_ord_rpart, ~factor(., order = T)) # remove order for those variables for which I WANT ORDER
      
      if (input$type == "rpart") {
        return_df <- rpart::rpart(
          stats::reformulate(response = input$target_rpart , termlabels = c(input$split_num_rpart, input$split_fact_rpart, input$split_ord_rpart)),                                                  # target attribute based on training attributes
          data = dfct ,                                                               # data to be used
          parms = list(split = input$index_rpart),
          #method = input$method_rt,
          control = rpart.control(minbucket = input$minbucket_rpart,  # 120 min 15 minutes sampling*number of days
                                  cp = input$cp_rpart ,                                          # nessun vincolo sul cp permette lo svoluppo completo dell'albero
                                  # xval = (dim(dfct)[1] - 1 ),                        # k-fold leave one out LOOCV
                                  xval = input$xval_rpart,
                                  maxdepth = input$maxdepth_rpart
          ))
        return(reactive(return_df))
      }
    })
  })
}

# module_cartApp <- function() {
#   ui <- fluidPage(
#     column(width = 4, 
#            cartUI("cart", mtcars)
#     )
#   )
#   server <- function(input, output, session) {
#   }
#   shinyApp(ui, server)
# }
# module_cartApp()

