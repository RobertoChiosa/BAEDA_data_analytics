#################################################################################
###############            Copyright © BAEDA Lab 2020             ###############
#################################################################################

## app.R ##
cat("\014")             # clears the console
rm(list = ls())         # remove all variables of the workspace
source("packages.R")    # load necessary packages
source("functions.R")   # load user defined functions
source("header.R")      # load header
source("sidebar.R")     # load sidebar
source("body.R")        # load body

# global variables
unitsMeasure <- read.csv("./data/units.csv", header = TRUE, sep = ",", check.names = FALSE)
# USER INTERFACE ----------------------------------------------------------------------
ui <- dashboardPage(skin = "black",
                    header,             # loaded from scripts
                    sidebar,            # loaded from scripts
                    body                # loaded from scripts
)

# SERVER FUNCTION ----------------------------------------------------------------------
server <- function(input, output) {
  
  # definition of global reactive values
  data <- reactiveValues(df_default = NULL,
                         df_in = NULL, # original uploded dataset
                         df_out = NULL # modified dataset
  )
  
  # Sidebr functions ----------------------------------------------------------------------
  output$selected_sidebar_tab <- renderText({
    paste("You've selected:", input$tabs)
  })

  # 3rd (SUB)TAB "Manage" ----------------------------------------------------------------------
  # OPTION TO READ LARGER FILES
  options(shiny.maxRequestSize = 100*1024^2)
  
  # this permits to load the default dataset
  observeEvent(input$tabs, { 
    data$df_in <- read.csv("./data/data.csv", header = T, sep = ",", dec = ".", check.names = FALSE) # as default loads the data.csv dataset

    # function to automatically find date column
    coldate <- sapply(data$df_in,   function(x) !all(is.na(as.Date(as.character(x), format = "%Y-%m-%d %H:%M:%S"))))
    
    data$df_in <- data$df_in %>%
      mutate(
        Date_Time = as.POSIXct(data$df_in[,coldate] , format = "%Y-%m-%d %H:%M:%S" , tz = "Etc/GMT+12"),
        Day_Type = wday(Date_Time, label = TRUE, locale = Sys.setlocale("LC_TIME","en_US.UTF-8")),
        Day_Type = factor(Day_Type, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
        Month = month(Date_Time, label = TRUE, locale = Sys.setlocale("LC_TIME","en_US.UTF-8")),
        Year = year(Date_Time)
      ) %>%
      mutate_if(is.numeric, ~round(., input$decimalDigits))
    
    data$df_out <- data$df_in # initializate the output dataset

    })
  
  
  # create a reactive dataframe df when the file is loaded
  observeEvent(input$file,{
    inFile <- input$file
    data$df_in <- if (!is.null(inFile)) {
      read.csv(inFile$datapath, header = input$header, sep = input$separator, dec = input$decomal, check.names = FALSE)}
    
    # function to automatically find date column
    coldate <- sapply(data$df_in,   function(x) !all(is.na(as.Date(as.character(x), format = "%Y-%m-%d %H:%M:%S"))))
    
    data$df_in <- data$df_in %>%
      mutate(
        Date_Time = as.POSIXct(data$df_in[,coldate] , format = "%Y-%m-%d %H:%M:%S" , tz = "Etc/GMT+12"),
        Day_Type = wday(Date_Time, label = TRUE, locale = Sys.setlocale("LC_TIME","en_US.UTF-8")),
        Day_Type = factor(Day_Type, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
        Month = month(Date_Time, label = TRUE, locale = Sys.setlocale("LC_TIME","en_US.UTF-8")),
        Year = year(Date_Time)
      ) %>%
      mutate_if(is.numeric, ~round(., input$decimalDigits))
    
    data$df_out <- data$df_in # initializate the output dataset
  })
  
  # box output function
  output$manage_outputBox <- renderPrint({
    if (input$display_buttons == "str") { str(data$df_out) }
    else if (input$display_buttons == "summary") { summary(data$df_out) }
    # else if (input$display_buttons == "info"){ file.info(path) }
  })
  
  # 4th (SUB)TAB "View" ----------------------------------------------------------------------
  
  # displays the table
  output$dataframe_table <- renderDataTable(
    datatable(
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      data$df_out,
      filter = 'top',
      extensions = c('Buttons'),
      options = list(scrollX = 500,
                     autoWidth = TRUE,
                     deferRender = TRUE,
                     scroller = TRUE,
                     paging = TRUE,
                     pageLength = 10, # length of the displayed page
                     buttons = list('excel', 'csv', # buttons
                                    list(extend = 'colvis', targets = 0, visible = FALSE)),
                     dom = 'lBfrtip',
                     fixedColumns = TRUE),
      rownames = FALSE
    )
  )
  
  # permits to modify the column name and update the dataframe
  output$modifyColumns <- renderUI({
    tagList(
      selectInput("columnName", label = "Select column to modify:",
                  choices = c("", colnames(data$df_out)),
                  selected = NULL), # chose numerical variable
      conditionalPanel("input.columnName != '' ",
                       textInput("new_columnName", label = "Change column name:", value = ""), # chose numerical variable
                       selectizeInput("units", label = "Units of measurements:",
                                      choices = unitsMeasure$Symbol, # all admitted unit
                                      selected = NULL,
                                      multiple = FALSE,
                                      options = list(maxOptions = 5)
                       )
      )
    )
  })
  
  # permits to create a pivot table
  output$pivotTable <- renderUI({
    tagList(
      selectInput("columnPivot", label = "Select pivot column:",
                  choices = c("", colnames(dplyr::select_if(data$df_out, is.factor))),
                  selected = NULL,
                  multiple = TRUE), # chose numerical variable
      conditionalPanel("input.columnPivot != '' ",
                       selectInput("functionPivot", label = "Select summary function:",
                                   choices = c("", "mean","min","max", "sd"),
                                   selected = NULL), # chose numerical variable
                       numericInput("decimalDigitsPivot", label = "Decimal digits", value = 2, step = 1, min = 0), # add condition on variable
      )
    )
  })
  
  
  # observe the button and saves the changes of the dataframe in the variable df_out
  # cambia il nome alle colonne e aggiunge unità di misura
  observeEvent(input$tableUpdateButton,{ 
    unitsString <- if (input$units == "") {NULL} else {paste("[",input$units, "]")} # add unit of measures if units selected
    nameString <- if (input$new_columnName == "") {NULL} else {input$new_columnName} # change column name if new name in input
    
    columnString <- gsub(" ", "", paste(nameString, unitsString)) # combine strings in format *NAME* [*UNIT*]
    
    # change column name
    if (!is_empty(columnString)) {colnames(data$df_out)[colnames(data$df_out) == input$columnName] <- columnString}
    
    # create summary table
    if (!is.null(input$columnPivot)) {
      if (input$functionPivot == "mean") {data$df_out <- data$df_out %>% ddply(input$columnPivot, numcolwise(mean, na.rm = TRUE))}
      else if (input$functionPivot == "min") {data$df_out <- data$df_out %>% ddply(input$columnPivot, numcolwise(min, na.rm = TRUE))}
      else if (input$functionPivot == "max") {data$df_out <- data$df_out %>% ddply(input$columnPivot, numcolwise(max, na.rm = TRUE))}
    }
    
    data$df_out <- data$df_out %>% mutate_if(is.numeric, ~round(., input$decimalDigitsPivot)) # round the pivotted table
  })
  

  
  # 5th (SUB)TAB "Visualize" ----------------------------------------------------------------------
  output$downloadplotButton <- downloadHandler(
    filename = function() { 
      paste(input$chart, "plot", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      ggsave(file, width = 5, height = 4 )
    })
  
  # input box - select variables - HISTOGRAM
  output$inBoxHistogram <- renderUI({
    if (!is.null(input$file)) {# risolve errore nel caso non ci sia un file di input
      tagList(
        selectInput("variable", label = "Variable:", choices = colnames(dplyr::select_if(data$df_out, is.numeric))), # chose numerical variable
        sliderInput(inputId = "bins", label = "Number of bins:", min = 1, max = 100, value = 30),
        selectInput("fillvariable", label = "Fill Variable:", choices = c("None", colnames(dplyr::select_if(data$df_out, is.factor))) ),
        selectInput("facetvariable", label = "Facet Variable:", choices = c("None", colnames(dplyr::select_if(data$df_out, is.factor))) ),
        tags$hr(),
        column("Style", width = 6,
               checkboxInput("checkbox_flip", label = "Flip", value = FALSE),
               checkboxInput("checkbox_density", label = "Density", value = FALSE)
        ),
        column("Scale", width = 6,
               checkboxInput("checkbox_logx", label = "Log-X", value = FALSE),
               checkboxInput("checkbox_logy", label = "Log-Y", value = FALSE)
        )
      )}
  })
  # output box - plot - HISTOGRAM
  output$outBoxHistogram <- renderPlot({
    if (is.null(input$file)) {NULL} else {# risolve errore nel caso non ci sia un file di input
      #shiny::req(input$variable)
      #validate(need(input$variable %in% colnames(myData),message="Incorrect column name."))
      plot <- ggplot(data = data$df_out,
                     mapping =  aes(x = data$df_out[,input$variable],
                                    fill = if (input$fillvariable == "None") {NULL} else {data$df_out[,input$fillvariable]} ),
      ) + theme_bw()
      
      # densità
      if (input$checkbox_density == TRUE) {
        plot <- plot + geom_density(aes(y = ..density..,
                                        fill = if (input$fillvariable == "None") {NULL} else {data$df_out[,input$fillvariable]}),
                                    alpha = 0.4,
                                    bw = input$bins/5,
                                    na.rm = TRUE)  # aggiungo distribuzione
      } else{plot <- plot + geom_histogram(bins = input$bins,
                                           na.rm = TRUE)} #
      # flip
      if (input$checkbox_flip == TRUE) {plot <- plot + coord_flip()}
      # tema
      plot <- plot + labs( x = input$variable, fill = input$fillvariable ) + theme(legend.position = "bottom")
      # add wrap
      if (input$facetvariable == "None") {NULL} else {plot <- plot + facet_wrap(~ data$df_out[,input$facetvariable])}
      # log
      # flip
      if (input$checkbox_logx == TRUE) {plot <- plot + scale_x_continuous(trans = 'log10') }
      if (input$checkbox_logy == TRUE) {plot <- plot + scale_y_continuous(trans = 'log10') }
      
      plot
    } # end else
  })
  
  # input box - select variables - BAR BLOT
  output$inBoxBar <- renderUI({
    tagList(
      selectInput("variableX_Box", label = "X:", choices = colnames(dplyr::select_if(data$df_out, is.factor))), # chose numerical variable
      selectInput("variableY_Box", label = "Y:", choices = colnames(data$df_out)), # chose numerical variable
      selectInput("fillvariable_Box", label = "Fill:", choices = c("NULL", colnames(dplyr::select_if(data$df_out, is.factor)))),
      selectInput("facetvariable_Box", label = "Facet:", choices = c("NULL", colnames(dplyr::select_if(data$df_out, is.factor)))),
      sliderInput("width_Box", label = "Bar width:", min = 0.1, max = 1, value = 0.9),
      
      tags$hr(),
      column("", width = 6,
             checkboxInput("checkbox_flip_Box", label = "Flip", value = FALSE)
      ),
      column("", width = 6,
             checkboxInput("checkbox_logy_Box", label = "Log-Y", value = FALSE),
             checkboxInput("polar_Box", label = "Polar", value = FALSE)
      )
    )
  })
  # output box - plot - BAR BLOT
  plotBar <- eventReactive(input$plotButton,{
    aes_x <- input$variableX_Box
    aes_y <- input$variableY_Box
    aes_fill <- input$fillvariable_Box
    
    arg_stat <- "identity"
    plot <- ggplot(data = data$df_out, mapping =  aes_string(x = aes_x, y = aes_y, fill = aes_fill)) + 
      geom_bar(stat = arg_stat, width = input$width_Box) + 
      theme_bw()
    
    # flip
    if (input$checkbox_flip_Box == TRUE) {plot <- plot + coord_flip()}
    # tema
    plot <- plot + labs( x = input$variableX_Box, y = input$variableY_Box, fill = input$fillvariable_Box ) + theme(legend.position = "bottom")
    # add wrap
    if (input$facetvariable_Box == "NULL") {NULL} else {plot <- plot + facet_wrap(~ data$df_out[,input$facetvariable_Box])}
    # log
    if (input$checkbox_logy_Box == TRUE) {plot <- plot + scale_y_continuous(trans = 'log10') }
    # log
    if (input$polar_Box == TRUE) {plot <- plot + coord_polar(start = 0) }
    
    plot
  })
  output$outBoxBar <- renderPlot({plotBar()})
}


shinyApp(ui, server)
