#################################################################################
###############            Copyright © BAEDA Lab 2020             ###############
#################################################################################

## app.R ##
cat("\014")             # clears the console
rm(list = ls())         # remove all variables of the workspace

# global variables
unitsMeasure <- read.csv("./data/units.csv", header = TRUE, sep = ",", check.names = FALSE) # available units of measure
unitsMeasure[-1,]
dateFormats <- c("%Y-%m-%d %H:%M:%S", 
                 "%m/%d %H:%M:%S", # energy+
                 "%m/%d  %H:%M:%S",  # energy+ 2 spazi
                 " %m/%d    %H:%M:%S",  # energy+ 2 spazi +1
                 "%d/%m/%y %H:%M") # accepted date formats

source("packages.R")    # load necessary packages
source("functions.R")   # load user defined functions
source("header.R")      # load header
source("sidebar.R")     # load sidebar
source("body.R")        # load body

# USER INTERFACE ----------------------------------------------------------------------
ui <- dashboardPage(skin = "black",
                    header,             # loaded from scripts
                    sidebar,            # loaded from scripts
                    body                # loaded from scripts
)

# SERVER FUNCTION ----------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Sidebar functions ----------------------------------------------------------------------
  output$selected_sidebar_tab <- renderText({
    paste("You've selected:", input$tabs)
  })
  
  showModal(modalDialog(
    title = "Important message",
    "This is an important message!",
    easyClose = TRUE
  ))
  
  # shinyalert("Welcome!", "Now you can perform advanced data analytics tasks on your energy data. Simply upload a new file in the Manage tab ans start exploring.",
  #            imageUrl = "BAEDA-logo-dashboard.png", imageWidth = 400)
  # 
  
  # 3rd (SUB)TAB "Manage" ----------------------------------------------------------------------
  
  # this option permits to read larger files than shiny default
  options(shiny.maxRequestSize = 100*1024^2)
  
  # reactive value where we will store all the loaded dataframes
  data <- reactiveValues()
  
  # create a reactive dataframe df when the file is loaded and add
  observeEvent(input$file,{
    inFile <- input$file                                    # input file loaded
    nome <- inFile$name                                     # input file name
    # validate that the file is in the rigth format
    admitted <- gsub( " ", "", paste("\\.", c("", "csv", "xls"), "$" ))
    # validated_file has TRUE if the value is acceptable FALSE if not accepted
    validated <- grepl(admitted[1], nome) | grepl(admitted[2], nome) | grepl(admitted[3], nome)
    
    shinyFeedback::feedbackDanger("file", !validated, "Format not accepted")
    req(validated)
    
    # reads the input file and assigms it to the reactive value data
    data[[nome]] <- switch(input$type, # condition on the file type
                           csv = read.csv(file = inFile$datapath, 
                                          header = input$header, 
                                          sep = input$separator, 
                                          dec = input$decomal, 
                                          stringsAsFactors = input$strAsFact,
                                          check.names = FALSE)
    )
    
    if (input$timestamp == T) {
      # function to automatically find date column
      coldate <- sapply(data[[nome]],   function(x) !all(is.na(as.Date(as.character(x), format = dateFormats))))
      
      # create date time columns
      data[[nome]] <- data[[nome]] %>%
        mutate(
          Date_Time = as.POSIXct(data[[nome]][,coldate] , format = "%Y-%m-%d %H:%M:%S" , tz = input$timezone), # depend on selected timezone
          Week_Day = wday(Date_Time, label = TRUE, locale = "en_US", week_start = getOption("lubridate.week.start", 1)), # week start on monday
          Month = month(Date_Time, label = TRUE, locale = "en_US"),
          Month_Day = mday(Date_Time),
          Year = year(Date_Time),
          Year_Day = mday(Date_Time),
          Hour = hour(Date_Time),
          Minute = minute(Date_Time)
        ) 
    }
    
    # round to selected decimal digits
    data[[nome]] <- data[[nome]] %>% mutate_if(is.numeric, ~round(., input$decimalDigits))
    
  })
  
  # change dataframe name by adding another dataframe 
  observeEvent(input$new_dataframe_name_search,{
    data[[input$new_dataframe_name]] <-  data[[input$dataframe]][input[["dataframe_table_rows_all"]], ]
    shinyalert(title = "Dataframe successfully renamed",
               text = paste("You can find ''", input$new_dataframe_name, "'' in the dataset dropdown"), type = "success")
  })
  
  # create a reactive list of loaded dataframes
  reactive_list <- reactive({ 
    req(input$file) # when new file loaded the list is updated
    names(data)
  })
  
  # when the list changes change the inputs as well
  observeEvent(reactive_list(),{
    # when new file loaded and new name given the select is updated
    updateSelectInput(session, "dataframe",
                      choices = reactive_list(),        # update choiches with all loaded data
                      selected = reactive_list()[length(reactive_list())]    # selected the last loaded file
    )
    #  when new file loaded new name given the remove select is updated
    updateSelectInput(session, "dataframe_remove",
                      choices = reactive_list(),        # update choiches with all loaded data
                      selected = reactive_list()[length(reactive_list())]    # selected the last loaded file
    )
  })
  
  # sets the output dataframe to the one selected 
  output_df <- reactive({
    data[[input$dataframe]]
  }) 
  
  
  # box output function
  output$manage_outputBox <- renderPrint({
    req(input$file) # waits to print unti a file is loaded
    switch (input$display_buttons,
            str = str(output_df()),
            summary = summary(output_df()),
            skim = skim(output_df()),
    )
  })
  
  # 4th (SUB)TAB "View" ----------------------------------------------------------------------
  # Display datatable ----------------------------------------------------------------------
  output$dataframe_table <- renderDataTable(
    datatable(
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      output_df(),
      filter = 'top',
      # style = "foundation",
      extensions = c('FixedHeader'),
      options = list(autoWidth = TRUE,
                     scrollX = 500,
                     deferRender = TRUE,
                     scroller = TRUE,
                     dom = 'lBfrtip',
                     fixedColumns = list(leftColumns = 2),
                     fixedHeader = TRUE
      ),
      rownames = FALSE
    )
  )
  
  
  # keep column ----------------------------------------------------------------------
  output$keepColumns <- renderUI({
    req(input$file)
    tagList(
      pickerInput("keepColumnName", label = "Select column to keep:",
                  choices = colnames( output_df()),
                  selected = colnames( output_df()),
                  options = list(`actions-box` = TRUE),multiple = T)
    )
  })
  
  # Rename column ----------------------------------------------------------------------
  output$modifyColumns <- renderUI({
    req(input$file)
    tagList(
      selectInput("columnName", label = "Select column to modify:",
                  choices = c("", colnames( output_df() )),
                  selected = NULL), # chose numerical variable
      conditionalPanel("input.columnName != '' ",
                       selectizeInput("units", label = "Units of measurements:",
                                      choices = unitsMeasure$Symbol, # all admitted unit
                                      selected = NULL,
                                      multiple = FALSE,
                                      options = list(maxOptions = 5)
                       ),
                       searchInput(inputId = "new_columnName", label = NULL, 
                                   placeholder = "New name..", 
                                   value = NULL, # initial value
                                   btnSearch = icon("plus"), btnReset = icon("backspace"), # icons
                                   width = "100%")
      )
    )
  })
  
  observeEvent(input$new_columnName_search,{
    unitsString <- if (input$units == "") {NULL} else {paste("[",input$units, "]")} # add unit of measures if units selected
    nameString <- if (input$new_columnName == "") {NULL} else {input$new_columnName} # change column name if new name in input
    columnString <- gsub(" ", "", paste(nameString, unitsString)) # combine strings in format *NAME* [*UNIT*]
    # change column name
    if (!is_empty(columnString)) { colnames( data[[input$dataframe]])[colnames( data[[input$dataframe]]) == input$columnName] <- columnString}
    shinyalert(title = "Column successfully renamed",
               text = paste("Name ''", input$new_columnName, "''assigned to ''", input$columnName, "''"), type = "success")
  })
  
  # Pivot table ----------------------------------------------------------------------
  output$pivotTable <- renderUI({
    req(input$file)
    tagList(
      selectInput("variablePivot", label = "Select categorical variable:",
                  choices = c("", colnames(dplyr::select_if( output_df() , is.factor))),
                  selected = NULL,
                  multiple = TRUE), # chose numerical variable
      selectInput("columnsPivot", label = "Select columns to keep:",
                  choices = c("", colnames(dplyr::select_if( output_df() , is.numeric))),
                  selected = NULL,
                  multiple = TRUE), # chose numerical variable
      selectInput("functionPivot", label = "Select summary function:",
                  choices = c("", "mean","min","max", "sd"),
                  selected = NULL), # chose numerical variable
      numericInput("decimalDigitsPivot", label = "Decimal digits", value = 2, step = 1, min = 0), # add condition on variable
      
    )
  })
  
  observeEvent(input$pivotTableButton,{ 
    # create summary table
    if (!is.null(input$variablePivot)) {
      switch (input$functionPivot,
              mean = data[[input$dataframe]] <-  data[[input$dataframe]] %>% ddply(input$variablePivot, numcolwise(mean, na.rm = TRUE)),
              min = data[[input$dataframe]] <-  data[[input$dataframe]] %>% ddply(input$variablePivot, numcolwise(min, na.rm = TRUE)),
              max = data[[input$dataframe]] <-  data[[input$dataframe]] %>% ddply(input$variablePivot, numcolwise(max, na.rm = TRUE))
      )
    }
    data[[input$dataframe]] <-  mutate_if(data[[input$dataframe]], is.numeric, ~round(., input$decimalDigitsPivot)) %>% # round the pivotted table
      select(input$variablePivot, input$columnsPivot)
  })
  
  # Download filtered ----------------------------------------------------------------------
  output$download_filtered <- downloadHandler(
    filename = function() {  paste('data-', Sys.Date(), '.csv', sep='') },
    content = function(file) {
      write.csv(data[[input$dataframe]][input[["dataframe_table_rows_all"]], ],
                file = file,
                row.names = F)
    }
  )
  
  # 5th (SUB)TAB "Visualize" ----------------------------------------------------------------------
  output$downloadplotButton <- downloadHandler(
    filename = function() { 
      paste(input$chart, "plot", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      ggsave(file, width = 5, height = 4 )
    })
  
  # Histogram ----------------------------------------------------------------------
  # input box - select variables - HISTOGRAM
  output$inBoxHistogram <- renderUI({
    if (!is.null(input$file)) {# risolve errore nel caso non ci sia un file di input
      tagList(
        selectInput("variable", label = "Variable:", choices = colnames(dplyr::select_if( output_df(), is.numeric))), # chose numerical variable
        sliderInput(inputId = "bins", label = "Number of bins:", min = 1, max = 100, value = 30),
        selectInput("fillvariable", label = "Fill Variable:", choices = c("None", colnames(dplyr::select_if( output_df(), is.factor))) ),
        selectInput("facetvariable", label = "Facet Variable:", choices = c("None", colnames(dplyr::select_if( output_df(), is.factor))) ),
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
      plot <- ggplot(data =  output_df(),
                     mapping =  aes(x =  output_df()[,input$variable],
                                    fill = if (input$fillvariable == "None") {NULL} else { output_df()[,input$fillvariable]} ),
      ) + theme_bw()
      
      # densità
      if (input$checkbox_density == TRUE) {
        plot <- plot + geom_density(aes(y = ..density..,
                                        fill = if (input$fillvariable == "None") {NULL} else { output_df()[,input$fillvariable]}),
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
      if (input$facetvariable == "None") {NULL} else {plot <- plot + facet_wrap(~  output_df()[,input$facetvariable])}
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
      selectInput("variableX_Box", label = "X:", choices = colnames( output_df())), # chose numerical variable
      selectInput("variableY_Box", label = "Y:", choices = colnames( output_df())), # chose numerical variable
      selectInput("fillvariable_Box", label = "Fill:", choices = c("NULL", colnames(dplyr::select_if( output_df(), is.factor)))),
      # radioButtons("positionMultipleBar","Position:", c("staked", "dodge"), inline = T, selected = NULL),
      selectInput("facetvariable_Box", label = "Facet:", choices = c("NULL", colnames(dplyr::select_if( output_df(), is.factor)))),
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
    plot <- ggplot(data =  output_df(), mapping =  aes_string(x = aes_x, y = aes_y, fill = aes_fill)) + 
      geom_bar(stat = arg_stat, width = input$width_Box ) +  # position = input$positionMultipleBar
      theme_bw()
    
    # flip
    if (input$checkbox_flip_Box == TRUE) {plot <- plot + coord_flip()}
    # tema
    plot <- plot + labs( x = input$variableX_Box, y = input$variableY_Box, fill = input$fillvariable_Box ) + theme(legend.position = "bottom")
    # add wrap
    if (input$facetvariable_Box == "NULL") {NULL} else {plot <- plot + facet_wrap(~  output_df()[,input$facetvariable_Box])}
    # log
    if (input$checkbox_logy_Box == TRUE) {plot <- plot + scale_y_continuous(trans = 'log10') }
    # log
    if (input$polar_Box == TRUE) {plot <- plot + coord_polar(start = 0) }
    
    plot
  })
  output$outBoxBar <- renderPlot({plotBar()})
  
  # Time Series ----------------------------------------------------------------------
  # input box - select variables - TIME SERIES
  output$inBoxTimeSeries <- renderUI({
    tagList(
      selectInput("variableY_TimeSeries", label = "Y variables:", 
                  choices = c("", colnames(dplyr::select_if( output_df(), is.numeric))),
                  multiple = TRUE
      )
    )
  })
  # output box - plot - TIME SERIES
  plotTimeSeries <- eventReactive(input$plotButton,{
    
    qxts <- xts( output_df()[,c(input$variableY_TimeSeries) ] , order.by = (output_df()$Date_Time), tzone = input$timezone)
    
    hc <- highchart( type = "stock")
    for (i in c(1:length(input$variableY_TimeSeries))) {
      hc <-  hc %>% hc_add_series(qxts[,i],  type = "spline", name = input$variableY_TimeSeries[i]) 
    }
    
    hc %>%
      hc_legend( enabled = TRUE, align = "bottom", verticalAlign = "top", layout = "horizontal", x = 0, y = 0) %>%
      hc_tooltip(shared = TRUE, split = FALSE, pointFormat = paste('{point.series.name}= {point.y:.2f}<br>'))  %>%
      hc_rangeSelector( verticalAlign = "top", selected = 4,
                        buttons = list(
                          list(count = 1,
                               text = 'All',
                               type = 'all'),
                          list(count = 1,
                               text = '1yr',
                               type = 'year'),
                          list(count = 6,
                               text='6mo',
                               type='month'),
                          list(count = 1,
                               text='1mo',
                               type='month'),
                          list(count = 7,
                               text='1w',
                               type='day'),
                          list(count = 1,
                               text='1d',
                               type='day'),
                          list(count = 6,
                               text='6h',
                               type='hour')
                        )
      )# end buttons
  })
  output$outBoxTimeSeries <- renderHighchart({plotTimeSeries()})
}


shinyApp(ui, server)
