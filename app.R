#################################################################################
###############            Copyright © BAEDA Lab 2020             ###############
#################################################################################

## app.R ##
cat("\014")             # clears the console
rm(list = ls())         # remove all variables of the workspace

# global variables
unitsMeasure <- read.csv("./data/units.csv", header = TRUE, sep = ",", check.names = FALSE) # available units of measure
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
ui <- dashboardPage(skin = "black",     # sets overall appearance 
                    header,             # loaded from scripts
                    sidebar,            # loaded from scripts
                    body                # loaded from scripts
)

# SERVER FUNCTION ----------------------------------------------------------------------
server <- function(input, output, session) {
  
  ###### SIDEBAR functions ----------------------------------------------------------------------
  
  # forces startup modal dialog to open when the application starts
  toggleModal(session, "startupModal", toggle = "open")
  # when clicked a the upload modal is sown again
  observeEvent(input$upload,{
    toggleModal(session, "startupModal", toggle = "open")
  })
  
  ###### TAB "Manage" ----------------------------------------------------------------------
  
  # this option permits to read larger files than shiny default
  options(shiny.maxRequestSize = 100*1024^2)
  
  # reactive value where we will store all the loaded dataframes
  data <- reactiveValues()
  
  # load file ----------------------------------------------------------------------
  # create a reactive dataframe df when the file is loaded and add
  observeEvent(input$file,{
    inFile <- input$file   # input file loaded
    nome <- inFile$name    # input file name
    
    # validate that the file is in the right format
    # this is the only accepted file given the one chosen
    admitted <- gsub( " ", "", paste("\\.", input$type, "$" )) 
    
    # validated is TRUE if the value is acceptable FALSE if not acceptable
    validated <- grepl(admitted, nome) 
    
    # gives error feedback if the file is not in the format required/selected and STOPS the execution
    shinyFeedback::hideFeedback("file")
    if (validated == TRUE) { 
      shinyFeedback::feedbackSuccess("file", TRUE, "Format accepted")
    } else { 
      shinyFeedback::feedbackDanger("file", TRUE, "Format not accepted") 
    }
    
    # the execution CONTINUES only if the file is accepted
    req(validated, cancelOutput = TRUE)
    
    id <- showNotification("Reading data...", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    
    
    # reads the input file and assigms it to the reactive value data
    data[[nome]] <- switch(input$type, # condition on the file type
                           csv = read.csv(file = inFile$datapath, header = input$header, sep = input$separator, 
                                          dec = input$decomal, stringsAsFactors = input$strAsFact, check.names = FALSE),
                           rds = readRDS(file = inFile$datapath)
                           # xls = read_excel(path = inFile$datapath)
    )
    
    # create the correct timestamp and timezone input to validate the following condition
    timestamp <- gsub(" ", "", paste("timestamp_", input$type))
    timezone <- gsub(" ", "", paste("timezone_", input$type))
    
    if (input[[timestamp]] == T) {
      # function to automatically find date column
      coldate <- sapply(data[[nome]],   function(x) !all(is.na(as.Date(as.character(x), format = dateFormats))))
      
      # in no timestamp column can be found notify the user
      if (any(coldate) == FALSE) {
        shinyalert(title = "No timestamp column found!", 
                   paste("However, you can find <b>", nome, "</b> in the dataframe dropdown"),
                   type = "warning",
                   closeOnEsc = TRUE,
                   closeOnClickOutside = TRUE,
                   html = TRUE
        )
      } else { # if timestamp columns found create date time columns
        data[[nome]] <- data[[nome]] %>%
          mutate(
            Date_Time = as.POSIXct(data[[nome]][,coldate] , format = "%Y-%m-%d %H:%M:%S" , tz = input[[timezone]]), # depend on selected timezone
            Week_Day = wday(Date_Time, label = TRUE, week_start = getOption("lubridate.week.start", 1)), # week start on monday
            Month = as.ordered(month(Date_Time, label = TRUE)),
            Month_Day = mday(Date_Time),
            Year = year(Date_Time),
            Year_Day = mday(Date_Time),
            Hour = hour(Date_Time),
            Minute = minute(Date_Time),
            min_dec = as.numeric(paste(Hour, Minute*100/60, sep = "."))
          ) 
        shinyalert(title = "Dataframe successfully added",
                   text = paste("You can find <b>", nome, "</b> in the dataframe dropdown <br> We created some useful new variables..."), 
                   type = "success",
                   closeOnEsc = TRUE,
                   closeOnClickOutside = TRUE,
                   html = TRUE
        )
      }
    } else { # no timestamp check box selected
      shinyalert(title = "Dataframe successfully added",
                 text = paste("You can find <b>", nome, "</b> in the dataframe dropdown"), 
                 type = "success",
                 closeOnEsc = TRUE,
                 closeOnClickOutside = TRUE,
                 html = TRUE
      )
    }
  })
  
  # Dataframe dropdown creation ----------------------------------------------------------------------
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
  
  # value boxes ----------------------------------------------------------------------
  output$valueBox1 <- renderValueBox({
    req(input$file) # requires that a file is loaded
    valueBox(length(input$dataframe_table_rows_all), "Number of rows", icon = icon("arrows-alt-v"), color = "orange")
  })
  
  output$valueBox2 <- renderValueBox({
    req(input$file) # requires that a file is loaded
    valueBox(length(input$keepColumnName), "Number of columns", icon = icon("arrows-alt-h"), color = "blue")
  })
  
  # Rename dataframe ----------------------------------------------------------------------
  # change dataframe name by adding another dataframe 
  observeEvent(input$new_dataframe_name_search,{
    data[[input$new_dataframe_name]] <-  data[[input$dataframe]][input[["dataframe_table_rows_all"]], input$keepColumnName]
    shinyalert(title = "Dataframe successfully renamed",
               text = paste("You can find <b>", input$new_dataframe_name, "</b> in the dataframe dropdown"), 
               type = "success",
               closeOnEsc = TRUE,
               closeOnClickOutside = TRUE,
               html = TRUE
    )
  })
  
  # Display datatable ----------------------------------------------------------------------
  output$dataframe_table <- renderDT(
    data[[input$dataframe]][,input$keepColumnName],
    filter = 'top',
    # style = "foundation",
    extensions = c('FixedHeader'),
    options = list(
      autoWidth = TRUE, # permits to adapt the columns to the width of the box
      scrollX = 500, # permits to scroll along x
      deferRender = TRUE,
      scroller = TRUE,
      dom = 'lBfrtip',
      fixedColumns = list(leftColumns = 2),
      fixedHeader = TRUE
    ),
    # rownames = FALSE
  )
  # keep column ----------------------------------------------------------------------
  output$keepColumns <- renderUI({
    req(input$file) # requires that a file is loaded
    tagList(
      pickerInput("keepColumnName", label = "Select column to keep:",
                  choices = colnames( data[[input$dataframe]]  ),
                  selected = colnames( data[[input$dataframe]] ),
                  options = list(`actions-box` = TRUE),multiple = T)
    )
  })
  
  # Rename column ----------------------------------------------------------------------
  output$modifyColumns <- renderUI({
    req(input$file)
    tagList(
      selectInput("columnName", label = "Select column to modify:",
                  choices = c("", input$keepColumnName),
                  selected = NULL), # chose numerical variable
      conditionalPanel("input.columnName != '' ",
                       selectizeInput("units", label = "Units of measurements:",
                                      choices = c("",unitsMeasure$Symbol), # all admitted unit
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
               text = paste("Name <b>", input$new_columnName, "</b> assigned to <b>", input$columnName, "</b>"), 
               type = "success",
               closeOnEsc = TRUE,
               closeOnClickOutside = TRUE,
               html = TRUE
    )
  })
  
  # Pivot table ----------------------------------------------------------------------
  output$pivotTable <- renderUI({
    req(input$file)
    tagList(
      selectInput("variablePivot", label = "Select categorical variable:",
                  choices = c("", colnames(dplyr::select_if( data[[input$dataframe]][,input$keepColumnName] , is.factor))),
                  selected = NULL,
                  multiple = TRUE), # chose numerical variable
      selectInput("columnsPivot", label = "Select columns to keep:",
                  choices = c("", colnames(dplyr::select_if( data[[input$dataframe]][,input$keepColumnName] , is.numeric))),
                  selected = NULL,
                  multiple = TRUE), # chose numerical variable
      column(width = 8, style = "padding-left:0px; padding-right:0px;",
             selectInput("functionPivot", label = "Select summary function:",
                         choices = c("", "mean","min","max", "sd"),
                         selected = NULL), # chose numerical variable
      ),
      column(width = 4,  style = "padding-left:10px; padding-right:0px;",
             numericInput("decimalDigitsPivot", label = "Digits", value = 2, step = 1, min = 0), # add condition on variable
      ),
      actionButton("pivotTableButton", "Pivot!", width = '100%')
    )
  })
  
  observeEvent(input$pivotTableButton,{ 
    req(input$variablePivot, input$columnsPivot, input$functionPivot)
    switch (input$functionPivot,
            mean = data[[input$dataframe]] <-  data[[input$dataframe]] %>% ddply(input$variablePivot, numcolwise(mean, na.rm = TRUE)),
            min = data[[input$dataframe]] <-  data[[input$dataframe]] %>% ddply(input$variablePivot, numcolwise(min, na.rm = TRUE)),
            max = data[[input$dataframe]] <-  data[[input$dataframe]] %>% ddply(input$variablePivot, numcolwise(max, na.rm = TRUE))
    )
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
  
  
  ###### TAB "Visualize" ----------------------------------------------------------------------
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
    req(input$file)
    tagList(
      selectInput("variable", label = "Variable:", choices = colnames(dplyr::select_if( data[[input$dataframe]], is.numeric))), # chose numerical variable
      sliderInput(inputId = "bins", label = "Number of bins:", min = 1, max = 100, value = 30),
      sliderInput(inputId = "hours_slider_hist", label = "Hours:", min = 0, max = 24, value = c(0, 24) ),
      sliderTextInput(
        inputId = "month_slider_hist",
        label = "Month:",
        choices = unique(data[[input$dataframe]][,"Month"]),
        selected = unique(data[[input$dataframe]][,"Month"])[c(1,length(unique(data[[input$dataframe]][,"Month"])))]
      ),
      column(width = 6,  style = "padding-left:0px; padding-right:5px;",
             selectInput("fillvariable", label = "Fill Variable:", choices = c("None", colnames(dplyr::select_if( data[[input$dataframe]], is.factor))) ),
      ),
      column(width = 6,  style = "padding-left:5px; padding-right:0px;",
             selectInput("facetvariable", label = "Facet Variable:", choices = c("None", colnames(dplyr::select_if( data[[input$dataframe]], is.factor))) ),
      ),
      conditionalPanel("input.facetvariable != 'None'", numericInput("nrowvariable", "Number of facet rows", value = 3, min = 1)), 
      tags$hr(),
      column("Style", width = 6,
             checkboxInput("checkbox_flip", label = "Flip", value = FALSE),
             checkboxInput("checkbox_density", label = "Density", value = FALSE)
      ),
      column("Scale", width = 6,
             checkboxInput("checkbox_logx", label = "Log-X", value = FALSE),
             checkboxInput("checkbox_logy", label = "Log-Y", value = FALSE)
      )
    )
  })
  # output box - plot - HISTOGRAM
  output$outBoxHistogram <- renderPlot({
    req(input$file)
    
    data_plot <- data[[input$dataframe]] %>%
      filter( min_dec >= input$hours_slider_hist[1], 
              min_dec <= input$hours_slider_hist[2],
              Month >= input$month_slider_hist[1], 
              Month <= input$month_slider_hist[2]
              )
    
    plot <- ggplot(data =  data_plot,
                   mapping =  aes(x =  data_plot[,input$variable],
                                  fill = if (input$fillvariable == "None") {NULL} else { data_plot[,input$fillvariable]} ),
    ) + theme_bw()
    
    # densità
    if (input$checkbox_density == TRUE) {
      plot <- plot + geom_density(aes(y = ..density..,
                                      fill = if (input$fillvariable == "None") {NULL} else { data_plot[,input$fillvariable]}),
                                  alpha = 0.4,
                                  bw = input$bins/5,
                                  na.rm = TRUE)  # aggiungo distribuzione
    } else{plot <- plot + geom_histogram(bins = input$bins, na.rm = TRUE)} #
    # flip
    if (input$checkbox_flip == TRUE) {plot <- plot + coord_flip()}
    # tema
    plot <- plot + labs( x = input$variable, fill = input$fillvariable ) + theme(legend.position = "bottom")
    # add wrap
    if (input$facetvariable == "None") {NULL} else {plot <- plot + facet_wrap(~  data_plot[,input$facetvariable], nrow = input$nrowvariable)}
    # log
    # flip
    if (input$checkbox_logx == TRUE) {plot <- plot + scale_x_continuous(trans = 'log10') }
    if (input$checkbox_logy == TRUE) {plot <- plot + scale_y_continuous(trans = 'log10') }
    
    plot
  })
  
  # Boxplot ----------------------------------------------------------------------
  # input box - select variables - BOXPLOT
  output$inBoxBoxplot <- renderUI({
    req(input$file)
    tagList(
      column(width = 6,  style = "padding-left:0px; padding-right:5px;",
             selectInput("variableX_Box", label = "Variable X:", choices = c("None", colnames(dplyr::select_if( data[[input$dataframe]], is.factor))) ), # chose numerical variable
             selectInput("fillvariable_Box", label = "Fill Variable:", choices = c("None", colnames(dplyr::select_if( data[[input$dataframe]], is.factor))) ),
      ),
      column(width = 6,  style = "padding-left:5px; padding-right:0px;",
             selectInput("variableY_Box", label = "Variable Y:", choices = colnames(dplyr::select_if( data[[input$dataframe]], is.numeric))), # chose numerical variable
             selectInput("facetvariable_Box", label = "Facet Variable:", choices = c("None", colnames(dplyr::select_if( data[[input$dataframe]], is.factor))) ),
      ),
      conditionalPanel("input.facetvariable_Box != 'None'", numericInput("nrowvariable_Box", "Number of facet rows", value = 3, min = 1)), 
      tags$hr(),
      column("Style", width = 6,
             checkboxInput("checkbox_flip_Box", label = "Flip", value = FALSE)
      ),
      column("Scale", width = 6,
             checkboxInput("checkbox_logy_Box", label = "Log-Y", value = FALSE)
      )
    )
  })
  # output box - plot - BOXPLOT
  output$outBoxBoxplot <- renderPlot({
    req(input$file)
    
    if (input$variableX_Box == "None") {
      plot <- ggplot(data =  data[[input$dataframe]],
                     mapping =  aes(y =  data[[input$dataframe]][,input$variableY_Box],
                                    fill = if (input$fillvariable_Box == "None") {NULL} else { data[[input$dataframe]][,input$fillvariable_Box]} ),
      ) + labs( y = input$variableY_Box, fill = input$fillvariable_Box ) 
    } else {
      plot <- ggplot(data =  data[[input$dataframe]],
                     mapping =  aes(x =  data[[input$dataframe]][,input$variableX_Box], y =  data[[input$dataframe]][,input$variableY_Box],
                                    fill = if (input$fillvariable_Box == "None") {NULL} else { data[[input$dataframe]][,input$fillvariable_Box]} ),
      ) + labs( x = input$variableX_Box, y = input$variableY_Box, fill = input$fillvariable_Box ) 
    }
    
    
    plot <- plot + theme_bw() + geom_boxplot(na.rm = TRUE)
    
    # flip
    if (input$checkbox_flip_Box == TRUE) {plot <- plot + coord_flip()}
    # tema
    plot <- plot + theme(legend.position = "bottom")
    # add wrap
    if (input$facetvariable_Box == "None") {NULL} else {plot <- plot + facet_wrap(~  data[[input$dataframe]][,input$facetvariable_Box], nrow = input$nrowvariable_Box)}
    # log
    if (input$checkbox_logy_Box == TRUE) {plot <- plot + scale_y_continuous(trans = 'log10') }
    
    plot
  })
  
  # Carpet ----------------------------------------------------------------------
  # input box - select variables - Carpet
  output$inBoxCarpet <- renderUI({
    req(input$file)
    tagList(
      selectInput("variable_Carpet", label = "Variable:", choices = colnames(dplyr::select_if( data[[input$dataframe]], is.numeric)) ) # chose numerical variable
    )
  })
  # output box - plot - Carpet
  output$outBoxCarpet <- renderPlot({
    req(input$file)
    timezone <- gsub(" ", "", paste("timezone_", input$type))
    plot <- ggplot(data =  data[[input$dataframe]], 
                   mapping =  aes(x =  as.POSIXct(format(ymd_hms(data[[input$dataframe]][,"Date_Time"]), "%H:%M:%S"),"%H:%M:%S", tz = input[[timezone]]), 
                                  y =  date(data[[input$dataframe]][,"Date_Time"]),
                                  fill = data[[input$dataframe]][,input$variable_Carpet]
                   )
    ) + 
      geom_tile() +
      scale_y_date(
        breaks = scales::date_breaks("1 month"),                    # specify breaks every two months
        labels = scales::date_format("%b" , tz = input[[timezone]]),  # specify format of labels anno mese
        expand = c(0,0)                                     # espande l'asse y affinche riempia tutto il box in verticale
      ) +
      scale_x_datetime(
        breaks = scales::date_breaks("4 hour"),                     # specify breaks every 4 hours
        labels = scales::date_format(("%H:%M") , tz = input[[timezone]]),# specify format of labels ora minuti
        expand = c(0,0)                                     # espande l'asse x affinche riempia tutto il box in orizzontale
      ) +
      theme_bw() + labs( x = "Hour" , y = "Date", fill = input$variable_Carpet) + 
      facet_wrap(~year(data[[input$dataframe]][,"Date_Time"]), scales = "free", nrow = 1)
    
    plot
  })
  
}


shinyApp(ui, server)
