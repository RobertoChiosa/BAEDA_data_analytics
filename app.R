#################################################################################
###############            Copyright © BAEDA Lab 2020             ###############
###############                     -------                       ###############
###############                  Roberto Chiosa                   ###############
###############             roberto.chiosa@polito.it              ###############
#################################################################################

cat("\014")             # clears the console
rm(list = ls())         # remove all variables of the workspace

# global variables
unitsMeasure <- read.csv("./data/units.csv", header = TRUE, sep = ",", check.names = FALSE) # available units of measure

source("packages.R")    # load necessary packages
source("functions.R")   # load user defined functions
source("header.R")      # load header script
source("sidebar.R")     # load sidebar script
source("body.R")        # load body script

# USER INTERFACE ----------------------------------------------------------------------
ui <- dashboardPage(skin = "black",     # sets overall appearance 
                    header,             # loaded from external script
                    sidebar,            # loaded from external script
                    body                # loaded from external script
)

# SERVER FUNCTION ----------------------------------------------------------------------
server <- function(input, output, session) {
  
  ###### 1) SIDEBAR FUNCTIONS ----------------------------------------------------------------------
  
  # forces start-up modal dialog to open when the application starts
  toggleModal(session, "startupModal", toggle = "open")
  
  # 1.1) Upload new dataframe ----------------------------------------------------------------------
  # when clicked a the upload modal is sown again
  observeEvent(input$upload,{ 
    toggleModal(session, "startupModal", toggle = "open")
  })
  
  # 1.2) Add calendar variables ----------------------------------------------------------------------
  # when clicked it adds calendar variables to the selected dataframe
  observeEvent(input$add_calendar_columns, {
    req(input$file)                                     # # the execution CONTINUES only if a file is present
    
    data[[input$dataframe]] <- add_calendar_variables(  # find it in functions.R
      input[[data_results[["timestamp"]] ]],            # gets the timestamp checkbox value
      input[[data_results[["timezone"]] ]],             # gets the timezone checkbox value
      data[[input$dataframe]]
    )
  })
  
  ###### 2) "MANAGE" TAB ----------------------------------------------------------------------
  # global environment and global options
  data <- reactiveValues()                     # reactive value to store the loaded dataframes
  data_results <- reactiveValues()             # reactive value where we will store all the loaded dataframes
  options(shiny.maxRequestSize = 100*1024^2)   # this option permits to read larger files than shiny default
  
  # 2.1) Load file ----------------------------------------------------------------------
  # create a reactive dataframe df when the file is loaded and add
  observeEvent(input$file,{
    inFile <- input$file   # input file loaded
    nome <- inFile$name    # input file name
    
    # validate that the file is in the right format
    admitted <- gsub( " ", "", paste("\\.", input$type, "$" ))  # this is the only accepted file given the one chosen
    validated <- grepl(admitted, nome)                          # validated is TRUE if the value is acceptable FALSE if not acceptable
    
    # gives error feedback if the file is not in the format required/selected and STOPS the execution
    shinyFeedback::hideFeedback("file")
    if (validated == TRUE) { shinyFeedback::feedbackSuccess("file", TRUE, "Format accepted")} 
    else { shinyFeedback::feedbackDanger("file", TRUE, "Format not accepted") }
    
    # the execution CONTINUES only if the file is accepted
    req(validated, cancelOutput = TRUE)
    
    # notification that the file is being loaded
    id <- showNotification("Reading data...", duration = NULL, closeButton = FALSE, type = "message")
    on.exit(removeNotification(id), add = TRUE)
    
    # reads the input file and assigns it to the reactive value data
    data[[nome]] <- switch(input$type, # condition on the file type
                           csv = read.csv(file = inFile$datapath, header = input$header, sep = input$separator, 
                                          dec = input$decomal, stringsAsFactors = input$strAsFact, check.names = FALSE),
                           rds = readRDS(file = inFile$datapath)
                           # xls = read_excel(path = inFile$datapath)
    )
    
    # saves the selected timezone and timestamp column in the global environment
    data_results[["timestamp"]] <- gsub(" ", "", paste("timestamp_", input$type))
    data_results[["timezone"]] <- gsub(" ", "", paste("timezone_", input$type))
    
  })
  
  # 2.2) Dataframe dropdown creation ----------------------------------------------------------------------
  # create a reactive list of loaded dataframes. When new file loaded the list is updated
  reactive_list <- reactive({ 
    req(input$file) 
    names(data)
  })
  
  # when the list changes the sidebar change the inputs as well see selection_dataframe() function
  observeEvent(reactive_list(),{
    updateSelectInput(session, "dataframe",                               # when new file loaded and new name given the select is updated
                      choices = reactive_list(),                          # update choices with all loaded data
                      selected = reactive_list()[length(reactive_list())] # selected the last loaded file
    )
  })
  
  # 2.3) Value boxes ----------------------------------------------------------------------
  # number of rows in the current dataframe value box
  output$valueBox1 <- renderValueBox({
    req(input$file) # requires that a file is loaded
    valueBox(length(input$dataframe_table_rows_all), "Number of rows", icon = icon("arrows-alt-v"), color = "orange")
  })
  # number of columns in the current dataframe value box
  output$valueBox2 <- renderValueBox({
    req(input$file) # requires that a file is loaded
    valueBox(length(input$keepColumnName), "Number of columns", icon = icon("arrows-alt-h"), color = "blue")
  })
  
  # 2.4) Rename dataframe ----------------------------------------------------------------------
  # change dataframe name by adding another dataframe (don't know how to overwrite)
  observeEvent(input$new_dataframe_name_search,{
    data[[input$new_dataframe_name]] <-  data[[input$dataframe]][input$dataframe_table_rows_all, input$keepColumnName]
    # success notification
    shinyalert(title = "Dataframe successfully renamed",
               text = paste("You can find <b>", input$new_dataframe_name, "</b> in the dataframe dropdown"), 
               type = "success",
               closeOnEsc = TRUE,
               closeOnClickOutside = TRUE,
               html = TRUE
    )
  })
  
  # 2.5) Display datatable ----------------------------------------------------------------------
  # displays through datatable function the actual selected dataframe
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
  # 2.6) Keep column ----------------------------------------------------------------------
  # selection of all the available columns and possibility to exclude some
  output$keepColumns <- renderUI({
    req(input$file) # requires that a file is loaded
    tagList(
      pickerInput("keepColumnName", label = "Select column to keep:",
                  choices = colnames( data[[input$dataframe]]  ), # all available columns in the original dataframe
                  selected = colnames( data[[input$dataframe]] ), # by default all selected
                  options = list(`actions-box` = TRUE),multiple = T) 
    )
  })
  
  # 2.7) Rename column ----------------------------------------------------------------------
  # permits to rename the column by adding the unit of measure as well
  # UI side
  output$modifyColumns <- renderUI({
    req(input$file)
    tagList(
      selectInput("columnName", label = "Select column to modify:",
                  choices = c("", input$keepColumnName),
                  selected = NULL), 
      # if a column is selected it shows the options
      conditionalPanel("input.columnName != '' ",
                       selectizeInput("units", label = "Units of measurements:",
                                      choices = c("",unitsMeasure$Symbol), # all admitted unit # implement with groups
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
  # SERVER side
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
  
  # 2.8) Add column ----------------------------------------------------------------------
  # permits to add column based on given condition TO BE IMPLEMENTED on SERVER SIDE
  # UI side
  output$addColumn <- renderUI({
    req(input$file)
    tagList(
      textInput("expression", "IF ELSE expression", placeholder = "if_else(CONDITION, TRUE, FALSE)"),
      searchInput(inputId = "add_columnName", label = NULL, 
                  placeholder = "Column name..", 
                  value = NULL, # initial value
                  btnSearch = icon("plus"), btnReset = icon("backspace"), # icons
                  width = "100%")
    )
  })
  # SERVER side
  observeEvent(input$add_columnName_search,{
    
    # # if_else(festivo == "S",1,2)
    # data[[input$dataframe]] <-  data[[input$dataframe]] %>%
    #   mutate( New = parse_quo(input$expression, env = caller_env())  )
    
    shinyalert(title = "Column successfully added",
               text = paste("Column <b>", input$add_columnName, "</b> added to <b>", input$dataframe, "</b>"), 
               type = "success",
               closeOnEsc = TRUE,
               closeOnClickOutside = TRUE,
               html = TRUE
    )
  })
  
  # 2.9) Pivot table ----------------------------------------------------------------------
  # permits to pivot tables based on selection of categorical values
  # UI side
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
  # SERVER side
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
  
  # 2.10) Download filtered dataframe ----------------------------------------------------------------------
  # permits the download of the filtered dataframe, only csv available now, implement with modal like upload
  output$download_filtered <- downloadHandler(
    filename = function() {  paste('dataframe-', Sys.Date(), '.csv', sep='') },
    content = function(file) {
      
      write.csv(data[[input$dataframe]][input[["dataframe_table_rows_all"]], ],
                file = file,
                row.names = F)
    }
  )
  
  ###### 3) TAB "Visualize" ----------------------------------------------------------------------
  
  # the download button has to be implemented with options
  output$downloadplotButton <- downloadHandler(
    filename = function() { 
      paste(input$chart, "_plot_", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      ggsave(file, width = 5, height = 5 )
    })
  
  # 3.1) Histogram ----------------------------------------------------------------------
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
        choices = base::sort(unique(data[[input$dataframe]][,"Month"])),
        selected = base::sort(unique(data[[input$dataframe]][,"Month"]))[c(1, length(base::sort(unique(data[[input$dataframe]][,"Month"]))))]
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
  output$outBoxHistogram <- renderPlotly({
    req(input$file, input$hours_slider_hist, input$month_slider_hist)
    
    data_plot <- data[[input$dataframe]] %>%
      dplyr::filter( min_dec >= input$hours_slider_hist[1], 
                     min_dec <= input$hours_slider_hist[2],
                     Month >= input$month_slider_hist[1], 
                     Month <= input$month_slider_hist[2]
      )
    
    plot <- ggplot(data =  data_plot,
                   mapping =  aes(x =  data_plot[,input$variable],
                                  fill = if (input$fillvariable == "None") {NULL} else { data_plot[,input$fillvariable]},
                   ),
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
    plot <- plot + labs( x = input$variable, fill = input$fillvariable )
    # add wrap
    if (input$facetvariable == "None") {NULL} else {plot <- plot + facet_wrap(~  data_plot[,input$facetvariable], nrow = input$nrowvariable)}
    # log
    # flip
    if (input$checkbox_logx == TRUE) {plot <- plot + scale_x_continuous(trans = 'log10') }
    if (input$checkbox_logy == TRUE) {plot <- plot + scale_y_continuous(trans = 'log10') }
    
    plot <- ggplotly(plot, tooltip = c("y"))
    plot
  })
  
  # 3.2) Boxplot ----------------------------------------------------------------------
  # input box - select variables - Boxplot
  output$inBoxBoxplot <- renderUI({
    req(input$file)
    tagList(
      column(width = 6,  style = "padding-left:0px; padding-right:5px;",
             selectInput("variableX_Box", label = "Variable X:", choices = c("None", colnames(dplyr::select_if( data[[input$dataframe]], is.factor))) ) # chose numerical variable
      ),
      column(width = 6,  style = "padding-left:5px; padding-right:0px;",
             selectInput("variableY_Box", label = "Variable Y:", choices = colnames(dplyr::select_if( data[[input$dataframe]], is.numeric))) # chose numerical variable
      ),
      sliderInput(inputId = "hours_slider_box", label = "Hours:", min = 0, max = 24, value = c(0, 24) ),
      sliderTextInput(
        inputId = "month_slider_box",
        label = "Month:",
        choices = base::sort(unique(data[[input$dataframe]][,"Month"])),
        selected = base::sort(unique(data[[input$dataframe]][,"Month"]))[c(1, length(base::sort(unique(data[[input$dataframe]][,"Month"]))))]
      ),
      column(width = 6,  style = "padding-left:0px; padding-right:5px;",
             selectInput("fillvariable_Box", label = "Fill Variable:", choices = c("None", colnames(dplyr::select_if( data[[input$dataframe]], is.factor))) ),
      ),
      column(width = 6,  style = "padding-left:5px; padding-right:0px;",
             selectInput("facetvariable_Box", label = "Facet Variable:", choices = c("None", colnames(dplyr::select_if( data[[input$dataframe]], is.factor))) ),
      ),
      conditionalPanel("input.facetvariable_Box != 'None'", numericInput("nrowvariable_Box", "Number of facet rows", value = 3, min = 1)), 
      
      column("Style", width = 6,
             checkboxInput("checkbox_flip_Box", label = "Flip", value = FALSE)
      ),
      column("Scale", width = 6,
             checkboxInput("checkbox_logy_Box", label = "Log-Y", value = FALSE)
      ),
      
      tags$hr(),
      p("Outliers options"),
      column(width = 3,  style = "padding-left:0px; padding-right:5px;",
             numericInput('outliers_coef', 'Coef', value = 1.5, step = 0.1),
      ),
      column(width = 3,  style = "padding-left:0px; padding-right:5px;",
             numericInput('outliers_alpha', 'Alpha', value = 0.2, step = 0.2),
      ),
      column(width = 3,  style = "padding-left:0px; padding-right:5px;",
             numericInput('outliers_shape', 'Shape', value = 1, step = 1, min = 1),
      ),
      column(width = 3,  style = "padding-left:0px; padding-right:5px;",
             numericInput('outliers_size', 'Size', value = 1, step = 1, min = 1),
      ),
      column(width = 6,  style = "padding-left:0px; padding-right:5px;",
             selectInput("outliers_fill", label = "Fill:", choices = c("black","red", "blue")), 
      ),
      column(width = 6,  style = "padding-left:0px; padding-right:5px;",
             selectInput("outliers_color", label = "Color:", choices = c("black","red", "blue")), 
      ),
      
      
      
    )
  })
  # output box - plot - Boxplot
  output$outBoxBoxplot <- renderPlot({
    req(input$file, input$hours_slider_box, input$month_slider_box)
    
    data_plot <- data[[input$dataframe]] %>%
      dplyr::filter( min_dec >= input$hours_slider_box[1], 
                     min_dec <= input$hours_slider_box[2],
                     Month >= input$month_slider_box[1], 
                     Month <= input$month_slider_box[2]
      )
    
    if (input$variableX_Box == "None") {
      plot <- ggplot(data =  data_plot,
                     mapping =  aes(y =  data_plot[,input$variableY_Box],
                                    fill = if (input$fillvariable_Box == "None") {NULL} else { data_plot[,input$fillvariable_Box]} 
                                    )
      ) + labs( y = input$variableY_Box, fill = input$fillvariable_Box ) 
    } else {
      plot <- ggplot(data =  data_plot,
                     mapping =  aes(x =  data_plot[,input$variableX_Box], y =  data_plot[,input$variableY_Box],
                                    fill = if (input$fillvariable_Box == "None") {NULL} else { data_plot[,input$fillvariable_Box]} ),
      ) + labs( x = input$variableX_Box, y = input$variableY_Box, fill = input$fillvariable_Box ) 
    }
    
    plot <- plot + theme_bw() + 
      # stat_boxplot(geom ='errorbar') +
      geom_boxplot(
                   outlier.colour = input$outliers_color, 
                   outlier.fill = input$outliers_fill, 
                   outlier.shape = input$outliers_shape, 
                   outlier.size = input$outliers_size, 
                   outlier.alpha = input$outliers_alpha,
                   coef = input$outliers_coef,  na.rm = TRUE) 
    
    # flip
    if (input$checkbox_flip_Box == TRUE) {plot <- plot + coord_flip()}
    # tema
    plot <- plot + theme(legend.position = "bottom")
    # add wrap
    if (input$facetvariable_Box == "None") {NULL} else {plot <- plot + facet_wrap(~  data_plot[,input$facetvariable_Box], nrow = input$nrowvariable_Box)}
    # log
    if (input$checkbox_logy_Box == TRUE) {plot <- plot + scale_y_continuous(trans = 'log10') }
    
    # plot <- ggplotly(plot)
    plot
  })
  
  # 3.3) Carpet ----------------------------------------------------------------------
  # input box - select variables - Carpet
  output$inBoxCarpet <- renderUI({
    req(input$file)
    
    date_max = max( data[[input$dataframe]][, "Date_Time"] )
    date_min = min( data[[input$dataframe]][, "Date_Time"] )
    
    tagList(
      # dateRangeInput("daterange_Carpet", "Date range:",
      #                start  = if_else(date_max-date_min > lubridate::years(1), date_max - lubridate::days(365), date_min),
      #                end    = date_max,
      #                min    = date_min,
      #                max    = date_max,
      #                format = "yyyy-mm-dd",
      #                separator = " - "),
      selectInput("variable_Carpet", label = "Variable:", choices = colnames(dplyr::select_if( data[[input$dataframe]], is.numeric)) ), # chose numerical variable
      selectInput("facetvariable_Carpet", label = "Facet Variable:", choices = c("None", colnames(dplyr::select_if( data[[input$dataframe]], is.factor))) ),
      radioButtons("brewer", "Choose theme:", choices = c("theme1","theme2"), inline = TRUE)
    )
  })
  # output box - plot - Carpet
  output$outBoxCarpet <- renderPlotly({
    req(input$file, input$brewer)
    
    if(input$brewer == "theme1"){ cols <- brewer.pal(9, "YlOrRd")}else{cols <- rev(brewer.pal(9, "Spectral"))}
    
    timezone <- gsub(" ", "", paste("timezone_", input$type))
    
    data_plot <- data[[input$dataframe]]
    
    # data_plot <- data[[input$dataframe]] %>%
    #   dplyr::filter(Date >= input$daterange_Carpet[1], Date <= input$daterange_Carpet[2])
    
    plot <- ggplot(data =  data_plot, 
                   mapping =  aes(x =  as.POSIXct(format(Date_Time, "%H:%M:%S"), "%H:%M:%S", tz = input[[timezone]]), 
                                  y =  date(data_plot[,"Date_Time"]),
                                  fill = data_plot[,input$variable_Carpet],
                                  text = paste(' Time:', format(Date_Time, "%H:%M:%S"), 
                                               '<br> Date: ', date(Date_Time), '<br>',
                                               input$variable_Carpet, ':', data_plot[,input$variable_Carpet]
                                  )
                   )
    ) + 
      geom_tile(na.rm = TRUE) +
      scale_fill_gradientn(colours = cols) + # creates a two colour gradient (low-high) for the variable fill=z=power
      scale_y_date(
        breaks = scales::date_breaks("1 month"),                    # specify breaks every two months
        labels = scales::date_format("%Y-%b" , tz = input[[timezone]]),  # specify format of labels anno mese
        expand = c(0,0)                                     # espande l'asse y affinche riempia tutto il box in verticale
      ) +
      scale_x_datetime(
        breaks = scales::date_breaks("4 hour"),                     # specify breaks every 4 hours
        labels = scales::date_format(("%H:%M") , tz = input[[timezone]]),# specify format of labels ora minuti
        expand = c(0,0)                                     # espande l'asse x affinche riempia tutto il box in orizzontale
      ) +
      theme_bw() + labs( x = "Hour" , y = "Date", fill = input$variable_Carpet)
    
    # add wrap
    if (input$facetvariable_Carpet == "None") {NULL} else {plot <- plot + facet_wrap(~  data_plot[,input$facetvariable_Carpet],  scales = "free", nrow = 1)}
    
    plot <- ggplotly(plot, tooltip = c("text"), dynamicTicks = TRUE)
    plot
  })
  
  
  # 3.4) Lineplot ----------------------------------------------------------------------
  # input box - select variables - Lineplot
  output$inBoxLineplot <- renderUI({
    req(input$file)
    
    # date_max = max( data[[input$dataframe]][, "Date_Time"] )
    # date_min = min( data[[input$dataframe]][, "Date_Time"] )
    
    tagList(
      # dateRangeInput("daterange_Line", "Date range:",
      #                start  = if_else(date_max-date_min > lubridate::years(1), date_max- lubridate::days(365), date_min),
      #                end    = date_max,
      #                min    = date_min,
      #                max    = date_max,
      #                format = "yyyy-mm-dd",
      #                separator = " - "),x
      selectInput("variableY_Line", label = "Variable Y:",
                  choices = colnames(dplyr::select_if( data[[input$dataframe]], is.numeric))), # chose numerical variable
    )
  })
  # output box - plot - Lineplot
  output$outBoxLineplot <- renderPlotly({
    req(input$variableY_Line)
    
    timezone <- gsub(" ", "", paste("timezone_", input$type))
    
    data_plot <- data[[input$dataframe]]
    # data_plot <- data[[input$dataframe]] %>%
    #   dplyr::filter(Date_Time >= input$daterange_Line[1], Date_Time <= input$daterange_Line[2])
    
    plot <- ggplot(data =  data_plot,
                   mapping =  aes(x = as.POSIXct(data_plot$Date_Time, "%Y-%m-%d %H:%M:%S", tz = input[[timezone]]),
                                  y =  data_plot[,input$variableY_Line],
                                  text = paste(' Time:', format(data_plot$Date_Time, "%H:%M:%S"), 
                                               '<br> Date: ', data_plot$Date, '<br>',
                                               input$variableY_Line, ':', data_plot[,input$variableY_Line]
                                  ), 
                                  group = 1 # solve ggplotly problem
                   )
    ) + 
      geom_line(na.rm = TRUE) +
      theme_bw() +
      labs( x = "Date Time", y = input$variableY_Line)
    
    # # add wrap
    # if (input$facetvariable_Box == "None") {NULL} else {plot <- plot + facet_wrap(~  data_plot[,input$facetvariable_Box], nrow = input$nrowvariable_Box)}
    
    
    plot <- ggplotly(plot, tooltip = c("text"), dynamicTicks = TRUE)
    
    plot
  })
  
  
  # 3.5) Scatterplot ----------------------------------------------------------------------
  # input box - select variables - Scatterplot
  output$inBoxScatterplot <- renderUI({
    req(input$file)
    
    tagList(
      column(width = 6,  style = "padding-left:0px; padding-right:5px;",
             selectInput("variableX_Scatter", label = "Variable X:", choices = colnames(dplyr::select_if( data[[input$dataframe]], is.numeric)) ) # chose numerical variable
      ),
      column(width = 6,  style = "padding-left:5px; padding-right:0px;",
             selectInput("variableY_Scatter", label = "Variable Y:", choices = colnames(dplyr::select_if( data[[input$dataframe]], is.numeric))) # chose numerical variable
      ),
      sliderInput(inputId = "hours_slider_Scatter", label = "Hours:", min = 0, max = 24, value = c(0, 24) ),
      sliderTextInput(
        inputId = "month_slider_Scatter",
        label = "Month:",
        choices = base::sort(unique(data[[input$dataframe]][,"Month"])),
        selected = base::sort(unique(data[[input$dataframe]][,"Month"]))[c(1, length(base::sort(unique(data[[input$dataframe]][,"Month"]))))]
      ),
      column(width = 6,  style = "padding-left:0px; padding-right:5px;",
             selectInput("colorvariable_Scatter", label = "Color Variable:", choices = c("None", colnames(dplyr::select_if( data[[input$dataframe]], is.factor))) ),
      ),
      column(width = 6,  style = "padding-left:5px; padding-right:0px;",
             selectInput("facetvariable_Scatter", label = "Facet Variable:", choices = c("None", colnames(dplyr::select_if( data[[input$dataframe]], is.factor))) ),
      ),
      conditionalPanel("input.facetvariable_Scatter != 'None'", numericInput("nrowvariable_Scatter", "Number of facet rows", value = 3, min = 1)), 
      
      column("Style", width = 6,
             checkboxInput("checkbox_flip_Scatter", label = "Flip", value = FALSE)
      ),
      column("Scale", width = 6,
             checkboxInput("checkbox_logy_Scatter", label = "Log-Y", value = FALSE)
      )
    )
  })
  # output box - plot - Scatterplot
  
  output$outBoxScatterplot <- renderPlot({
    req(input$file, input$hours_slider_Scatter, input$month_slider_Scatter)
    
    data_plot <- data[[input$dataframe]] %>%
      dplyr::filter( min_dec >= input$hours_slider_Scatter[1], 
                     min_dec <= input$hours_slider_Scatter[2],
                     Month >= input$month_slider_Scatter[1], 
                     Month <= input$month_slider_Scatter[2]
      )
    
    # data_plot <- data_plot %>%
    #   mutate(Date = date(Date_Time), X = data_plot[,input$variableX_Scatter], Y = data_plot[,input$variableY_Scatter]) %>%
    #   select(Date, X, Y) %>%
    #   dplyr::group_by(Date)  %>%
    #   summarise_all(.funs = c(mean="mean"))
    # 
    #   dplyr::summarize(X = mean(X,na.rm = T),
    #                    Y = mean(Y,na.rm = T)) %>%
    #   ungroup()
    
    
    plot <- ggplot(data =  data_plot,
                   mapping =  aes(x =  data_plot[,input$variableX_Scatter], 
                                  y =  data_plot[,input$variableY_Scatter],
                                  color = if (input$colorvariable_Scatter == "None") {NULL} else { data_plot[,input$colorvariable_Scatter]}
                   )
    ) + geom_point(alpha = 0.5) + 
      labs( x = input$variableX_Scatter, y = input$variableY_Scatter, color = input$colorvariable_Scatter )
    
    # flip
    if (input$checkbox_flip_Scatter == TRUE) {plot <- plot + coord_flip()}
    # tema
    plot <- plot + theme(legend.position = "bottom")
    # add wrap
    if (input$facetvariable_Scatter == "None") {NULL} else {plot <- plot + facet_wrap(~  data_plot[,input$facetvariable_Scatter], nrow = input$nrowvariable_Scatter)}
    # log
    if (input$checkbox_logy_Scatter == TRUE) {plot <- plot + scale_y_continuous(trans = 'log10') }
    
    #plot <- ggplotly(plot, tooltip = c("x", "y"))
    plot
  })
  
  ###### 4) "CLUSTERING" TAB ----------------------------------------------------------------------
  
  # 4.1) Clustering input parameters ----------------------------------------------------------------------
  output$clustering_inbox <- renderUI({
    req(input$file) # requires that a file is loaded
    tagList(
      column(width = 6,  style = "padding-left:0px; padding-right:0px;",
             selectInput('cluster_variable', 'Variable:', choices = colnames(dplyr::select_if( data[[input$dataframe]], is.numeric)) ),
             selectInput('cluster_distance', 'Distance:', 
                         choices = list(
                           General = c("euclidean","maximum", "manhattan", "canberra", "binary", "minkowski"),
                           Partitive = c("pearson" , "abspearson" , "abscorrelation", "correlation", "spearman", "kendall")
                         )
             ),
             numericInput('cluster_number', 'Number of clusters', value = 2, min = 1)
      ),
      column(width = 6,  style = "padding-left:10px; padding-right:0px;",
             selectInput('cluster_normalization', 'Normalization:', choices = c("none","maxmin", "max", "min","zscore")),
             selectInput('cluster_method', 'Clustering method:', 
                         choices = list(
                           Hierarchical = c("ward.D2", "ward.D", "single","complete", "average", "mcquitty", "median", "centroid"),
                           Partitive = c("kmeans", "kmeans++", "FDL")
                         ),
                         selected = 'ward.D2'
             ),
             # NBCLUST
             tags$div(title = "By selecting yes the NBclust package will evaluate the optimal number of clusters",
                      radioGroupButtons(inputId = "radio_nbclust", label = "Search optimal number", choices = c("Yes", "No"), justified = TRUE
                      )
             )
      )
    )
  })
  
  # 4.2) NBClust input parameters ----------------------------------------------------------------------
  output$clustering_inbox_nbclust <- renderUI({
    
    if(input$radio_nbclust == "Yes"){# if we want to evaluate the optimal number of clusters
      # validation of nbclust function
      valid_dist <- input$cluster_distance %in% c("euclidean","maximum", "manhattan", "canberra", "binary", "minkowski")
      valid_meth <- !(input$cluster_method %in% c("kmeans++", "FDL") )
      
      shiny::validate(
        need(valid_dist == TRUE,  "Sorry, the selected distance is not supported for the optimal number of clusters evaluation"),
        need(valid_meth == TRUE, "Sorry, the selected clustering method is not supported for the optimal number of clusters evaluation")
      )
      # the user interface buttons
      tagList(
        selectInput('index_nbclust', 'Select index:', 
                    choices = c("kl", "ch", "hartigan", "ccc", "scott", "marriot", "trcovw", "tracew", "friedman", "rubin", "cindex", "db", "silhouette", "duda", "pseudot2", "beale", "ratkowsky", "ball", "ptbiserial", "gap", "frey", "mcclain", "gamma", "gplus", "tau", "dunn", "hubert", "sdindex", "dindex", "sdbw", "all", "alllong"),
                    selected = 'silhouette'
        ),
        sliderInput(inputId = "cluster_number_nbclust", label = "Number of clusters:", min = 2, max = 8, value = c(2, 8) ),
      )
    } # else NULL
  })
  
  # 4.3) Clustering post rocessing input parameters ----------------------------------------------------------------------
  output$clustering_inbox_postprocessing <- renderUI({
    req(input$cluster_button) # merge and discard inpute
    tagList(
      hr(),
      column(width = 8,  style = "padding-left:0px; padding-right:0px;",
             selectizeInput('cluster_merge', label = NULL, choices = c("Select cluster to merge..."='', unique(data_results[["Clustering_df"]]$Cluster) ), multiple = TRUE )),
      column(width = 4,  style = "padding-left:10px; padding-right:0px;", 
             actionButton('cluster_merge_button', 'Merge!',  width = '100%')),
      column(width = 8,  style = "padding-left:0px; padding-right:0px;",
             selectizeInput('cluster_discard', label = NULL, choices = c('Select cluster to discard...'='', unique(data_results[["Clustering_df"]]$Cluster) ), multiple = TRUE)),
      column(width = 4,  style = "padding-left:10px; padding-right:0px;", 
             actionButton('cluster_discard_button', 'Discard!',  width = '100%')),
      searchInput(inputId = "clustering_dataframe_name", 
                  label = "Save clustering results dataframe", 
                  placeholder = "New name..", 
                  value = NULL, # initial value
                  btnSearch = icon("plus"), btnReset = icon("backspace"), # icons
                  width = "100%")
    )
  })
  
  # 4.4) Clustering process ----------------------------------------------------------------------
  observeEvent(input$cluster_button,{
    # validation
    req(input$file)                                                   # require a uploaded file
    validate_df <- "Date_Time" %in% colnames(data[[input$dataframe]]) # require this column to be created
    req(validate_df)                                                  # stops execution if no datetime found
    
    # notification of process
    id <- showNotification("Performing clustering...", duration = NULL, closeButton = FALSE, type = "message")
    on.exit(removeNotification(id), add = TRUE)
    
    # process and create clustering dataframe
    df1 <- data[[input$dataframe]]  %>%
      mutate(Date = date(Date_Time),
             Time = format(round_date(data[[input$dataframe]]$Date_Time, "5 mins") , "%H:%M:%S"),
             X = data[[input$dataframe]][,input$cluster_variable]) %>%
      select(Date, Time, X)
    
    # normalize data given input command
    switch(input$cluster_normalization,
           none = df1 <- df1,
           zscore = df1 <- df1 %>% dplyr::mutate(X = (X-mean(X))/sd(X) ) ,
           max = df1 <- df1 %>% dplyr::group_by(Date) %>% dplyr::mutate(X = X/max(X)) %>% dplyr::ungroup(),
           min = df1 <- df1 %>% dplyr::group_by(Date) %>% dplyr::mutate(X = X/min(X)) %>% dplyr::ungroup(),
           maxmin = df1 <- df1 %>% dplyr::group_by(Date) %>% dplyr::mutate(X = (X-min(X))/(max(X)-min(X)) )  %>% dplyr::ungroup()
    )
    
    # create M N matrix
    df1 <- distinct(df1)  # checks that the keys are unique, some problems arise when timezone
    df2 <- pivot_wider(df1, names_from = Time, values_from = X) %>% # use pivot_wider instead of spread
      na.omit() # omits na when coercing
    df3 <- df2[ 2:dim(df2)[2] ]  # keep only times
    
    # check consistency of method and distance
    shinyFeedback::hideFeedback("cluster_method")
    shinyFeedback::hideFeedback("cluster_distance")
    
    # if TRUE incompatible condition partitive distance but hierarchical method
    inconsistent = input$cluster_distance %in% c("pearson" , "abspearson" , "abscorrelation", "correlation", "spearman", "kendall") & 
      input$cluster_method %in%  c("ward.D2", "ward.D", "single","complete", "average", "mcquitty", "median", "centroid") 
    
    if ( inconsistent == TRUE ) { # incompatible condition
      shinyFeedback::feedbackDanger("cluster_method", TRUE, "Inconsistent method") 
      shinyFeedback::feedbackDanger("cluster_distance", TRUE, "Inconsistent distance") 
    } 
    
    # continues the execution if there is consistence between method and distance
    # general+partitive/hierarchical OR partitive+partitive
    req(!inconsistent)
    
    if (input$cluster_method == "kmeans") {
      clust_res <- Kmeans(df3, input$cluster_number, method = input$cluster_distance) # perform clustering
      df2$Cluster <- clust_res$cluster                                                # add labels to dataframe
    } else if (input$cluster_method == "kmeans++") {
      clust_res <- kmeanspp(df3, input$cluster_number)                                # perform clustering
      df2$Cluster <- clust_res$cluster                                                # add labels to dataframe
    } else{ # hierarchical
      diss_matrix <- dist(df3, input$cluster_distance)                                # calculate distance matrix    
      hcl <- hclust(diss_matrix, method = input$cluster_method)                       # perform clustering
      df2$Cluster <- cutree(hcl, input$cluster_number)                                # add labels to dataframe
    }
    
    # merge cluster information with the original dataframe
    df1 <- merge.data.frame(df1, df2[c("Date", "Cluster")])
    # saves df1 in memory as result
    data_results[["Clustering_df"]] <- df1
  })
  
  # 4.5) Merge clusters ----------------------------------------------------------------------
  observeEvent(input$cluster_merge_button, {
    df_tmp <- data_results[["Clustering_df"]]                         # gets resulting dataframe with labels
    tmp <- min(input$cluster_merge)                                   # gets the minimum value of cluster to be merged
    df_tmp$Cluster[ df_tmp$Cluster %in% input$cluster_merge] <- tmp   # assign to all the cluster labels the minimum cluster label
    data_results[["Clustering_df"]] <- df_tmp                         # save resulting dataframe in results memory
  })
  
  
  # 4.6) Discard clusters ----------------------------------------------------------------------
  observeEvent(input$cluster_discard_button, {
    df_tmp <- data_results[["Clustering_df"]]                         # gets resulting dataframe with labels
    df_tmp <- df_tmp[! df_tmp$Cluster %in% input$cluster_discard,]    # keeps only those without the selected label
    data_results[["Clustering_df"]] <- df_tmp                         # save resulting dataframe in results memory
  })
  
  # 4.7) Plot daily profiles clusters ----------------------------------------------------------------------
  output$out_clustering_preview <- renderPlot({
    # requires a performed clustering and a result dataframe in memory
    req(input$cluster_button, data_results[["Clustering_df"]])
    
    df1 <- data_results[["Clustering_df"]] # load actual cluster dataframe
    
    # manipulates the dataframe to add cluster label with count 
    conteggio <- df1 %>% 
      pivot_wider(names_from = Time, values_from = X)  %>%
      dplyr::group_by(Cluster) %>%
      dplyr::count(Cluster)
    
    # creates label with number of profiles
    conteggio$Cluster_lab <- paste("Cluster", conteggio$Cluster, "( n =", conteggio$n, ")")
    
    # ricreated the original dataframe but with labels and count
    df1 <- merge.data.frame(df1, conteggio[c("Cluster", "Cluster_lab")])
    
    # calculates the centroid for each cluster
    centr <- ddply(df1, c("Cluster_lab","Time"), summarise, X = mean(X))
    
    # graphical parameters
    clusters_colors <- brewer.pal(12, "Paired")
    line_color <- "gray"
    line_size <- 0.5
    line_alpha <- 0.7
    timezone <- gsub(" ", "", paste("timezone_", input$type))
    
    plot <- ggplot() +
      geom_line(data = df1,
                aes(x = as.POSIXct(Time, format="%H:%M:%S" , tz = input[[timezone]]) ,
                    y = X,
                    group = Date,
                ), na.rm = T,
                color = line_color,
                alpha = line_alpha,
                size = line_size) +
      geom_line(data = centr,
                aes(x = as.POSIXct(Time, format="%H:%M:%S" , tz = input[[timezone]]) ,
                    y = X,
                    color = as.factor(Cluster_lab)),
                size = line_size*2, na.rm = T) +
      scale_color_manual(values = clusters_colors[c(1:input$cluster_number)]) +
      scale_x_datetime(
        breaks = date_breaks("4 hour"),                     # specify breaks every 4 hours
        labels = date_format(("%H:%M") , tz = input[[timezone]]),  # specify format of labels
        expand = c(0,0)                                     # expands x axis
      ) +
      scale_y_continuous(
        limits = c(0,ceiling(max(df1$X)) ),       # set limits from 0 to higher power consumption
        expand = c(0,0)                                       # expands x axis
      ) +
      theme_bw() +                                           # white bakground with lines
      ggplot2::theme(
        legend.position = "none",                     # legend position on the top of the graph
        strip.text = element_text(size = 12), # facet wrap title fontsize
        axis.title.x = element_text(size=15,margin = margin(t = 20, r = 20, b = 0, l = 0)),
        axis.title.y = element_text(size=15,margin = margin(t = 20, r = 20, b = 0, l = 0)),
        axis.text.x = element_text(size=12, angle=45, vjust = .5),
        axis.text.y = element_text(size=12 , vjust=.3),
      ) +
      labs(x = "Time", y = input$cluster_variable, color = "Cluster") +
      facet_wrap(~Cluster_lab)
    
    plot
  })
  
  
  # 4.8) Plot dendogram ----------------------------------------------------------------------
  # 
  # # dendogram by date plot
  # output$out_clustering_dendogram <- renderPlot({
  #   # notification of process
  #   id <- showNotification("Plotting clusters...", duration = NULL, closeButton = FALSE, type = "message")
  #   on.exit(removeNotification(id), add = TRUE)
  #   
  #   hcl <- as.dendrogram(hcl) 
  #   
  #   # Color the branches based on the clusters:
  #   hcl <- color_branches(hcl, k=input$cluster_number, col = clusters_colors[c(1:input$cluster_number)]) #, groupLabels=iris_species)
  #   
  #   plot(hcl, leaflab = "none", # no leaf labels (usually too many to be readable)
  #        ylab = "Height", xlab = "", main = "", sub = "")
  #   
  #   # if( input$cluster_number >=2){
  #   #   rect.hclust(hcl, k = input$cluster_number, border = c(brewer.pal( input$cluster_number, "Set1")) )
  #   # }
  #   
  # })
  # 
  
  # 4.10) Add clustering dataframe ----------------------------------------------------------------------
  # permits to add the clustering dataframe to the dropdown
  # it saves a copy from result memory to data reactive values
  observeEvent(input$clustering_dataframe_name_search,{
    data[[input$clustering_dataframe_name]] <-  data_results[["Clustering_df"]]
    # success notification
    shinyalert(title = "Dataframe successfully saved",
               text = paste("You can find <b>", input$clustering_dataframe_name, "</b> in the dataframe dropdown"), 
               type = "success",
               closeOnEsc = TRUE,
               closeOnClickOutside = TRUE,
               html = TRUE
    )
  })
  
}

# runs the app
shinyApp(ui, server)
