
cat("\014")             # clears the console
rm(list = ls())         # remove all variables of the workspace
# packages ----------------------------------------------------------------------

library(shiny)                  # construct shiny apps
library(shinydashboard)         # construct shiny dashboards
library(markdown)               # create markdown reports
library(rmarkdown)              # create R markdown reports
library(readtext)
library(shinycssloaders)        # enable cool loaders when loading graphs
library(dashboardthemes)        # extend classic themed with cool themes
library(tidyverse)              # data manipulation 
# library(ggplot2)              # (loaded with tidyverse)
# library(purrr)                # (loaded with tidyverse)
# library(tibble)               # (loaded with tidyverse)
# library(dplyr)                # (loaded with tidyverse)
# library(tidyr)                # (loaded with tidyverse)
# library(stringr)              # (loaded with tidyverse)
# library(readr)                # (loaded with tidyverse)
# library(forcats)              # (loaded with tidyverse)
library(lubridate)              # easy to work with dates and times
library(readxl)                 # read and import excel files
library(DT)                 # to use output datatable
library(skimr) # for statistical summary
# library(ggExtra)                # distributions on border
# library(RColorBrewer)           # Pacchetto per creare palette di colori più accattivanti
# library(rpart)                  # make classification tree
# library(rpart.plot)             # plot classification tree
# library(partykit)               # regression and classification tree plot and tools
# library(scales)                 # Graphical scales map data to aesthetics
# library(MLmetrics)              # measure regression, classification and ranking performance.

# library(imputeTS)               # manage missing values
# library(jmotif)                 # sax manipulations
# library(ggpubr)                 # multiple plots in line or column
# library(stringr)                # multiple plots in line or column
# library(networkD3)              # draw sankey plots
# library(NbClust)                # cluster validation measures
# library(knitr)
# library(magrittr)
# library(dendextend)
# library(ggsci)
# library(zoo)
# library(qcc)
# library(Hmisc)                  # permette di riportare info in lag
# library(evtree)                 # labelling without overlap
# library(moments)                # para uso das kewness e kurtosis
# library(partykit)               # to draw and display trees
# library(party)                  # to draw and display trees
# library(arules)                 # to make association rules
# library(treemap)                # to make tree plot
# library(arulesViz)              # to draw association rules graphs
# library(rjson)                  # read json files
library(rsconnect)              # deploy on shinyapp.io
library("shinyWidgets") # add htms widgets
library("shinyhelper") # add help feature

# app ----------------------------------------------------------------------

dateFormats <- c("%Y-%m-%d %H:%M:%S", "%m/%d %H:%M:%S", "%d/%m/%y %H:%M") # accepted date formats

ui <- fluidPage(
  tags$style(".fa-plus {color:green}"), # change icon color
  tags$style(".fa-backspace {color:red}"), # change icon color
  sidebarPanel(
    # select the dataset you want to use for the analysis - default none
    tags$div(title = "This is the actual dataframe used for your analysis",
             selectInput("dataframe", label = "Dataframe r:", choices = c(" "))
    ),     
    tags$hr(), # add horizontal line
    # chose wether or not load a new file ----------------------------------------------------------------------
    awesomeCheckbox("new_file_upload_chackbox", "Upload new file:", value = FALSE, status = "primary"),
    conditionalPanel("input.new_file_upload_chackbox == true",
                     # type of files that can be loaded
                     selectInput("type", "Chose file type:", c("","csv", "xlm"), selected = NULL),
                     # the user wants to load a csv file
                     conditionalPanel("input.type == 'csv'",
                                      checkboxInput("timestamp", "Timestamp column?", value = TRUE),
                                      checkboxInput("header", "Header?", value = TRUE),
                                      checkboxInput("strAsFact", "String as Factor?", value = TRUE),
                                      selectInput("separator", "Separator;:", c("Comma (,)" = ",", "Semicolon (;)" = ";")),
                                      selectInput("decimal", "Decimal:", c("Point (.)" = ".","Comma (,)" = ",")),
                                      numericInput("decimalDigits", label = "Decimal digits:", value = 2, step = 1, min = 0), # add condition on variable
                     ), # end conditional csv
                     # the user wants to load a xlm file
                     conditionalPanel("input.type == 'xlm'",
                                      p("WARNING: file not supported")
                                      
                     ), # end conditional csv
                     conditionalPanel("input.type != ''",
                                      fileInput("file",paste("Upload file:"), multiple = TRUE, 
                                                accept = c("text/csv",
                                                           "text/comma-separated-values,text/plain",
                                                           ".csv")
                                      )
                     )
    ), # end conditional new file upload name
    
    # chose wether or not rename dataframe ----------------------------------------------------------------------
    awesomeCheckbox("new_dataframe_name_chackbox", "Rename dataframe:", value = FALSE),
    conditionalPanel("input.new_dataframe_name_chackbox == true", # if we want to rename 
                     searchInput(inputId = "new_dataframe_name", label = NULL, 
                                 placeholder = "New name..", 
                                 value = NULL, # initial value
                                 btnSearch = icon("plus"), btnReset = icon("backspace"), # icons
                                 width = "100%")
    ), # end conditional new dataframe name
    
    # chose wether or not remove dataframe ----------------------------------------------------------------------
    awesomeCheckbox("remove_dataframe_chackbox", "Remove dataframe:", value = FALSE),
    conditionalPanel("input.remove_dataframe_chackbox == true", # if we want to rename 
                     selectInput('dataframe_remove', NULL, c(""), multiple = TRUE, selectize = FALSE),
                     actionButton("dataframe_remove_button", "Remove", width = '100%', icon = icon("trash"),
                                  style = "color: #fff; background-color: red; border-color: #red"),
    )
  ),
  mainPanel(
    radioButtons("display_buttons","Display:", c("str", "summary","skim"), inline = T),
    verbatimTextOutput("manage_outputBox") # see server function
  )
)

server <- function(input, output, session) {
  
  options(shiny.maxRequestSize = 100*1024^2)
  
  data <- reactiveValues()
  
  # create a reactive dataframe df when the file is loaded
  observeEvent(input$file,{
    inFile <- input$file                                    # input file loaded
    nome <- inFile$name                                     # input file name
    # reads the input file and assigms it to the reactive value data
    data[[nome]] <- if (!is.null(inFile)) {
      # condition on the file type
      if (input$type == "csv") {       
        # read csv file
        read.csv(file = inFile$datapath, 
                 header = input$header, 
                 sep = input$separator, 
                 dec = input$decomal, 
                 stringsAsFactors = input$strAsFact,
                 check.names = FALSE)}
    }
    
    if (input$timestamp == T) {
      # function to automatically find date column
      coldate <- sapply(data[[nome]],   function(x) !all(is.na(as.Date(as.character(x), format = dateFormats))))
      # create date time columns
      data[[nome]] <- data[[nome]] %>%
        mutate(
          Date_Time = as.POSIXct(data[[nome]][,coldate] , format = "%Y-%m-%d %H:%M:%S" , tz = "Europe/Rome"),
          Week_Day = wday(Date_Time, label = TRUE, locale = Sys.getlocale("LC_TIME"), week_start = getOption("lubridate.week.start", 1)),
          Month = month(Date_Time, label = TRUE, locale = Sys.getlocale("LC_TIME")),
          Month_Day = mday(Date_Time),
          Year = year(Date_Time),
          Year_Day = mday(Date_Time),
          Hour = hour(Date_Time),
          Minute = minute(Date_Time)
        ) 
    }
    data[[nome]] <- data[[nome]] %>%
      mutate_if(is.numeric, ~round(., input$decimalDigits))
    
  })
  
  # change dataframe name by adding another dataframe 
  observeEvent(input$new_dataframe_name_search,
               data[[input$new_dataframe_name]] <-  data[[input$dataframe]]
  )
  
  
  # change select dataframe when new file is loaded
  observeEvent(list(input$new_dataframe_name_search, input$file),
               updateSelectInput(session, "dataframe",
                                 choices = names(data),        # update choiches with all loaded data         
                                 selected = input$file$name    # selected the last loaded file
               )
  )
  
  
  observe({
    
    # change select dataframe when new file is loaded
    updateSelectInput(session, "dataframe_remove",
                      choices = names(data),        # update choiches with all loaded data
                      selected = input$file$name    # selected the last loaded file
    )
    
    # the dataset used is always the one selected
    output_df <- data[[input$dataframe]]
    
    # box output function
    output$manage_outputBox <- renderPrint({
      if (input$display_buttons == "str") { str(output_df) }
      else if (input$display_buttons == "summary") { summary(output_df) }
      else if (input$display_buttons == "skim") { skim(output_df) }
    })
  })
  
} 
shinyApp(ui = ui, server = server)