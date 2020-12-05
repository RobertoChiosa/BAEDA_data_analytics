#################################################################################
###############            Copyright © BAEDA Lab 2020             ###############
#################################################################################

selection_dataframe <- function() {
  tagList(
    # select the dataset you want to use for the analysis - default none
    tags$div(title = "This is the actual dataframe used for your analysis",
             selectInput("dataframe", label = "Dataframe:", choices = c("None"))
    )
  )
}

manage_inbox <- function() {
  tagList(
    # type of files that can be loaded
    selectInput("type", "Chose file type:", c("", "csv", "xls"), selected = NULL),
    # the user wants to load a csv file
    conditionalPanel("input.type == 'csv'",
                     column(width = 6, checkboxInput("header", "Header?", value = TRUE) ),
                     column(width = 6, checkboxInput("strAsFact", "String as Factor?", value = TRUE) ),
                     column(width = 6, selectInput("separator", "Separator;:", c("Comma (,)" = ",", "Semicolon (;)" = ";")) ),
                     column(width = 6, selectInput("decimal", "Decimal:", c("Point (.)" = ".","Comma (,)" = ",")) ),
                     numericInput("decimalDigits", label = "Decimal digits:", value = 2, step = 1, min = 0), # add condition on variable
                     checkboxInput("timestamp", "Timestamp column?", value = TRUE),
                     conditionalPanel("input.timestamp == true",
                                      selectInput("timezone", "Timezone:", choices = OlsonNames(), selected = Sys.timezone()),
                     )
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
  )
}

view_inbox <- function() {
  tagList(
    # Rename column ----------------------------------------------------------------------
    awesomeCheckbox("modifyColumns_chackbox", "Rename column:", value = FALSE),
    conditionalPanel("input.modifyColumns_chackbox == true", # if we want to rename 
                     uiOutput("modifyColumns"),
    ),
    # Pivot ----------------------------------------------------------------------
    awesomeCheckbox("pivotTable_chackbox", "Pivot table:", value = FALSE),
    conditionalPanel("input.pivotTable_chackbox == true", # if we want to rename 
                     uiOutput("pivotTable"),
                     actionButton("pivotTableButton", "Pivot!", width = '100%'),
                     br()
    ),
    # Rename dataframe ----------------------------------------------------------------------
    awesomeCheckbox("new_dataframe_name_chackbox", "Save current dataframe:", value = FALSE),
    conditionalPanel("input.new_dataframe_name_chackbox == true", # if we want to rename 
                     searchInput(inputId = "new_dataframe_name", label = NULL, 
                                 placeholder = "New name..", 
                                 value = NULL, # initial value
                                 btnSearch = icon("plus"), btnReset = icon("backspace"), # icons
                                 width = "100%")
    ),
    # Download ----------------------------------------------------------------------
    downloadButton("download_filtered", "Download Filtered Data", style = "width:100%;"),   
  )
}

