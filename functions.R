#################################################################################
###############            Copyright © BAEDA Lab 2020             ###############
#################################################################################

###### SIDEBAR functions ----------------------------------------------------------------------
selection_dataframe <- function() {
  tagList(
    # select the dataset you want to use for the analysis - default none
    tags$div(title = "This is the actual dataframe used for your analysis",
             selectInput("dataframe", label = "Dataframe:", choices = c("None")),
             actionButton("upload", "Upload a new dataframe ...", icon = icon("plus"), width = "87%"),
             #actionButton("upload", NULL, icon = icon("calendar-alt"), width = "100%")
    ),
    #column(9, style="display:inline-block; padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:0px; margin-bottom: 0px;", selectInput("dataframe", label = NULL, choices = c("None"))  ),
    #column(3, style="display:inline-block; padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:0px;margin-bottom: 0px;", actionButton("tt", "+")),
  )
}

###### ADD CALENDARVARIABLES function ----------------------------------------------------------------------


add_calendar_variables <- function(nome, df, timezone, dateFormats) {
  
  # function to automatically find date column
  coldate <- sapply(df,   function(x) !all(is.na(as.Date(as.character(x), format = dateFormats))))
  
  # in no timestamp column can be found notify the user
  if (any(coldate) == FALSE) {
    # warning notification
    shinyalert(title = "No timestamp column found!", 
               paste("However, you can find <b>", nome, "</b> in the dataframe dropdown"),
               type = "warning",
               closeOnEsc = TRUE,
               closeOnClickOutside = TRUE,
               html = TRUE
    )
    df_out <- df
  } else { # if timestamp columns found create date time columns
    df_out <- df %>%
      mutate(
        Date_Time = as.POSIXct(df[,coldate] , format = "%Y-%m-%d %H:%M:%S" , tz = timezone), # depend on selected timezone
        Date = as.Date(Date_Time), # week start on monday
        Week_Day = wday(Date_Time, label = TRUE, week_start = getOption("lubridate.week.start", 1)), # week start on monday
        Month = month(Date_Time, label = TRUE), # ordered factor
        Month_Day = mday(Date_Time), # numeric
        Year = as.ordered(year(Date_Time)), # ordered factor
        Year_Day = mday(Date_Time), # numeric
        Hour = hour(Date_Time), # numeric
        Minute = minute(Date_Time), # numeric
        min_dec = as.numeric(paste(Hour, Minute*100/60, sep = ".")) # numeric
      ) 
    # success notification
    shinyalert(title = "Dataframe successfully added",
               text = paste("You can find <b>", nome, "</b> in the dataframe dropdown <br> We created some useful new variables..."), 
               type = "success",
               closeOnEsc = TRUE,
               closeOnClickOutside = TRUE,
               html = TRUE
    )
  }
  return(df_out)
}

###### MODAL ----------------------------------------------------------------------
load_file_modal <- function(failed = FALSE) {
  bsModal(id = 'startupModal',
          trigger = 'open',
          size = 'medium',
          tags$head(tags$style("#startupModal .modal-footer{ display:none}")), # removes footer default
          title = 
            HTML('
            <p align="center">
               <a href="https://www.researchgate.net/lab/Building-Automation-and-Energy-Data-Analytics-Lab-Alfonso-Capozzoli">
               <img src="BAEDA-logo-dashboard.png" alt="Logo" height="80">
               </a>
            <h3 align="center"> <i> Student Version </i> </h3>
            <p align="center">
               Now you can perform advanced data analytics tasks on your energy data.
            </p>
            </p>
            <br /> 
            '),
          column(width = 12, align = 'center', 
                 # type of files that can be loaded
                 selectInput("type", "Chose the type of file:", c("", "csv", "rds", "xls"), selected = NULL),
                 
                 # the user wants to load a csv file
                 conditionalPanel("input.type == 'csv'",
                                  column(width = 6, selectInput("separator", "Separator:", c("Comma (,)" = ",", "Semicolon (;)" = ";")) ),
                                  column(width = 6, selectInput("decimal", "Decimal:", c("Point (.)" = ".","Comma (,)" = ",")) ),
                                  column(width = 4, checkboxInput("header", "Header?", value = TRUE) ),
                                  column(width = 4, checkboxInput("timestamp_csv", "Timestamp column?", value = TRUE) ),
                                  column(width = 4, checkboxInput("strAsFact", "String as Factor?", value = TRUE) ),
                                  conditionalPanel("input.timestamp_csv == true", 
                                                   selectInput("timezone_csv", "Timezone:", choices = OlsonNames(), selected = "Europe/Rome"),
                                  ),
                 ),
                 
                 # the user wants to load a rds file
                 conditionalPanel("input.type == 'rds'",
                                  checkboxInput("timestamp_rds", "Timestamp column?", value = TRUE),
                                  conditionalPanel("input.timestamp_rds == true", 
                                                   selectInput("timezone_rds", "Timezone:", choices = OlsonNames(), selected = "Europe/Rome"),
                                  ),
                 ),
                 
                 # the user wants to load a xlm file
                 conditionalPanel("input.type == 'xls'",
                                  h2("WARNING: file still not supported")
                 ), 
                 
                 conditionalPanel("input.type != ''",
                                  fileInput("file",paste("Upload file:") )
                 ),
                 
                 
          ),
          
          # dont' know why but i have to create a footer with a transparent action button
          footer = tagList(column(12, align = "center",  modalButton("Cancel")),
                           actionButton("upload", "", style = "color: #ffffff; background-color: #ffffff; border-color: #ffffff")
          )
  )
}

###### TAB "Manage" ----------------------------------------------------------------------
manage_inbox <- function() {
  tagList(
    uiOutput("keepColumns"),
    # Rename column ----------------------------------------------------------------------
    awesomeCheckbox("modifyColumns_chackbox", "Rename column?", value = FALSE),
    conditionalPanel("input.modifyColumns_chackbox == true", # if we want to rename 
                     uiOutput("modifyColumns"),
    ),
    # Add column ----------------------------------------------------------------------
    awesomeCheckbox("addColumns_chackbox", "Add column?", value = FALSE),
    conditionalPanel("input.addColumns_chackbox == true", # if we want to rename 
                     uiOutput("addColumn"),
    ),
    # Pivot ----------------------------------------------------------------------
    awesomeCheckbox("pivotTable_chackbox", "Pivot table?", value = FALSE),
    conditionalPanel("input.pivotTable_chackbox == true", # if we want to rename 
                     uiOutput("pivotTable")
    ),
    # Rename dataframe ----------------------------------------------------------------------
    awesomeCheckbox("new_dataframe_name_chackbox", "Save current dataframe?", value = FALSE),
    conditionalPanel("input.new_dataframe_name_chackbox == true", # if we want to rename 
                     searchInput(inputId = "new_dataframe_name", label = NULL, 
                                 placeholder = "New name..", 
                                 value = NULL, # initial value
                                 btnSearch = icon("plus"), btnReset = icon("backspace"), # icons
                                 width = "100%")
    ),
    # Download ----------------------------------------------------------------------
    downloadButton("download_filtered", "Download Filtered Dataframe (as csv)", style = "width:100%;"),   
  )
}
