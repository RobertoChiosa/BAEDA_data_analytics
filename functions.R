#################################################################################
###############            Copyright © BAEDA Lab 2020             ###############
###############                     -------                       ###############
###############                  Roberto Chiosa                   ###############
###############             roberto.chiosa@polito.it              ###############
#################################################################################

###### SIDEBAR functions ----------------------------------------------------------------------
# UI function for the sidebar dropdown
selection_dataframe <- function() {
  tagList(
    # select the dataset you want to use for the analysis - default none
    tags$div(title = "This is the actual dataframe used for your analysis",
             selectInput("dataframe", label = "Dataframe:", choices = c("None")),
             actionButton("upload", "  Upload a new dataframe", icon = icon("plus"), width = "87%"),
             actionButton("add_calendar_columns", "  Add calendar variables", icon = icon("calendar-alt"), width = "87%"),
    ),
    #column(9, style="display:inline-block; padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:0px; margin-bottom: 0px;", selectInput("dataframe", label = NULL, choices = c("None"))  ),
    #column(3, style="display:inline-block; padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:0px;margin-bottom: 0px;", actionButton("tt", "+")),
  )
}

###### ADD CALENDARVARIABLES function ----------------------------------------------------------------------
# function that performs the adding of calendar variables
add_calendar_variables <- function(timestamp, timezone, df) {
  
  # notification of process
  id <- showNotification("Calculating...", duration = NULL, closeButton = FALSE, type = "message")
  on.exit(removeNotification(id), add = TRUE)
  
  # allowed time formats - update accordingly
  dateFormats <- c("%Y-%m-%d %H:%M:%S",       # ISO date format
                   "%m/%d %H:%M:%S",          # energy+
                   "%m/%d  %H:%M:%S",         # energy+ 2 spazi
                   " %m/%d    %H:%M:%S",      # energy+ 2 spazi +1
                   "%d/%m/%y %H:%M")          # accepted date formats
  
  if (timestamp == T) {
    # function to automatically find date column
    coldate <- sapply(df,   function(x) !all(is.na(as.Date(as.character(x), format = dateFormats))))
    
    # in no timestamp column can be found notify the user
    if (any(coldate) == FALSE) {
      df_out <- df
      # warning notification
      shinyalert(title = "No timestamp column found!",
                 paste("It was not possible to add the desired columns"),
                 type = "error",
                 closeOnEsc = TRUE,
                 closeOnClickOutside = TRUE,
                 html = TRUE
      )
    } else if (sum(coldate[TRUE])>1)  { # if timestamp columns found create date time columns
      df_out <- df
      # warning notification
      shinyalert(title = "Multiple timestamp column found!",
                 paste("It was not possible to add the desired columns"),
                 type = "error",
                 closeOnEsc = TRUE,
                 closeOnClickOutside = TRUE,
                 html = TRUE
      )
    }else{
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
        )%>% na.omit()
      # success notification
      shinyalert(title = "Columns successfully added",
                 text = paste("You can find the updated dataframe in the dataframe dropdown"),
                 type = "success",
                 closeOnEsc = TRUE,
                 closeOnClickOutside = TRUE,
                 html = TRUE
      )
    }
  } else { # no timestamp check box selected
    df_out <- df
    shinyalert(title = "No timestamp column found!",
               paste("You didn't check the timestamp checkbox upon upload"),
               type = "warning",
               closeOnEsc = TRUE,
               closeOnClickOutside = TRUE,
               html = TRUE
    )
  }
  return(df_out)
}

###### MODAL ----------------------------------------------------------------------
# function that renders the modal UI
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
                 selectInput("type", "Chose the type of file:", 
                             c("", "Comma-separated values (.csv)" = "csv","R object (.rds)"="rds"), selected = NULL),
                 
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

###### "MANAGE" TAB UI  ----------------------------------------------------------------------
# assembles different server UI in the rigth way
manage_inbox <- function() {
  tagList(
    uiOutput("keepColumns"),
    # 2.7) Rename column ----------------------------------------------------------------------
    awesomeCheckbox("modifyColumns_chackbox", "Rename column", value = FALSE),
    conditionalPanel("input.modifyColumns_chackbox == true", # if we want to rename 
                     uiOutput("modifyColumns"),
    ),
    # 2.8) Add column ----------------------------------------------------------------------
    awesomeCheckbox("addColumns_chackbox", "Add column", value = FALSE),
    conditionalPanel("input.addColumns_chackbox == true", # if we want to rename 
                     uiOutput("addColumn"),
    ),
    # 2.9) Pivot table ----------------------------------------------------------------------
    awesomeCheckbox("pivotTable_chackbox", "Pivot table", value = FALSE),
    conditionalPanel("input.pivotTable_chackbox == true", # if we want to rename 
                     uiOutput("pivotTable")
    ),
    # 2.4) Rename dataframe ----------------------------------------------------------------------
    searchInput(inputId = "new_dataframe_name", label = "Save current dataframe", 
                placeholder = "New name..", 
                value = NULL, # initial value
                btnSearch = icon("plus"), btnReset = icon("backspace"), # icons
                width = "100%"),
    # 2.10) Download filtered dataframe ----------------------------------------------------------------------
    downloadButton("download_filtered", "Download Filtered Dataframe (as csv)", style = "width:100%;"),   
  )
}
