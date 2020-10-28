library(shiny)
if (file.exists("File_Format.rds")) file.remove("File_Format.rds")
do.call(file.remove, list(list.files("Selected_Files", full.names = TRUE)))

ui <- fluidPage(
  
  # tableOutput("contents"),
  sidebarPanel(
    fileInput("file1", "Choose CSV File",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    # Horizontal line ----
    tags$hr(),
    
    # Input: Checkbox if file has header ----
    checkboxInput("header", "Header", TRUE),
    
    # Input: Select separator ----
    radioButtons("sep", "Separator",
                 choices = c(Comma = ",",
                             Semicolon = ";",
                             Tab = "\t"),
                 selected = ","),
    
    # Input: Select quotes ----
    radioButtons("quote", "Quote",
                 choices = c(None = "",
                             "Double Quote" = '"',
                             "Single Quote" = "'"),
                 selected = '"'),
    
    # Horizontal line ----
    tags$hr(),
    
    # Upload Button
    actionButton("uploadId", "Upload")
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    
    # # Output: Data file ----
    
    uiOutput("manage"),
    
    # Input: Select number of rows to display ----
    uiOutput("select"),
    
    # Display Button
    actionButton("displayid", "Display"),
    
    
    tableOutput("contents")
    
    
  )
)


########### Server ###########

server <- function(input, output, session) {
  
  
  # Copy uploaded files to local folder
  observeEvent(input$uploadId,{
    if (is.null(input$file1) ) {    return(NULL)  }  
    file.copy(from = input$file1$datapath, to =  paste0('Selected_Files/',input$file1$name )  )
    df <- list(file = input$file1$name , header= input$header,
               sep = input$sep,dec = input$dec,
               quote = input$quote,
               index = input$uploadId)
    if(input$uploadId > 1){
      old_df <- readRDS("File_Format.rds")
      df <- sapply(names(old_df),function(n){c(old_df[[n]],df[[n]])},simplify=FALSE)
    }
    saveRDS(df, "File_Format.rds")
    
  })
  
  # Load all the uplaoded files to a list
  datasetlist <- eventReactive(input$uploadId,{
    # Selected_Files <- list.files("Selected_Files/")
    File_Format <- readRDS("File_Format.rds")
    datalist <- list()
    datalist <- lapply(1:length(File_Format[[1]]), function(d) read.csv(paste0("Selected_Files/",File_Format$file[d] ),
                                                                        header = File_Format$header[d],
                                                                        sep = File_Format$sep[d],
                                                                        dec = File_Format$dec[d],
                                                                        quote = File_Format$quote[d]))
    names(datalist) <- paste(File_Format$index, File_Format$file,sep = ". ")
    return(datalist)
  })
  
  output$manage <- renderUI({
    data <- datasetlist()
    selectInput("dataset", "Dataset", choices = names(data), selected = names(data))
  })
  
  output$select <- renderUI({
    data <- datasetlist()
    radioButtons("disp", "Display", choices = c(Head = "head",All = "all"),
                 selected = "head")
  })
  
  # Display Selected File
  observeEvent(input$displayid, {
    output$contents <- renderTable({
      
      data <- datasetlist()
      sub_df <- data[[paste0(input$dataset)]]
      if (isolate(input$disp == "head")) {
        return(head(sub_df))
      }
      else {
        return(sub_df)
      }
    })
  })
  
}
shinyApp(ui, server)