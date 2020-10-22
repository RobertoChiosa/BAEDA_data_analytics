#################################################################################
###############                 © BAEDA Lab, 2020                 ###############
#################################################################################

## app.R ##
cat("\014")             # clears the console
rm(list = ls())         # remove all variables of the workspace
source("packages.R")    # load necessary packages
source("functions.R")   # load user defined functions
source("header.R")      # load header
source("sidebar.R")     # load sidebar
source("body.R")        # load body

# GLOBAL VARIABLES
myData<-NULL
TimeColumnName="Date/Time"
OneDayStep <-96
InterestingVaraible<-NULL
imageContent <-NULL
columnNames<-NULL
values<-NULL

# USER INTERFACE ----------------------------------------------------------------------
ui <- dashboardPage(skin = "black",
                    header,             # loaded from scripts
                    sidebar,            # loaded from scripts
                    body                # loaded from scripts
)

# SERVER FUNCTION ----------------------------------------------------------------------
server <- function(input, output) {
  ########################################################################
  # SIDEBAR FUNCTIONS
  ########################################################################
  output$selected_sidebar_tab <- renderText({
    paste("You've selected:", input$tabs)
  })
  
  ### 1st Subtab "Manage" ############################# 
  # OPTION TO READ LARGER FILES
  options(shiny.maxRequestSize = 100*1024^2)
  
  observeEvent(input$file,{
    # SELECTION OF DATASET COLUMN
    inFile <- input$file
    myData <- if(is.null(inFile)){NULL}else{
        read.csv(inFile$datapath, header = input$header, sep = input$separator, dec = input$decomal, check.names = FALSE)}
    
    output$manage_outputBox <- renderPrint({
      if (input$display_buttons == "str") { str(myData) } 
      else if (input$display_buttons == "summary"){ summary(myData) }
      else if (input$display_buttons == "info"){ file.info(inFile$datapath) }
    })
    
    output$dataframe_table <- DT::renderDataTable(
      DT::datatable(
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, it will be a data frame with 'name',
        # 'size', 'type', and 'datapath' columns. The 'datapath'
        # column will contain the local filenames where the data can
        # be found.
        myData,
        filter = 'top', extensions = c('Buttons', 'Scroller'),
        options = list(scrollX = 500,
                       autoWidth = TRUE,
                       deferRender = TRUE,
                       scroller = TRUE,
                       paging = TRUE,
                       pageLength = 10,
                       # buttons = list('excel',
                       #                list(extend = 'colvis', 
                       #                     targets = 0, 
                       #                     visible = FALSE)),
                       dom = 'lBfrtip',
                       fixedColumns = TRUE), 
        rownames = FALSE
      )
    )
    
    # histogram functions
    output$inBoxHistogram <- renderUI({
      tagList(
        selectInput("variable", label = "Variable:", choices = colnames(dplyr::select_if(myData, is.numeric))), # chose numerical variable
        sliderInput(inputId = "bins", label = "Number of bins:", min = 1, max = 100, value = 30),
        selectInput("fillvariable", label = "Fill Variable:", choices = c("None", colnames(dplyr::select_if(myData, is.factor))) ),
        selectInput("facetvariable", label = "Facet Variable:", choices = c("None", colnames(dplyr::select_if(myData, is.factor))) ),
        
        column("Style", width = 6, 
               checkboxInput("checkbox_flip", label = "Flip", value = FALSE), 
               checkboxInput("checkbox_density", label = "Density", value = FALSE) 
        ),
        column("Scale", width = 6, 
               checkboxInput("checkbox_logx", label = "Log-X", value = FALSE),
               checkboxInput("checkbox_logy", label = "Log-Y", value = FALSE)
        ),
      )
    })
    
    output$outBoxHistogram <- renderPlot({
      
      #shiny::req(input$variable)
      #validate(need(input$variable %in% colnames(myData),message="Incorrect column name."))
      plot <- ggplot(data = myData, 
                     mapping =  aes(x = myData[,input$variable],
                                    fill = if (input$fillvariable == "None") {NULL} else {myData[,input$fillvariable]} ),
      ) + theme_bw()
      
      # densità
      if (input$"checkbox_density" == TRUE) {
        plot <- plot + geom_density(aes(y = ..density.., 
                                        fill = if (input$fillvariable == "None") {NULL} else {myData[,input$fillvariable]}), 
                                    alpha = 0.4,
                                    bw = input$bins/5,
                                    na.rm = TRUE)  # aggiungo distribuzione
      } else{plot <- plot + geom_histogram(bins = input$bins,
                                           na.rm = TRUE)} # 
      # flip
      if (input$"checkbox_flip" == TRUE) {plot <- plot + coord_flip()}
      # tema
      plot <- plot + labs( x = input$variable, fill = input$fillvariable ) + theme(legend.position = "bottom")
      # add wrap
      if (input$facetvariable == "None") {NULL} else {plot <- plot + facet_wrap(~ myData[,input$facetvariable])} 
      # log
      # flip
      if (input$"checkbox_logx" == TRUE) {plot <- plot + scale_x_continuous(trans = 'log10') }
      if (input$"checkbox_logy" == TRUE) {plot <- plot + scale_y_continuous(trans = 'log10') }
      
      plot
    })
  },
  ignoreInit = TRUE)
}


shinyApp(ui, server)
