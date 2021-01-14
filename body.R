#################################################################################
###############            Copyright © BAEDA Lab 2020             ###############
###############                     -------                       ###############
###############                  Roberto Chiosa                   ###############
###############             roberto.chiosa@polito.it              ###############
#################################################################################

chartTypes <- c("","Carpet","Line Plot","Scatter Plot", "Box Plot","Histogram") # add here more chart types as we go on adding
loadingGifColor <- "#0dc5c1"
body <- dashboardBody(
  useShinyalert(),                                   # use html popups
  shinyFeedback::useShinyFeedback(),                 # use html feedbacks
  tags$style(".fa-plus {color:green}"),              # change plus icon color
  tags$style(".fa-calendar-alt {color:green}"),      # change calendar icon color
  tags$style(".fa-backspace {color:red}"),           # change backspace icon color
  # shinyDashboardThemes(theme = "purple_gradient"), # cool dashboard theme
  
  load_file_modal(),      # modal dialog window that permits the input of file
  tabItems(
    ###### TAB "Manage" ----------------------------------------------------------------------
    tabItem(tabName = "manage",
            fluidRow( 
              box( title = "Table options", width = 4, manage_inbox() ), # INPUT BOX
              box( width = 8, 
                   fluidRow(
                     valueBoxOutput("valueBox1", width = 6), 
                     valueBoxOutput("valueBox2", width = 6)
                   ),
                   fluidRow(
                     column(width = 12, 
                            DTOutput("dataframe_table") %>% withSpinner(color = loadingGifColor) 
                     )
                   )
              ) # end box
            )
    ),
    ###### TAB "Visualize" ----------------------------------------------------------------------
    tabItem(tabName = "visualize",
            fluidRow( 
              # INPUT BOX
              box( title = "Chart options", width = 4,
                   selectInput("chart", label = "Chart Type:", choices = chartTypes,
                               selected = NULL),
                   helpText("Note: We require the column Date_Time, Date, Year, Month, min_dec, Hours. If an error displays add them by clicking the button \"Add calendar variables\" in the sidebar."),
                   conditionalPanel("input.chart == 'Histogram'", uiOutput("inBoxHistogram")), # Histogram
                   conditionalPanel("input.chart == 'Line Plot'", uiOutput("inBoxLineplot")), # Line Plot
                   conditionalPanel("input.chart == 'Scatter Plot'", uiOutput("inBoxScatterplot")), # Scatter Plot
                   conditionalPanel("input.chart == 'Box Plot'", uiOutput("inBoxBoxplot")), # Boxplot
                   conditionalPanel("input.chart == 'Carpet'", uiOutput("inBoxCarpet")), # Carpet
                   # actionButton("plotButton", "Plot", width = '100%', style = "color: #fff; background-color: red; border-color: #red"),
                   downloadButton('downloadplotButton', 'Download', style = "width:100%;"),
              ), # END INPUT BOX
              box(width = 8, # OUTPUT BOX
                  conditionalPanel("input.chart == 'Histogram'", plotlyOutput("outBoxHistogram", height = "600px") %>% withSpinner(color = loadingGifColor)), # Histogram
                  conditionalPanel("input.chart == 'Line Plot'", plotlyOutput("outBoxLineplot", height = "600px") %>% withSpinner(color = loadingGifColor)), # Line Plot
                  conditionalPanel("input.chart == 'Scatter Plot'", plotOutput("outBoxScatterplot", height = "600px") %>% withSpinner(color = loadingGifColor)), # Scatter Plot
                  conditionalPanel("input.chart == 'Box Plot'", plotOutput("outBoxBoxplot", height = "600px") %>% withSpinner(color = loadingGifColor)), # Boxplot
                  conditionalPanel("input.chart == 'Carpet'", plotlyOutput("outBoxCarpet", height = "600px") %>% withSpinner(color = loadingGifColor)), # Carpet
              ), # END OUTPUT BOX
            )
    ), # END 5th (SUB)TAB "Visualize"
    
    
    ###### TAB "Advanced" ----------------------------------------------------------------------
    tabItem(tabName = "clustering",
            fluidRow( 
              box( title = "Clustering options", width = 4, 
                   helpText("Note 1: In this section we perform a daily load profile clustering. It is not intended to be a generical clustering process."),
                   helpText("Note 2: We require the column Date_Time, Date, Year, Month, min_dec, Hours. If an error displays add them by clicking the button \"Add calendar variables\" in the sidebar."),
                   uiOutput("clustering_inbox"),
                   uiOutput("clustering_inbox_postprocessing")),
              box( width = 8, 
                   plotOutput("out_clustering_preview", height = "400px") %>% withSpinner(color = loadingGifColor),
                   plotOutput("out_clustering_dendogram", height = "400px")
              ) # end box
            )
    ),
    
    # documentation
    tabItem(tabName = "help", includeMarkdown("docs/wiki/complete.md") )
  ),

  # © e link alle istituzioni
  absolutePanel(
    HTML(
      "© 2020 
      <a href='https://www.denerg.polito.it/'>BAEDA Lab</a> | 
      <a href='https://www.denerg.polito.it/'>Dipartimento Energia</a> | 
      <a href='https://www.denerg.polito.it/'>Politecnico di Torino</a>
      "
    ),
    bottom = "1%", 
    right = "1%", 
    fixed = TRUE,
    color = "#3c4c54"
  )
)
