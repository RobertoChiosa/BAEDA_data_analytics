#################################################################################
###############            Copyright © BAEDA Lab 2020             ###############
#################################################################################

chartTypes <- c("","Carpet","Line Plot", "Box Plot","Histogram") # add here more chart types as we go on adding
loadingGifColor <- "#0dc5c1"
body <- dashboardBody(
  useShinyalert(),                                   # use html popups
  shinyFeedback::useShinyFeedback(),                 # use html feedbacks
  tags$style(".fa-plus {color:green}"),              # change plus icon color
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
                   helpText("Note: we require the column Date_Time, Year, Month, min_dec, Hours. If an error displays please go back in the manage tab and select those columns."),
                   conditionalPanel("input.chart == 'Histogram'", uiOutput("inBoxHistogram")), # histogram
                   conditionalPanel("input.chart == 'Line Plot'", uiOutput("inBoxLineplot")), # histogram
                   conditionalPanel("input.chart == 'Box Plot'", uiOutput("inBoxBoxplot")), # Boxplot
                   conditionalPanel("input.chart == 'Carpet'", uiOutput("inBoxCarpet")), # Carpet
                   # actionButton("plotButton", "Plot", width = '100%', style = "color: #fff; background-color: red; border-color: #red"),
                   downloadButton('downloadplotButton', 'Download', style = "width:100%;"),
              ), # END INPUT BOX
              box(width = 8, # OUTPUT BOX
                  conditionalPanel("input.chart == 'Histogram'", plotlyOutput("outBoxHistogram", height = "600px") %>% withSpinner(color = loadingGifColor)), # histogram
                  conditionalPanel("input.chart == 'Line Plot'", plotlyOutput("outBoxLineplot", height = "600px") %>% withSpinner(color = loadingGifColor)), # histogram
                  conditionalPanel("input.chart == 'Box Plot'", plotOutput("outBoxBoxplot", height = "600px") %>% withSpinner(color = loadingGifColor)), # Boxplot
                  conditionalPanel("input.chart == 'Carpet'", plotlyOutput("outBoxCarpet", height = "600px") %>% withSpinner(color = loadingGifColor)), # Carpet
              ), # END OUTPUT BOX
            )
    ), # END 5th (SUB)TAB "Visualize"
    
    
    ###### TAB "Advanced" ----------------------------------------------------------------------
    tabItem(tabName = "clustering",
            fluidRow( 
              box( title = "Clustering options", width = 4, uiOutput("clustering_inbox") ), # INPUT BOX
              box( width = 8, 
                   plotOutput("out_clustering_preview", height = "400px") %>% withSpinner(color = loadingGifColor),
                   plotOutput("out_clustering_dendogram", height = "400px") %>% withSpinner(color = loadingGifColor)
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
