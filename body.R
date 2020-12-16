#################################################################################
###############            Copyright © BAEDA Lab 2020             ###############
#################################################################################

chartTypes <- c("","Box Plot","Histogram") # add here more chart types as we go on adding
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
                   conditionalPanel("input.chart == 'Histogram'", uiOutput("inBoxHistogram")), # histogram
                   conditionalPanel("input.chart == 'Box Plot'", uiOutput("inBoxBoxplot")), # Boxplot
                   actionButton("plotButton", "Plot", width = '100%', style = "color: #fff; background-color: red; border-color: #red"),
                   downloadButton('downloadplotButton', 'Download', style = "width:100%;"),
              ), # END INPUT BOX
              box(width = 8, # OUTPUT BOX
                  conditionalPanel("input.chart == 'Histogram'", plotOutput("outBoxHistogram") %>% withSpinner(color = loadingGifColor)), # histogram
                  conditionalPanel("input.chart == 'Box Plot'", plotOutput("outBoxBoxplot") %>% withSpinner(color = loadingGifColor)), # Boxplot
                  
              ), # END OUTPUT BOX
            )
    ), # END 5th (SUB)TAB "Visualize"
    
    # documentation
    tabItem(tabName = "help", includeMarkdown("docs/wiki/complete.md")
    )
  )
)