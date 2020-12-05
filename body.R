#################################################################################
###############            Copyright © BAEDA Lab 2020             ###############
#################################################################################
chartTypes <- c("", "Time Series") # add here more chart types as we go on adding
loadingGifColor <- "#0dc5c1"
body <- dashboardBody(
  useShinyalert(), # popups
  shinyFeedback::useShinyFeedback(),              # use feedbacks
  tags$style(".fa-plus {color:green}"),           # change plus icon color
  tags$style(".fa-backspace {color:red}"),        # change backspace icon color
  # shinyDashboardThemes(theme = "purple_gradient"), # cool dashboard theme
  tabItems(
    # # 1st TAB "Welcome" ----------------------------------------------------------------------
    # tabItem(tabName = "welcome", includeMarkdown("README.md")), # END # 1st TAB "Welcome"
    
    # 3rd (SUB)TAB "Manage" ----------------------------------------------------------------------
    tabItem(tabName = "manage",
            fluidRow(
              # INPUT BOX
              box( title = "Upload a new file", width = 4, manage_inbox() ),
              # OUTPUT BOX
              box( title = "File preview",  width = 8, 
                radioButtons("display_buttons","", c("str", "summary","skim"), inline = T),
                verbatimTextOutput("manage_outputBox") # see server function
              ) 
            )
    ), # END 3rd (SUB)TAB "Manage"
    
    # 4th (SUB)TAB "View" ----------------------------------------------------------------------
    tabItem(tabName = "view", # 2nd Subtab "View"
            fluidRow( 
              box( title = "Table options", width = 4, view_inbox() ), # INPUT BOX
              box( width = 8, dataTableOutput("dataframe_table") %>% withSpinner(color = loadingGifColor) )  # OUTPUT BOX
            )
    ), # END 4th (SUB)TAB "View"
    
    # 5th (SUB)TAB "Visualize" ----------------------------------------------------------------------
    tabItem(tabName = "visualize",
            fluidRow( 
              # INPUT BOX
              box( title = "Chart options", width = 4,
                   selectInput("chart", label = "Chart Type:", choices = chartTypes,
                               selected = NULL),
                   conditionalPanel("input.chart == 'Time Series'", uiOutput("inBoxTimeSeries")), # 
                   conditionalPanel("input.chart == 'Histogram'", uiOutput("inBoxHistogram")), # histogram
                   conditionalPanel("input.chart == 'Bar Plot'", uiOutput("inBoxBar")), # bar plot
                   conditionalPanel("input.chart == 'Carpet'", uiOutput("inBoxCarpet")), # carpet plot
                   conditionalPanel("input.chart == 'Scatter'", uiOutput("inBoxScatter")), # scatter plot
                   actionButton("plotButton", "Plot", width = '100%', style = "color: #fff; background-color: red; border-color: #red"),
                   downloadButton('downloadplotButton', 'Download', style = "width:100%;"),
              ), # END INPUT BOX
              box(width = 8, # OUTPUT BOX
                  conditionalPanel("input.chart == 'Time Series'", highchartOutput("outBoxTimeSeries")), # histogram
                  conditionalPanel("input.chart == 'Histogram'", plotOutput("outBoxHistogram") %>% withSpinner(color = loadingGifColor)), # histogram
                  conditionalPanel("input.chart == 'Bar Plot'", plotOutput("outBoxBar") %>% withSpinner(color = loadingGifColor)), # bar plot
                  conditionalPanel("input.chart == 'Carpet'", plotOutput("outBoxCarpet") %>% withSpinner(color = loadingGifColor)), # carpet plot
                  conditionalPanel("input.chart == 'Scatter'", plotOutput("outBoxScatter")) # scatter plot
                  
              ), # END OUTPUT BOX
            )
    ) # END 5th (SUB)TAB "Visualize"
  )
)