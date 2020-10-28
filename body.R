#################################################################################
###############            Copyright © BAEDA Lab 2020             ###############
#################################################################################
chartTypes <- c("", "Bar Plot") # add here more chart types as we go on adding

body <- dashboardBody(
  # shinyDashboardThemes(theme = "purple_gradient"), # cool dashboard theme
  tabItems(
    # 1st TAB "Welcome" ----------------------------------------------------------------------
    tabItem(tabName = "welcome", includeMarkdown("README.md")), # END # 1st TAB "Welcome"
    
    # 3rd (SUB)TAB "Manage" ----------------------------------------------------------------------
    tabItem(tabName = "manage",
            fluidRow( 
              box( width = 3,  # INPUT BOX
                   selectInput("type", "Load data of type:", c("csv"), selected = NULL),
                   conditionalPanel("input.type == 'csv'",
                                    checkboxInput("header", "Header", value = TRUE),
                                    column(width = 6, selectInput("separator", "Separator",c(",",";")) ),
                                    column(width = 6, selectInput("decimal", "Decimal",c(".",",")) ),
                                    fileInput("file","Upload CSV files", multiple = TRUE, 
                                              accept = c("text/csv",
                                                         "text/comma-separated-values,text/plain",
                                                         ".csv")
                                    ),
                                    numericInput("decimalDigits", label = "Decimal digits", value = 2, step = 1, min = 0), # add condition on variable
                                    radioButtons("display_buttons","Display:", c("str", "summary","info"), inline = T)
                                    
                   )
              ), # END INPUT BOX
              box( # OUTPUT BOX
                width = 9, 
                verbatimTextOutput("manage_outputBox") # see server function
              )  # END OUTPUT BOX 
            )
    ), # END 3rd (SUB)TAB "Manage"
    
    # 4th (SUB)TAB "View" ----------------------------------------------------------------------
    tabItem(tabName = "view", # 2nd Subtab "View"
            fluidRow( 
              box( width = 3, # INPUT BOX
                   actionButton("tableUpdateButton", "Update", width = '100%', style = "color: #fff; background-color: red; border-color: #red"),
                   downloadButton('downloadTableButton', 'Download', style = "width:100%;"),

                   uiOutput("modifyColumns"),
                   uiOutput("pivotTable")
              ), # END INPUT BOX
              box( # OUTPUT BOX
                width = 9,
                dataTableOutput("dataframe_table") %>% withSpinner(color = "#0dc5c1"),
              )  # END OUTPUT BOX
            )
    ), # 4th (SUB)TAB "View"
    
    
    # 5th (SUB)TAB "Visualize" ----------------------------------------------------------------------
    tabItem(tabName = "visualize",
            fluidRow( 
              box( width = 3,# INPUT BOX
                   actionButton("plotButton", "Plot", width = '100%', style = "color: #fff; background-color: red; border-color: #red"),
                   downloadButton('downloadplotButton', 'Download', style = "width:100%;"),
                   selectInput("chart", label = "Chart Type:", choices = chartTypes,
                               selected = NULL),
                   conditionalPanel("input.chart == 'Histogram'", uiOutput("inBoxHistogram")), # histogram
                   conditionalPanel("input.chart == 'Bar Plot'", uiOutput("inBoxBar")), # bar plot
                   conditionalPanel("input.chart == 'Carpet'", uiOutput("inBoxCarpet")) # carpet plot
              ), # END INPUT BOX
              box(width = 9, # OUTPUT BOX
                  conditionalPanel("input.chart == 'Histogram'", plotOutput("outBoxHistogram") %>% withSpinner(color = "#0dc5c1")), # histogram
                  conditionalPanel("input.chart == 'Bar Plot'", plotOutput("outBoxBar") %>% withSpinner(color = "#0dc5c1")), # bar plot
                  conditionalPanel("input.chart == 'Carpet'", plotOutput("outBoxCarpet") %>% withSpinner(color = "#0dc5c1")) # bar plot
                  
              ), # END OUTPUT BOX
            )
    ) # END 5th (SUB)TAB "Visualize"
  )
)