#################################################################################
###############                 © BAEDA Lab, 2020                 ###############
#################################################################################

body <- dashboardBody(
  # shinyDashboardThemes(theme = "blue_gradient"), # cool dashboard theme
  tabItems(
    # 1st TAB "Welcome" ----------------------------------------------------------------------
    tabItem(tabName = "welcome", includeMarkdown("Welcome.md")), # END # 1st TAB "Welcome"

    # 3rd (SUB)TAB "Manage" ----------------------------------------------------------------------
    tabItem(tabName = "manage",
            fluidRow( 
              box( width = 3,  # INPUT BOX
                   selectInput("type", "Load data of type:",c("None", "csv")),
                   conditionalPanel("input.type == 'csv'",
                                    checkboxInput("header", "Header", value = TRUE),
                                    column(width = 6, selectInput("separator", "Separator",c(",",";")) ),
                                    column(width = 6, selectInput("decimal", "Decimal",c(".",",")) ),
                                    fileInput("file","Upload CSV files", multiple = TRUE, 
                                              accept = c("text/csv","text/comma-separated-values,text/plain",".csv")
                                    ),
                                    radioButtons("display_buttons","Display:", c("info","str", "summary"), inline=T)
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
              ), # END INPUT BOX
              box( # OUTPUT BOX
                width = 9,
                DT::dataTableOutput("dataframe_table") # see server function
              )  # END OUTPUT BOX
            )
    ), # 4th (SUB)TAB "View"
    
    
    # 5th (SUB)TAB "Visualize" ----------------------------------------------------------------------
    tabItem(tabName = "visualize",
            fluidRow( 
              box( width = 3,# INPUT BOX
                   selectInput("chart", label = "Chart Type:", choices = c("None", "Histogram")),
                   conditionalPanel("input.chart == 'Histogram'",
                                    uiOutput("inBoxHistogram")
                   )
              ), # END INPUT BOX
              box(width = 9, # OUTPUT BOX
                  conditionalPanel("input.chart == 'Histogram'",
                                   plotOutput("outBoxHistogram")
                  )
              )  # END OUTPUT BOX
            )
    ) # END 5th (SUB)TAB "Visualize"
  )
)