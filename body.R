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
  useShinyjs(),                                      # add dependencies to js
  useShinyFeedback(),                                # use html feedbacks
  tags$style(".fa-plus {color:green}"),              # change plus icon color
  tags$style(".fa-calendar-alt {color:green}"),      # change calendar icon color
  tags$style(".fa-backspace {color:red}"),           # change backspace icon color
  # shinyDashboardThemes(theme = "purple_gradient"), # cool dashboard theme
  
  load_file_modal(),      # modal dialog window that permits the input of file
  tabItems(
    ###### TAB "Manage" ----------------------------------------------------------------------
    tabItem(tabName = "manage",
            fluidRow( 
              # INPUT BOX
              box( title = "Table options", width = 4, manage_inbox() ), 
              # OUTPUT BOX
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
              ) 
            )
    ),
    ###### TAB "Visualize" ----------------------------------------------------------------------
    tabItem(tabName = "visualize",
            fluidRow( 
              # INPUT BOX
              box( title = "Chart options", width = 4,
                   selectInput("chart", label = "Chart Type:", choices = chartTypes,selected = NULL),
                   #conditionalPanel("input.chart != ''",  loadingButton("plot_button", "Plot", class = "btn btn-success", loadingLabel = "Plotting...", loadingSpinner = "spinner", style = "width: 100%;")),
                   conditionalPanel("input.chart != ''",  actionButton("plot_button", "Plot", class = "btn-success", icon = icon("chart-bar"), width = "100%")),
                   helpText("Note: We require the column Date_Time, Date, Year, Month, min_dec, Hours. If an error displays add them by clicking the button \"Add calendar variables\" in the sidebar."),
                   conditionalPanel("input.chart == 'Histogram'",     uiOutput("inBoxHistogram")),    # Histogram
                   conditionalPanel("input.chart == 'Line Plot'",     uiOutput("inBoxLineplot")),     # Line Plot
                   conditionalPanel("input.chart == 'Scatter Plot'",  uiOutput("inBoxScatterplot")),  # Scatter Plot
                   conditionalPanel("input.chart == 'Box Plot'",      uiOutput("inBoxBoxplot")),      # Boxplot
                   conditionalPanel("input.chart == 'Carpet'",        uiOutput("inBoxCarpet")),       # Carpet
                   # actionButton("plotButton", "Plot", width = '100%', style = "color: #fff; background-color: red; border-color: #red"),
                   downloadButton('downloadplotButton', 'Download', style = "width:100%;"),
              ), 
              # OUTPUT BOX
              box(width = 8,
                  conditionalPanel("input.chart == 'Histogram'",    plotlyOutput("outBoxHistogram", height = "600px") %>% withSpinner(color = loadingGifColor)), # Histogram
                  conditionalPanel("input.chart == 'Line Plot'",    plotlyOutput("outBoxLineplot",  height = "600px") %>% withSpinner(color = loadingGifColor)), # Line Plot
                  conditionalPanel("input.chart == 'Scatter Plot'", plotOutput("outBoxScatterplot", height = "600px") %>% withSpinner(color = loadingGifColor)), # Scatter Plot
                  conditionalPanel("input.chart == 'Box Plot'",     plotOutput("outBoxBoxplot",     height = "600px") %>% withSpinner(color = loadingGifColor)), # Boxplot
                  conditionalPanel("input.chart == 'Carpet'",       plotlyOutput("outBoxCarpet",    height = "600px") %>% withSpinner(color = loadingGifColor)), # Carpet
              ), 
            )
    ), # END 5th (SUB)TAB "Visualize"
    
    
    ###### TAB "Advanced" ----------------------------------------------------------------------
    tabItem(tabName = "clustering",
            fluidRow( 
              # INPUT BOX
              box( title = "Clustering options", width = 4, 
                   helpText("Note 1: In this section we perform a daily load profile clustering. It is not intended to be a generical clustering process."),
                   helpText("Note 2: We require the column Date_Time, Date, Year, Month, min_dec, Hours. If an error displays add them by clicking the button \"Add calendar variables\" in the sidebar."),
                   uiOutput("clustering_inbox"),
                   uiOutput("clustering_inbox_nbclust"),
                   uiOutput("clustering_results_nbclust"),
                   actionButton("cluster_button", "Perform cluster", class = "btn-success", icon = icon("chart-bar"), width = "100%"),
                   uiOutput("clustering_inbox_postprocessing")),
              # OUTPUT BOX
              box( width = 8, 
                   plotOutput("out_clustering_preview", height = "400px") %>% withSpinner(color = loadingGifColor),
                   plotOutput("out_clustering_dendogram", height = "400px")
              ) 
            )
    ),
    tabItem(tabName = "cart",
            fluidRow( 
              # INPUT BOX
              box( title = "CART options", width = 4, 
                   uiOutput("cart_inbox"),
                   actionButton("cart_button", "Perform CART", class = "btn-success", icon = icon("chart-bar"), width = "100%"),
              ),
              # OUTPUT BOX
              box( width = 8,
                   dropdownButton( size = "sm",
                     tags$h3("Graphical parameters"),
                     numericInput(inputId = 'out_cart_tree_fontsize', label = 'Fontsize:', value = 11),
                     numericInput(inputId = 'out_cart_tree_tnex', label = 'Terminal nodes extension:', value = 2.5),
                     circle = TRUE, status = "primary", icon = icon("gear"), width = "400px",
                     tooltip = tooltipOptions(title = "Click to modify plot inputs")
                   ),
                   plotOutput("out_cart_tree", height = "400px") %>% withSpinner(color = loadingGifColor),
                   column(width = 6, style = "padding-left:0px; padding-right:0px;",
                          dropdownButton( size = "sm",
                            tags$h3("Graphical parameters"),
                            selectInput("out_cart_cp_color", "Line color",choices = c("red", "green", "blue")),
                            selectInput("out_cart_cp_upper", "Upper",choices = c("size", "splits", "none")),
                            numericInput('out_cart_cp_lty', label = 'Line Type:', value = 2),
                            circle = TRUE, status = "primary", icon = icon("gear"), width = "400px",
                            tooltip = tooltipOptions(title = "Click to see plot inputs")
                          ),
                          plotOutput("out_cart_cp", height = "400px") %>% withSpinner(color = loadingGifColor)
                   ),
                   column(width = 6,
                          verbatimTextOutput("out_cart_cptable")%>% withSpinner(color = loadingGifColor)
                   )
                   
              ) 
            )
    ),
    
    # documentation
    tabItem(tabName = "help",  includeMarkdown("docs/wiki/complete.md") )
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
