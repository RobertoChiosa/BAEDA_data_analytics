sidebar <- shinydashboard::dashboardSidebar(
  disable = FALSE,                  # if you want to hide the header set to TRUE        
  # width = 250,                    # set custom width of the sidebar
  shinydashboard::sidebarMenu(
    id = "tabs",  
    shiny::tagList(
      shiny::selectInput(  "dataframe",  "Dataframe:", choices = c("None")),
      shiny::actionButton( "upload",     "Upload a new dataframe", icon = icon("plus"), width = "87%", class = "btn-success")
      # shiny::downloadButton( "download",   "download", icon = icon("download"), 
      #                        style = "class: btn btn-default action-button; width:100%; padding-left:20px; padding-right:10px; ")
    ),
    shinydashboard::menuItem( "Wrangling",         tabName = "manage",         icon = icon("database") ),   # wrangling but in code called manage
    shinydashboard::menuItem( "Pre-processing",    tabName = "preprocessing",  icon = icon("cogs") ),  
    shinydashboard::menuItem( "Visualization",     tabName = "visualization",  icon = icon("chart-bar") ),  
    shinydashboard::menuItem( "Classification",    tabName = "classification", icon = icon("sitemap"),              badgeLabel = "advanced", badgeColor = "green"),
    shinydashboard::menuItem( "Clustering",        tabName = "clustering",     icon = icon("braille"),              badgeLabel = "advanced", badgeColor = "green")
    # shinydashboard::menuItem( "Neural network",    tabName = "ann",            icon = icon("connectdevelop"),       badgeLabel = "advanced", badgeColor = "green"),
    # shinydashboard::menuItem( "Forecasting",       tabName = "forecasting",    icon = icon("fast-forward"),         badgeLabel = "advanced", badgeColor = "green"),
    # shinydashboard::menuItem( "M&V",               tabName = "mv",             icon = icon("balance-scale"),        badgeLabel = "advanced", badgeColor = "green"),
    # shinydashboard::menuItem( "Anomaly Detection", tabName = "add",            icon = icon("exclamation-triangle"), badgeLabel = "advanced", badgeColor = "green")
    # 
  )
)