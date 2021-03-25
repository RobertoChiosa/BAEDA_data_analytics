sidebar <- shinydashboard::dashboardSidebar(
  disable = FALSE,                  # if you want to hide the header set to TRUE        
  # width = 250,                    # set custom width of the sidebar
  shinydashboard::sidebarMenu(
    id = "tabs",  
    shiny::tagList(
      shiny::selectInput(  "dataframe",  "Dataframe:", choices = c("None")),
      shiny::actionButton( "upload",     "Upload a new dataframe", icon = icon("plus"), width = "87%"),
      shiny::actionButton( "add_calendar_columns", "  Add calendar variables", icon = icon("calendar-alt"), width = "87%"),
    ),
    shinydashboard::menuItem( "Manage",         tabName = "manage",         icon = icon("database") ),  
    shinydashboard::menuItem( "Pre-processing", tabName = "preprocessing",  icon = icon("cogs") ),  
    shinydashboard::menuItem( "Visualize",      tabName = "visualize",      icon = icon("chart-bar") ),  
    shinydashboard::menuItem( "Classification", tabName = "classification", icon = icon("sitemap"), badgeLabel = "advanced", badgeColor = "green"),
    shinydashboard::menuItem( "Clustering",     tabName = "clustering",     icon = icon("braille"), badgeLabel = "advanced", badgeColor = "green"),
    shinydashboard::menuItem( "Neural network", tabName = "ann",            icon = icon("connectdevelop"), badgeLabel = "advanced", badgeColor = "green")
  )
)