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
    shinydashboard::menuItem( "Visualize",      tabName = "visualize",      icon = icon("chart-bar") ),  
    shinydashboard::menuItem( "Classification", tabName = "classification", icon = icon("sitemap"))
  )
)