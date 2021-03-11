sidebar <- dashboardSidebar(
  disable = FALSE,                  # if you want to hide the header set to TRUE        
  # width = 250,                    # set custom width of the sidebar
  sidebarMenu(
    id = "tabs",  
    menuItem("Visualize", tabName = "visualize", icon = icon("chart-bar") ),  
    menuItem("Classification", tabName = "classification", icon = icon("sitemap"))
  )
)