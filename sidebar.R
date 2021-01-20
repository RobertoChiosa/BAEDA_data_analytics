#################################################################################
###############            Copyright © BAEDA Lab 2020             ###############
###############                     -------                       ###############
###############                  Roberto Chiosa                   ###############
###############             roberto.chiosa@polito.it              ###############
#################################################################################

sidebar <- dashboardSidebar(
  disable = FALSE,                  # if you want to hide the header set to TRUE        
  # width = 250,                    # set custom width of the sidebar
  sidebarMenu(            
    id = "tabs",  
    selection_dataframe(),
    menuItem("Manage", tabName = "manage", icon = icon("cogs")),                      # TAB "MANAGE" 
    menuItem("Visualize", tabName = "visualize", icon = icon("chart-bar") ),          # TAB "VISUALIZE" 
    menuItem("Advanced", tabName = "advanced", icon = icon("project-diagram"),        # TAB "ADVANCED" 
             menuSubItem("Clustering", tabName = "clustering")                        # TAB "CLUSTERING"                     
    ),
    menuItem("Help", tabName = "help", icon = icon("book"))                           # TAB "HELP" 
  )
)
