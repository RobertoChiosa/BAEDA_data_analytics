#################################################################################
###############            Copyright © BAEDA Lab 2020             ###############
#################################################################################

sidebar <- dashboardSidebar(
  disable = FALSE,                  # if you want to hide the header set to TRUE        
  # width = 250,                    # set custom width of the sidebar
  sidebarMenu(            
    id = "tabs",  
    selection_dataframe(),
    menuItem("Manage", tabName = "manage", icon = icon("cogs")),                      # TAB "Manage" 
    menuItem("Visualize", tabName = "visualize", icon = icon("chart-bar") ),          # TAB "Visualize" 
    menuItem("Advanced", tabName = "advanced", icon = icon("project-diagram"),        # TAB "Advanced" 
             menuSubItem("Clustering", tabName = "clustering")                        # TAB "Clustering"                     
    ),
    menuItem("Help", tabName = "help", icon = icon("book"))                           # TAB "Help" 
  )
)
