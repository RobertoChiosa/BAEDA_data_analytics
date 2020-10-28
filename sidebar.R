#################################################################################
###############            Copyright © BAEDA Lab 2020             ###############
#################################################################################

sidebar <- dashboardSidebar(
  disable = FALSE,                # if you want to hide the header set to TRUE        
  #width = 250,                    # set custom width of the sidebar
  sidebarMenu(            
    id = "tabs",                  # tabs id
    menuItem("Welcome", tabName = "welcome", icon = icon("home")),                # 1st TAB "Welcome"
    menuItem("Explore", tabName = "explore", icon = icon("chart-bar"),            # 2nd TAB "Explore"
             menuSubItem("Manage", tabName = "manage"),                               # 3rd (SUB)TAB "Manage"
             menuSubItem("View", tabName = "view"),                                   # 4th (SUB)TAB "View"
             menuSubItem("Visualize", tabName = "visualize")                          # 5th (SUB)TAB "Visualize"
    ),
    menuItem("Advanced", tabName = "advanced", icon = icon("project-diagram"),          # 6th TAB "Explore"
             menuSubItem("ADD", tabName = "add"),                                     # 7th (SUB)TAB "ADD"
             menuSubItem("FDD", tabName = "fdd")                                      # 8th (SUB)TAB "FDD"
    ),
    menuItem("Source code", icon = icon("code"), href = "https://github.com/RobertoChiosa/BAEDA_data_analytics")
  ),
  textOutput("selected_sidebar_tab"),           # print the selected tab in the sidebar to keep track
  textOutput("selected_tabBox_tab")             # print the selected tab in the output tabbox
  
)
