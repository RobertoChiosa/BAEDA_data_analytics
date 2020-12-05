#################################################################################
###############            Copyright © BAEDA Lab 2020             ###############
#################################################################################

header <- dashboardHeader(
  disable = FALSE,                # if you want to hide the header set to TRUE    
   title = tags$a(href = "https://www.researchgate.net/lab/Building-Automation-and-Energy-Data-Analytics-Lab-Alfonso-Capozzoli",
                  tags$img(src = 'BAEDA-logo-dashboard.png', heigth = '200', width = '200')),   
  # title = "BAEDA Lab Analytics", # title displayed over the sidebar
  # MESSAGE MENU ----------------------------------------------------------------------
  dropdownMenu(type = "messages",
               messageItem(
                 from = "Sales Dept",
                 message = "Sales are steady this month."
               ),
               messageItem(
                 from = "New User",
                 message = "How do I register?",
                 icon = icon("question"),
                 time = "13:45"
               ),
               messageItem(
                 from = "Support",
                 message = "The new server is ready.",
                 icon = icon("life-ring"),
                 time = "2014-12-01"
               )
  ), # END MESSAGE MENU 
  
  # NOTIFICATION MENU ----------------------------------------------------------------------
  dropdownMenu(type = "notifications",
               notificationItem(
                 text = "5 new users today",
                 icon("users")
               ),
               notificationItem(
                 text = "12 items delivered",
                 icon("truck"),
                 status = "success"
               ),
               notificationItem(
                 text = "Server load at 86%",
                 icon = icon("exclamation-triangle"),
                 status = "warning"
               )
  ), # END NOTIFICATION MENU
  
  # TASK MENU ----------------------------------------------------------------------
  dropdownMenu(type = "tasks", badgeStatus = "success",
               taskItem(value = 90, color = "green",
                        "Documentation"
               ),
               taskItem(value = 17, color = "aqua",
                        "Project X"
               ),
               taskItem(value = 75, color = "yellow",
                        "Server deployment"
               ),
               taskItem(value = 80, color = "red",
                        "Overall project"
               )
  ) # END TASK MENU
)
