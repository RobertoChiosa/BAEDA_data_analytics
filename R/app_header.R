#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
header <-
  shinydashboard::dashboardHeader(
    disable = FALSE,
    # if you want to hide the header set to TRUE
    title = tags$a(
      href = "http://www.baeda.polito.it/",
      tags$img(
        src = 'www/BAEDA-logo-dashboard.png',
        heigth = '200',
        width = '200'
      )
    ),
    # title = "BAEDA Lab Analytics", # title displayed over the sidebar
    
    # MESSAGE MENU ------------------------------------
    shinydashboard::dropdownMenu(
      type = "messages",
      shinydashboard::messageItem(from = "Welcome Roberto",
        message = "Let's explore the dashboard."),
      shinydashboard::messageItem(
        from = "New User",
        message = "How do I register?",
        icon = shiny::icon("question"),
        time = format(Sys.time(), "%H:%M")
      ),
      shinydashboard::messageItem(
        from = "Support",
        message = "See the documentation.",
        icon = shiny::icon("life-ring"),
        time = format(Sys.Date(), "%d %b %Y")
      )
    ),
    # END MESSAGE MENU
    
    # NOTIFICATION MENU ------------------------------------
    shinydashboard::dropdownMenu(
      type = "notifications",
      shinydashboard::notificationItem(text = "5 new users today",
        shiny::icon("users")),
      shinydashboard::notificationItem(text = "12 items delivered",
        shiny::icon("truck"),
        status = "success"),
      shinydashboard::notificationItem(
        text = "Server load at 86%",
        icon = shiny::icon("triangle-exclamation"),
        status = "warning"
      )
    ),
    # END NOTIFICATION MENU
    
    # TASK MENU ------------------------------------
    shinydashboard::dropdownMenu(
      type = "tasks",
      badgeStatus = "success",
      shinydashboard::taskItem(value = 90, color = "green",
        "Documentation"),
      shinydashboard::taskItem(value = 17, color = "aqua",
        "Project X"),
      shinydashboard::taskItem(value = 75, color = "yellow",
        "Server deployment"),
      shinydashboard::taskItem(value = 80, color = "red",
        "Overall project")
    ) # END TASK MENU
  )
