#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    dashboardPage(
      skin = "black",                             # sets overall appearance 
      header,
      sidebar,
      dashboardBody(
        shiny::tags$style(".fa-plus {color:green}"),              # change plus icon color
        shiny::tags$style(".fa-calendar-alt {color:green}"),      # change calendar icon color
        shiny::tags$style(".fa-backspace {color:red}"),           # change backspace icon color
        
        # modal opens
        mod_load_ext_file_ui_modal("load_ext_file_ui_1"),
        
        # rest of the tabs
        tabItems(
          tabItem(tabName = "visualize",
                  box( title = "Chart options", width = 4,
                       selectInput("chart", label = "Chart Type:", 
                                   choices = c("","Carpet","Line Plot","Scatter Plot", "Box Plot","Histogram"),
                                   selected = NULL),
                       conditionalPanel("input.chart == 'Histogram'",  mod_histogram_ui_input("histogram_ui_1") ),    # Histogram
                  ),
                  box( width = 8,
                       conditionalPanel("input.chart == 'Histogram'",  mod_histogram_ui_output("histogram_ui_1") )
                  )
                  
          ),
          tabItem(tabName = "classification",
                  fluidRow(
                    column(width = 4,
                           # INPUT BOX
                           box( title = "Classification options", width = 12, 
                                mod_cart_ui_input("cart_ui_1")
                           )
                    ),
                    column(width = 8,
                           # INPUT BOX
                           box( width = 12,  mod_cart_ui_output("cart_ui_1", type = "tree")  ),
                           box( width = 6,   mod_cart_ui_output("cart_ui_1", type = "cp")    ),
                           box( width = 6,   mod_cart_ui_output("cart_ui_1", type = "CM")    )
                    )
                  )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @importFrom shinyFeedback useShinyFeedback
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'eDASH'
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
    shinyFeedback::useShinyFeedback(),
  )
}

