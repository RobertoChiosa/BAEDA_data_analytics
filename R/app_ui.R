#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @importFrom  shinyBS bsPopover
#' @noRd

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    dashboardPage(
      skin = "black",
      # sets overall appearance
      header,
      sidebar,
      dashboardBody(
        # shiny::tags$style(".fa-plus {color:green}"),
        # change plus icon color
        # shiny::tags$style(".fa-calendar-alt {color:green}"),
        # change plus icon color
        # shiny::tags$style(".fa-refresh {color:green}"),
        # change calendar icon color
        #shiny::tags$style(".fa-backspace {color:red}"),
        # change backspace icon color
        
        # modal opens
        mod_load_ext_file_ui_modal("load_ext_file_ui_1"),
        
        ##### 
        # MANAGE
        tabItems(
          tabItem(tabName = "manage",
                  fluidRow(
                    column(width = 4, style = "padding-left:0px; padding-right:0px;",
                           mod_manage_ui_input("manage_ui_1"),
                           # rename column / variable from a dataset
                           mod_manage_renameColumn_ui("manage_renameColumn_ui_1"),
                           # change type
                           mod_manage_manage_changeType_ui("manage_manage_changeType_ui_1"),
                           # add column / variable from a dataset
                           mod_manage_addColumn_ui("manage_addColumn_ui_1")
                    ),
                    column(width = 8, style = "padding-left:0px; padding-right:0px;",
                           box(width = 12,
                               mod_manage_ui_output("manage_ui_1")
                           )
                    )
                    
                  )
          ),
          ##### 
          # PREPROCESSING
          tabItem(tabName = "preprocessing",
                  fluidRow(
                    column(width = 4, style = "padding-left:0px; padding-right:0px;",
                           box(width = 12,
                               title = shiny::HTML(
                                 "Preprocessing options
                                         <a
                                         id=\"button\"
                                         data-toggle=\"tooltip\"
                                         title=\" Preprocessing options.\"
                                         class=\"dropdown\">
                                         <i class=\"fa fa-info-circle\"></i>
                                         </a>"
                               ),
                               p("This section permits to perform data pre processing on the loaded dataset.
                                 The available modules permit to identify outliers through different techniques and
                                 replace NA through imputation")
                               
                           )
                    ),
                    column(width = 8, style = "padding-left:0px; padding-right:0px;",
                           box(width = 12,
                               
                           )
                    )
                  )),
          ##### 
          # VISUALIZATION
          tabItem(tabName = "visualization",
                  fluidRow(
                    column(width = 4, style = "padding-left:0px; padding-right:0px;",
                           box(width = 12,
                               title = shiny::HTML(
                                 "Chart options
                                         <a
                                         id=\"button\"
                                         data-toggle=\"tooltip\"
                                         title=\" Chart options.\"
                                         class=\"dropdown\">
                                         <i class=\"fa fa-info-circle\"></i>
                                         </a>"
                               ),
                               p("This section permits to perform visualize datacontained in the loaded dataset.
                                Different kind of plots are available."),
                               selectInput(
                                 "chart",
                                 label = "Chart Type:",
                                 choices = c(
                                   "Histogram",
                                   "Box Plot",
                                   "Carpet",
                                   "Line Plot",
                                   "Scatter Plot"
                                 ),
                                 selected = NULL
                               ),
                           ),
                           box(
                             conditionalPanel( "input.chart == 'Histogram'",  mod_histogram_ui_input("histogram_ui_1")),
                             solidHeader = T, collapsible = T, collapsed = TRUE, width = 12,
                             title = "Plot Parameters", status = "primary"
                           )
                    ),
                    column(width = 8, style = "padding-left:0px; padding-right:0px;",
                           box(width = 12,
                               conditionalPanel("input.chart == 'Histogram'",mod_histogram_ui_output("histogram_ui_1"))
                           )
                    )
                  ),
          ),
          ##### 
          # CLASSIFICATION
          tabItem(tabName = "classification",
                  # INPUT BOX
                  fluidRow(
                    column(width = 4, style = "padding-left:0px; padding-right:0px;",
                           mod_cart_ui_input("cart_ui_1")
                    ),
                    column(
                      width = 8, style = "padding-left:0px; padding-right:0px;",
                      # INPUT BOX
                      fluidRow(
                        shinydashboard::box(width = 12,  mod_cart_ui_output("cart_ui_1", type = "tree"))
                      ),
                      fluidRow(
                        shinydashboard::box(width = 6,   mod_cart_ui_output("cart_ui_1", type = "cp")),
                        shinydashboard::box(width = 6,   mod_cart_ui_output("cart_ui_1", type = "CM"))
                      )
                    )
                  )
          ),
          ##### 
          # CLUSTERING
          tabItem(tabName = "clustering",
                  fluidRow(
                    box(width = 4,
                        title = shiny::HTML(
                          "Clustering options
                                         <a
                                         id=\"button\"
                                         data-toggle=\"tooltip\"
                                         title=\" Clustering options.\"
                                         class=\"dropdown\">
                                         <i class=\"fa fa-info-circle\"></i>
                                         </a>"
                        ),
                        mod_clustering_ui_input("clustering_ui_1")
                    ),
                    box(width = 8,
                        mod_clustering_ui_output("clustering_ui_1")
                    )
                  )
          ),
          ##### 
          # ANN
          tabItem(tabName = "ann",
                  fluidRow(
                    box( width = 4,
                         title = shiny::HTML(
                           "ANN options
                                         <a
                                         id=\"button\"
                                         data-toggle=\"tooltip\"
                                         title=\" Artificial Neural Network options.\"
                                         class=\"dropdown\">
                                         <i class=\"fa fa-info-circle\"></i>
                                         </a>"
                         )
                    ),
                    box(width = 8, 
                    )
                  )
          ),
          ##### 
          # FORECASTING
          tabItem(tabName = "forecasting",
                  fluidRow(
                    box(width = 4,
                        title = shiny::HTML(
                          "Forecasting options
                                         <a
                                         id=\"button\"
                                         data-toggle=\"tooltip\"
                                         title=\" Artificial Neural Network options.\"
                                         class=\"dropdown\">
                                         <i class=\"fa fa-info-circle\"></i>
                                         </a>"
                        )
                    ),
                    box(width = 8, 
                    )
                  )
          ),
          ##### 
          # MV
          tabItem(tabName = "mv",
                  fluidRow(
                    box( width = 4,
                         title = shiny::HTML(
                           "M&V options
                                         <a
                                         id=\"button\"
                                         data-toggle=\"tooltip\"
                                         title=\" Artificial Neural Network options.\"
                                         class=\"dropdown\">
                                         <i class=\"fa fa-info-circle\"></i>
                                         </a>"
                         )
                    ),
                    box(width = 8,
                    )
                  )
          ),
          tabItem(tabName = "add",
                  fluidRow(
                    box(width = 4,
                        title = shiny::HTML(
                          "ADD options
                                         <a
                                         id=\"button\"
                                         data-toggle=\"tooltip\"
                                         title=\" Artificial Neural Network options.\"
                                         class=\"dropdown\">
                                         <i class=\"fa fa-info-circle\"></i>
                                         </a>"
                        )
                    ),
                    box(width = 8,
                    )
                  )
          )
        ),
        mod_footer_ui()
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
golem_add_external_resources <- function() {
  add_resource_path('www', app_sys('app/www'))
  
  tags$head(
    favicon(),
    bundle_resources(path = app_sys('app/www'),
                     app_title = 'eDASH - Student'),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    shinyFeedback::useShinyFeedback(),
    shinyjs::useShinyjs()
  )
}
