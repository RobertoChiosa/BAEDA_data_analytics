




# get names as in package dlookr
# https://github.com/choonghyunryu/dlookr/blob/da09ec981df12ab6c0d861f09a0facf5c8ea929d/R/utils.R
get_class <- function(df) {
  vars <- sapply(names(df), function(.x.) class(dplyr::pull(df, .x.))[1])
  data.frame(variable = names(vars), class = vars, row.names = NULL)
}

# converts the colnames of datase t to list with class
variable_list_with_class <-  function( dataset ) {
  # creates list with class
  tmp_df     <- get_class(dataset)
  var_name   <- tmp_df$variable
  var_fct    <- tmp_df$class
  
  var_part1  <- var_name
  var_part2  <- gsub(" ", "", paste("{", var_fct, "}"))
  names(var_name) <- paste(var_part1, var_part2)
  return(var_name)
}

# 
# library(shiny)
# 
# ui <- fluidPage(
#   
#   uiOutput("select"),
#   verbatimTextOutput("text")
#   
# )
# 
# server <- function(input, output, session) {
#   df <- reactive({read.csv("/Users/robi/Desktop/dashboard-student-old/data/data_tot.csv")})
#   
#   output$select <- renderUI({
#     selectInput("select_input", NULL, choices = variable_list_with_class(df()))
#   })
#     
#   output$text <- renderPrint({
#     
#     input$select_input
# 
#   })
# }
# 
# shinyApp(ui, server)