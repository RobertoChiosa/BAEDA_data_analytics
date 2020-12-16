# chartTypes <- c("","Bar Plot") # add here more chart types as we go on adding
# # dataframe <- read.csv("/Users/robi/Desktop/BAEDA_data_analytics/data/data.csv", header = T, sep = ",", dec = ".", check.names = FALSE)
# #dataframe <- read.csv("/Users/robi/Desktop/BAEDA_data_analytics/data/run.csv", header = T, sep = ",", dec = ".", check.names = FALSE)
# dataframe <- dataframe %>%
#   mutate(
#     Date_Time = as.POSIXct(`Date/Time` , format = "%m/%d  %H:%M:%S" , tz = "Etc/GMT+12"),
#     Day_Type = wday(Date_Time, label = TRUE, locale = Sys.setlocale("LC_TIME","en_US.UTF-8")),
#     Day_Type = factor(Day_Type, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
#     Month = month(Date_Time, label = TRUE, locale = Sys.setlocale("LC_TIME","en_US.UTF-8")),
#     Year = year(Date_Time)
#   )
# 
# ui <- fluidPage(
#   sidebarLayout(
#     sidebarPanel(
#       selectInput("chart", label = "Chart Type:", choices = chartTypes, selected = NULL),
#       conditionalPanel("input.chart == 'Bar Plot'", uiOutput("inBoxBar")) # bar plot
#     ),
#     mainPanel(
#       plotOutput("outBoxBar") %>% withSpinner(color = "#0dc5c1")) # bar plot
#     )
#   )
# 
# 
# 
# # Define server logic required to draw a histogram ----
# server <- function(input, output) {
# 
#   output$inBoxBar <- renderUI({
#     tagList(
#       selectInput("variableX", label = "Variable X:", choices = colnames(dplyr::select_if(dataframe, is.factor))), # chose numerical variable
#       selectInput("variableY", label = "Variable Y:", choices = colnames(dataframe)), # chose numerical variable
#       actionButton("plotButton", "Plot", style = "color: #fff; background-color: green; border-color: #green")
#     )
#   })
#   
#   plot_data <- eventReactive(input$plotButton,{
#     aes_x <- input$variableX
#     aes_y <- input$variableY
#     arg_stat <- "identity"
#     
#     plot <- ggplot(data = dataframe, mapping =  aes_string(x = aes_x, y = aes_y, fill = NULL)) + 
#       geom_bar(stat = arg_stat) + 
#       theme_bw()
#     plot
#   })
#   
#   output$outBoxBar <- renderPlot({ plot_data() })
# }
# 
# shinyApp(ui, server)