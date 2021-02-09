lineplotInput <- function(id, dataframe){
  ns <- NS(id)
  tagList(
    selectInput(ns("variableY"), label = "Variable Y:",
                choices = colnames(dplyr::select_if( dataframe, is.numeric))), # chose numerical variable
    selectInput(ns("colorvariable"), label = "Color Variable:", choices = c("None", colnames(dplyr::select_if( dataframe , is.factor))) ),
  )
}

lineplotOutput <- function(id){
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("plot"))
  )
}

lineplotServer <- function(input, output, session, dataframe){
  output$plot <- renderPlotly({ 
    plot <- ggplot(data =  dataframe,
                   mapping =  aes(x = as.POSIXct(Timestamp, "%Y-%m-%d %H:%M:%S", tz = "Europe/Rome"),
                                  y =  dataframe[,input$variableY],
                                  text = paste(
                                    # ' Time:', format(Date_Time, "%H:%M:%S"), 
                                    #            '<br> Date: ', Date, '<br>',
                                    input$variableY, ':', dataframe[,input$variableY]
                                  ), 
                                  group = 1, # solve ggplotly problem
                                  color = if (input$colorvariable == "None") {NULL} else { dataframe[,input$colorvariable]} 
                   )
    ) + 
      geom_line(na.rm = TRUE)+
      theme_bw() +
      labs( x = "Date Time", y = input$variableY)
    
    
    plot <- ggplotly(plot, tooltip = c("text"), dynamicTicks = TRUE) 
    
    resetLoadingButton("plot_button")         # reset the loading button
    
    plot %>%
      layout(hovermode = "x unified",
             showlegend = T,
             autosize = T,
             # legend = list(orientation = 'h',
             #               bgcolor = "#E2E2E2",
             #               bordercolor = "#FFFFFF",
             #               borderwidth = 2),
             yaxis = list( autorange = TRUE),
             xaxis = list( autorange = TRUE,
                           rangeselector =  list(
                             buttons = list( 
                               list(count = 1, label = 'All', step = 'all'),
                               list(count = 1, label = 'Year', step = 'year', stepmode = 'backward'),
                               list(count = 6, label = 'Semester', step = 'month', stepmode = 'backward'),
                               list(count = 1, label = 'Month', step = 'month', stepmode = 'backward'),
                               list(count = 7, label = 'Week', step = 'day', stepmode = 'backward'),
                               list(count = 6, label = '6 Hours', step = 'hour', stepmode = 'backward')
                             ))
             )
      )
  })
}

testApp <- function(){
  ui <- fluidPage(
    column(width = 6, lineplotInput("input", data) ),
    column(width = 6, lineplotOutput("input") )
  )
  
  server <- function(input, output, session){
    df <- reactiveValues()
    df$data <- readRDS("./data/data_tot.rds")
    
    callModule(lineplotServer, "input", df$data)
  }
  
  shinyApp(ui,server)
}

testApp()
