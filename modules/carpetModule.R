carpetModuleUI_input <- function(id, df) {
  ns <- NS(id)
  
  date_max = max( df[, "Date_Time"] )
  date_min = min( df[, "Date_Time"] )
  
  tagList(
    # dateRangeInput("daterange_Carpet", "Date range:",
    #                start  = if_else(date_max-date_min > lubridate::years(1), date_max - lubridate::days(365), date_min),
    #                end    = date_max,
    #                min    = date_min,
    #                max    = date_max,
    #                format = "yyyy-mm-dd",
    #                separator = " - "),
    selectInput(ns("variable"), label = "Variable:", choices = colnames(dplyr::select_if( df, is.numeric)) ), # chose numerical variable
    selectInput(ns("facetvariable"), label = "Facet Variable:", choices = c("None", colnames(dplyr::select_if( df, is.factor))) ),
    radioButtons(ns("brewer"), "Choose theme:", choices = c("theme1","theme2"), inline = TRUE)
  )
}

carpetModuleUI_plot <- function(id) {
  ns <- NS(id)
  
  tagList(
    plotOutput(ns("plot")),
    sliderInput(ns("year"), "Select Year", value = 1952, 
                min = 1952, max = 2007, step = 5,  
                animate = animationOptions(interval = 500))
  )
}

carpetModule <- function(input, output, session, data) {
  
  # collect one year of data
  ydata <- reactive({
    filter(data, year == input$year)
  })
  
  xrange <- range(data$gdpPercap)
  yrange <- range(data$lifeExp)
  
  output$plot <- renderPlot({
    
    # draw background plot with legend
    plot(data$gdpPercap, data$lifeExp, type = "n", 
         xlab = "GDP per capita", ylab = "Life Expectancy", 
         panel.first = {
           grid()
           text(mean(xrange), mean(yrange), input$year, 
                col = "grey90", cex = 5)
         })
    
    legend("bottomright", legend = levels(data$continent), 
           cex = 1.3, inset = 0.01, text.width = diff(xrange)/5,
           fill = c("#E41A1C99", "#377EB899", "#4DAF4A99", 
                    "#984EA399", "#FF7F0099"))
    
    # Determine bubble colors
    cols <- c("Africa" = "#E41A1C99",
              "Americas" = "#377EB899",
              "Asia" = "#4DAF4A99",
              "Europe" = "#984EA399",
              "Oceania" = "#FF7F0099")[ydata()$continent]
    
    # add bubbles
    symbols(ydata()$gdpPercap, ydata()$lifeExp, circles = sqrt(ydata()$pop),
            bg = cols, inches = 0.5, fg = "white", add = TRUE)
  })
}