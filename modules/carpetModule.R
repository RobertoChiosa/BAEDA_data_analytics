module_carpetApp <- function() {
  library(shiny)
  data <- readRDS("./data/data_tot.rds")
  ui <- fluidPage(
    column(width = 4,
           carpetModuleUI_input("carpet", data)
    ),
    column(width = 4,
           carpetModuleUI_plot("carpet")
    )
    
  )
  server <- function(input, output, session) {
    carpetModule("carpet", reactive(data), )
  }
  shinyApp(ui, server)
}
module_carpetApp()


carpetModuleUI_input <- function(id, data) {
  ns <- NS(id)
  
  date_max = max( data[, "Timestamp"] )
  date_min = min( data[, "Timestamp"] )
  
  tagList(
    dateRangeInput(ns("daterange"), "Date range:",
                   start  = dplyr::if_else(date_max-date_min > lubridate::years(1), date_max - lubridate::days(365), date_min),
                   end    = date_max,
                   min    = date_min,
                   max    = date_max,
                   format = "yyyy-mm-dd",
                   separator = " - "),
    selectInput(ns("variable"), label = "Variable:", choices = colnames(dplyr::select_if( data, is.numeric)) ), # chose numerical variable
    selectInput(ns("facetvariable"), label = "Facet Variable:", choices = c("None", colnames(dplyr::select_if( data, is.factor))) ),
    radioButtons(ns("brewer"), "Choose theme:", choices = c("theme1","theme2"), inline = TRUE)
  )
}

carpetModuleUI_plot <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("outBoxCarpet"))
  )
}

carpetModule <- function(id, input, output, session, data) {
  data_plot <- data
  output$outBoxCarpet <- renderPlotly({
    
    if(input$brewer == "theme1"){ cols <- brewer.pal(9, "YlOrRd")}else{cols <- rev(brewer.pal(9, "Spectral"))}

    # data_plot <- data[[input$dataframe]] %>%
    #   dplyr::filter(Date >= input$daterange_Carpet[1], Date <= input$daterange_Carpet[2])
    
    plot <- ggplot(data =  data_plot, 
                   mapping =  aes(x =  as.POSIXct(format(Timestamp, "%H:%M:%S"), "%H:%M:%S", tz = input[[timezone]]), 
                                  y =  date(data_plot[,"Timestamp"]),
                                  fill = data_plot[,input$variable_Carpet],
                                  text = paste(' Time:', format(Timestamp, "%H:%M:%S"), 
                                               '<br> Date: ', date(Timestamp), '<br>',
                                               input$variable, ':', data_plot[,input$variable]
                                  )
                   )
    ) + 
      geom_tile(na.rm = TRUE) +
      scale_fill_gradientn(colours = cols) + # creates a two colour gradient (low-high) for the variable fill=z=power
      scale_y_date(
        breaks = scales::date_breaks("1 month"),                    # specify breaks every two months
        labels = scales::date_format("%Y-%b" , tz = "Europe/Rome"),  # specify format of labels anno mese
        expand = c(0,0)                                     # espande l'asse y affinche riempia tutto il box in verticale
      ) +
      scale_x_datetime(
        breaks = scales::date_breaks("4 hour"),                     # specify breaks every 4 hours
        labels = scales::date_format(("%H:%M") , tz = "Europe/Rome"),# specify format of labels ora minuti
        expand = c(0,0)                                     # espande l'asse x affinche riempia tutto il box in orizzontale
      ) +
      theme_bw() + labs( x = "Hour" , y = "Date", fill = input$variable)
    
  })
})
