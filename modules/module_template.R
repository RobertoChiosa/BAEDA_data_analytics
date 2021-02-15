library(shiny)

sliderTextUI <- function(id, label = "Slide me"){ 
  ns <- NS(id)
  tagList(
    sliderInput(ns("slider"), label, 0, 100, 1),
    textOutput(ns("num"))
  )
} 

sliderText <- function(input, output, session, add){  
  output$num <- renderText({
    paste("input$slider + add == ", input$slider + add)
  })
  
  return( reactive({input$slider}) )
}


APP <- function(){
  
  ui <- fluidPage(
    sliderTextUI("hello", label = "ciao"),
    p("input$slider == ", textOutput("value"))
  )
  server <- function(input, output, session) {
    numero <- callModule(sliderText, "hello", 5)
    
    output$value <- renderText({ numero() })
    
  } 
  shinyApp(ui, server)
  
}

APP()
