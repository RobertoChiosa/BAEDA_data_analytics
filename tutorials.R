# Example 1: Simple modal
if (interactive()) {
  library(shiny)
  library(shinyalert)
  
  shinyApp(
    ui = fluidPage(
      useShinyalert(),  # Set up shinyalert
      actionButton("btn", "Click me")
    ),
    server = function(input, output) {
      observeEvent(input$btn, {
        # Show a simple modal
        shinyalert(title = "You did it!", type = "success")
      })
    }
  )
}

# Example 2: Input modal calling another modal in its callback
if (interactive()) {
  library(shiny)
  library(shinyalert)
  
  shinyApp(
    ui = fluidPage(
      useShinyalert(),  # Set up shinyalert
      actionButton("btn", "Greet")
    ),
    server = function(input, output) {
      observeEvent(input$btn, {
        shinyalert(
          title = "What is your name?", type = "input",
          callbackR = function(value) { shinyalert(paste("Welcome", value)) }
        )
      })
    }
  )
}