library(shiny)
ui <- fluidPage(
  h3("Action Buttons"),
  actionButton("default", "Default"),
  actionButton("primary", "Primary", class = "btn-primary"),
  actionButton("secondary", "Secondary", class = "btn-secondary"),
  actionButton("success", "Success", class = "btn-success"),
  actionButton("info", "Info", class = "btn-info"),
  actionButton("warning", "Warning", class = "btn-warning"),
  actionButton("danger", "Danger", class = "btn-danger"),
  actionButton("link", "Link", class = "btn-link"),
  
  h3("Download Buttons"),
  downloadButton("default", "Default"),
  downloadButton("primary", "Primary", class = "btn-primary"),
  downloadButton("secondary", "Secondary", class = "btn-secondary"),
  downloadButton("success", "Success", class = "btn-success"),
  downloadButton("info", "Info", class = "btn-info"),
  downloadButton("warning", "Warning", class = "btn-warning"),
  downloadButton("danger", "Danger", class = "btn-danger"),
  downloadButton("link", "Link", class = "btn-link"),
  
  h3("Bookmark Buttons"),
  bookmarkButton("Default"),
  bookmarkButton("Primary", class = "btn-primary"),
  bookmarkButton("Secondary", class = "btn-secondary"),
  bookmarkButton("Success", class = "btn-success"),
  bookmarkButton("Info", class = "btn-info"),
  bookmarkButton("Warning", class = "btn-warning"),
  bookmarkButton("Danger", class = "btn-danger"),
  bookmarkButton("Link", class = "btn-link")
)
server <- function(input, output, session) {}
shinyApp(ui, server)