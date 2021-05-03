#' Copyright and link Footer
#'
#' @noRd 
#'
#' @description This module provides a footer with copyright and link to BAEDA Lab, DENERG and POLITO websites.
#' @importFrom shiny absolutePanel HTML

mod_footer_ui <- function(){
  shiny::absolutePanel(
    shiny::HTML(
        "© Copyright 2021 |
      <a href='http://www.baeda.polito.it/'>BAEDA Lab</a> | 
      <a href='https://www.denerg.polito.it/'>Dipartimento Energia</a> | 
      <a href='https://www.polito.it/'>Politecnico di Torino</a>
      "
      ),
      bottom = "1%", 
      right = "1%", 
      fixed = TRUE,
      color = "#3c4c54"
    )
}

## To be copied in the UI
# mod_footer_ui()
