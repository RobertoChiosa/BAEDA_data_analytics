custom_module_template_manage <- function(name, path, export, ph_ui = " ", ph_server = " "){
  write_there <- function(...){
    write(..., file = path, append = TRUE)
  }
  
  write_there(sprintf("#' %s UI Function", name))
  write_there("#'")
  write_there("#' @description A shiny Module for manage tab.")
  write_there("#'")
  write_there("#' @param id,input,output,session Internal parameters for {shiny}.")
  write_there("#'")
  if (export){
    write_there(sprintf("#' @rdname mod_%s", name))
    write_there("#' @export ") 
  } else {
    write_there("#' @noRd ") 
  }
  write_there("#'")
  write_there("#' @importFrom shiny NS tagList ") 
  write_there(sprintf("mod_%s_ui <- function(id){", name))
  write_there("  ns <- NS(id)")
  write_there("  tagList(")
  
  write_there("    box(")
  write_there("      solidHeader = T, collapsible = T, collapsed = TRUE, width = 12,")
  write_there(       sprintf("title = '%s', status = 'primary' ", name))
  write_there("  )")
  
  write_there(ph_ui)
  write_there("  )")
  write_there("}")
  write_there("    ")
  
  if (packageVersion("shiny") < "1.5"){
    
    write_there(sprintf("#' %s Server Function", name))
    write_there("#'")
    if (export){
      write_there(sprintf("#' @rdname mod_%s", name))
      write_there("#' @export ") 
    } else {
      write_there("#' @noRd ") 
    }
    write_there(sprintf("mod_%s_server <- function(input, output, session){", name))
    write_there("  ns <- session$ns")
    write_there(ph_server)
    write_there("}")
    write_there("    ")
    
    write_there("## To be copied in the UI")
    write_there(sprintf('# mod_%s_ui("%s_ui_1")', name, name))
    write_there("    ")
    write_there("## To be copied in the server")
    write_there(sprintf('# callModule(mod_%s_server, "%s_ui_1")', name, name))
    
    
  } else {
    
    write_there(sprintf("#' %s Server Functions", name))
    write_there("#'")
    if (export){
      write_there(sprintf("#' @rdname mod_%s", name))
      write_there("#' @export ") 
    } else {
      write_there("#' @noRd ") 
    }
    write_there(sprintf("mod_%s_server <- function(id){", name))
    write_there("  moduleServer( id, function(input, output, session){")
    write_there("    ns <- session$ns")
    write_there(ph_server)
    write_there("  })")
    write_there("}")
    write_there("    ")
    
    write_there("## To be copied in the UI")
    write_there(sprintf('# mod_%s_ui("%s_ui_1")', name, name))
    write_there("    ")
    write_there("## To be copied in the server")
    write_there(sprintf('# mod_%s_server("%s_ui_1")', name, name))
    
  }
}