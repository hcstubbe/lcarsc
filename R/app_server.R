#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny shinydashboard DT
#' @importFrom golem get_golem_options
#' @noRd
app_server <- function( input, output, session ) {

  production_mode = get_golem_options("production_mode")


  # Your application server logic
  if(production_mode == "production"){
    mod_module_launcher_server("mod_module_launcher_1")
  }
  if(production_mode == "editor"){
    mod_module_launcher_edit_server("mod_module_launcher_edit_1")
  }
}
