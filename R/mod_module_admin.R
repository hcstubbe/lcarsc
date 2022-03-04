#' module_admin UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_module_admin_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' module_admin Server Functions
#'
#' @noRd
mod_module_admin_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_module_admin_ui("module_admin_1")

## To be copied in the server
# mod_module_admin_server("module_admin_1")
