#' module_reports_editor UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_module_reports_editor_ui <- function(id){
  ns <- NS(id)
  tagList(
    h4("REPORTS EDITOR")
  )
}

#' module_reports_editor Server Functions
#'
#' @noRd
mod_module_reports_editor_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_module_reports_editor_ui(ns("module_reports_editor_1"))

## To be copied in the server
# mod_module_reports_editor_server("module_reports_editor_1")
