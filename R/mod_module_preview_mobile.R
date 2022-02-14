#' module_preview_mobile UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_module_preview_mobile_ui <- function(id){
  ns <- NS(id)
  tagList(
    h4("Preview mobile")
  )
}

#' module_preview_mobile Server Functions
#'
#' @noRd
mod_module_preview_mobile_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_module_preview_mobile_ui("module_preview_mobile_1")

## To be copied in the server
# mod_module_preview_mobile_server("module_preview_mobile_1")
