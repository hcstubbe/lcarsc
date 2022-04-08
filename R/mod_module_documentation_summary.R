#' module_documentation_summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_module_documentation_summary_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("summary"))
  )
}

#' module_documentation_summary Server Functions
#'
#' @noRd
mod_module_documentation_summary_server <- function(id,
                                                    rv_in){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$summary = renderUI({rv_in$pid()})

  })
}

## To be copied in the UI
# mod_module_documentation_summary_ui(ns("module_documentation_summary_1"))

## To be copied in the server
# mod_module_documentation_summary_server("module_documentation_summary_1")
