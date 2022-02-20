#' module_deploy UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_module_deploy_ui <- function(id){

  ns <- NS(id)

  tagList(
    actionButton(inputId = "menu_item3", label = "Deploy")
  )

}

#' module_deploy Server Functions
#'
#' @noRd
mod_module_deploy_server <- function(id){
  moduleServer( id, function(input, output, session){

  })
}

## To be copied in the UI
# mod_module_deploy_ui("module_deploy_1")

## To be copied in the server
# mod_module_deploy_server("module_deploy_1")
