#' module_admin UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom golem get_golem_options
#' @importFrom RMariaDB dbReadTable
#'
#'
mod_module_admin_ui <- function(id){
  ns <- NS(id)

  # Check if user is admin
  if(!get_golem_options("user_is_admin")){
    return(NULL)
  }else{
    tagList(
      fluidPage(
        fluidRow(
          shinydashboard::box(title = "Access database",
                              status = "info",
                              collapsible = TRUE,
                              collapsed = FALSE,
                              width = 12,
                              solidHeader = TRUE

          ),
          shinydashboard::box(title = "Danger zone",
                              status = "danger",
                              collapsible = TRUE,
                              collapsed = TRUE,
                              width = 12,
                              solidHeader = TRUE

          )
        )
      )
    )
  }

}

#' module_admin Server Functions
#'
#' @noRd
mod_module_admin_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    if(get_golem_options("user_is_admin")){

    }



  })
}

## To be copied in the UI
# mod_module_admin_ui("module_admin_1")

## To be copied in the server
# mod_module_admin_server("module_admin_1")
