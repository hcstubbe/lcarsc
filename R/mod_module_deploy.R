#' module_deploy UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyvalidate InputValidator sv_required
#' @importFrom golem get_golem_options
#'
#' @noRd
#'
mod_module_deploy_ui <- function(id){

  ns <- NS(id)

  tagList(
    strong("Caution: moving from 'editor' to 'production' cannot be reversed!"),
    br(),
    br(),
    textInput(ns("confirm_deployment"), label = "Type 'activate production'", placeholder = "Fill to confirm"),
    actionButton(inputId = ns("deploy"), label = "Deploy")
  )

}

#' module_deploy Server Functions
#'
#' @noRd
mod_module_deploy_server <- function(id){
  moduleServer( id, function(input, output, session){

    ns = session$ns


    # Observe deployment command ----
    observeEvent(input$deploy,{
      iv$enable()
      if (iv$is_valid()) {

        pool = golem::get_golem_options("pool")
        pool_config = golem::get_golem_options("pool_config")

        make_widget_tables(pool = pool,
                           pool_config = pool_config,
                           write_widget_tables = FALSE)

        start_config = RMariaDB::dbReadTable(pool_config, "start_config")
        prod_mode = start_config$production_mode

        if(prod_mode != "production"){
          RMariaDB::dbRemoveTable(conn = pool_config, name = "start_config")
          RMariaDB::dbCreateTable(conn = pool_config,
                                  name = "start_config",
                                  fields = data.frame(production_mode = "production"))
          RMariaDB::dbAppendTable(conn = pool_config,
                                  name = "start_config",
                                  value = data.frame(production_mode = "production"))
        }else{
          warning("The prodcution mode has been activated (by other user?)! Latest changes mgith not have been saved!")
        }

        close()
        session$reload()
      }
    })


    # Observe mandatory fields ----
    iv <- InputValidator$new()
    iv$add_rule("confirm_deployment", sv_required())
    iv$add_rule("confirm_deployment", function(value) {
      if (value != "activate production") {
        "Type 'activate production' to confirm!"
      }
    })


    close <- function() {
      removeModal()
      iv$disable()
    }

  })
}

## To be copied in the UI
# mod_module_deploy_ui("module_deploy_1")

## To be copied in the server
# mod_module_deploy_server("module_deploy_1")
