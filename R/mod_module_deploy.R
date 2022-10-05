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
    strong("Caution:"), "moving from 'editor' to 'production' ", strong("cannot be reversed completely!"),
    br(),
    br(),
    "Please make sure, ", strong("you tested"), "the eCRF under 'Preview'!",
    br(),
    br(),
    "The deployment will ", strong("update the eCRF from the data base."), " Make sure, you tested using 'Preview' after updating!",
    br(),
    br(),
    br(),
    strong("Please acknowledge the following:"),
    shiny::checkboxInput(inputId = ns("check_groups"), "User groups are checked (see database setup)."),
    shiny::checkboxInput(inputId = ns("check_tested"), "The eCRF has been tested."),
    shiny::checkboxInput(inputId = ns("check_limited_rollback"), "Changes cannot be rolled back completely."),
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

    pool = golem::get_golem_options("pool")
    pool_config = golem::get_golem_options("pool_config")


    # Observe deployment command ----
    observeEvent(input$deploy,{



      isadmin = user_is_admin(pool_config = pool_config,
                    start_as_admin = get_golem_options("user_is_admin"))

      iv$enable()
      if (iv$is_valid()) {

        start_config = RMariaDB::dbReadTable(pool_config, "start_config")
        prod_mode = start_config$production_mode
        tested_ecrf = start_config$tested_ecrf

        if(isadmin == FALSE){
          warning("The user must be admin to deploy!")
          shiny::showNotification(ui = "The user must be admin to deploy!", duration = NULL, type = "error")
        }else if(prod_mode != "production" & tested_ecrf == TRUE & isadmin == TRUE){
          make_widget_tables(pool = pool,
                             pool_config = pool_config,
                             ecrf_test = FALSE)

          RMariaDB::dbRemoveTable(conn = pool_config, name = "start_config")
          RMariaDB::dbCreateTable(conn = pool_config,
                                  name = "start_config",
                                  fields = data.frame(production_mode = "production", tested_ecrf = 'FALSE'))
          RMariaDB::dbAppendTable(conn = pool_config,
                                  name = "start_config",
                                  value = data.frame(production_mode = "production", tested_ecrf = 'FALSE'))
          close()
          session$reload()
        }else if(prod_mode != "production" & tested_ecrf != TRUE & isadmin == TRUE){
          warning("The eCRF has not been built and tested after latest changes!")
          shiny::showNotification(ui = "The eCRF has not been built and tested after latest changes! Hit build in the editor and test!", duration = NULL, type = "error")
        }else if(isadmin == TRUE){
          warning("The prodcution mode has been activated (by other user?)! Latest changes might not have been saved!")
          shiny::showNotification(ui = "The prodcution mode has been activated (by other user?)! Latest changes might not have been saved!", duration = NULL, type = "error")
        }else{
          warning("Unkown error occured in mod_module_deploy!")
          shiny::showNotification(ui = "Unkown error occured in mod_module_deploy!", duration = NULL, type = "error")
        }
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
    iv$add_rule("check_groups", sv_equal(TRUE, message_fmt = "Required"))
    iv$add_rule("check_tested", sv_equal(TRUE, message_fmt = "Required"))
    iv$add_rule("check_limited_rollback", sv_equal(TRUE, message_fmt = "Required"))


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
