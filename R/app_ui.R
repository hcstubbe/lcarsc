#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @importFrom shiny tagList NS
#' @importFrom golem get_golem_options
#' @noRd
app_ui <- function(request) {

  pool_config = get_golem_options("pool_config")
  prod_mod = get_golem_options("production_mode")

  prod_mod = get_production_mode(production_mode = prod_mod,
                                        pool_config = pool_config )



  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    if(prod_mod == "production"){
      mod_module_launcher_ui("mod_module_launcher_1")
    }else if(prod_mod == "editor"){
      mod_module_launcher_edit_ui("mod_module_launcher_edit_1")
    }

  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @importFrom shiny tags
#' @noRd
golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'lcarsc'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

