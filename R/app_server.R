#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @importFrom golem get_golem_options
#' @noRd
app_server <- function( input, output, session ) {

  pool_config = golem::get_golem_options("pool_config")
  prod_mod = golem::get_golem_options("production_mode")

  prod_mod = get_production_mode(production_mode = prod_mod,
                                 pool_config = pool_config )


  # Your application server logic
  if(prod_mod == "production"){
    mod_module_launcher_server("mod_module_launcher_1")
  }
  if(prod_mod == "editor"){
    mod_module_launcher_edit_server("mod_module_launcher_edit_1")
  }
}
