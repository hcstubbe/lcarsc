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
    tryCatch(
      mod_module_launcher_server("mod_module_launcher_1")
      ,
      error = function(e) {
        showNotification(paste0("Starting production launcher failed: ", e), duration = 10, type = "error")
        return(NULL)
      }
    )
  }
  if(prod_mod == "editor"){
    tryCatch(
      mod_module_launcher_edit_server("mod_module_launcher_edit_1")
      ,
      error = function(e) {
        showNotification(paste0("Starting production launcher failed: ", e), duration = 10, type = "error")
        return(NULL)
      }
    )
  }
}
