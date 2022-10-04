#' get_production_mode
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @importFrom RMariaDB dbListTables dbCreateTable dbAppendTable
#'
#' @noRd
#'
#'
get_production_mode = function(production_mode, pool_config){


  if( !is.null(production_mode ) ){
    return(production_mode)
  }


  tabels_config = RMariaDB::dbListTables(pool_config)
  if( "start_config" %in% tabels_config ){
    start_config = RMariaDB::dbReadTable(pool_config, "start_config")
    production_mode = start_config$production_mode
    if(length(production_mode) == 0){
    production_mode = "editor"
    RMariaDB::dbCreateTable(conn = pool_config,
                            name = "start_config",
                            fields = data.frame(production_mode = production_mode, tested_ecrf = 'FALSE'))
    RMariaDB::dbAppendTable(conn = pool_config,
                            name = "start_config",
                            value = data.frame(production_mode = production_mode, tested_ecrf = 'FALSE'))
    }
    return(production_mode)
  }else{
    production_mode = "editor"
    RMariaDB::dbCreateTable(conn = pool_config,
                            name = "start_config",
                            fields = data.frame(production_mode = production_mode, tested_ecrf = 'FALSE'))
    RMariaDB::dbAppendTable(conn = pool_config,
                            name = "start_config",
                            value = data.frame(production_mode = production_mode, tested_ecrf = 'FALSE'))
    return(production_mode)

  }




}
