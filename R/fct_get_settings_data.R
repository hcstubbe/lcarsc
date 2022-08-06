#' get_settings_data
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
get_settings_data = function(pool_config){
  if(RMariaDB::dbExistsTable(pool_config, "server_settings_tbl")){
    settgins_data = RMariaDB::dbReadTable(pool_config, "server_settings_tbl")
  }else{
    settgins_data = data.frame(add_child_visits = FALSE,
                               add_samples_panel = FALSE)
  }
  for(i in c("add_child_visits",
             "add_samples_panel")){
    if(is.null(settgins_data[,i])){
      settgins_data[,i] = FALSE
    }
  }
  return(settgins_data)

}
