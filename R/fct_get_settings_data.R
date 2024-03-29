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
    if(!("add_child_visits" %in% colnames(settgins_data))){
      settgins_data$add_child_visits = FALSE
    }
    if(!("allow_manual_pid" %in% colnames(settgins_data))){
      settgins_data$allow_manual_pid = FALSE
    }
    if(!("add_samples_panel" %in% colnames(settgins_data))){
      settgins_data$add_samples_panel = FALSE
    }
  }else{
    settgins_data = data.frame(add_child_visits = FALSE,
                               allow_manual_pid = FALSE,
                               add_samples_panel = FALSE)
  }

  for(i in c("add_child_visits",
             "allow_manual_pid",
             "add_samples_panel")){
    if(is.null(settgins_data[,i])){
      settgins_data[,i] = FALSE
    }
    if(is.na(settgins_data[,i])){
      settgins_data[,i] = FALSE
    }
  }

  return(settgins_data)

}
