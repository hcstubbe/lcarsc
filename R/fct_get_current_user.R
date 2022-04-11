#' get_current_user
#'
#' @description This function reads the current user using
#'
#' @importFrom golem get_golem_options
#' @importFrom RMariaDB dbReadTable
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

get_current_user = function(return_user = TRUE) {
  db_settgins_data = RMariaDB::dbReadTable(get_golem_options("pool_config"), "server_db_settings_tbl")

  if(return_user){
    env_user_name = db_settgins_data$env_user_name
    user_name = Sys.getenv(env_user_name)
    return(user_name)
  }else{
    env_user_group = db_settgins_data$env_user_group
    user_group = Sys.getenv(env_user_group)
    return(user_group)
  }

}
