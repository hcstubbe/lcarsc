#' user_is_admin
#'
#' @description A fct function
#'
#' @return Returns TRUE if the user is member of the admin group. The user must be member of the admin group ONLY.
#'
#' @param start_as_admin If start_as_admin is TRUE, the user will be 'admin'.
#'
#' @importFrom golem get_golem_options
#' @importFrom RMariaDB dbReadTable
#'
#' @noRd
user_is_admin = function(pool_config, start_as_admin = FALSE) {

  if(!is.null(start_as_admin)){
    if(start_as_admin == TRUE){
      return(TRUE)
    }else if(start_as_admin == FALSE){
      return(FALSE)
    }else{
      stop("start_as_admin must be either TRUE, FALSE or NULL!")
    }
  }


  # Get database settings data
  db_settgins_data = RMariaDB::dbReadTable(pool_config, "server_db_settings_tbl")


  # Check if user is admin
  env_user_group = db_settgins_data$env_user_group
  user_group = Sys.getenv(env_user_group)
  admin_group = db_settgins_data$group_admin

  is_admin = user_group == admin_group

  if (length(is_admin) == 0) {
    return(FALSE)
  }
  if( admin_group == "" ) {
    return(FALSE)
  }

  return(is_admin)

}
