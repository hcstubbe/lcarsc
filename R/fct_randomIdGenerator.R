#' randomIdGenerator
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

# Generates the random id
randomIdGenerator = function(exisiting_IDs = NULL){


  # Get prefix
  pool_config = get_golem_options("pool_config")
  server_settings_tbl_id = "server_settings_tbl"
  if(is.element(server_settings_tbl_id, RMariaDB::dbListTables(pool_config))){
    db_settgins_data = RMariaDB::dbReadTable(pool_config, server_settings_tbl_id)
    pid_prefix = db_settgins_data$pid_prefix
  }else{
    pid_prefix = NULL
  }


  # This function generates random IDs
  generate_pid = function(){
    letters = c("A","B","C","E","F","H","K","L","M","N","P","T","W","X","Z")
    paste(sample(letters, size = 1),
          sample(100:999, size = 1),
          sample(letters, size = 1),
          sample(100:999, size = 1),
          sep = "-")
  }


  # Generate PID
  pid = generate_pid()

  if( !is.null(pid_prefix) ){
    if(pid_prefix != "" & length(pid_prefix) > 0){
      pid = paste0(pid_prefix, "-", pid)
    }
  }


  # Test whether PID exists, if existing IDs are provided and replace if needed
  if(!is.null(exisiting_IDs) & length(exisiting_IDs) > 0){
    count = 0
    while (pid %in% exisiting_IDs) {
      pid = generate_pid()
      count = count + 1
      if(count > 1000){
        stop("Cannot generate unique PID after 1000 repetitions!")
      }
    }
  }


  pid

}
