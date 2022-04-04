#' randomIdGenerator
#'
#' @description This function generates random IDs and checks, if ID already exists in the database
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

# Generates the random id
randomIdGenerator = function(exisiting_IDs = NULL, noletters = FALSE){


  # Get prefix
  pool_config = get_golem_options("pool_config")
  server_settings_tbl_id = "server_settings_tbl"
  if(is.element(server_settings_tbl_id, RMariaDB::dbListTables(pool_config))){
    db_settgins_data = RMariaDB::dbReadTable(pool_config, server_settings_tbl_id)
    id_prefix = db_settgins_data$pid_prefix
  }else{
    id_prefix = NULL
  }

  # This function generates random IDs
  generate_id = function(noletters, id_prefix){

    letters = c("A","B","C","E","F","H","K","L","M","N","P","T","W","X","Z")

    if(noletters == TRUE){
      letters = ""
    }

    id = paste0(sample(letters, size = 1),
                sample(100:999, size = 1),
                sample(letters, size = 1),
                sample(100:999, size = 1))

    if( !is.null(id_prefix) ){
      if(id_prefix != "" & length(id_prefix) > 0){
        id = paste0(id_prefix, "-", id)
      }
    }

    return(id)
  }


  # Generate ID
  id = generate_id(noletters, id_prefix)




  # Test whether ID exists, if existing IDs are provided and replace if needed
  if(!is.null(exisiting_IDs) & length(exisiting_IDs) > 0){
    count = 0
    while (id %in% exisiting_IDs) {
      id = generate_id(noletters, id_prefix)
      count = count + 1
      if(count > 1000){
        stop("Cannot generate unique ID after 1000 repetitions!")
      }
    }
  }


  id

}
