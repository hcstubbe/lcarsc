#' db_replace_tables
#'
#' @description Replaces existing tables with named list of tables. If a table does not exists, a new table is created.
#'
#' @return None.
#'
#' @importFrom RMariaDB dbRemoveTable dbCreateTable dbAppendTable dbExistsTable
#'
#' @noRd
db_replace_tables = function(conn, table_list){

  for ( i in names(table_list) ) {

    if(RMariaDB::dbExistsTable(conn = conn,
                               name = i)){
      RMariaDB::dbRemoveTable(conn = conn,
                              name = i)
    }

    RMariaDB::dbCreateTable(conn = conn,
                            name = i,
                            fields = table_list[[i]])

    RMariaDB::dbAppendTable(conn = conn,
                            name = i,
                            value = table_list[[i]])

  }

}
