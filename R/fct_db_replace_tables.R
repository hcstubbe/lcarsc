#' db_replace_tables
#'
#' @description Replaces existing tables with named list of tables. If a table does not exists, a new table is created.
#'
#' @return None.
#'
#' @importFrom RMariaDB dbRemoveTable dbCreateTable dbAppendTable dbExistsTable
#'
#' @noRd
db_replace_tables = function(pool, table_list){

  for ( i in names(table_list) ) {

    if(RMariaDB::dbExistsTable(pool, i)){
      RMariaDB::dbRemoveTable(conn = pool,
                              name = i)
    }

    RMariaDB::dbCreateTable(conn = pool,
                            name = i,
                            fields = table_list[[i]])

    RMariaDB::dbAppendTable(conn = pool,
                            name = i,
                            value = table_list[[i]])

  }

}
