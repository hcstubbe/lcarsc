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

    sql_fields = dplyr::recode(unlist(lapply(table_list[[i]], class)),
                               "integer" = "INTEGER",
                               "double" = "DOUBLE",
                               "character" = "TEXT",
                               "logical" = "BOOLEAN")
    names(sql_fields) = names(table_list[[i]])

    RMariaDB::dbCreateTable(conn = conn,
                            name = i,
                            fields = sql_fields)

    RMariaDB::dbAppendTable(conn = conn,
                            name = i,
                            value = table_list[[i]])

  }

}
