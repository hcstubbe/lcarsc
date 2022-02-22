#' db_replace_tables
#'
#' @description Replaces existing tables with named list of tables. If a table does not exists, a new table is created.
#'
#' @return None.
#'
#' @importFrom RMariaDB dbRemoveTable dbCreateTable dbAppendTable dbExistsTable
#' @importFrom dplyr recode mutate across
#'
#' @noRd
db_replace_tables = function(conn, table_list){

  for ( i in names(table_list) ) {

    table_list_i = table_list[[i]] %>% dplyr::mutate(across(where(is.logical), as.character))

    if(RMariaDB::dbExistsTable(conn = conn,
                               name = i)){
      RMariaDB::dbRemoveTable(conn = conn,
                              name = i)
    }

    sql_fields = dplyr::recode(unlist(lapply(table_list_i, class)),
                               "integer" = "INTEGER",
                               "double" = "DOUBLE",
                               "character" = "TEXT",
                               "logical" = "TEXT")
    names(sql_fields) = names(table_list_i)

    RMariaDB::dbCreateTable(conn = conn,
                            name = i,
                            fields = sql_fields)

    RMariaDB::dbAppendTable(conn = conn,
                            name = i,
                            value = table_list_i)

  }

}
