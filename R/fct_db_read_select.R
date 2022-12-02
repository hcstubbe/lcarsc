#' db_read_select
#'
#' @description This function reads, orders and filters widget data.
#'
#' @return Returns a filtered and ordered table from the database.
#'
#' @noRd
#' @importFrom RMariaDB dbReadTable

# Read table and filter
db_read_select = function(pool,
                          tbl_id,
                          pid_x = NULL,
                          filter_deleted_rows = TRUE,
                          use.pid = TRUE,
                          filter_sumitted_rows = FALSE,
                          order.by = NULL,
                          filter_origin = NULL,
                          order_desc = FALSE,
                          oder_by_date = FALSE,
                          entry_id = NULL,
                          filter_entry_id = FALSE,
                          row_id = NULL,
                          select_cols = "*",
                          count_rows = FALSE
                          ){


  if(filter_entry_id == TRUE & length(entry_id) == 0){
    return(NULL)
  }

  if(use.pid == TRUE & length(pid_x) == 0){
    return(NULL)
  }

  if(use.row_id == TRUE & length(row_id) == 0){
    return(NULL)
  }

  select_cols = paste(select_cols, collapse = ", ")


  sql_params = if(is.null(row_id)){
    paste(collapse = " AND ",
          c(
            use.pid = if(use.pid == TRUE){paste0("pid = '", pid_x, "'")}else{NULL},
            filter_deleted_rows = if(filter_deleted_rows == TRUE){"deleted_row = 0"}else{NULL},
            filter_sumitted_rows = if(filter_sumitted_rows == TRUE){"submitted_row = 1"}else{NULL},
            filter_entry_id = if(filter_entry_id == TRUE){paste0("entry_id_parent = '", entry_id, "'")}else{NULL},
            filter_origin = if(!is.null(filter_origin)){paste0("origin_of_var = '", filter_origin, "'")}else{NULL}
          )
    )
  }else{
    paste0("row_id = '", row_id, "'")
  }

  # Get row count only
  if(count_rows == TRUE){
    if(sql_params != ""){
      db_cmd = paste(sep = " ", "SELECT COUNT(*) FROM", tbl_id, "WHERE", sql_params)
    }else{
      db_cmd = paste(sep = " ", "SELECT COUNT(*) FROM", tbl_id)
    }
    row_count = (RMariaDB::dbGetQuery(pool, db_cmd))
    row_count = as.numeric(row_count[[1]])
    return(row_count)
  }


  # Get table content
  if(sql_params != ""){
    db_cmd = paste(sep = " ", "SELECT", select_cols, "FROM", tbl_id, "WHERE", sql_params)
  }else{
    db_cmd = paste(sep = " ", "SELECT", select_cols, "FROM", tbl_id)
  }

  tab_i = (RMariaDB::dbGetQuery(pool, db_cmd))


  if(!is.null(order.by)){
    if(oder_by_date == TRUE){
      new_order = as.POSIXct(as.character(tab_i[,order.by]), format = "%a %b %d %H:%M:%S %Y")
    }else{
      new_order = tab_i[,order.by]
    }
    new_order = order(new_order, decreasing = order_desc)
    tab_i = tab_i[new_order,]
  }


  return(tab_i)
}
