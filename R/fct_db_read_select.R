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
                          pid_x,
                          filter_deleted_rows = TRUE,
                          use.pid = TRUE,
                          filter_sumitted_rows = FALSE,
                          order.by = NULL,
                          filter_origin = NULL,
                          order_desc = FALSE,
                          oder_by_date = FALSE){


  tab_i = dbReadTable(pool, tbl_id)


  if(!is.null(order.by)){
    if(oder_by_date == TRUE){
      new_order = as.POSIXct(as.character(tab_i[,order.by]), format = "%a %b %d %H:%M:%S %Y")
    }
    new_order = order(new_order, decreasing = order_desc)
    tab_i = tab_i[new_order,]
  }


  if(filter_deleted_rows == TRUE){
    tab_i = tab_i %>% filter(deleted_row == FALSE)
  }


  if(filter_sumitted_rows == TRUE){
    tab_i = tab_i %>% filter(submitted_row == TRUE)
  }


  if(use.pid == FALSE & is.null(filter_origin)){
    return(tab_i)
  }


  if(!is.null(filter_origin)){
    if(length(filter_origin) > 0){
      tab_i = tab_i %>% filter(origin_of_var == filter_origin & deleted_row == FALSE)
    }else{
      tab_i = data.frame(origin = character(0))
    }
    return(tab_i)
  }


  if(length(pid_x) == 0){
    return(NULL)
  }
  tab_i = tab_i %>% filter(pid == pid_x)


  return(tab_i)
}
