#' db_read_select
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @importFrom RMariaDB dbReadTable

# Read table and filter
db_read_select = function(pool, tbl_id, pid_x, filter_deleted_rows = TRUE, use.pid = TRUE, filter_sumitted_rows = FALSE, order.by = NULL, filter_panel = NULL){
  tab_i = dbReadTable(pool, tbl_id)

  if(!is.null(order.by)){
    tab_i = tab_i[order(tab_i[,order.by]),]
  }

  if(filter_deleted_rows == TRUE){
    tab_i = tab_i %>% filter(deleted_row == FALSE)
  }

  if(filter_sumitted_rows == TRUE){
    tab_i = tab_i %>% filter(submitted_row == TRUE)
  }

  if(use.pid == FALSE & is.null(filter_panel)){
    return(tab_i)
  }

  if(!is.null(filter_panel)){
    tab_i = tab_i %>% filter(panel == filter_panel)
    return(tab_i)
  }

  if(length(pid_x) == 0){
    return(NULL)
  }
  tab_i = tab_i %>% filter(pid == pid_x)

  tab_i
}
