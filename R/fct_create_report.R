#' create_report
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @importFrom knitr spin
#'
#' @noRd
create_report = function(pool){

  report_data = db_read_select(pool = pool,
                               tbl_id = "report_editor_table_vars",
                               pid_x = NULL,
                               filter_deleted_rows = TRUE,
                               use.pid = FALSE)
  report_data = report_data %>% dplyr::select(which(colnames(.) == "inputId"):ncol(.))

  return(report_data)

  # r_code = "plot(cars)
  #           print(3)"
  #
  # knitr::spin(text = r_code)

}
