#' read_widget_data
#'
#' @description Reads widget data from temp folder if exists.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
read_widget_data = function(app_data_internal){
  if(file.exists("tmp_widgetdata/tmp_widgetdata.RDS")){
    app_data_internal = readRDS("~/lcarsc/tmp_widgetdata/tmp_widgetdata.RDS")
  }
  return(app_data_internal)
}
