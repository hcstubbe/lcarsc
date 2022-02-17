#' read_widget_data
#'
#' @description Reads widget data from temp folder if exists.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
read_widget_data = function(app_data_internal, production_mode, db_path = "tmp_widgetdata/tmp_widgetdata.RDS"){
  if(production_mode != "editor"){
    return(NULL)
  }
  if(file.exists(db_path)){
    app_data_internal = readRDS("~/lcarsc/tmp_widgetdata/tmp_widgetdata.RDS")
  }
  return(app_data_internal)
}

read_widget_data_path = function(production_mode, widget_data_path = "tmp_widgetdata/tmp_widgetdata.RDS"){
  if(production_mode != "editor"){
    return(NULL)
  }

  return(widget_data_path)

}
