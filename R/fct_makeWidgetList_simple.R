#' makeWidgetList_simple
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

# Function for simple widgets
makeWidgetList_simple = function(widget_data, ns, pid, tbl_id){
  define_widget(widget_data = widget_data, ns = ns, pid = pid, tbl_id = tbl_id, selection = 1:nrow(widget_data))
}
