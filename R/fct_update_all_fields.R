#' update_all_fields
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
#' @importFrom shiny updateTextInput
#'
update_all_fields = function(session, db_data, widget_data){

  saveRDS(list(db_data = db_data, widget_data = widget_data), "zz_update.RDS")
  widget_ids = widget_data$inputId


  lapply(widget_ids, function(x){

    widget_type = widget_data[widget_data$inputId == x,"type"]

    if(widget_type == "textInput"){
      shiny::updateTextInput(inputId = x, session = session, value = db_data[,x])
    }


  })

}
