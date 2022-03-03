#' update_all_fields
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
#' @importFrom shiny updateTextInput updateNumericInput updateSelectInput updateRadioButtons
#' @importFrom shinyWidgets updateAirDateInput
#'
update_all_fields = function(session, db_data, widget_data){

  widget_ids = widget_data$inputId


  lapply(widget_ids, function(x){

    widget_type = widget_data[widget_data$inputId == x,"type"]

    if(widget_type == "textInput"){
      shiny::updateTextInput(inputId = x, session = session, value = db_data[,x])
    }

    if(widget_type == "numericInput" | widget_type == "numericInputCouter"){
      shiny::updateNumericInput(inputId = x, session = session, value = db_data[,x])
    }

    val_x = db_data[,x]
    if(widget_type == "dateInput" & !is.null(val_x)){
      if(is.na(val_x)){
        val_x = NULL
      }
      shinyWidgets::updateAirDateInput(session = session, inputId = x, value = val_x)
    }

    if(widget_type == "checkboxInput"){
      shiny::updateCheckboxInput(inputId = x, session = session, value = db_data[,x])
    }

    if(widget_type == "selectInput" | widget_type == "selectInputFromDatabase"){
      selected = db_data[,x]
      if(is.null(selected)){
        selected = character(0)
      }else if(length(selected) == 0){
        selected = character(0)
      }else if(is.na(selected)){
        selected = character(0)
      }
      shiny::updateSelectInput(inputId = x, session = session, selected = selected)
    }

    if(widget_type == "radioButtons"){
      selected = db_data[,x]
      if(is.null(selected)){
        selected = character(0)
      }else if(length(selected) == 0){
        selected = character(0)
      }else if(is.na(selected)){
        selected = character(0)
      }
      shiny::updateRadioButtons(inputId = x, session = session, selected = selected)
    }


  })

}
