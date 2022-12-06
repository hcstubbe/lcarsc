#' updateSelectizeInputFromDb
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' updateSelectizeInputFromDb
#'
#' @description A fct function for updating selectizeInputs from database
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
updateSelectizeInputFromDb = function(session, pool, tbl_id, widget_data){


  widget_ids = widget_data$inputId

  lapply(widget_ids, function(x){

    widget_type = widget_data[widget_data$inputId == x,"type"]

    if(widget_type == "selectInputFromDatabase"){
      choices_var_col = widget_data[widget_data$inputId == x,"choicesFromVar"]
      choice_name_col = widget_data[widget_data$inputId == x,"namesFromVar"]
      tbl_id = widget_data[widget_data$inputId == x,"tbl_id"]
      db_cmd = paste(sep = " ", "SELECT", paste(choices_var_col, choice_name_col, sep = ", "), "FROM", tbl_id)

      choice_data = RMariaDB::dbGetQuery(pool, db_cmd)
      choices = choice_data[,choices_var_col]
      names(choices) = choice_data[,choice_name_col]

      shiny::updateSelectizeInput(inputId = x, session = session, selected = NULL, choices =  choices, server = TRUE)
    }

  })

}

