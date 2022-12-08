#' updateSelectizeInputFromDb
#'
#' @description A fct function for updating selectizeInputs from database
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

updateSelectizeInputFromDb = function(session, pool, tbl_id, widget_data, selected = NULL, add_choices = NULL){


  widget_ids = widget_data$inputId

  lapply(widget_ids, function(x){

    widget_type = widget_data[widget_data$inputId == x,"type"]
    widget_type = widget_type[widget_type == "selectInputFromDatabase"]

    if(length(widget_type) > 0){
      choices_var_col = widget_data[widget_data$inputId == x,"choicesFromVar"]
      choice_name_col = widget_data[widget_data$inputId == x,"namesFromVar"]
      tbl_id = widget_data[widget_data$inputId == x,"tbl_id"]
      db_cmd = paste(sep = " ", "SELECT", paste(choices_var_col, choice_name_col, sep = ", "), "FROM", tbl_id)

      choice_data = RMariaDB::dbGetQuery(pool, db_cmd)
      choices = choice_data[,choices_var_col]
      names(choices) = choice_data[,choice_name_col]
      choices = c("NA" = NA, choices)

      # if(sum(!is.na(selected)) > 0 & !(selected %in% choices)){
      #   if(is.null(names(selected))){ names(selected) = selected }
      #   choices = c(choices, selected)
      # }

      shiny::updateSelectizeInput(inputId = x, session = session, selected = selected, choices = choices, server = TRUE)
    }

  })

}


updateSelectizeInputFromDb_single = function(x, session, pool, tbl_id, widget_data, add_choices = NULL, selected = NULL){

  widget_type = widget_data[widget_data$inputId == x,"type"]
  widget_type = widget_type[widget_type == "selectInputFromDatabase"]

  if(length(widget_type) > 0){
    choices_var_col = widget_data[widget_data$inputId == x,"choicesFromVar"]
    choice_name_col = widget_data[widget_data$inputId == x,"namesFromVar"]
    tbl_id = widget_data[widget_data$inputId == x,"tbl_id"]
    db_cmd = paste(sep = " ", "SELECT", paste(choices_var_col, choice_name_col, sep = ", "), "FROM", tbl_id)

    choice_data = RMariaDB::dbGetQuery(pool, db_cmd)
    choices = choice_data[,choices_var_col]
    names(choices) = choice_data[,choice_name_col]

    if(sum(!is.na(selected)) > 0 & !(selected %in% choices)){
      if(is.null(names(selected))){ names(selected) = selected }
      choices = c(choices, selected)
    }

    shiny::updateSelectizeInput(inputId = x, session = session, selected = selected, choices = choices, server = TRUE)
  }

}

