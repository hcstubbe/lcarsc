#' update_all_fields
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
#' @importFrom shiny updateTextInput updateNumericInput updateSelectInput updateRadioButtons
#'
update_all_fields = function(session, db_data, widget_data, tbl_id = NULL){

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
      shiny::updateDateInput(session = session, inputId = x, value = val_x)
    }

    if(widget_type == "checkboxInput"){
      shiny::updateCheckboxInput(inputId = x, session = session, value = db_data[,x])
    }

    if(widget_type == "selectInput"){
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

    if(widget_type == "selectInputFromDatabase"){
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


    # This updates the checkboxGroupInputFromDatabase input for the entry_ids
    if(widget_type == "checkboxGroupInputFromDatabase" & x == "parent_ids"){
      selected = db_data[,x]
      selected_choicesFromVar = (widget_data %>% filter(inputId == x))$choicesFromVar
      pool = get_golem_options("pool")
      sql_df = loadData(pool, tbl_id) %>% filter(deleted_row == FALSE)
      all_values = sql_df[order(sql_df$order),selected_choicesFromVar]
      selected_value = db_data[,selected_choicesFromVar]
      choices = all_values[!all_values %in% selected_value]
      choices = choices[!is.na(choices) & !duplicated(choices) & choices != ""]
      names(choices) = choices
      # Remove child visits
      pool = get_golem_options("pool")
      all_visits = loadData(pool, "editor_table_visit") %>% filter(deleted_row == FALSE & is_child == FALSE)
      choices = choices[choices %in% all_visits$visit_id_visits]
      selected = unlist(strsplit(x = selected, split = ";;;;;"))
      shiny::updateCheckboxGroupInput(inputId = x, session = session, selected = selected, choices = choices)
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
#' update_all_fields
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
#' @importFrom shiny updateTextInput updateNumericInput updateSelectInput updateRadioButtons
#'
update_all_fields = function(session, db_data, widget_data, tbl_id, pool){

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
      shiny::updateDateInput(session = session, inputId = x, value = val_x)
    }

    if(widget_type == "checkboxInput"){
      shiny::updateCheckboxInput(inputId = x, session = session, value = db_data[,x])
    }

    if(widget_type == "selectInput"){
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

    if(widget_type == "selectInputFromDatabase"){
      selected = db_data[,x]
      if(is.null(selected)){
        selected = character(0)
      }else if(length(selected) == 0){
        selected = character(0)
      }else if(is.na(selected)){
        selected = character(0)
      }
      updateSelectizeInputFromDb_single(x, session, pool, tbl_id, widget_data, add_choices = NULL, selected = db_data[,x])
    }


    # This updates the checkboxGroupInputFromDatabase input for the entry_ids
    if(widget_type == "checkboxGroupInputFromDatabase" & x == "parent_ids"){
      selected = db_data[,x]
      selected_choicesFromVar = (widget_data %>% filter(inputId == x))$choicesFromVar
      pool = get_golem_options("pool")
      sql_df = loadData(pool, tbl_id) %>% filter(deleted_row == FALSE)
      all_values = sql_df[order(sql_df$order),selected_choicesFromVar]
      selected_value = db_data[,selected_choicesFromVar]
      choices = all_values[!all_values %in% selected_value]
      choices = choices[!is.na(choices) & !duplicated(choices) & choices != ""]
      names(choices) = choices
      # Remove child visits
      pool = get_golem_options("pool")
      all_visits = loadData(pool, "editor_table_visit") %>% filter(deleted_row == FALSE & is_child == FALSE)
      choices = choices[choices %in% all_visits$visit_id_visits]
      selected = unlist(strsplit(x = selected, split = ";;;;;"))
      shiny::updateCheckboxGroupInput(inputId = x, session = session, selected = selected, choices = choices)
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
