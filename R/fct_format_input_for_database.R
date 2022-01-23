#' format_input_for_database
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
format_input_for_database = function(input_data,
                                     pid,
                                     input_uuid,
                                     visit_id,
                                     widgets_table,
                                     all_visits,
                                     create_new_pid){

  input_data = lapply(input_data, function(x){ # convert NULL to NA
    y = length(x)
    if(y == 0){
      NA
    }else{x}
  })

  input_data = data.frame(input_data)

  # Replace "New choice" with actual value from new choice text input
  choicesFromVar = !sapply(widgets_table$choicesFromVar, function(x) is.null(x) | x == "" | is.na(x))
  choicesFromVar = names(choicesFromVar) %in% colnames(input_data) & choicesFromVar
  if(any(choicesFromVar)){
    for(i in which(choicesFromVar)){
      if(input_data[,widgets_table$choicesFromVar[i]] != ""){
        input_data[,widgets_table$inputId[i]] = input_data[,widgets_table$choicesFromVar[i]]
      }
    }
  }


  # Set conditional variables to NA, if their condition (appear_if) is FALSE
  if(any(widgets_table$conditional == TRUE)) {
    appear_if = widgets_table$appear_if
    appear_if[appear_if == ""] = NA
    appear_if = sub("input.", "input_data$", appear_if, fixed = TRUE)
    appear_if = gsub(" input.", "input_data$", appear_if, fixed = TRUE)
    appear_if = gsub("!input.", "!input_data$", appear_if, fixed = TRUE)
    appear_if = gsub("|input.", "|input_data$", appear_if, fixed = TRUE)
    appear_if = gsub("&input.", "&input_data$", appear_if, fixed = TRUE)

    appear_if = sapply(appear_if,
                       function(x) eval(parse(text = x)),
                       simplify = TRUE,
                       USE.NAMES = FALSE)
    appear_if[is.na(appear_if)] = FALSE
    widgets_table$appear_if_true = appear_if

    inputs_to_set_na = widgets_table$inputId[widgets_table$appear_if_true == FALSE & widgets_table$conditional == TRUE]
    for (i in inputs_to_set_na) {
      input_data[1,i] = NA
    }
  }

  if(create_new_pid){
    pool = get_golem_options("pool")
    input_data$pid = randomIdGenerator(exisiting_IDs = loadData(pool, "inclusion_dataset")$pid)
  }else{
    input_data$pid = pid
  }

  input_data$row_id = input_uuid
  input_data$visit_id = all_visits[all_visits[,"visit_id"] == visit_id,"visit_title"]
  input_data$user_modified = Sys.getenv("SHINYPROXY_USERNAME")
  input_data$date_modified = as.character(date())
  input_data$deleted_row = FALSE
  input_data$submitted_row = FALSE

  if(!is.null(input_data$inputId)){
    input_data$inputId = make.names(input_data$inputId)
  }
  if(!is.null(input_data$visit_id_visits)){
    input_data$visit_id_visits = make.names(input_data$visit_id_visits)
  }

  return(input_data)

}
