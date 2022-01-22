#' format_input_for_database
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
format_input_for_database = function(data, pid, visit_id){
  data = lapply(data, function(x){ # convert NULL to NA
    y = length(x)
    if(y == 0){
      NA
    }else{x}
  })

  data = data.frame(data, pid, input_uuid)
  # Replace "New choice" with actual value from new choice text input
  choicesFromVar = !sapply(widgets_table$choicesFromVar, function(x) is.null(x) | x=="" | is.na(x))
  choicesFromVar = names(choicesFromVar) %in% colnames(data) & choicesFromVar
  if(any(choicesFromVar)){
    for(i in which(choicesFromVar)){
      if(data[,widgets_table$choicesFromVar[i]] != ""){
        data[,widgets_table$inputId[i]] = data[,widgets_table$choicesFromVar[i]]
      }
    }
  }

  if(create_new_pid){
    data$pid = randomIdGenerator(exisiting_IDs = loadData(pool, "inclusion_dataset")$pid)
  }else{
    data$pid = pid
  }
  data$row_id = input_uuid
  data$visit_id = all_visits[all_visits[,"visit_id"] == visit_id,"visit_title"]
  data$user_modified = Sys.getenv("SHINYPROXY_USERNAME")
  data$date_modified = as.character(date())
  data$deleted_row = FALSE
  data$submitted_row = FALSE
  if(!is.null(data$inputId)){
    data$inputId = make.names(data$inputId)
  }
  if(!is.null(data$visit_id_visits)){
    data$visit_id_visits = make.names(data$visit_id_visits)
  }
  return(data)
}
