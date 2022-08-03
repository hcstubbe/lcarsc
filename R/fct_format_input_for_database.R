#' format_input_for_database
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @importFrom golem get_golem_options
#'
#' @noRd
format_input_for_database = function(input_data,
                                     pid,
                                     input_uuid,
                                     visit_id,
                                     widgets_table,
                                     all_visits,
                                     create_new_pid,
                                     create_sample_id,
                                     sample_id_name,
                                     noletters_smp_id,
                                     tbl_id,
                                     entry_id){

  input_data = lapply(input_data, function(x){ # convert NULL to NA
    y = length(x)
    if(y == 0){
      NA
    }else{x}
  })

  # Convert dates into characters
  input_data = lapply(input_data, function(x) {
    if(class(x) == "Date"){
      x = as.character(x)
    }
    return(x)
  })

  input_data = data.frame(input_data)

  # Replace "New choice" with actual value from new choice text input
  choicesFromVar = !sapply(widgets_table$choicesFromVar, function(x) is.null(x) | x == "" | is.na(x))
  choicesFromVar = names(choicesFromVar) %in% colnames(input_data) & choicesFromVar
  if(any(choicesFromVar)){
    for(i in which(choicesFromVar)){
      if(input_data[,widgets_table$choicesFromVar[i]] != "" & widgets_table$type[i] != "checkboxGroupInputFromDatabase"){
        input_data[,widgets_table$inputId[i]] = input_data[,widgets_table$choicesFromVar[i]]
      }
    }
  }


  # Set conditional variables to NA, if their condition (appear_if) is FALSE
  if(any(widgets_table$conditional == TRUE)) {
    appear_if = widgets_table$appear_if
    appear_if[appear_if == ""] = NA
    appear_if = sub("input.", "input_data$", appear_if, fixed = TRUE)
    appear_if = gsub("input.", "input_data$", appear_if, fixed = TRUE)
    appear_if = gsub("!input.", "!input_data$", appear_if, fixed = TRUE)
    appear_if = gsub("|input.", "|input_data$", appear_if, fixed = TRUE)
    appear_if = gsub("&input.", "&input_data$", appear_if, fixed = TRUE)

    appear_if = sapply(appear_if,
                       function(x) {
                         y = tryCatch(eval(parse(text = x)), error = function(e) NA)
                         if(length(y) == 0){
                           y = NA
                         }
                         y
                       },
                       simplify = TRUE,
                       USE.NAMES = FALSE)
    appear_if = unlist(appear_if)

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
  input_data$user_modified = get_current_user()
  input_data$date_modified = as.character(date())
  input_data$deleted_row = FALSE
  input_data$submitted_row = FALSE
  input_data$entry_id = entry_id

  if(!is.null(input_data$inputId)){
    input_data$inputId = make.names(input_data$inputId)
  }

  if(!is.null(input_data$visit_id_visits)){
    input_data$visit_id_visits = make.names(input_data$visit_id_visits)
  }


  if(create_sample_id == TRUE & !is.null(sample_id_name)){
    pool = get_golem_options("pool")
    exisiting_sampleIDs = loadData(pool, tbl_id)[, sample_id_name]
    input_data[, sample_id_name] = randomIdGenerator(exisiting_IDs = exisiting_sampleIDs, noletters = noletters_smp_id)
  }

  return(input_data)

}
