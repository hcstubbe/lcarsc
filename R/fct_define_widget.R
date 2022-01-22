#' define_widget
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd


# Function definfing each widget
define_widget = function(widget_data, ns, pid, tbl_id, selection){

  wlist_all = list()


  for(i in selection){

    wlist_i = NULL

    if(widget_data$type[i] == "numericInput"& widget_data$conditional[i] == FALSE){
      wlist_i = list(numericInput(inputId = ns(widget_data$inputId[i]),
                                  label = sub(" NA","", paste(widget_data$label[i],
                                                              widget_data$unit[i])),
                                  value = widget_data$value[i],
                                  max = widget_data$max[i],
                                  min = widget_data$min[i],
                                  step = widget_data$step[i]))
    }

    if(widget_data$type[i] == "radioButtons" & widget_data$conditional[i] == FALSE){
      choices = list(widget_data$choice1[[i]],
                     widget_data$choice2[[i]],
                     widget_data$choice3[[i]],
                     widget_data$choice4[[i]],
                     widget_data$choice5[[i]],
                     widget_data$choice6[[i]],
                     widget_data$choice7[[i]])
      choices = choices[!is.na(choices)]
      choices = choices[!is.na(choices) & !duplicated(choices) & choices != ""]
      wlist_i = list(
        radioButtons(inputId = ns(widget_data$inputId[i]),
                     label = widget_data$label[i],
                     choices = choices,
                     selected = widget_data$selected[i])
      )
    }

    if(widget_data$type[i] == "selectInput" & widget_data$conditional[i] == FALSE){
      choices = list(widget_data$choice1[[i]],
                     widget_data$choice2[[i]],
                     widget_data$choice3[[i]],
                     widget_data$choice4[[i]],
                     widget_data$choice5[[i]],
                     widget_data$choice6[[i]],
                     widget_data$choice7[[i]])
      choices = choices[!is.na(choices) & !duplicated(choices) & choices != ""]
      names(choices) = choices
      wlist_i = list(selectInput(inputId = ns(widget_data$inputId[i]),
                                 label = widget_data$label[i],
                                 choices = choices,
                                 selected = widget_data$selected[i])
      )
    }

    if(widget_data$type[i] == "selectInputFromDatabase" & widget_data$conditional[i] == FALSE){
      if(is.na(widget_data$tbl_id[[i]]) | widget_data$tbl_id[[i]] == ""){
        sql_df = loadData(pool, tbl_id) %>% filter(deleted_row == FALSE) # here loadData does not match the pid. Therefore the choices will be selected from all possible values from the referenced table.
        choices = c(sql_df[,widget_data$choicesFromVar[i]] %>% as.factor() %>% levels(),
                    sql_df[,widget_data$inputId[i]] %>% as.factor() %>% levels())
      }else{
        sql_df = loadData(pool, widget_data$tbl_id[[i]]) %>% filter(deleted_row == FALSE) # here loadData does not match the pid. Therefore the choices will be selected from all possible values from the referenced table.
        choices = c(sql_df[,widget_data$choicesFromVar[i]] %>% as.factor() %>% levels())
      }

      choices = c(widget_data$choice1[[i]],
                  widget_data$choice2[[i]],
                  widget_data$choice3[[i]],
                  widget_data$choice4[[i]],
                  widget_data$choice5[[i]],
                  widget_data$choice6[[i]],
                  widget_data$choice7[[i]],
                  choices)
      choices = choices[!is.na(choices) & !duplicated(choices) & choices != ""]
      names(choices) = choices
      wlist_i = list(selectInput(inputId = ns(widget_data$inputId[i]),
                                 label = widget_data$label[i],
                                 choices = choices,
                                 selected = widget_data$selected[i])
      )
    }

    if(widget_data$type[i] == "checkboxInput" & widget_data$conditional[i] == FALSE){
      wlist_i = list(checkboxInput(inputId = ns(widget_data$inputId[i]),
                                   label = widget_data$label[i],
                                   value = as.logical(widget_data$value[i]))
      )
    }

    if(widget_data$type[i] == "dateInput" & widget_data$conditional[i] == FALSE){
      wlist_i = list(dateInput(inputId = ns(widget_data$inputId[i]),
                               label = widget_data$label[i],
                               value = NULL
      )
      )
    }


    if(widget_data$type[i] == "textInput" & widget_data$conditional[i] == FALSE){
      wlist_i = list(textInput(inputId = ns(widget_data$inputId[i]),
                               label = widget_data$label[i], value = widget_data$value[i])
      )
    }

    if(widget_data$type[i] == "textInputForChoicesFromVar" & widget_data$conditional[i] == FALSE){
      wlist_i = list(textInput(inputId = ns(widget_data$inputId[i]),
                               label = widget_data$label[i], value = widget_data$value[i])
      )
    }



    if(widget_data$type[i] == "numericInputCouter" & widget_data$conditional[i] == FALSE){
      var_count = max(db_read_select(pool, tbl_id, "vars")[,"order_of_var"]) + 1
      wlist_i = list(numericInput(inputId = ns(widget_data$inputId[i]),
                                  label = sub(" NA","", paste(widget_data$label[i],
                                                              widget_data$unit[i])),
                                  value = var_count,
                                  max = widget_data$max[i],
                                  min = widget_data$min[i],
                                  step = widget_data$step[i]))
    }
    ####---------- Conditional inputs ----------####

    if(widget_data$type[i] == "textInput" & widget_data$conditional[i] == TRUE){
      wlist_i = list(
        conditionalPanel(condition = widget_data$appear_if[i],
                         textInput(inputId = ns(widget_data$inputId[i]),
                                   label = widget_data$label[i], value = widget_data$value[i]),
                         ns = ns)
      )
    }

    if(widget_data$type[i] == "textInputForChoicesFromVar" & widget_data$conditional[i] == TRUE){
      wlist_i = list(
        conditionalPanel(condition = widget_data$appear_if[i],
                         textInput(inputId = ns(widget_data$inputId[i]),
                                   label = widget_data$label[i], value = widget_data$value[i]),
                         ns = ns)
      )
    }

    if(widget_data$type[i] == "numericInput" & widget_data$conditional[i] == TRUE){
      wlist_i = list(
        conditionalPanel(condition = widget_data$appear_if[i],
                         numericInput(inputId = ns(widget_data$inputId[i]),
                                      label = sub(" NA","", paste(widget_data$label[i],
                                                                  widget_data$unit[i])),
                                      value = widget_data$value[i],
                                      max = widget_data$max[i],
                                      min = widget_data$min[i],
                                      step = widget_data$step[i]),
                         ns = ns)
      )
    }


    if(widget_data$type[i] == "checkboxInput" & widget_data$conditional[i] == TRUE){
      wlist_i = list(
        conditionalPanel(condition = widget_data$appear_if[i],
                         checkboxInput(inputId = ns(widget_data$inputId[i]),
                                       label = widget_data$label[i],
                                       value = as.logical(widget_data$value[i])
                         ),
                         ns = ns)
      )
    }

    if(widget_data$type[i] == "dateInput" & widget_data$conditional[i] == TRUE){
      wlist_i = list(
        conditionalPanel(condition = widget_data$appear_if[i],
                         dateInput(inputId = ns(widget_data$inputId[i]),
                                   label = widget_data$label[i],
                                   value = NULL
                         ),
                         ns = ns)
      )
    }


    if(widget_data$type[i] == "radioButtons" & widget_data$conditional[i] == TRUE){
      choices = list(widget_data$choice1[[i]],
                     widget_data$choice2[[i]],
                     widget_data$choice3[[i]],
                     widget_data$choice4[[i]],
                     widget_data$choice5[[i]],
                     widget_data$choice6[[i]],
                     widget_data$choice7[[i]])
      choices = choices[!is.na(choices) & !duplicated(choices) & choices != ""]
      names(choices) = choices
      wlist_i = list(
        conditionalPanel(condition = widget_data$appear_if[i],
                         radioButtons(inputId = ns(widget_data$inputId[i]),
                                      label = widget_data$label[i],
                                      choices = choices,
                                      selected = widget_data$selected[i]),
                         ns = ns)
      )
    }

    if(widget_data$type[i] == "selectInput" & widget_data$conditional[i] == TRUE){
      choices = list(widget_data$choice1[[i]],
                     widget_data$choice2[[i]],
                     widget_data$choice3[[i]],
                     widget_data$choice4[[i]],
                     widget_data$choice5[[i]],
                     widget_data$choice6[[i]],
                     widget_data$choice7[[i]])
      choices = choices[!is.na(choices) & !duplicated(choices) & choices != ""]
      names(choices) = choices
      wlist_i = list(
        conditionalPanel(condition = widget_data$appear_if[i],
                         selectInput(inputId = ns(widget_data$inputId[i]),
                                     label = widget_data$label[i],
                                     choices = choices,
                                     selected = widget_data$selected[i]),
                         ns = ns)
      )
    }

    if(widget_data$type[i] == "selectInputFromDatabase" & widget_data$conditional[i] == TRUE){
      choices = c(db_read_select(pool, tbl_id, pid)[,widget_data$choicesFromVar[i]] %>% as.factor() %>% levels(),
                  db_read_select(pool, tbl_id, pid)[,widget_data$inputId[i]] %>% as.factor() %>% levels())
      choices = c(widget_data$choice1[[i]],
                  widget_data$choice2[[i]],
                  widget_data$choice3[[i]],
                  widget_data$choice4[[i]],
                  widget_data$choice5[[i]],
                  widget_data$choice6[[i]],
                  widget_data$choice7[[i]],
                  choices)
      choices = choices[!is.na(choices) & !duplicated(choices) & choices != ""]
      names(choices) = choices
      wlist_i = list(
        conditionalPanel(condition = widget_data$appear_if[i],
                         selectInput(inputId = ns(widget_data$inputId[i]),
                                     label = widget_data$label[i],
                                     choices = choices,
                                     selected = widget_data$selected[i]),
                         ns = ns)
      )
    }


    wlist_all = c(wlist_all, wlist_i)

  }
  wlist_all
}
