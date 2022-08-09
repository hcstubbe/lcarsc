#' make_widget_tables
#'
#' @description This function creates the widget tables and stores them in the config database
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @import dplyr
#' @importFrom readr write_csv
#' @importFrom RMariaDB dbReadTable dbListTables dbRemoveTable
#' @importFrom golem get_golem_options
#'
make_widget_tables = function(pool,
                              pool_config) {


  # Read input widget data
  visits = RMariaDB::dbReadTable(pool, "editor_table_visit") %>%
    dplyr::filter(deleted_row == FALSE) %>%
    dplyr::select(order, visit_id_visits , visit_title, is_child, parent_ids, show_col_1, show_col_2, show_col_3) %>%
    base::rbind(list(order = NA, visit_id_visits = "samples", visit_title = "Samples", is_child = FALSE, parent_ids = NA, show_col_1 = NA, show_col_2 = NA, show_col_3 = NA)) %>%
    dplyr::arrange(order) %>%
    dplyr::na_if("") %>%
    dplyr::na_if("NA")

  # Force inclusion visit
  if(!any(visits$visit_id_visits == "vi")){
    visits = base::rbind(list(order = 0, visit_id_visits = "vi", visit_title = "Inclusion", is_child = FALSE, parent_ids = NA, show_col_1 = NA, show_col_2 = NA, show_col_3 = NA), visits)
  }

  # Set NA values from is_child to FALSE
  if(any(is.na(visits$is_child))){
    visits$is_child[is.na(visits$is_child)] = FALSE
  }

  visits$parent_ids = sapply(visits$parent_ids, function(x){
    if(is.na(x)){
      return(NA)
    }else{
      y = unlist(strsplit(x, ";;;;;"))
      has_parent = y %in% visits$visit_id_visits[visits$is_child == FALSE]
      if(any(has_parent) == FALSE){
        return(NA)
      }else if(all(has_parent)){
        return(x)
      }else{
        x = y[has_parent]
        if(length(x) == 0){
          return(NA)
        }
        x = paste(collapse = ";;;;;", x)
        return(x)
      }
    }
  })

  vars = RMariaDB::dbReadTable(pool, "editor_table_vars") %>% filter(deleted_row == FALSE)%>%
    dplyr::arrange(order_of_var)

  show_cols = t(sapply(visits$visit_id_visits, function(x){
    input_ids_x = vars %>% filter(visit_for_var == x) %>% select(inputId)
    input_ids_x = input_ids_x$inputId

    show_cols = visits[visits$visit_id_visits == x, c("show_col_1", "show_col_2", "show_col_3")]
    show_cols[!(show_cols %in% input_ids_x)] = NA
    show_cols
  }))
  colnames(show_cols) = c("show_col_1", "show_col_2", "show_col_3")

  show_cols = data.frame(show_cols)

  visits$show_col_1 = unlist(show_cols$show_col_1)
  visits$show_col_2 = unlist(show_cols$show_col_2)
  visits$show_col_3 = unlist(show_cols$show_col_3)
  visits = visits %>% dplyr::na_if("NA")


  # Determine which tabs are displayed for each visit
  visit_tabs = levels(as.factor(vars$panel))
  visit_tabs_logic = c()
  for(i in visits$visit_id_visits){
    vars_i = vars %>% filter(visit_for_var == i)
    visit_tabs_i = levels(as.factor(vars_i$panel))
    visit_tabs_i = visit_tabs %in% visit_tabs_i
    names(visit_tabs_i) = make.unique(make.names(visit_tabs))
    visit_tabs_logic = rbind(visit_tabs_logic, visit_tabs_i)
  }
  rownames(visit_tabs_logic) = visits$visit_id_visits
  visits = cbind(visits, visit_tabs_logic)
  visits = cbind(visits, choicesFromVar = NA)
  visits$inclusion_other_visit = visits$visit_id_visits %in% c("diagnosis", "medication", "samples", "vi") # inclusion visits needs to have the id "vi"!
  visits$inclusion_criteria = FALSE
  visits$inclusion_criteria = visits$visit_id_visits == "vi"


  # Make tabs

  # Dataframe to fill
  panel_tabs_temp = data.frame(list("position" = integer(0),
                                    "tab_id" = character(0),
                                    "tab_title" = character(0),
                                    "panel1up" = character(0),
                                    "panel2up" = character(0),
                                    "panel1down" = character(0),
                                    "panel2down" = character(0)))
  # Populate tab dataframe
  tab_names = vars$panel[!duplicated(vars$panel)]
  vars$panel = factor(vars$panel)
  levels(vars$panel) = make.unique(make.names(levels(vars$panel)))
  vars$panel = as.character(vars$panel)
  tab_ids = vars$panel[!duplicated(vars$panel)]

  panel_tabs_temp = data.frame()
  for(i in 1:length(tab_ids)){
    panel_tabs_temp = rbind(panel_tabs_temp,
                            list(position = i,
                                 tab_id = tab_ids[i],
                                 tab_title = tab_names[i],
                                 panel1up = "",
                                 panel2up = "",
                                 panel1down = "",
                                 panel2down = ""
                            ))
  }

  panel_tabs = data.frame()
  for(i in 1:nrow(panel_tabs_temp)){
    panels_i = make.names(paste(vars[(vars$panel %in% panel_tabs_temp$tab_id[i]),"panel"],
                                vars[(vars$panel %in% panel_tabs_temp$tab_id[i]),"subgroup"], sep = "_"))
    panels_i = make.unique(make.names(panels_i[!duplicated(panels_i)]))

    mtx = t(matrix(c(panels_i, rep("", (4-length(panels_i)) %% 4)), nrow = 4))
    mtx = cbind(panel_tabs_temp$tab_title[i], mtx)
    mtx = cbind(panel_tabs_temp$tab_id[i], mtx)
    if(nrow(mtx) > 1){
      mtx[,2] = paste(mtx[,2], 1:nrow(mtx))
    }

    mtx = cbind("", mtx)

    colnames(mtx) = c("position", "tab_id", "tab_title", "panel1up", "panel2up", "panel1down", "panel2down")

    panel_tabs = rbind(panel_tabs, mtx)
  }


  panel_tabs$position = 1:nrow(panel_tabs)

  # Make widget table
  var_table = data.frame(list(
    inputId = character(0),
    widget_tab = character(0),
    data_type = character(0),
    r_class = character(0),
    label = character(0),
    label_translation = character(0),
    mandatory = logical(0),
    plausible = character(0),
    lower = double(0),
    upper = double(0),
    unit = character(0),
    value = character(0),
    min = double(0),
    max = double(0),
    step = double(0),
    width = double(0),
    type = character(0),
    panel = character(0),
    subgroup = character(0),
    selected = character(0),
    choice1 = character(0),
    choice2 = character(0),
    choice3 = character(0),
    choice4 = character(0),
    choice5 = character(0),
    choice6 = character(0),
    choice7 = character(0),
    choice8 = character(0),
    choice9 = character(0),
    choice10 = character(0),
    choice11 = character(0),
    choice12 = character(0),
    choice1_translation = character(0),
    choice2_translation = character(0),
    choice3_translation = character(0),
    choice4_translation = character(0),
    choice5_translation = character(0),
    choice6_translation = character(0),
    choice7_translation = character(0),
    choice8_translation = character(0),
    choice9_translation = character(0),
    choice10_translation = character(0),
    choice11_translation = character(0),
    choice12_translation = character(0),
    widget = logical(0),
    form_required = logical(0),
    conditional = logical(0),
    appear_if = character(0)))



  for(i in 1:nrow(vars)){
    vars_i = list(
      inputId = paste(vars$visit_for_var[i], vars$inputId[i], sep = "_"),
      widget_tab = "visit",
      data_type = ifelse((vars$type[i] == "numericInput" & vars$step[i] == 1 & !is.na(vars$step[i])), "INTEGER",
                         ifelse(vars$type[i] == "numericInput", "DOUBLE",
                                ifelse(vars$type[i] == "checkboxInput", "BOOLEAN", "TEXT"))),
      r_class = ifelse((vars$type[i] == "numericInput" & vars$step[i] == 1 & !is.na(vars$step[i])), "integer",
                       ifelse(vars$type[i] == "numericInput", "double",
                              ifelse(vars$type[i] == "checkboxInput", "logical", "character"))),
      label = vars$label[i],
      label_translation = vars$label_translation[i],
      mandatory = vars$mandatory[i],
      plausible = NA,
      lower = vars$lower[i],
      upper = vars$upper[i],
      unit = vars$unit[i],
      value = as.character(ifelse(vars$type[i] == "numericInput",
                                  vars$value_numeric[i],
                                  ifelse(vars$type[i] == "checkboxInput",
                                         FALSE,
                                         vars$value_char[i]))),
      min = vars$min[i],
      max = vars$max[i],
      step = vars$step[i],
      width = vars$width[i],
      type = vars$type[i],
      panel = make.names(paste(vars$panel[i], vars$subgroup[i], sep = "_")),
      subgroup = vars$subgroup[i],
      selected = vars$selected[i],
      choice1 = vars$choice1[i],
      choice2 = vars$choice2[i],
      choice3 = vars$choice3[i],
      choice4 = vars$choice4[i],
      choice5 = vars$choice5[i],
      choice6 = vars$choice6[i],
      choice7 = vars$choice7[i],
      choice8 = vars$choice8[i],
      choice9 = vars$choice9[i],
      choice10 = vars$choice10[i],
      choice11 = vars$choice11[i],
      choice12 = vars$choice12[i],
      choice1_translation = vars$choice1_translation[i],
      choice2_translation = vars$choice2_translation[i],
      choice3_translation = vars$choice3_translation[i],
      choice4_translation = vars$choice4_translation[i],
      choice5_translation = vars$choice5_translation[i],
      choice6_translation = vars$choice6_translation[i],
      choice7_translation = vars$choice7_translation[i],
      choice8_translation = vars$choice8_translation[i],
      choice9_translation = vars$choice9_translation[i],
      choice10_translation = vars$choice10_translation[i],
      choice11_translation = vars$choice11_translation[i],
      choice12_translation = vars$choice12_translation[i],
      widget = TRUE,
      form_required = FALSE,
      conditional = as.logical(vars$conditional[i]),
      appear_if = vars$appear_if[i])

    var_table = rbind(var_table, data.frame(lapply(vars_i, function(x) if(is.null(x)){NA}else{x})))
  }


  # Add required variables (i.e. required for technical reasons)
  var_table = internal_app_data$widgets_template %>% rbind(var_table)
  for(i in visits$visit_id_visits){
    vars %>% filter(visit_for_var == i)
    var_table[i] = var_table[,"inputId"] %in% paste((vars %>% filter(visit_for_var == i))[,"visit_for_var"], (vars %>% filter(visit_for_var == i))[,"inputId"], sep = "_") | var_table$widget_tab == "all"
  }

  var_table$diagnosis = var_table$widget_tab == "diagnosis" | var_table$widget_tab == "all"
  var_table$medication = var_table$widget_tab == "medication" | var_table$widget_tab == "all"


  # Make variable names unique
  var_table$inputId = make.unique(var_table$inputId, sep = "_")


  # Save results and write tables to file and database
  colnames(visits)[colnames(visits) == "visit_id_visits"] = "visit_id"
  widget_tables = c(list("visits" = visits, "panel_tabs" = panel_tabs, "widgets" = var_table), internal_app_data[c("widgets_editor", "visits_editor", "widgets_template")])


  # Update tables on database
  db_replace_tables(conn = pool_config, table_list = widget_tables) # replace configuration tables

  widget_tables

}
