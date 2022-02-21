#' make_widget_tables
#'
#' @description A fct function
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
                              pool_config,
                              write_widget_tables = FALSE,
                              remove_old_tables = FALSE) {

  # Get app_data_internal
  app_data_internal = golem::get_golem_options("app_data_internal")

  # Remove all old tables
  if(remove_old_tables){
    tbl_ids = dbListTables(pool)
    tbl_ids = tbl_ids[!tbl_ids %in% c("medication_table", "diagnoses_table", "editor_table_visit", "editor_table_vars")]
    if(length(tbl_ids) > 0){
      for (i in tbl_ids) {
        dbRemoveTable(pool, i)
      }
    }
  }


  # Read input widget data
  visits = dbReadTable(pool, "editor_table_visit") %>% filter(deleted_row == FALSE) %>% select(order, visit_id_visits , visit_title) %>%
    rbind(list(order = NA, visit_id_visits = "diagnosis", visit_title = "Diagnosis"))  %>%
    rbind(list(order = NA, visit_id_visits = "medication", visit_title = "Medication")) %>%
    arrange(order)

  # Force inclusion visit
  if(!any(visits$visit_id_visits == "vi")){
    visits = rbind(list(order = 0, visit_id_visits = "vi", visit_title = "Inclusion"), visits)
  }

  vars = dbReadTable(pool, "editor_table_vars") %>% filter(deleted_row == FALSE)%>%
    arrange(order_of_var)


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
  visits$inclusion_other_visit = visits$visit_id_visits %in% c("diagnosis", "medication", "vi") # inclusion visits needs to have the id "vi"!
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
    panels_i = make.names(paste(vars[(vars$panel %in% panel_tabs_temp$tab_id[i]),"panel"], vars[(vars$panel %in% panel_tabs_temp$tab_id[i]),"subgroup"], sep = "_"))
    panels_i = make.unique(make.names(panels_i[!duplicated(panels_i)]))

    mtx = t(matrix(c(panels_i, rep("", 4-length(panels_i) %% 4)), nrow = 4))
    mtx = cbind(panel_tabs_temp$tab_title[i], mtx)
    mtx = cbind(panel_tabs_temp$tab_id[i], mtx)
    if(length(panels_i) > 4){
      mtx[,1] = paste(mtx[,1], 1:nrow(mtx), sep = "_")
    }
    mtx = cbind("", mtx)

    colnames(mtx) = c("position", "tab_id", "tab_title", "panel1up", "panel2up", "panel1down", "panel2down")

    panel_tabs = rbind(panel_tabs, mtx)
  }

  panel_tabs$position = 1:nrow(panel_tabs)
  # Make vars file



  # Make widget table
  var_table = data.frame(list(
    inputId = character(0),
    widget_tab = character(0),
    data_type = character(0),
    r_class = character(0),
    label = character(0),
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
      mandatory = vars$mandatory[i],
      plausible = NA,
      lower = vars$lower[i],
      upper = vars$upper[i],
      unit = vars$unit[i],
      value = as.character(ifelse(vars$type[i] == "numericInput", vars$value_numeric[i], ifelse(vars$type[i] == "checkboxInput", FALSE, vars$value_char[i]))),
      min = vars$min[i],
      max = vars$max[i],
      step = vars$step[i],
      width = vars$width[i],
      type = vars$type[i],
      panel = make.names(paste(vars$panel[i], vars$subgroup[i], sep = "_")),
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
      widget = TRUE,
      form_required = FALSE,
      conditional = as.logical(vars$conditional[i]),
      appear_if = vars$appear_if[i])

    var_table = rbind(var_table, data.frame(lapply(vars_i, function(x) if(is.null(x)){NA}else{x})))
  }


  # Add required variables (i.e. required for technical reasons)
  var_table = app_data_internal$widgets_template %>% rbind(var_table)
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
  widget_tables = list("visits" = visits, "panel_tabs" = panel_tabs, "widgets" = var_table)


  # Update tables on database
  db_replace_tables(pool = pool_config, table_list = widget_tables)

  # if(write_widget_tables == TRUE){
  #   dir.create(file.path("widgets"), showWarnings = FALSE)
  #   lapply(names(widget_tables), function(x) write_csv(widget_tables[[x]], paste0("widgets/", x, ".csv")))
  #   app_data_internal = create_app_data_internal(lang_sel = app_data_internal$lang_sel)
  #   saveRDS(app_data_internal, "widgets/app_data_internal.RDS")
  # }

  widget_tables$app_data_internal = app_data_internal
  widget_tables

}
