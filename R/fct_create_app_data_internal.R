#' create_app_data_internal
#'
#' @description This function gathers widget data and adds language locale for fixed labels to store/update internal data
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
#' @import dplyr
#'
create_app_data_internal = function(lang_sel){
  all_visits = utils::read.csv('widgets/visits.csv')
  app_data_internal = list(all_visits = all_visits,
                           ordered_visits = all_visits %>% filter(!is.na(order)) %>% arrange(order),
                           widgets_table_global = utils::read.csv("widgets/widgets.csv"),
                           all_tabs = utils::read.csv('widgets/panel_tabs.csv'),
                           visit_choices = all_visits$visit_id[!all_visits$inclusion_other_visit],
                           widgets_table_global_widgets = utils::read.csv("widgets/widgets_editor.csv"),
                           all_visits_editor = utils::read.csv('widgets/visits_editor.csv'),
                           widgets_template = utils::read.csv('widgets/widgets_template.csv'),
                           lang_sel = lang_sel)
  app_data_internal
}
