## code to prepare `widgets` dataset goes here

library(dplyr)
library(readr)

app_data_internal = list(all_visits = read.csv('widgets/visits.csv'),
                ordered_visits = all_visits %>% filter(!is.na(order)) %>% arrange(order),
                widgets_table_global = read.csv("widgets/widgets.csv"),
                all_tabs = read.csv('widgets/panel_tabs.csv'),
                visit_choices = all_visits$visit_id[!all_visits$inclusion_other_visit],
                widgets_table_global_widgets = read.csv("widgets/widgets_editor.csv"),
                all_visits_editor = read.csv('widgets/visits_editor.csv'),
                widgets_template = read.csv('widgets/widgets_template.csv'))

# Import widget data
usethis::use_data(app_data_internal,
                    overwrite = TRUE,
                  internal = TRUE)
