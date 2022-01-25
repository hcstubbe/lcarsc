## code to prepare `widgets` dataset goes here

library(dplyr)
library(readr)

# Import widget data
all_visits = read.csv('widgets/visits.csv')
ordered_visits = all_visits %>% filter(!is.na(order)) %>% arrange(order)
widgets_table_global = read.csv("widgets/widgets.csv")
all_tabs = read.csv('widgets/panel_tabs.csv')
visit_choices = all_visits$visit_id[!all_visits$inclusion_other_visit]
widgets_table_global_widgets = read.csv("widgets/widgets_editor.csv")
all_visits_editor = read.csv('widgets/visits_editor.csv')
widgets_template = read.csv('widgets/widgets_template.csv')


usethis::use_data(all_visits, overwrite = TRUE, internal = FALSE)
usethis::use_data(ordered_visits, overwrite = TRUE, internal = FALSE)
usethis::use_data(widgets_table_global, overwrite = TRUE, internal = FALSE)
usethis::use_data(all_tabs, overwrite = TRUE, internal = FALSE)
usethis::use_data(visit_choices, overwrite = TRUE, internal = FALSE)
usethis::use_data(widgets_table_global_widgets, overwrite = TRUE, internal = FALSE)
usethis::use_data(all_visits_editor, overwrite = TRUE, internal = FALSE)
usethis::use_data(widgets_template, overwrite = TRUE, internal = FALSE)
