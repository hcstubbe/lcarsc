#' load_widget_data
#'
#' @description This function creates widget data
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
#' @import dplyr
#' @importFrom RMariaDB dbReadTable
#' @importFrom utils read.csv
#'
load_widget_data = function(pool_config, production_mode){



  db_read_app_data = function(x_table, conn, prod_mod){

    if ( RMariaDB::dbExistsTable(conn = conn, name = x_table) ){
      x_table = RMariaDB::dbReadTable(conn = conn, name = x_table)
    } else if(prod_mod == "editor"){  # Try to read table from widgets folder, if in editor mode.
      x_table = utils::read.csv(paste0("widgets/",x_table, ".csv"))
    } else {
      stop(paste0(x_table, " could not be found!"))
    }

    return(x_table)
  }


  prod_mod = get_production_mode(production_mode = production_mode,
                                 pool_config = pool_config)


  all_visits = db_read_app_data(conn = pool_config, x_table = "visits", prod_mod = prod_mod)
  widget_data = list(all_visits = all_visits,
                     ordered_visits = all_visits %>% filter(!is.na(order)) %>% arrange(order),
                     widgets_table_global = db_read_app_data(conn = pool_config, x_table = "widgets", prod_mod = prod_mod),
                     all_tabs = db_read_app_data(conn = pool_config, x_table = "panel_tabs", prod_mod = prod_mod),
                     visit_choices = all_visits$visit_id[!all_visits$inclusion_other_visit],
                     widgets_table_global_widgets = utils::read.csv("widgets/widgets_editor.csv"),
                     all_visits_editor = utils::read.csv('widgets/visits_editor.csv'),
                     widgets_template = utils::read.csv('widgets/widgets_template.csv'),
                     lang_sel = lang_sel)
  widget_data
}
