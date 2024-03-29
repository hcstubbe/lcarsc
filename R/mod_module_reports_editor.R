#' module_reports_editor UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_module_reports_editor_ui <- function(id){
  ns = NS(id)
  tagList(
    mod_module_edit_tab_ui(ns("mod_module_report_editor_vars"))
  )
}

#' module_reports_editor Server Functions
#'
#' @noRd
mod_module_reports_editor_server <- function(id) {
  moduleServer(id, function(input, output, session) {


    # Requirements ----
    ns = session$ns
    pool = get_golem_options("pool")
    widget_data_input = load_widget_data(pool_config = golem::get_golem_options("pool_config"),
                                         production_mode = golem::get_golem_options("production_mode"))
    widgets_table_global_widgets_report = widget_data_input$widgets_table_global_widgets_report
    widgets_table_global_widgets_report = widgets_table_global_widgets_report[widgets_table_global_widgets_report$inputId != "origin_of_variable",]



    # Run when starting module ----

    ## Start sub-module server

    ## Start sub-module server
    rv_downstream_vars = reactiveValues()
    rv_downstream_vars$visit_id = reactive({"report_editor"})
    rv_downstream_vars$pid = reactive({"vars"})
    mod_module_edit_tab_server(id = "mod_module_report_editor_vars",
                               widget_tab_selection = "vars",
                               tbl_id = "report_editor_table_vars",
                               rv_in = rv_downstream_vars,
                               show_vals = c('Segment ID' = "inputId",
                                             'Type' = "type",
                                             'Text' = "display_text",
                                             'Value for replacement' = "value_replaced",
                                             'Query visit' = "visit_id_for_query",
                                             'inputId for query' = "inputId_for_query",
                                             'Report visit' = "visit_for_var"),
                               simple = TRUE,
                               modal_width = ".modal-dialog{ width:400px}",
                               widgets_table_global = widgets_table_global_widgets_report,
                               all_visits = NULL,
                               visit_id = "report_editor",
                               add.copy.btn = TRUE,
                               num_entries = 20,
                               order.by = "order_of_var",
                               dom = "lfrtip",
                               editor_filter_visit_id = TRUE,
                               use_move_order = TRUE,
                               keep_copy_order = FALSE,
                               is_editor_or_vi = TRUE)
  })
}

## To be copied in the UI
# mod_module_reports_editor_ui(ns("module_reports_editor_1"))

## To be copied in the server
# mod_module_reports_editor_server("module_reports_editor_1")
