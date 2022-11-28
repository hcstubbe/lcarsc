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
    all_visits_report_editor = widget_data_input$all_visits_report_editor
    widgets_table_global_widgets_report = widget_data_input$widgets_table_global_widgets_report
    widgets_table_global_widgets_report = widgets_table_global_widgets_report[widgets_table_global_widgets_report$inputId != "origin_of_variable",]



    # Run when starting module ----

    ## Start sub-module server

    # mod_module_editor_controls_server("mod_module_editor_controls")

    ## Start sub-module server
    rv_downstream_vars = reactiveValues()
    rv_downstream_vars$visit_id = reactive({"report_editor"})
    rv_downstream_vars$pid = reactive({"vars"})
    mod_module_edit_tab_server(id = "mod_module_report_editor_vars",
                               widget_tab_selection = "vars",
                               tbl_id = "report_editor_table_vars",
                               rv_in = rv_downstream_vars,
                               show_vals = c('Input ID' = "inputId",
                                             Label = "label",
                                             'visit_id' = "visit_for_var",
                                             Panel = "panel",
                                             'Subgroup/Sub-panel' = "subgroup",
                                             'Input type' = "type"),
                               simple = TRUE,
                               modal_width = ".modal-dialog{ width:400px}",
                               widgets_table_global = widgets_table_global_widgets_report,
                               all_visits = all_visits_report_editor,
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
