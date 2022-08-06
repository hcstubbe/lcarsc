#' module_editor_launcher UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList fluidPage fluidRow navbarPage tabPanel moduleServer reactiveValues
#' @importFrom shinydashboard box
#' @importFrom golem get_golem_options
#' @importFrom DT datatable
#' @import dplyr
#'
mod_module_editor_launcher_ui <- function(id) {
  ns = NS(id)
  tagList(
    fluidPage(
      fluidRow(mod_module_editor_controls_ui(ns("mod_module_editor_controls"))),
      br(),
      fluidRow(shinydashboard::box(title = NULL, status = "info", width = 12,
                                   navbarPage("Editor",
                                              tabPanel("Visits",
                                                       mod_module_edit_tab_ui(ns("mod_module_editor_visit"))
                                              ),
                                              tabPanel("Variables",
                                                       mod_module_edit_tab_ui(ns("mod_module_editor_vars"))
                                              )
                                   )))
    )

  )
}


#' module_editor_launcher Server Functions
#'
#' @noRd
mod_module_editor_launcher_server <- function(id) {
  moduleServer(id, function(input, output, session) {


    # Requirements ----
    ns = session$ns
    pool = get_golem_options("pool")
    widget_data_input = load_widget_data(pool_config = golem::get_golem_options("pool_config"),
                                         production_mode = golem::get_golem_options("production_mode"))
  	all_visits_editor = widget_data_input$all_visits_editor
  	widgets_table_global_widgets = widget_data_input$widgets_table_global_widgets
  	widgets_table_global_widgets = widgets_table_global_widgets[widgets_table_global_widgets$inputId != "origin_of_variable",]



    # Run when starting module ----

    ## Start sub-module server

    mod_module_editor_controls_server("mod_module_editor_controls")

    rv_downstream_visit = reactiveValues()
    rv_downstream_visit$pid = reactive({"visits"})
    mod_module_edit_tab_server(id = "mod_module_editor_visit",
                           widget_tab_selection = "visits",
                           tbl_id = "editor_table_visit",
                           rv_in = rv_downstream_visit,
                           show_vals = c('Visit title' = "visit_title",
                                         'Visit ID' = "visit_id_visits",
                                         'Is dependent/sub visit' = "is_child",
                                         "Order" = "order"),
                           simple = TRUE,
                           modal_width = ".modal-dialog{ width:400px}",
                           widgets_table_global = widgets_table_global_widgets,
                           all_visits = all_visits_editor,
                           visit_id = "editor",
                           add.copy.btn = TRUE,
                           order.by = "order",
                           search_field = TRUE,
                           use_move_order = FALSE,
                           keep_copy_order = TRUE)



    ## Start sub-module server
    rv_downstream_vars = reactiveValues()
    rv_downstream_vars$visit_id = reactive({"editor"})
    rv_downstream_vars$pid = reactive({"vars"})
    mod_module_edit_tab_server(id = "mod_module_editor_vars",
                           widget_tab_selection = "vars",
                           tbl_id = "editor_table_vars",
                           rv_in = rv_downstream_vars,
                           show_vals = c('Input ID' = "inputId",
                                         Label = "label",
                                         'visit_id' = "visit_for_var",
                                         Panel = "panel",
                                         'Subgroup/Sub-panel' = "subgroup",
                                         'Input type' = "type"),
                           simple = TRUE,
                           modal_width = ".modal-dialog{ width:400px}",
                           widgets_table_global = widgets_table_global_widgets,
                           all_visits = all_visits_editor,
                           visit_id = "editor",
                           add.copy.btn = TRUE,
                           num_entries = 20,
                           order.by = "order_of_var",
                           search_field = TRUE,
                           editor_filter_visit_id = TRUE,
                           use_move_order = TRUE,
                           keep_copy_order = FALSE)


  })
}


## To be copied in the UI
# mod_module_editor_launcher_ui("module_editor_launcher_1")

## To be copied in the server
# mod_module_editor_launcher_server("module_editor_launcher_1")
