#' module_editor_launcher UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import dplyr
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
    rv_downstream = reactiveValues()

    # Auxiliary functions ----

    # Functions for loading data for rendering data table
    computeFT = function(){
      y = loadData("input_vars")[,c("date_modified")]
      y
    }
    load_dt_for_render = function(){
      DT::datatable(computeFT(),
                    options = list(pageLength = 5), selection = c("single"))}



    # Run when starting module ----

    ## Start sub-module servers

    mod_module_editor_controls_server("mod_module_editor_controls")

    rv_downstream_visit = reactiveValues()
    rv_downstream_visit$pid = reactive({"visits"})
    mod_module_edit_tab_server(id = "mod_module_editor_visit",
                           widget_tab_selection = "visits",
                           tbl_id = "editor_table_visit",
                           rv_in = rv_downstream_visit,
                           show_vals = c('Visit title' = "visit_title", 'visit_id' = "visit_id_visits", "Order" = "order"),
                           simple = TRUE,
                           modal_width = ".modal-dialog{ width:400px}",
                           widgets_table_global = widgets_table_global_widgets,
                           all_visits = all_visits_editor,
                           visit_id = "editor",
                           add.copy.btn = TRUE,
                           order.by = "order")



    ## Start sub-module servers
    rv_downstream_vars = reactiveValues()
    rv_downstream_vars$visit_id = reactive({"editor"})
    rv_downstream_vars$pid = reactive({"vars"})
    mod_module_edit_tab_server(id = "mod_module_editor_vars",
                           widget_tab_selection = "vars",
                           tbl_id = "editor_table_vars",
                           rv_in = rv_downstream_vars,
                           show_vals = c('Input ID' = "inputId", Label = "label", 'visit_id' = "visit_for_var", Panel = "panel", 'Subgroup/Sub-panel' = "subgroup",'Input type' = "type"),
                           simple = TRUE,
                           modal_width = ".modal-dialog{ width:400px}",
                           widgets_table_global = widgets_table_global_widgets,
                           all_visits = all_visits_editor,
                           visit_id = "editor",
                           add.copy.btn = TRUE,
                           num_entries = 200,
                           order.by = "order_of_var")


  })
}


## To be copied in the UI
# mod_module_editor_launcher_ui("module_editor_launcher_1")

## To be copied in the server
# mod_module_editor_launcher_server("module_editor_launcher_1")
