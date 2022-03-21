#' module_library UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_module_library_ui <- function(id){
  ns = NS(id)
  tagList(
    fluidPage(
      fluidRow(mod_module_library_controls_ui(ns("mod_module_library_controls"))),
      br(),
      fluidRow(shinydashboard::box(title = NULL, status = "info", width = 12,
                                   navbarPage("Library",
                                              tabPanel("Variables",
                                                       mod_module_edit_tab_ui(ns("mod_module_library_vars"))
                                              )
                                   )
      ),
      uiOutput(ns('sel_row')))
    )

  )
}

#' module_library Server Functions
#'
#' @noRd
mod_module_library_server <- function(id){
  moduleServer(id, function(input, output, session) {


    # Requirements ----
    ns = session$ns
    pool = get_golem_options("pool")
    widget_data_input = load_widget_data(pool_config = golem::get_golem_options("pool_config"),
                                         production_mode = golem::get_golem_options("production_mode"))
    widgets_table_global_widgets = widget_data_input$widgets_table_global_widgets
    widgets_table_global_widgets = widgets_table_global_widgets[widgets_table_global_widgets$inputId != "visit_for_var",]

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
    rv_downstream_vars = reactiveValues()
    rv_downstream_vars$visit_id = reactive({"library"})
    rv_downstream_vars$pid = reactive({"vars"})
    sel_row = mod_module_edit_tab_server(id = "mod_module_library_vars",
                                         widget_tab_selection = "vars",
                                         tbl_id = "library_table_vars",
                                         rv_in = rv_downstream_vars,
                                         show_vals = c('Input ID' = "inputId",
                                                       'Label' = "label",
                                                       'Panel' = "panel",
                                                       'Subgroup/Sub-panel' = "subgroup",
                                                       'Input type' = "type"),
                                         simple = TRUE,
                                         modal_width = ".modal-dialog{ width:400px}",
                                         widgets_table_global = widgets_table_global_widgets,
                                         all_visits = NULL,
                                         visit_id = "library",
                                         add.copy.btn = TRUE,
                                         num_entries = 200,
                                         order.by = "order_of_var",
                                         select_multiple = TRUE)


    mod_module_library_controls_server("mod_module_library_controls", selected_row = sel_row)


    # Handle uploads ----

    observe({
      if (is.null(input$vars_upload)) return()
      input_csv_vars = read.csv(input$vars_upload$datapath)
      tryCatch(dbAppendTable(pool,
                             "editor_table_vars",
                             input_csv_vars),
               error = function(e) showNotification("Data not saved: check format!", type = "error"))
    })

    output$sel_row = shiny::renderUI({h4(paste(sel_row(), collapse = ", "))})

  })
}

## To be copied in the UI
# mod_module_library_ui(ns("module_library_1"))

## To be copied in the server
# mod_module_library_server("module_library_1")
