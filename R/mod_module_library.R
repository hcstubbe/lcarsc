#' module_library UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_module_library_ui <- function(id) {
  ns = NS(id)
  tagList(
    fluidRow(
      column(5,
             box(title = (internal_app_data$lang_sel$module_documentation_pt_list_title), width = 12, status = "primary", solidHeader = TRUE,
                 actionButton(ns("update_pull_user"), label = internal_app_data$lang_sel$update_pull, icon("sync", verify_fa = FALSE)),
                 br(),
                 br(),
                 br(),
                 DT::dataTableOutput(ns("responses_user")))
      ),
      column(7,
             uiOutput(ns("visit_submission_panel"))
      )
    )
  )
}

#' module_library Server Functions
#'
#' @noRd
mod_module_library_server <- function(id, data_table1, data_table2, preview = FALSE) {
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
      y = loadData(pool, "library_table_vars")
      if(is.null(y)){
        return(y)
      }else{
        panel = list(Panel = levels(factor(y$panel)))
        panel = data.frame(panel)
        return(panel)
      }
    }
    load_dt_for_render = function(){
      DT::datatable(computeFT(),
                    options = list(pageLength = 5), selection = c("single"))}



    # Run when starting module ----


    ## Start sub-module servers
    get_panel = reactive({
      x_tab = computeFT()
      row_selection = input$responses_user_rows_selected
      x_panel = x_tab[row_selection, "Panel"]
      x_panel
      })
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
                                         select_multiple = TRUE,
                                         filter_panel = get_panel)



    # Observers ----
    ## Update participant table ----
    observeEvent(input$update_pull_user, {
      output$responses_user <- DT::renderDataTable({
        load_dt_for_render()
      })
    })

    # Render UI elements ----

    ## Load data from database for included patients
    output$responses_user <- DT::renderDataTable({
      load_dt_for_render()
    })


    ##
    output$docu_tab_ui = renderUI({
      mod_module_edit_tab_ui(id = ns("mod_module_library_vars"))
    })

    # Render menu when participant is selected
    output$visit_submission_panel = renderUI({

      if(length(input$responses_user_rows_selected) == 1){
        div(
          shinydashboard::box(
            title = (internal_app_data$lang_sel$module_documentation_visit_menu),width = 12, status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
            wellPanel(h4("Here!")
            ),
            uiOutput(ns("docu_tab_ui"))
          )
        )

      }else{
        return(NULL)
      }
    })
  })
}

## To be copied in the UI
# mod_module_library_ui(ns("module_library_1"))

## To be copied in the server
# mod_module_library_server("module_library_1")
