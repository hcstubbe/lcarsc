#' module_new_pat UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_module_new_pat_ui <- function(id) {
  ns = NS(id)
  tagList(
    fluidRow(
      column(10,
             box(
               title = lang_sel$menu_inclusion, width = 12, status = "primary", solidHeader = TRUE,
               div(
                 mod_module_edit_tab_ui(id = ns("mod_module_edit_tab_inclusion"))
               )
             ),
             box(title = ("Upload patient list"), width = 12, status = "primary", solidHeader = TRUE,
                 fileInput(ns("pat_upload"), "Upload data file (CSV)",
                           multiple = FALSE,
                           accept = c(".csv"))
             )
      )
    )
  )
}

#' module_new_pat Server Functions
#'
#' @noRd
mod_module_new_pat_server <- function(id, visit_id, data_table) {
  moduleServer(id, function(input, output, session) {

    # Upload participant file ----
    observe({
      if (is.null(input$pat_upload)) return()

      input_csv = read.csv(input$pat_upload$datapath)

      df_generic = list(row_id = sapply(1:nrow(input_csv), function(x) UUIDgenerate()),
                        user_modified = "NA",
                        pid = input_csv$Patnr,
                        date_modified = as.character(date()),
                        visit_id = "vi",
                        deleted_row = "FALSE",
                        submitted_row = "TRUE",
                        Patnr = input_csv$Patnr,
                        Fall = input_csv$Fall,
                        documented_case = "FALSE") %>% data.frame
      dbAppendTable(pool, data_table, df_generic)
    })


    ## Start sub-module server
    rv_downstream_visit = reactiveValues()
    rv_downstream_visit$pid = reactive({"init"})
    mod_module_edit_tab_server(id = "mod_module_edit_tab_inclusion",
                           widget_tab_selection = 'visit',
                           tbl_id = "inclusion_dataset",
                           rv_in = rv_downstream_visit,
                           show_vals = c(PID = 'pid', Date = 'date_modified', Visit = 'visit_id', Submitted = 'submitted_row'),
                           simple = FALSE,
                           modal_width = '.modal-dialog{ width:95%}',
                           widgets_table_global = widgets_table_global[widgets_table_global[,"vi"],],
                           all_tabs = all_tabs,
                           all_visits = all_visits,
                           visit_id = "vi",
                           create_new_pid = TRUE,
                           order.by = NULL)

  })

}

## To be copied in the UI
# mod_module_new_pat_ui("module_new_pat_1")

## To be copied in the server
# mod_module_new_pat_server("module_new_pat_1")