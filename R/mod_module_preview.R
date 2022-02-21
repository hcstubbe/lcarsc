#' module_preview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom pool dbPool
#' @importFrom RSQLite SQLite
mod_module_preview_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(type = "tabs",
                tabPanel("Inclusion", br(),
                         mod_module_new_pat_ui(ns("mod_module_new_pat_test"))
                         ),
                tabPanel("Documentation", br(),
                         mod_module_documentation_ui(ns("mod_module_documentation_test"))
                         )
    )
  )
}

#' module_preview Server Functions
#'
#' @noRd
mod_module_preview_server <- function(id){
  moduleServer( id, function(input, output, session){

    # Remove preview db file on starting the module
    if(file.exists("editor_preview_temp.sqlite3")){
      file.remove("editor_preview_temp.sqlite3")
    }

    # Launch module servers ----
    # Module in Tab 1
    mod_module_new_pat_server(id = "mod_module_new_pat_test",
                              visit_id = "vi",
                              data_table = "inclusion_dataset",
                              app_data_internal_submodule = golem::get_golem_options("app_data_internal"),
                              preview = TRUE)

    # Module in Tab 2
    mod_module_documentation_server(id = "mod_module_documentation_test",
                                    data_table1 = "inclusion_dataset",
                                    data_table2 = "scientific_dataset",
                                    app_data_internal_submodule = golem::get_golem_options("app_data_internal"),
                                    preview = TRUE)

  })
}

## To be copied in the UI
# mod_module_preview_ui("module_preview_1")

## To be copied in the server
# mod_module_preview_server("module_preview_1")
