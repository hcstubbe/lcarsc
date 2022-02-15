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


    lang_sel = app_data_internal$lang_sel


    # Launch module servers ----

    # Module in Tab 1
    mod_module_new_pat_server(id = "mod_module_new_pat_test",
                              visit_id = "vi",
                              data_table = "inclusion_dataset")

    # Module in Tab 2
    mod_module_documentation_server(id = "mod_module_documentation_test",
                                    data_table1 = "inclusion_dataset",
                                    data_table2 = "scientific_dataset")

  })
}

## To be copied in the UI
# mod_module_preview_ui("module_preview_1")

## To be copied in the server
# mod_module_preview_server("module_preview_1")
