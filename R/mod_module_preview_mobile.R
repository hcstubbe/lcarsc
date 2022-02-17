#' module_preview_mobile UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList div h4
#' @importFrom golem get_golem_options
#' @importFrom RSQLite SQLite
#' @importFrom lcarsM run_app
#'
mod_module_preview_mobile_ui <- function(id){
  ns <- NS(id)

  tagList(
    uiOutput(ns("app_preview"))
  )

   # create_app_ui(h1("test"))





}

#' module_preview_mobile Server Functions
#'
#' @noRd
mod_module_preview_mobile_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$app_preview = renderUI({
      create_app_ui(lcarsM::run_app(production_mode = "editor",
                                    database_driver = RSQLite::SQLite(),
                                    dbuser = 'user',
                                    dbpassword = 'user',
                                    dbhost = 'dbeditor',
                                    dbname = 'mydb_werwer.sqlite3',
                                    options = list(host = '0.0.0.0', port = 3838)),
                    landscape = TRUE)
    })

  })
}

## To be copied in the UI
# mod_module_preview_mobile_ui("module_preview_mobile_1")

## To be copied in the server
# mod_module_preview_mobile_server("module_preview_mobile_1")
