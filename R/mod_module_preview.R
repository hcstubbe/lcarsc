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
  server_settings_tbl_id = "server_settings_tbl"
  pool_config = get_golem_options("pool_config")
  if(is.element(server_settings_tbl_id, RMariaDB::dbListTables(pool_config))){
    db_settgins_data = RMariaDB::dbReadTable(pool_config, server_settings_tbl_id)
  }else{
    db_settgins_data = data.frame()
  }

  tagList(
    tabsetPanel(type = "tabs",
                tabPanel("Introduction", br(),
                         shinydashboard::box(title = db_settgins_data$study_title,
                                             status = "primary", solidHeader = FALSE,
                                             db_settgins_data$study_introduction)
                ),
                tabPanel("Inclusion", br(),
                         mod_module_new_pat_ui(ns("mod_module_new_pat_test"))
                         ),
                tabPanel("Documentation", br(),
                         mod_module_documentation_ui(ns("mod_module_documentation_test"))
                         ),
                tabPanel("Contact", br(),
                         shinydashboard::box(title = "Contact", status = "primary", solidHeader = FALSE,
                                             width = 10,
                                             column(6,
                                                    h4(db_settgins_data$contact1_name), br(),
                                                    db_settgins_data$contact1_department, br(),
                                                    db_settgins_data$contact1_institute, br(),
                                                    db_settgins_data$contact1_street, br(),
                                                    db_settgins_data$contact1_city,
                                                    db_settgins_data$contact1_postal_code, br(),
                                                    paste0("Tel.: ", db_settgins_data$contact1_phone), br(),
                                                    paste0("Fax: ", db_settgins_data$contact1_fax), br(),
                                                    HTML(paste0("E-mail: <a href='mailto:", db_settgins_data$contact1_mail,"' target='_top'>", db_settgins_data$contact1_mail,"</a>"))
                                             ),
                                             column(6,
                                                    h4(db_settgins_data$contact2_name), br(),
                                                    db_settgins_data$contact2_department, br(),
                                                    db_settgins_data$contact2_institute, br(),
                                                    db_settgins_data$contact2_street, br(),
                                                    db_settgins_data$contact2_city,
                                                    db_settgins_data$contact2_postal_code, br(),
                                                    paste0("Tel.: ", db_settgins_data$contact2_phone), br(),
                                                    paste0("Fax: ", db_settgins_data$contact2_fax), br(),
                                                    HTML(paste0("E-mail: <a href='mailto:", db_settgins_data$contact2_mail,"' target='_top'>", db_settgins_data$contact2_mail,"</a>"))
                                             )
                         ))
    )
  )
}

#' module_preview Server Functions
#'
#' @noRd
mod_module_preview_server <- function(id){
  moduleServer( id, function(input, output, session){

    # Remove preview db file on starting the module
    if(file.exists("db_preview.sqlite3")){
      file.remove("db_preview.sqlite3")
    }

    # Launch module servers ----
    # Module in Tab 1
    mod_module_new_pat_server(id = "mod_module_new_pat_test",
                              visit_id = "vi",
                              data_table = "inclusion_dataset",
                              preview = TRUE)

    # Module in Tab 2
    mod_module_documentation_server(id = "mod_module_documentation_test",
                                    data_table1 = "inclusion_dataset",
                                    data_table2 = "scientific_dataset",
                                    preview = TRUE)

  })
}

## To be copied in the UI
# mod_module_preview_ui("module_preview_1")

## To be copied in the server
# mod_module_preview_server("module_preview_1")
