#' module_db_settings UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_module_db_settings_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      shinydashboard::box(width = 12, status = "primary",title = "Evnironmental variable names", solidHeader = TRUE,
                          textInput(ns("env_user_group"), label = "User group", value = "SHINYPROXY_USERGROUPS"),
                          textInput(ns("env_user_name"), label = "User name", value = "SHINYPROXY_USERNAME")),
      shinydashboard::box(width = 12, status = "primary",title = "User groups", solidHeader = TRUE,
                          textInput(ns("group_admin"), label = "Admin group", value = "admin"),
                          textInput(ns("group_reviewer"), label = "Reviewer group", value = "reviewer"),
                          textInput(ns("group_user"), label = "User group", value = "user"))
    )

  )
}

#' module_db_settings Server Functions
#'
#' @noRd
mod_module_db_settings_server <- function(id, rv){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    pool_config = get_golem_options("pool_config")
    server_db_settings_tbl_id = "server_db_settings_tbl"



    ## Gather input data ----
    form_input_ids = c(
      "env_user_group",
      "env_user_name",
      "group_admin",
      "group_reviewer",
      "group_user"
    )


    stdandard_values = list(
      "env_user_group" = "SHINYPROXY_USERGROUPS",
      "env_user_name" = "SHINYPROXY_USERNAME",
      "group_admin" = "admin",
      "group_reviewer" = "reviewer",
      "group_user" = "user"
    )



    # Populate database on first start of module with standard values, if table does not exist
    if (!is.element(server_db_settings_tbl_id, RMariaDB::dbListTables(pool_config))) {
      input_data_standard = data.frame(stdandard_values)
      input_list_standard = list(input_data_standard)
      names(input_list_standard) = server_db_settings_tbl_id
      db_replace_tables(conn = pool_config, table_list = input_list_standard)
    }

    # Save submission
    observeEvent(rv$save_db_settings_button,{
      input_data = sapply(form_input_ids,
                          function(x) make.names(input[[x]]),
                          simplify = FALSE,
                          USE.NAMES = TRUE)
      make.names(names = input_data, unique = TRUE)
      if(!all(sapply(input_data, is.null))){
        input_data = data.frame(input_data)
        input_list = list(input_data)
        names(input_list) = server_db_settings_tbl_id
        db_replace_tables(conn = pool_config, table_list = input_list)

        # update fields (e.g. if name had to be repaired)
        db_settgins_data = RMariaDB::dbReadTable(pool_config, server_db_settings_tbl_id)
        lapply(form_input_ids, function(x){
          shiny::updateTextInput(inputId = x, value = db_settgins_data[,x])
        })
      }
    })

    # Update
    shiny::observeEvent(rv$db_settings_menu_started,{
      if(is.element(server_db_settings_tbl_id, RMariaDB::dbListTables(pool_config))){
        db_settgins_data = RMariaDB::dbReadTable(pool_config, server_db_settings_tbl_id)
        lapply(form_input_ids, function(x){
          shiny::updateTextInput(inputId = x, value = db_settgins_data[,x])
        })
      }
    }, ignoreInit = TRUE)

  })
}

## To be copied in the UI
# mod_module_db_settings_ui("module_db_settings_1")

## To be copied in the server
# mod_module_db_settings_server("module_db_settings_1")
