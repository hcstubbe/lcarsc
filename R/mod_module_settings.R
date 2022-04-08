#' module_settings UI Function
#'
#' @description This modules manages the general server settings/information.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList fluidPage textInput textAreaInput wellPanel fluidPage column actionButton observeEvent
#' @importFrom shinydashboard box
#' @importFrom RMariaDB dbAppendTable
#'
mod_module_settings_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
        shinydashboard::box(width = 12, status = "primary",title = "General settings", solidHeader = TRUE,
                            checkboxInput(inputId = ns("add_diagnoses_panel"), label = "Add diagnoses panel"),
                            checkboxInput(inputId = ns("add_medication_panel"), label = "Add medication panel"),
                            checkboxInput(inputId = ns("add_samples_panel"), label = "Add samples panel"),
                            textInput(ns("pid_prefix"), label = "PID prefix")),
        shinydashboard::box(width = 12, status = "primary",title = "Study information", solidHeader = TRUE,
                            textInput(ns("study_title"), label = "Title"),
                            textInput(ns("study_title_short"), label = "Short study title"),
                            textAreaInput(ns("study_introduction"), label = "Introduction")),
        shinydashboard::box(width = 12, status = "primary",title = "Contacts", solidHeader = TRUE,
          column(6,
                 shinydashboard::box(width = 12, status = "primary",title = "Contact 1",
                                     textInput(ns("contact1_name"), label = "Name of contact"),
                                     textInput(ns("contact1_institute"), label = "Institute"),
                                     textInput(ns("contact1_department"), label = "Department"),
                                     textInput(ns("contact1_street"), label = "Street"),
                                     textInput(ns("contact1_city"), label = "City"),
                                     textInput(ns("contact1_postal_code"), label = "Postal code"),
                                     textInput(ns("contact1_phone"), label = "Phone"),
                                     textInput(ns("contact1_fax"), label = "Fax"),
                                     textInput(ns("contact1_mail"), label = "E-mail")
                 )
          ),
          column(6,
                 shinydashboard::box(width = 12, status = "primary",title = "Contact 2",
                                     textInput(ns("contact2_name"), label = "Name of contact"),
                                     textInput(ns("contact2_institute"), label = "Institute"),
                                     textInput(ns("contact2_department"), label = "Department"),
                                     textInput(ns("contact2_street"), label = "Street"),
                                     textInput(ns("contact2_city"), label = "City"),
                                     textInput(ns("contact2_postal_code"), label = "Postal code"),
                                     textInput(ns("contact2_phone"), label = "Phone"),
                                     textInput(ns("contact2_fax"), label = "Fax"),
                                     textInput(ns("contact2_mail"), label = "E-mail")
                 )
          )
        )
        )

  )
}

#' module_settings Server Functions
#'
#' @noRd
mod_module_settings_server <- function(id, rv){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    pool_config = get_golem_options("pool_config")
    server_settings_tbl_id = "server_settings_tbl"



    ## Gather input data ----
    form_input_bool_ids = c(
                   "add_diagnoses_panel",
                   "add_medication_panel",
                   "add_samples_panel"
    )
    form_input_ids = c(
                   "pid_prefix",
                   "study_title",
                   "study_title_short",
                   "study_introduction",
                   "contact1_name",
                   "contact1_institute",
                   "contact1_department",
                   "contact1_street",
                   "contact1_city",
                   "contact1_postal_code",
                   "contact1_phone",
                   "contact1_fax",
                   "contact1_mail",
                   "contact2_name",
                   "contact2_institute",
                   "contact2_department",
                   "contact2_street",
                   "contact2_city",
                   "contact2_postal_code",
                   "contact2_phone",
                   "contact2_fax",
                   "contact2_mail"
    )


    # Check if settings data table exits and populate if not
    if(!RMariaDB::dbExistsTable(pool_config, server_settings_tbl_id)){
      dbfields = c(rep("BOOLEAN", length(form_input_bool_ids)),
                   rep("TEXT", length(form_input_ids)))
      names(dbfields) = c(form_input_bool_ids, form_input_ids)
      RMariaDB::dbCreateTable(pool_config, name = server_settings_tbl_id, fields = dbfields)

      dbstdvals = c(as.list(rep(FALSE, length(form_input_bool_ids))),
                    as.list(rep("", length(form_input_ids))))
      names(dbstdvals) = c(form_input_bool_ids, form_input_ids)
      dbstdvals = data.frame(dbstdvals)
      RMariaDB::dbAppendTable(pool_config, name = server_settings_tbl_id, value = dbstdvals)
    }


    # Save submission
    observeEvent(rv$save_settings_button,{
      input_data = sapply(c(form_input_ids, form_input_bool_ids),
                          function(x) input[[x]],
                          simplify = FALSE,
                          USE.NAMES = TRUE)
      if(!all(sapply(input_data, is.null))){
        input_data = data.frame(input_data)
        input_list = list(input_data)
        names(input_list) = server_settings_tbl_id
        db_replace_tables(conn = pool_config, table_list = input_list)
      }
    })

    # Update
    shiny::observeEvent(rv$settings_menu_started,{
      if(is.element(server_settings_tbl_id, RMariaDB::dbListTables(pool_config))){
        db_settgins_data = RMariaDB::dbReadTable(pool_config, server_settings_tbl_id)
        lapply(form_input_bool_ids, function(x){
          x_val = db_settgins_data[,x]
          if(x_val == TRUE){
            x_val = TRUE
          }else{
            x_val = FALSE
          }
          shiny::updateCheckboxInput(inputId = x, value = x_val)
        })
        lapply(form_input_ids, function(x){
          shiny::updateTextInput(inputId = x, value = db_settgins_data[,x])
        })
      }
    }, ignoreInit = TRUE)

  })
}

## To be copied in the UI
# mod_module_settings_ui("module_settings_1")

## To be copied in the server
# mod_module_settings_server("module_settings_1")
