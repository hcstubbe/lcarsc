#' module_launcher UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @import dplyr
#' @importFrom shiny NS tagList
#' @importFrom shinydashboard dashboardPage
#' @importFrom golem get_golem_options
#' @importFrom RMariaDB dbReadTable
mod_module_launcher_ui <- function(id){
  ns = NS(id)

  server_settings_tbl_id = "server_settings_tbl"
  pool_config = get_golem_options("pool_config")
  settgins_data = RMariaDB::dbReadTable(pool_config, server_settings_tbl_id)

  tagList(
    div(
      shinydashboard::dashboardPage(
        skin = "blue",
        dashboardHeader(title = settgins_data$study_title_short,
                        tags$li(class = "dropdown",
                                tags$li(class = "dropdown",
                                        actionLink("login", " Logout", icon = icon("lock"),
                                                   onclick = "window.location.href='/logout'")
                                )
                        )),
        dashboardSidebar(
          sidebarMenu(
            menuItem(internal_app_data$lang_sel$tab_start, tabName = "start"),
            menuItem(internal_app_data$lang_sel$button_newpat, tabName = "new_pat"),
            menuItem(internal_app_data$lang_sel$main_menu_visit, tabName = "pat_list"),
            menuItem(internal_app_data$lang_sel$module_launcher_menu_contact, tabName = "contact"),
            if(get_golem_options("user_is_admin")){
              menuItem("Admin", tabName = "admin")
            }else{
              NULL
            }
          )
        ),
        dashboardBody(
          tabItems(
            tabItem("start",
                    shinydashboard::box(title = settgins_data$study_title,
                                        status = "primary", solidHeader = FALSE,
                                        settgins_data$study_introduction
                    )
            ),

            tabItem("new_pat",
                    mod_module_new_pat_ui(ns("mod_module_new_pat"))

            ),

            tabItem("pat_list",
                    mod_module_documentation_ui(ns("mod_module_documentation"))


            ),


            # Dashboard item providing Contact data
            tabItem("contact",
                    shinydashboard::box(title = "Contact", status = "primary", solidHeader = FALSE,
                                        width = 10,
                                        column(6,
                                               h4(settgins_data$contact1_name), br(),
                                               settgins_data$contact1_department, br(),
                                               settgins_data$contact1_institute, br(),
                                               settgins_data$contact1_street, br(),
                                               settgins_data$contact1_city,
                                               settgins_data$contact1_postal_code, br(),
                                               paste0("Tel.: ", settgins_data$contact1_phone), br(),
                                               paste0("Fax: ", settgins_data$contact1_fax), br(),
                                               HTML(paste0("E-mail: <a href='mailto:", settgins_data$contact1_mail,"' target='_top'>", settgins_data$contact1_mail,"</a>"))
                                        ),
                                        column(6,
                                               h4(settgins_data$contact2_name), br(),
                                               settgins_data$contact2_department, br(),
                                               settgins_data$contact2_institute, br(),
                                               settgins_data$contact2_street, br(),
                                               settgins_data$contact2_city,
                                               settgins_data$contact2_postal_code, br(),
                                               paste0("Tel.: ", settgins_data$contact2_phone), br(),
                                               paste0("Fax: ", settgins_data$contact2_fax), br(),
                                               HTML(paste0("E-mail: <a href='mailto:", settgins_data$contact2_mail,"' target='_top'>", settgins_data$contact2_mail,"</a>"))
                                        )
                    )
            ),
            if(get_golem_options("user_is_admin")){
              tabItem("admin",
                      mod_module_admin_ui(ns("module_admin_1")))
            }else{
              tabItem("admin")
            }
          )
        )
      )
    )
  )
}

#' module_launcher Server Functions
#'
#' @noRd
mod_module_launcher_server <- function(id){
  moduleServer(id, function(input, output, session) {


    # Launch module servers ----

    # Module in Tab 1
    tryCatch(
      mod_module_new_pat_server(id = "mod_module_new_pat",
                                visit_id = "vi",
                                data_table = "inclusion_dataset")
      ,
      error = function(e) {
        showNotification(paste0("Starting inclusion submodule failed: ", e), duration = 10, type = "error")
        return(NULL)
      }
    )

    # Module in Tab 2
    tryCatch(
      mod_module_documentation_server(id = "mod_module_documentation",
                                      data_table1 = "inclusion_dataset",
                                      data_table2 = "scientific_dataset")
      ,
      error = function(e) {
        showNotification(paste0("Starting documentation submodule failed: ", e), duration = 300, type = "error")
        return(NULL)
        }
    )

    if(get_golem_options("user_is_admin")){
      mod_module_admin_server("module_admin_1")
    }

  })
}

## To be copied in the UI
# mod_module_launcher_ui("module_launcher_1")

## To be copied in the server
# mod_module_launcher_server("module_launcher_1")
