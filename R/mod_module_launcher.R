#' module_launcher UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_module_launcher_ui <- function(id){
  ns = NS(id)
  tagList(
    div(
      dashboardPage(
        dashboardHeader(title = lang_sel$app_title),
        dashboardSidebar(
          sidebarMenu(
            menuItem(lang_sel$tab_start, tabName = "start"),
            menuItem(lang_sel$button_newpat, tabName = "new_pat"),
            menuItem(lang_sel$main_menu_visit, tabName = "pat_list"),
            menuItem(lang_sel$module_launcher_menu_contact, tabName = "contact"),
            menuItem("Editor", tabName = "editor")
          )
        ),
        dashboardBody(
          tabItems(
            tabItem("start",
                    shinydashboard::box(title = "The Post-COVID-Care-Study", status = "primary", solidHeader = FALSE,
                                        "For baseline, please fill out the required variables for patient inclusion. If you find the time to enter the additional sections, it would be great!", br(), br(),

                                        "Please make sure, you ", strong("noted"),  " the ", strong("PID"), "before submitting a new participant!", br(),
                                        "You need it to complete the follow-up.",br(), br(),

                                        "Please note that the form is ", strong("not saved until you save or submit"),   " the data.", br(), br(),

                                        "Please ", strong("sign out"),   " after working with the eCRF.", br(), br(),

                                        "For",  strong("questions"), "or", strong("problems"), "regarding the eCRF, please contact:", br(),
                                        "Dr. med. Hans Christian Stubbe", br(),
                                        "Marchioninistr. 15, 81377 München", br(),
                                        "Tel.: +49 (0)89 4400 0", br(),
                                        "Fax: +49 (0)89 4400 78856", br(),
                                        HTML("E-mail: <a href='mailto:hans_christian.stubbe@med.uni-muenchen.de' target='_top'>hans_christian.stubbe@med.uni-muenchen.de</a>")
                    )
            ),

            tabItem("new_pat",
                    mod_module_new_pat_ui(ns("mod_module_new_pat"))

            ),

            tabItem("pat_list",
                    mod_module_documentation_ui(ns("mod_module_documentation"))


            ),

            # tabItem("split_documentation",
            #         mod_module_split_documentation_ui(ns("mod_module_documentation_split"))
            #
            #
            #
            # ),

            # Dashboard item providing Contact data
            tabItem("contact",
                    shinydashboard::box(title = lang_sel$module_launcher_menu_contact_technical, status = "primary", solidHeader = FALSE,
                                        width = 10,
                                        column(6,
                                               h4("Dr. med. Hans Christian Stubbe"), br(),
                                               "Medizinische Klinik und Poliklinik II", br(),
                                               "LMU Klinikum München", br(),
                                               "Campus Großhadern", br(),
                                               "Marchioninistr. 15, 81377 München", br(),
                                               "Tel.: +49 (0)89 4400 0", br(),
                                               "Fax: +49 (0)89 4400 78856", br(),
                                               HTML("E-mail: <a href='mailto:hans_christian.stubbe@med.uni-muenchen.de' target='_top'>hans_christian.stubbe@med.uni-muenchen.de</a>")
                                        ),
                                        column(6,
                                               h4("Dr. rer. med. Ujjwal Mukund Mahajan"), br(),
                                               "Medizinische Klinik und Poliklinik II", br(),
                                               "LMU Klinikum München", br(),
                                               "Campus Großhadern", br(),
                                               "Marchioninistr. 15, 81377 München", br(),
                                               "Tel: +49 (0) 89 4400 76125", br(),
                                               "Fax:+49 (0) 89 4400 78856", br(),
                                               HTML("E-mail: <a href='mailto:ujjwal_mukund.mahajan@med.uni-muenchen.de' target='_top'>ujjwal_mukund.mahajan@med.uni-muenchen.de</a>")
                                        )
                    )
            ),
            tabItem("editor",
                    mod_module_editor_launcher_ui(ns("mod_module_editor"))



            )

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
    mod_module_new_pat_server(id = "mod_module_new_pat",
                          visit_id = "vi",
                          data_table = "inclusion_dataset")

    # Module in Tab 2
    mod_module_documentation_server(id = "mod_module_documentation",
                                data_table1 = "inclusion_dataset",
                                data_table2 = "scientific_dataset"
    )

    # # Module in Tab 2a
    # mod_module_split_documentation_server(id = "mod_module_documentation_split",
    #                             data_table1 = "inclusion_dataset",
    #                             data_table2 = "scientific_dataset"
    # )

    # Module editor
    mod_module_editor_launcher_server(id = "mod_module_editor")


  })
}

## To be copied in the UI
# mod_module_launcher_ui("module_launcher_1")

## To be copied in the server
# mod_module_launcher_server("module_launcher_1")
