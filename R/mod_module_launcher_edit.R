#' module_launcher UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @import dplyr
#' @importFrom shiny NS tagList div
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar dropdownMenu menuItem dashboardBody tabItems tabItem
#' @importFrom shinyWidgets sendSweetAlert show_toast
#' @import dplyr
#' @import purrr
mod_module_launcher_edit_ui <- function(id){

  ns = NS(id)

  # Create drop down menu items
  dropdown_menu_list = list(notificationItem("General settings", icon = shiny::icon("tools", verify_fa = FALSE), status = "primary"),
            notificationItem("Database setup", icon = shiny::icon("database", verify_fa = FALSE), status = "primary"),
            notificationItem("Deployment", status = "danger"))
  for(i in 1:length(dropdown_menu_list)){
    dropdown_menu_list[[i]]$children[[1]] <- a(href="#","onclick"=paste0("clickFunction('",paste0("menuitem_", i),"'); return false;"),
                               dropdown_menu_list[[i]]$children[[1]]$children)
  }



  tagList(
    div(
      shinydashboard::dashboardPage(skin = "yellow",
        dashboardHeader(title = "Editor",
                        dropdownMenu(.list = dropdown_menu_list, type = "notification", badgeStatus = NULL, icon = shiny::icon("gear", verify_fa = FALSE),headerText = "Setup")
                        ),
        dashboardSidebar(
          sidebarMenu(
            menuItem(internal_app_data$lang_sel$tab_start, tabName = "start"),
            menuItem(internal_app_data$lang_sel$module_launcher_menu_contact, tabName = "contact"),
            menuItem("Editor", tabName = "editor"),
            menuItem("Preview", tabName = "preview")
            # ,
            # menuItem("Preview mobile", tabName = "preview_mobile")

          )
        ),
        dashboardBody(
          tags$script(HTML(paste0("function clickFunction(link){
                      var rndm = Math.random();
                       Shiny.onInputChange('", ns("linkClicked"), "', {data:link, nonce: Math.random()});}")
          )),
          tabItems(
            tabItem("start",
                    shinydashboard::box(title = "LCARS-C", status = "primary", solidHeader = FALSE,
                                        "Please fill out the required visits and variables in the editor.", br(), br(),

                                        "Please ", strong("sign out"),   " after working with the eCRF.", br(), br(),

                                        "For",  strong("questions"), "or", strong("problems"), "regarding the eCRF, please contact:", br(),
                                        "Dr. med. Hans Christian Stubbe", br(),
                                        "Marchioninistr. 15, 81377 München", br(),
                                        "Tel.: +49 (0)89 4400 0", br(),
                                        "Fax: +49 (0)89 4400 78856", br(),
                                        HTML("E-mail: <a href='mailto:hans_christian.stubbe@med.uni-muenchen.de' target='_top'>hans_christian.stubbe@med.uni-muenchen.de</a>")
                    )
            ),

            # Dashboard item providing Contact data
            tabItem("contact",
                    shinydashboard::box(title = internal_app_data$lang_sel$module_launcher_menu_contact_technical, status = "primary", solidHeader = FALSE,
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
            )
            ,
            tabItem("editor",
                    mod_module_editor_launcher_ui(ns("mod_module_editor")))

            ,

            tabItem("preview",
                    mod_module_preview_ui(ns("module_preview_1")))
            # ,
            #
            #
            # if("lcarsM" %in% rownames(installed.packages())){
            #   tabItem("preview_mobile",
            #           mod_module_preview_mobile_ui(ns("module_preview_mobile_1")))
            # }else{
            #   NULL
            # }



          )
        )
      )
    )
  )
}

#' module_launcher Server Functions
#'
#' @noRd
mod_module_launcher_edit_server <- function(id){
  moduleServer(id, function(input, output, session) {

	ns = session$ns



    # Launch module servers ----

    # Module editor
    mod_module_editor_launcher_server(id = "mod_module_editor")

  	# Module preview
	  mod_module_preview_server(id = "module_preview_1")

#   	# Module preview mobile
# 	  if("lcarsM" %in% rownames(installed.packages())){
# 	    mod_module_preview_mobile_server(id = "module_preview_mobile_1")
# 	  }

  	# Module drop down menu
  	mod_module_deploy_server("module_deploy_1")

  	# Module general settings
  	rv_settings = reactiveValues()
  	rv_settings$settings_menu_started = reactive({0})
  	rv_settings$save_settings_button = reactive({0})
  	mod_module_settings_server("module_settings_1", rv = rv_settings)

  	# Module database settings
  	rv_db_settings = reactiveValues()
  	rv_db_settings$settings_menu_started = reactive({0})
  	rv_db_settings$save_settings_button = reactive({0})
  	mod_module_db_settings_server("module_db_settings_1", rv = rv_db_settings)


    # Observe drop down menu ----
  	observeEvent(input$linkClicked, {

      showModal(
        if( input$linkClicked$data == "menuitem_3" ){
          modalDialog(
            tags$head(tags$style(".modal-dialog{ width:50% }")),
            title = "Deployment",
            mod_module_deploy_ui(ns("module_deploy_1")))
        }else if(input$linkClicked$data == "menuitem_2"){
          modalDialog(
            tags$head(tags$style(".modal-dialog{ width:50% }")),
            title = "Database settings",
            mod_module_db_settings_ui(ns("module_db_settings_1")),
            footer = fluidRow(
              actionButton(ns("save_db_settings_button"), "Save", icon("save", verify_fa = FALSE)),
              actionButton(ns("close_sb_settings_button"), "Close", icon("close", verify_fa = FALSE))
            )
          )
        }else if(input$linkClicked$data == "menuitem_1"){
          modalDialog(
            tags$head(tags$style(".modal-dialog{ width:50% }")),
            title = "General settings",
            mod_module_settings_ui(ns("module_settings_1")),
            footer = fluidRow(
              actionButton(ns("save_settings_button"), "Save", icon("save", verify_fa = FALSE)),
              actionButton(ns("close_settings_button"), "Close", icon("close", verify_fa = FALSE))
            )
            )
        }
      )
  	  rv_settings$settings_menu_started = reactive({rv_settings$settings_menu_started() + 1})
  	  rv_db_settings$db_settings_menu_started = reactive({rv_db_settings$db_settings_menu_started() + 1})
  	})

  	# Observer for settings module
  	observeEvent(input$save_settings_button,{
      rv_settings$save_settings_button = reactive({rv_settings$save_settings_button() + 1})
      showNotification("Data saved", type = "message")
      # shinyWidgets::show_toast(title = "Update study info", text = "Data saved!", type = "success", position = "bottom")
  	})
  	observeEvent(input$close_settings_button,{
  	  shiny::removeModal()
  	})


  	# Observer for database settings module
  	observeEvent(input$save_db_settings_button,{
  	  rv_db_settings$save_db_settings_button = reactive({rv_db_settings$save_db_settings_button() + 1})
  	  showNotification("Data saved", type = "message")
  	  # shinyWidgets::show_toast(title = "Update study info", text = "Data saved!", type = "success", position = "bottom")
  	})
  	observeEvent(input$close_sb_settings_button,{
  	  shiny::removeModal()
  	})





  })
}

## To be copied in the UI
# mod_module_launcher_ui("module_launcher_1")

## To be copied in the server
# mod_module_launcher_server("module_launcher_1")
