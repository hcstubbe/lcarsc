#' module_launcher UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @importFrom shiny NS tagList div a
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar dropdownMenu menuItem dashboardBody tabItems tabItem notificationItem sidebarMenu
#' @importFrom shinyWidgets sendSweetAlert show_toast
#' @importFrom golem get_golem_options
#' @import dplyr
#' @import purrr
mod_module_launcher_edit_ui <- function(id){

  ns = NS(id)


  if(RMariaDB::dbExistsTable(get_golem_options("pool_config"), "server_settings_tbl")){
    add_mobile_app = (RMariaDB::dbReadTable(get_golem_options("pool_config"), "server_settings_tbl"))[,"add_mobile_app"]
  }else{
    add_mobile_app = FALSE
  }

  # Create drop down menu items
  dropdown_menu_list = list(notificationItem("General settings", icon = icon("tools", verify_fa = FALSE), status = "primary"),
            notificationItem("Database setup", icon = icon("database", verify_fa = FALSE), status = "primary"),
            notificationItem("Deployment", status = "danger", icon = icon("warning", verify_fa = FALSE)))
  for(i in 1:length(dropdown_menu_list)){
    dropdown_menu_list[[i]]$children[[1]] <- shiny::a(href="#","onclick"=paste0("clickFunction('",paste0("menuitem_", i),"'); return false;"),
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
            menuItem(internal_app_data$lang_sel$tab_start, tabName = "start", icon = icon("info", verify_fa = FALSE)),
            menuItem("Library", tabName = "library", icon = icon("book", verify_fa = FALSE)),
            menuItem("Editor", tabName = "editor", icon = icon("keyboard-o", verify_fa = FALSE)),
            menuItem("Preview", tabName = "preview", icon = icon("video-camera", verify_fa = FALSE))
            ,
            if("lcarsM" %in% rownames(installed.packages()) & add_mobile_app == TRUE){
              menuItem("Preview mobile", tabName = "preview_mobile", icon = icon("mobile", verify_fa = FALSE))
            }else{
                NULL
            },
            if(get_golem_options("user_is_admin")){
              menuItem("Admin", tabName = "admin", icon = icon("unlock", verify_fa = FALSE))
            }else{
              NULL
            },
            tags$footer(paste0("LCARS-C Version ", get_golem_options("version")), align = "right", style = "
                         text-align: center;
                         position:absolute;
                         bottom:0;
                         width:100%;
                         height:30px; /* Height of the footer */
                         color: grey;
                         padding: 5px;
                         background-color: black;
                         z-index: 1000;"

            )
            # ,
            # menuItem("Data", tabName = "Data")

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

                                        "This software is for research use only.", br(), br(),

                                        "Use the ",  strong("library"), "to select and add variables to your eCRF.", br(), br(),

                                        "Use the ",  strong("editor"), "to create new variables for your eCRF.", br(), br(),

                                        "In the drop-down menu (top-right corner) use ",  strong("General settings"), " to specify informations of your study.", br(),
                                        "Use ",  strong("Database settings"), " to name user groups and variables (e.g. when deploying on shinyproxy; see documentation).", br(),
                                        "Use ",  strong("Deployment"), " to deploy your eCRF into production. Be sure that you want to deploy and that you tested everything using ", strong("Preview"), ". This cannot be undone!", br(), br(),

                                        "Please ", strong("sign out"), " after working with this software."
                    )
            )
            ,
            tabItem("library",
                    mod_module_library_ui(ns("module_library_1")))
            ,
            tabItem("editor",
                    mod_module_editor_launcher_ui(ns("mod_module_editor")))

            ,

            tabItem("preview",
                    mod_module_preview_ui(ns("module_preview_1")))
            ,
            #
            #
            if("lcarsM" %in% rownames(installed.packages()) & add_mobile_app == TRUE){
              tabItem("preview_mobile",
                      mod_module_preview_mobile_ui(ns("module_preview_mobile_1")))
            }else{
              tabItem("Data",
                      div())
            },
            if(get_golem_options("user_is_admin")){
              tabItem("admin",
                      mod_module_admin_ui(ns("module_admin_1")))
            }else{
              tabItem("admin")
            }
            # ,
            # if("app_tbl" %in% dbListTables(golem::get_golem_options("pool"))){
            #   tabItem("Data",
            #           mod_module_data_center_ui(ns("module_data_center_1")))
            # }else{
            #   tabItem("Data",
            #           div())
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

  	# Module library
  	mod_module_library_server(id = "module_library_1")

    # Module editor
    mod_module_editor_launcher_server(id = "mod_module_editor")

  	# Module preview
	  mod_module_preview_server(id = "module_preview_1")

	  # Fill settings data with dummy data if it does not exist
	  if(RMariaDB::dbExistsTable(get_golem_options("pool_config"), "server_settings_tbl")){
	    add_mobile_app = (RMariaDB::dbReadTable(get_golem_options("pool_config"), "server_settings_tbl"))[,"add_mobile_app"]
	  }else{
	    add_mobile_app = FALSE
	  }


  	# Module preview mobile
	  if("lcarsM" %in% rownames(installed.packages()) & add_mobile_app == TRUE){
	    mod_module_preview_mobile_server(id = "module_preview_mobile_1")
	  }

	  #
# 	  if("app_tbl" %in% dbListTables(golem::get_golem_options("pool"))){
# 	    mod_module_data_center_server("module_data_center_1")
# 	  }


  	# Module drop down menu
  	mod_module_deploy_server(id = "module_deploy_1")

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

  	if(get_golem_options("user_is_admin")){
  	  mod_module_admin_server("module_admin_1")
  	}


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
