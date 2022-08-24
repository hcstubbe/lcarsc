#' module_new_pat UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom  golem get_golem_options
#' @importFrom shinyvalidate InputValidator sv_required
#' @importFrom uuid UUIDgenerate
#'
mod_module_new_pat_ui <- function(id) {
  ns = NS(id)

  settgins_data = golem::get_golem_options("settings_data")

  tagList(
    fluidRow(
      column(10,
             box(
               title = internal_app_data$lang_sel$menu_inclusion, width = 12, status = "primary", solidHeader = TRUE,
               div(
                 mod_module_edit_tab_ui(id = ns("mod_module_edit_tab_inclusion"))
               )
             ),
             if(settgins_data$allow_manual_pid == TRUE){
               box(title = ("Add new PID manually"), width = 12, status = "primary", solidHeader = TRUE,
                   textInput(ns("new_pid_manual"), "Enter new PID"),
                   actionButton(ns("add_pid"), "Save new PID!", icon = shiny::icon("save",
                                                                       verify_fa = FALSE))
               )
             }else{NULL}
             #,
             # box(title = ("Upload patient list"), width = 12, status = "primary", solidHeader = TRUE,
             #     fileInput(ns("pat_upload"), "Upload data file (CSV)",
             #               multiple = FALSE,
             #               accept = c(".csv"))
             # )
      )
    )
  )
}

#' module_new_pat Server Functions
#'
#' @noRd
mod_module_new_pat_server <- function(id, visit_id, data_table, preview = FALSE) {
  moduleServer(id, function(input, output, session) {

    ns = session$ns

    settgins_data = golem::get_golem_options("settings_data")

    # If the form is used for the preview, use local database
    prod_mod = get_production_mode(production_mode = get_golem_options("production_mode"),
                                   pool_config = get_golem_options("pool_config"))
    if(prod_mod == "editor"){
      pool = pool::dbPool(
        drv = RSQLite::SQLite(),
        dbname = "db_preview.sqlite3",
        host = "dbeditor",
        username = "user",
        password = "user"
      )
    }else if(prod_mod == "production"){
      pool = golem::get_golem_options("pool")
    }else{
      stop("Cannot determine deployment status (production or editor?)!")
    }

    widget_data_input = load_widget_data(pool_config = golem::get_golem_options("pool_config"),
                                         production_mode = golem::get_golem_options("production_mode"))


  	# Load required data
  	widgets_table_global = widget_data_input$widgets_table_global
  	all_visits = widget_data_input$all_visits
  	all_tabs = widget_data_input$all_tabs

    # Upload participant file ----
    # observe({
    #   if (is.null(input$pat_upload)) return()
    #
    #   input_csv = read.csv(input$pat_upload$datapath)
    #
    #   df_generic = list(row_id = sapply(1:nrow(input_csv), function(x) UUIDgenerate()),
    #                     user_modified = "NA",
    #                     pid = input_csv$Patnr,
    #                     date_modified = as.character(date()),
    #                     visit_id = "vi",
    #                     deleted_row = "FALSE",
    #                     submitted_row = "TRUE",
    #                     Patnr = input_csv$Patnr,
    #                     Fall = input_csv$Fall,
    #                     documented_case = "FALSE") %>% data.frame
    #   dbAppendTable(pool, data_table, df_generic)
    # })


    ## Start sub-module server
    rv_downstream_visit = reactiveValues()
    rv_downstream_visit$pid = reactive({"init"})
    mod_module_edit_tab_server(id = "mod_module_edit_tab_inclusion",
                           widget_tab_selection = 'visit',
                           tbl_id = "inclusion_dataset",
                           rv_in = rv_downstream_visit,
                           show_vals = c(PID = 'pid', Date = 'date_modified', User = 'user_modified', Visit = 'visit_id', Submitted = 'submitted_row'),
                           # show_user_vals = TRUE,
                           # n_show_vals = 7,
                           simple = FALSE,
                           modal_width = '.modal-dialog{ width:95%}',
                           widgets_table_global = widgets_table_global[widgets_table_global[,"vi"],],
                           all_tabs = all_tabs,
                           all_visits = all_visits,
                           visit_id = "vi",
                           create_new_pid = TRUE,
                           order.by = "date_modified",
                           order_desc = TRUE,
                           oder_by_date = TRUE,
                           preview = preview,
                           show_preliminary = TRUE,
                           is_editor_or_vi = TRUE)

    ## Add manual PID ----------------

    if(settgins_data$allow_manual_pid == TRUE){

      iv <- shinyvalidate::InputValidator$new()

      iv$add_rule("new_pid_manual", shinyvalidate::sv_required())

      iv$add_rule(
        "new_pid_manual",
        shinyvalidate::sv_regex("^[a-zA-Z0-9-]*$", "Only alphanumeric characters allowed")
      )

      id_prefix = settgins_data$pid_prefix

      if(!is.null(id_prefix)){
        iv$add_rule(
          "new_pid_manual",
          shinyvalidate::sv_regex(paste0("^", id_prefix, "-"), "The PID prefix does not match!")
        )
        iv$add_rule("new_pid_manual", ~ if(!is.null(.)){if (nchar(.) < (nchar(id_prefix) + 1 + 3)) {"Minimum length is 3 (not counting the prefix)!"}})
      }else{
        iv$add_rule("new_pid_manual", ~ if(!is.null(.)){if (nchar(.) <  3) {"Minimum length is 3!"}})
      }

      observeEvent(input$add_pid, {
        iv$enable()
        if(iv$is_valid()) {
          db_cmd = paste0("SELECT pid FROM inclusion_dataset WHERE deleted_row = 0")
          exisiting_IDs = (RMariaDB::dbGetQuery(pool, db_cmd))$pid
          if(!(input$new_pid_manual %in% exisiting_IDs)){
            showModal(modalDialog(
              title = paste0("Confirm new PID: ", input$new_pid_manual),
              "Please confirm, that you want to submit a new entry with the PID ", shiny::strong(input$new_pid_manual), "!",
              easyClose = TRUE,
              footer = div(actionButton(ns("confirm_submit_pid"), "Confirm!"), modalButton("Dismiss"))
            ))
          }
        }
      })

      observeEvent(input$confirm_submit_pid, {
        df_generic = list(row_id = uuid::UUIDgenerate(),
                          user_modified = get_current_user(),
                          pid = input$new_pid_manual,
                          date_modified = as.character(date()),
                          visit_id = "vi",
                          deleted_row = 0,
                          submitted_row = 0) %>% data.frame
        dbAppendTable(pool, data_table, df_generic)
        shiny::removeModal()
        iv$disable()
        shiny::updateTextInput(inputId = "new_pid_manual", value="")
      })

    }









  })

}

## To be copied in the UI
# mod_module_new_pat_ui("module_new_pat_1")

## To be copied in the server
# mod_module_new_pat_server("module_new_pat_1")
