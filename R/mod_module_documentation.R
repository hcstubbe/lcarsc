#' module_documentation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import dplyr
mod_module_documentation_ui  <- function(id) {
  ns = NS(id)
  tagList(
    fluidRow(
      column(5,
             box(title = (internal_app_data$lang_sel$module_documentation_pt_list_title), width = 12, status = "primary", solidHeader = TRUE,
                 actionButton(ns("update_pull_user"), label = internal_app_data$lang_sel$update_pull, icon("sync", verify_fa = FALSE)),
                 br(),
                 br(),
                 br(),
                 DT::dataTableOutput(ns("responses_user"))),
             mod_module_documentation_summary_ui(ns("module_documentation_summary_1"))
      ),
      column(7,
             uiOutput(ns("visit_submission_panel"))
      )
    )
  )
}

#' module_documentation Server Functions
#'
#' @noRd
mod_module_documentation_server <- function(id, data_table1, data_table2, preview = FALSE) {
  moduleServer(id, function(input, output, session) {

    # Requirements ----
    ns = session$ns
    widget_data_input = load_widget_data(pool_config = golem::get_golem_options("pool_config"),
                                         production_mode = golem::get_golem_options("production_mode"))

    # Get the data base connection
    pool = get_golem_options("pool")


    # Get the settings data
    server_settings_tbl_id = "server_settings_tbl"

    # Fill settigns data with dummy data if it does not exist
    if(RMariaDB::dbExistsTable(get_golem_options("pool_config"), server_settings_tbl_id)){
      settgins_data = RMariaDB::dbReadTable(get_golem_options("pool_config"), server_settings_tbl_id)
    }else{
      settgins_data = data.frame(add_diagnoses_panel = FALSE,
                                 add_medication_panel = FALSE,
                                 add_samples_panel = FALSE)
    }
    for(i in c("add_diagnoses_panel",
               "add_medication_panel",
               "add_samples_panel")){
      if(is.null(settgins_data[,i])){
        settgins_data[,i] = FALSE
      }
    }



    # If the form is used for the preview, use local database
    prod_mod = get_production_mode(production_mode = get_golem_options("production_mode"),
                                   pool_config = get_golem_options("pool_config"))
    if(prod_mod == "editor" & preview == TRUE){
      pool = pool::dbPool(
        drv = RSQLite::SQLite(),
        dbname = "db_preview.sqlite3",
        host = "dbeditor",
        username = "user",
        password = "user"
      )
    }

	widgets_table_global = widget_data_input$widgets_table_global
	all_visits = widget_data_input$all_visits
	all_tabs = widget_data_input$all_tabs
    ordered_visits = widget_data_input$ordered_visits
	visit_choices = widget_data_input$visit_choices

	rv_downstream = reactiveValues()

    # Auxiliary functions ----

    # Functions for loading data for rendering data table
    computeFT = function(){
      y = loadData(pool, data_table1) %>% filter(deleted_row == FALSE & submitted_row == TRUE) %>% select(c("pid", "date_modified"))
      y
    }
    load_dt_for_render = function(){
      DT::datatable(computeFT(),
                    options = list(pageLength = 5), selection = c("single"))}


    # Select non-inclusion visits
    ordered_visits = ordered_visits %>% filter(visit_id != "vi")



    ## Start sub-module servers
    rv_downstream_visit = reactiveValues()
    rv_downstream_visit$pid = reactive({computeFT()$pid[input$responses_user_rows_selected]})

    for(i in ordered_visits$visit_id){
      cmd_4_eval = paste("mod_module_edit_tab_server(id = paste('mod_module_edit_tab_visit','", i, "', sep = '_'),
                             widget_tab_selection = 'visit',
                             tbl_id = paste('visit_table', '", i, "', sep = '_'),
                             rv_in = rv_downstream_visit,
                             show_vals = c(PID = 'pid', Date = 'date_modified', Visit = 'visit_id', Submitted = 'submitted_row'),
                             simple = FALSE,
                             modal_width = '.modal-dialog{ width:95%}',
                             widgets_table_global = widgets_table_global[widgets_table_global[,i],],
                             all_tabs = all_tabs,
                             order.by = NULL,
                             preview = preview,
                             all_visits = all_visits,
                             visit_id = '", i, "')", sep = "")
      eval(parse(text = cmd_4_eval))
    }


    rv_downstream_summary = reactiveValues()
    rv_downstream_summary$pid = reactive({computeFT()$pid[input$responses_user_rows_selected]})
    mod_module_documentation_summary_server(id = "module_documentation_summary_1",
                                            rv_in = rv_downstream_summary)

    # Diagnoses field
    rv_downstream_diag = reactiveValues()
    rv_downstream_diag$pid = reactive({computeFT()$pid[input$responses_user_rows_selected]})
    if(settgins_data$add_diagnoses_panel == TRUE){
      mod_module_edit_tab_server(id = "mod_module_edit_tab_diag",
                                 widget_tab_selection = "diagnosis",
                                 tbl_id = "diagnoses_table",
                                 rv_in = rv_downstream_diag,
                                 show_vals = c(Diagnosis = "diag_name", Start = "diag_start", End = "diag_end", Submitted = "submitted_row"),
                                 widgets_table_global = widgets_table_global,
                                 all_visits = all_visits,
                                 visit_id = "diagnosis",
                                 order.by = NULL,
                                 preview = preview
      )
    }

    # Medication field
    if(settgins_data$add_medication_panel == TRUE){
      rv_downstream_med = reactiveValues()
      rv_downstream_med$pid = reactive({computeFT()$pid[input$responses_user_rows_selected]})
      mod_module_edit_tab_server(id = "mod_module_edit_tab_med",
                                 widget_tab_selection = "medication",
                                 tbl_id = "medication_table",
                                 rv_in = rv_downstream_med,
                                 show_vals = c(Substance = "med_substance", Dosis = "med_dosing", 'Application route' = "med_route", Start = "med_start", End = "med_end", Submitted = "submitted_row"),
                                 widgets_table_global = widgets_table_global,
                                 all_visits = all_visits,
                                 visit_id = "medication",
                                 order.by = NULL,
                                 preview = preview)
    }


    # Samples field
    if(settgins_data$add_samples_panel == TRUE){

      rv_downstream_smp = reactiveValues()
      rv_downstream_smp$pid = reactive({computeFT()$pid[input$responses_user_rows_selected]})
      mod_module_edit_tab_server(id = "mod_module_edit_tab_smp",
                                 widget_tab_selection = "samples",
                                 tbl_id = "samples_table",
                                 rv_in = rv_downstream_smp,
                                 show_vals = c(PID = 'pid', 'Sample ID' = 'smp_id', Date = 'date_modified', User = 'user_modified', Submitted = 'submitted_row'),
                                 widgets_table_global = widgets_table_global,
                                 all_visits = all_visits,
                                 visit_id = "samples",
                                 order.by = NULL,
                                 preview = preview,
                                 create_sample_id = TRUE,
                                 sample_id_name = "smp_id",
                                 noletters_smp_id = TRUE)

    }



    # Observers ----
    ## Update participant table ----
    observeEvent(input$update_pull_user, {
      output$responses_user <- DT::renderDataTable({
        load_dt_for_render()
      })
    })

    # Render UI elements ----

    ## Load data from database for included patients
    output$responses_user <- DT::renderDataTable({
      load_dt_for_render()
    })


    ##
    output$docu_tab_ui = renderUI({
      mod_module_edit_tab_ui(id = ns(paste("mod_module_edit_tab_visit", input$visit_selector, sep = "_")))
    })

    # Render menu when participant is selected
    names(visit_choices) = all_visits$visit_title[!all_visits$inclusion_other_visit]
    output$visit_submission_panel = renderUI({

      if(length(input$responses_user_rows_selected) == 1){
        div(
          shinydashboard::box(
            title = (internal_app_data$lang_sel$module_documentation_visit_menu),width = 12, status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
            wellPanel(
              selectInput(
                inputId = ns("visit_selector"),
                label = internal_app_data$lang_sel$module_documentation_visit_selector,
                choices = c(visit_choices)
              )
            ),
            uiOutput(ns("docu_tab_ui"))
          ),
          if(settgins_data$add_diagnoses_panel == TRUE){
            shinydashboard::box(
              title = (internal_app_data$lang_sel$module_documentation_diagnoses_info),width = 12, status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
              #strong(internal_app_data$lang_sel$module_documentation_diagnoses_info),
              mod_module_edit_tab_ui(ns("mod_module_edit_tab_diag"))
            )
          },
          if(settgins_data$add_medication_panel == TRUE){
            box(
              title = (internal_app_data$lang_sel$module_documentation_medication_info),width = 12, status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
              #strong(internal_app_data$lang_sel$module_documentation_medication_info),
              mod_module_edit_tab_ui(ns("mod_module_edit_tab_med"))
            )
          },
          if(settgins_data$add_samples_panel == TRUE){
            box(
              title = ("Samples"),width = 12, status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
              #strong(internal_app_data$lang_sel$module_documentation_medication_info),
              mod_module_edit_tab_ui(ns("mod_module_edit_tab_smp"))
            )
          }
        )

      }else{
        return(NULL)
      }
    })

  })
}

## To be copied in the UI
# mod_module_documentation_ui("module_documentation_1")

## To be copied in the server
# mod_module_documentation_server("module_documentation_1")
