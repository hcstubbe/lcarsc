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
             box(title = (lang_sel$module_documentation_pt_list_title), width = 12, status = "primary", solidHeader = TRUE,
                 actionButton(ns("update_pull_user"), label = lang_sel$update_pull, icon("sync")),
                 br(),
                 br(),
                 br(),
                 DT::dataTableOutput(ns("responses_user")))
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
mod_module_documentation_server <- function(id, data_table1, data_table2) {
  moduleServer(id, function(input, output, session) {

    # Requirements ----
    ns = session$ns
    pool = get_golem_options("pool")
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
                             all_visits = all_visits,
                             visit_id = '", i, "')", sep = "")
      eval(parse(text = cmd_4_eval))
    }



    rv_downstream_diag = reactiveValues()
    rv_downstream_diag$pid = reactive({computeFT()$pid[input$responses_user_rows_selected]})

    mod_module_edit_tab_server(id = "mod_module_edit_tab_diag",
                           widget_tab_selection = "diagnosis",
                           tbl_id = "diagnoses_table",
                           rv_in = rv_downstream_diag,
                           show_vals = c(Diagnosis = "diag_name", Start = "diag_start", End = "diag_end", Submitted = "submitted_row"),
                           widgets_table_global = widgets_table_global,
                           all_visits = all_visits,
                           visit_id = "diagnosis",
                           order.by = NULL
    )

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
                           order.by = NULL)



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
            title = (lang_sel$module_documentation_visit_menu),width = 12, status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
            wellPanel(
              selectInput(
                inputId = ns("visit_selector"),
                label = lang_sel$module_documentation_visit_selector,
                choices = c(visit_choices)
              )
            ),
            uiOutput(ns("docu_tab_ui"))
          ),
          shinydashboard::box(
            title = (lang_sel$module_documentation_diagnoses_info),width = 12, status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
            #strong(lang_sel$module_documentation_diagnoses_info),
            mod_module_edit_tab_ui(ns("mod_module_edit_tab_diag"))
          ),
          box(
            title = (lang_sel$module_documentation_medication_info),width = 12, status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
            #strong(lang_sel$module_documentation_medication_info),
            mod_module_edit_tab_ui(ns("mod_module_edit_tab_med"))
          )
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
