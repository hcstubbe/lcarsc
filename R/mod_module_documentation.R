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
  widget_data_input = load_widget_data(pool_config = golem::get_golem_options("pool_config"),
                                       production_mode = golem::get_golem_options("production_mode"))
  ordered_visits = widget_data_input$ordered_visits
  ordered_visits = ordered_visits %>% filter(visit_id != "vi" & is_child == FALSE & !is.na(is_child))
  visit_choices = ordered_visits$visit_id
  names(visit_choices) = ordered_visits$visit_title

  # Get settings data (fill with dummy data if it does not exist)
  settgins_data = golem::get_golem_options("settings_data")

  tagList(
    fluidRow(
      column(4,
             # uiOutput(ns("test_pid")),
             shinydashboard::box(
               title = (internal_app_data$lang_sel$module_documentation_visit_menu_choices),
               width = 12,
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               collapsed = FALSE,
               selectInput(
                 inputId = ns("visit_selector"),
                 label = internal_app_data$lang_sel$module_documentation_visit_selector,
                 choices = c(visit_choices)
               )
             ),
             shinydashboard::box(title = (internal_app_data$lang_sel$module_documentation_pt_list_title),
                 width = 12,
                 status = "primary",
                 solidHeader = TRUE,
                 actionButton(ns("update_pull_user"),
                              label = internal_app_data$lang_sel$update_pull,
                              icon("sync",
                                   verify_fa = FALSE)),
                 checkboxInput(inputId = ns("show_preliminary"),
                               label = "Show preliminary"),
                 br(),
                 br(),
                 DT::dataTableOutput(ns("responses_user"))),
             br(),
             br(),
             # mod_module_documentation_summary_ui(ns("module_documentation_summary_1"))
      ),
      if(settgins_data$add_child_visits == TRUE){column(4, # if(settigns_data$add_visit_summary == TRUE){4}else{8},
             uiOutput(ns("visit_submission_panel")),
             uiOutput(ns("ui_child_visits")))}else{NULL},
      if(TRUE){column(4, # if(settigns_data$add_visit_summary == TRUE){4}else{8},
                      uiOutput(ns("ui_report")))}else{NULL}


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


    # Get settings data
    settgins_data = golem::get_golem_options("settings_data")


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

  	user_show_vals = all_visits[all_visits$visit_id == "vi", c("show_col_1", "show_col_2", "show_col_3")]
  	if(any(!is.na(user_show_vals))){
  	  user_show_vals = user_show_vals[!is.na(user_show_vals)]
  	  user_show_vals_names = user_show_vals
  	  user_show_vals = paste("vi", user_show_vals, sep = "_")
  	  names(user_show_vals) = user_show_vals_names
  	  show_vals = c("pid", "date_modified", "user_modified", user_show_vals)
  	  names(show_vals) = c("PID", "Date", "User", user_show_vals_names)
  	  show_vals = show_vals[!is.na(show_vals)]
  	}else{
  	  show_vals = c("pid", "date_modified", "user_modified")
  	  names(show_vals) = c("PID", "Date", "User")
  	}

    computeFT = function(show_preliminary = FALSE){
      y = loadData(pool, data_table1)
      if(show_preliminary == TRUE){
        y = y %>% filter(deleted_row == FALSE & submitted_row == -1)
      }else{
        y = y %>% filter(deleted_row == FALSE & submitted_row == TRUE)
      }
      y = y %>%
        arrange(desc(as.POSIXct(as.character(date_modified), format = "%a %b %d %H:%M:%S %Y")))
      y = y[,show_vals]
      y
    }
    load_dt_for_render = function(){
      DT::datatable(computeFT(input$show_preliminary),
                    options = list(pageLength = 10,
                                   dom = "lftipr"),
                    selection = c("single"),
                    rownames = FALSE,
                    colnames = names(show_vals))}



    # Visit servers ----
    ordered_visits_parent = ordered_visits %>% filter(visit_id != "vi" & (is_child == FALSE | is.na(is_child)))
    rv_downstream_visit = reactiveValues()
    rv_downstream_visit$pid = reactive({computeFT(input$show_preliminary)$pid[input$responses_user_rows_selected]})

    rv_out_row = reactiveValues()
    for(i in ordered_visits_parent$visit_id){
      cmd_4_eval = paste("rv_out_row$row_selected_", i, " = mod_module_edit_tab_server(id = paste('mod_module_edit_tab_visit','", i, "', sep = '_'),
                             widget_tab_selection = 'visit',
                             tbl_id = paste('visit_table', '", i, "', sep = '_'),
                             rv_in = rv_downstream_visit,
                             show_vals = c(PID = 'pid', Date = 'date_modified', 'User' = 'user_modified'),
                             show_user_vals = TRUE,
                             simple = FALSE,
                             modal_width = '.modal-dialog{ width:95%}',
                             widgets_table_global = widgets_table_global[widgets_table_global[,i],],
                             all_tabs = all_tabs,
                             order.by = 'entry_number',
                             preview = preview,
                             dom = 'trp',
                             return_list = TRUE,
                             all_visits = all_visits,
                             is_editor_or_vi = FALSE,
                             visit_id = '", i, "')", sep = "")
      eval(parse(text = cmd_4_eval))
    }


    # Report server ----
    rv_downstream_report = reactiveValues()
    rv_downstream_report$pid = reactive({computeFT(input$show_preliminary)$pid[input$responses_user_rows_selected]})
    rv_downstream_report$selected_row_id = reactive({((reactiveValuesToList(rv_out_row))[[paste0("row_selected_", input$visit_selector)]])()[["row"]]})
    rv_downstream_report$selected_visit_id = reactive({input$visit_selector})
    rv_downstream_report$entry_id = reactive({
      db_cmd = paste0("SELECT entry_id FROM ", paste('visit_table', input$visit_selector, sep = '_'), " WHERE row_id = '", ((reactiveValuesToList(rv_out_row))[[paste0("row_selected_", input$visit_selector)]])()[["row"]], "'")
      entry_id = (RMariaDB::dbGetQuery(pool, db_cmd))$entry_id
      entry_id
    })
    mod_module_reports_server(id = "module_reports_1",
                              rv_in = rv_downstream_report,
                              widgets_table_global = widgets_table_global)



    # Child visit servers ----
    if(settgins_data$add_child_visits == TRUE){
      rv_downstream_child_visit = reactiveValues()
      rv_downstream_child_visit$pid = reactive({computeFT(input$show_preliminary)$pid[input$responses_user_rows_selected]})
      rv_downstream_child_visit$parent_row_id = reactive({((reactiveValuesToList(rv_out_row))[[paste0("row_selected_", input$visit_selector)]])()[["row"]]})
      rv_downstream_child_visit$parent_visit_id = reactive({input$visit_selector})

      rv_downstream_child_visit$entry_id = reactive({
        db_cmd = paste0("SELECT entry_id FROM ", paste('visit_table', input$visit_selector, sep = '_'), " WHERE row_id = '", ((reactiveValuesToList(rv_out_row))[[paste0("row_selected_", input$visit_selector)]])()[["row"]], "'")
        entry_id = (RMariaDB::dbGetQuery(pool, db_cmd))$entry_id
        entry_id
      })

      ordered_visits_child = ordered_visits %>% filter(visit_id != "vi" & is_child == TRUE & !is.na(is_child))
      for(i in ordered_visits_child$visit_id){
        cmd_4_eval = paste("mod_module_edit_tab_server(id = paste('mod_module_edit_tab_visit','", i, "', sep = '_'),
                             widget_tab_selection = 'visit',
                             tbl_id = paste('visit_table', '", i, "', sep = '_'),
                             rv_in = rv_downstream_child_visit,
                             show_vals = c(PID = 'pid', Date = 'date_modified', 'User' = 'user_modified'),
                             show_user_vals = TRUE,
                             simple = FALSE,
                             modal_width = '.modal-dialog{ width:95%}',
                             widgets_table_global = widgets_table_global[widgets_table_global[,i],],
                             all_tabs = all_tabs,
                             order.by = 'entry_number',
                             preview = preview,
                             dom = 'trp',
                             all_visits = all_visits,
                             is_child_visit = TRUE,
                             filter_entry_id = TRUE,
                             return_list = TRUE,
                             is_editor_or_vi = FALSE,
                             visit_id = '", i, "')", sep = "")
        eval(parse(text = cmd_4_eval))
      }
    }


    # Samples field server ----
    if(settgins_data$add_samples_panel == TRUE){

      rv_downstream_smp = reactiveValues()
      rv_downstream_smp$pid = reactive({computeFT(input$show_preliminary)$pid[input$responses_user_rows_selected]})
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
                                 noletters_smp_id = TRUE,
                                 is_editor_or_vi = TRUE)
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



    ## Render participant menu ----
    output$visit_submission_panel = renderUI({
      if(length(input$responses_user_rows_selected) == 1){
        div(
          shinydashboard::box(
            title = (paste(internal_app_data$lang_sel$module_documentation_visit_menu, ordered_visits$visit_title[ordered_visits$visit_id == input$visit_selector], sep = " ")),
            width = 12, status = "warning",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            uiOutput(ns("docu_tab_ui"))
          )
        )

      }else{
        return(NULL)
      }
    })

    ## Render child visit UIs ----
    if(settgins_data$add_child_visits == TRUE){
      output$ui_child_visits = renderUI({
        # Check if selected pid equals the pid returned from the child visit
        # and check, that exactly one row is selected in the participant list.
        if((sum(rv_downstream_child_visit$pid() == (((reactiveValuesToList(rv_out_row))[[paste0("row_selected_", input$visit_selector)]])()[["pid"]])) == 1) &
           (length(input$responses_user_rows_selected) == 1)){
          ui_list = list()
          selected_child_visits = sapply(strsplit(ordered_visits_child$parent_ids, ";;;;;"),
                                    function(x){
                                      any(x %in% input$visit_selector)
                                      }
                                    )
          selected_child_visits = ordered_visits_child[selected_child_visits,]
          if( nrow(selected_child_visits) > 0 ) {
            for ( i in 1:nrow(selected_child_visits) ) {
              ui_x = ui_x = mod_module_edit_tab_ui(id = ns(paste('mod_module_edit_tab_visit',selected_child_visits$visit_id[i], sep = '_')))
              ui_list = c(ui_list, ui_x)
            }
            lapply(1:length(ui_list), function(x) div(shinydashboard::box(
              title = selected_child_visits$visit_title[x],
              width = 12,
              status = "success",
              solidHeader = FALSE,
              collapsible = TRUE,
              collapsed = FALSE,
              ui_list[[x]]
            )))
          }else{
            ui_list = NULL
          }
        }
      })
    }






    ## Render report UI ----
       if(settgins_data$add_child_visits == TRUE){
      output$ui_report = renderUI({
        if(length(input$responses_user_rows_selected) == 1 &
           length(((reactiveValuesToList(rv_out_row))[[paste0("row_selected_", input$visit_selector)]])()[["row"]]) == 1){

          mod_module_reports_ui(ns("module_reports_1"))

         }
      })
    }

    # output$test_pid = renderUI({
    #   div(h4(((reactiveValuesToList(rv_out_row))[[paste0("row_selected_", input$visit_selector)]])()[["pid"]]),
    #       br(),
    #       h4(((reactiveValuesToList(rv_out_row))[[paste0("row_selected_", input$visit_selector)]])()[["row"]])
    #       )
    # })

  })
}

## To be copied in the UI
# mod_module_documentation_ui("module_documentation_1")

## To be copied in the server
# mod_module_documentation_server("module_documentation_1")
