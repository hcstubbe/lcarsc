#' module_documentation_summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList moduleServer uiOutput tagList renderUI actionButton
#' @importFrom shinydashboard box infoBox
#' @importFrom golem get_golem_options
#' @importFrom pool dbPool
#'
#'
mod_module_documentation_summary_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::uiOutput(ns("summary"))
  )
}

#' module_documentation_summary Server Functions
#'
#' @noRd
mod_module_documentation_summary_server <- function(id,
                                                    rv_in,
                                                    preview){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    pool = golem::get_golem_options("pool")

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

    widget_data_input = load_widget_data(pool_config = golem::get_golem_options("pool_config"),
                                         production_mode = golem::get_golem_options("production_mode"))
    all_visits = widget_data_input$all_visits
    visits_list = all_visits$visit_id[!all_visits$inclusion_other_visit]

    render_summary = function(start = TRUE) {

      if(start == TRUE){
        a_btn = shinydashboard::box(title = paste0("Summary: ", rv_in$pid()), width = 12, status = "info",
                                    shiny::actionButton(inputId = ns("update_summary"),
                                                        label = "Get summary"))
        return(a_btn)
      }

      if(length(rv_in$pid()) == 1){
        summary_box = shinydashboard::box(title = paste0("Summary: ", rv_in$pid()), width = 12, status = "info",
            actionButton(ns("update_summary"), "Update summary", icon = icon("sync", verify_fa = FALSE)), br(), br(),
            lapply(visits_list, function(x) {
              visit_tab_id = paste('visit_table', x, sep = '_')
              visit_data = RMariaDB::dbReadTable(pool, visit_tab_id)
              visit_data_unsubmitted = filter(visit_data, pid == rv_in$pid() & deleted_row == FALSE & submitted_row == FALSE)
              pid_entries_unsubmitted = nrow(visit_data_unsubmitted)
              visit_data_submitted = filter(visit_data, pid == rv_in$pid() & deleted_row == FALSE & submitted_row == TRUE)
              pid_entries_submitted = nrow(visit_data_submitted)

              return(
                fluidRow(
                  if(pid_entries_submitted > 0){
                    infoBox(
                      width = 6,
                      title = all_visits[all_visits$visit_id == x, "visit_title"],
                      value = pid_entries_submitted,
                      subtitle = "Submitted",
                      icon = icon("thumbs-up", lib = "glyphicon"),
                      color = "green",
                      fill=TRUE)
                  },
                  if(pid_entries_unsubmitted > 0){
                    infoBox(
                      width = 6,
                      title = all_visits[all_visits$visit_id == x, "visit_title"],
                      value = pid_entries_unsubmitted,
                      subtitle = "Recorded",
                      icon = icon("list", lib = "glyphicon"),
                      color = "yellow",
                      fill=TRUE)
                  },
                  if(pid_entries_submitted == 0 & pid_entries_unsubmitted == 0){
                    infoBox(
                      width = 6,
                      title = all_visits[all_visits$visit_id == x, "visit_title"],
                      value = NULL,
                      subtitle = "No entries",
                      icon = icon("question", lib = "font-awesome"),
                      color = "red",
                      fill=TRUE)
                  }
                )
              )
            })
        )
        return(summary_box)
      }

      return(NULL)

    }

    output$summary = renderUI({render_summary(start = TRUE)})

    observeEvent(input$update_summary, {
      output$summary = renderUI({render_summary(start = FALSE)})
    })

    observeEvent(rv_in$pid(), {
      output$summary = renderUI({render_summary(start = TRUE)})
    })

  })
}

## To be copied in the UI
# mod_module_documentation_summary_ui(ns("module_documentation_summary_1"))

## To be copied in the server
# mod_module_documentation_summary_server("module_documentation_summary_1")
