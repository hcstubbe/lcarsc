#' module_data_center UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList fluidRow
#' @importFrom shinydashboard box infoBox
#' @importFrom golem get_golem_options
#' @importFrom RMariaDB dbReadTable
#' @import dplyr
#'
mod_module_data_center_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(type = "tabs",
                tabPanel("Overview", br(),
                         actionButton(ns("update_summary"), "Update summary", icon = icon("sync", verify_fa = FALSE)), br(), br(),
                         br(),
                         uiOutput(ns("summary"))
                ),
                tabPanel("Scientific dataset", br(),
                         shinydashboard::box(title = "Scientific dataset",
                                            status = "primary",
                                            collapsible = FALSE,
                                            collapsed = FALSE,
                                            width = 12,
                                            solidHeader = TRUE,
                                            "Here are some comparisons")
                )
    )
  )
}

#' module_data_center Server Functions
#'
#' @noRd
mod_module_data_center_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    pool = golem::get_golem_options("pool")



    # Function for rendering summary data
    render_summary = function() {

      #### Get data ----
      widget_data_input = load_widget_data(pool_config = golem::get_golem_options("pool_config"),
                                           production_mode = golem::get_golem_options("production_mode"))
      all_visits = widget_data_input$all_visits

      visits_list = all_visits$visit_id[!all_visits$inclusion_other_visit]

      total_inclusions = nrow(filter(RMariaDB::dbReadTable(pool, "inclusion_dataset"),
                                     deleted_row == FALSE & submitted_row == TRUE))

      #### Render summary ----
      fluidPage(
        fluidRow(
          infoBox(
            width = 4,
            title = "Total inclusions",
            value = total_inclusions,
            icon = icon("list", lib = "glyphicon"),
            color = "blue",
            fill=TRUE)
        ),
        fluidRow(
          box(title = paste0("Summary per visit"), width = 12, status = "info",
              "Here, visits are summaized. ", strong("Repetitive visits"), " for the same individual with the same status are ", strong("omitted"), ".",
              br(),
              br(),
              lapply(visits_list, function(x) {
                visit_tab_id = paste('visit_table', x, sep = '_')
                visit_data = RMariaDB::dbReadTable(pool, visit_tab_id)
                visit_data_unsubmitted = filter(visit_data, deleted_row == FALSE & submitted_row == FALSE)
                visit_data_unsubmitted = filter(visit_data_unsubmitted, !duplicated(pid))
                pid_entries_unsubmitted = nrow(visit_data_unsubmitted)
                visit_data_submitted = filter(visit_data, deleted_row == FALSE & submitted_row == TRUE)
                visit_data_submitted = filter(visit_data_submitted, !duplicated(pid))
                pid_entries_submitted = nrow(visit_data_submitted)
                visit_data_all_recorded = filter(visit_data, deleted_row == FALSE)
                visit_data_all_recorded = filter(visit_data_all_recorded, !duplicated(pid))
                pid_data_all_recorded = nrow(visit_data_all_recorded)

                missing_entries = total_inclusions - pid_data_all_recorded

                return(
                  fluidRow(
                    if(pid_entries_submitted > 0){
                      infoBox(
                        width = 4,
                        title = all_visits[all_visits$visit_id == x, "visit_title"],
                        value = pid_entries_submitted,
                        subtitle = "Submitted",
                        icon = icon("thumbs-up", lib = "glyphicon"),
                        color = "green",
                        fill=TRUE)
                    },
                    if(pid_entries_unsubmitted > 0){
                      infoBox(
                        width = 4,
                        title = all_visits[all_visits$visit_id == x, "visit_title"],
                        value = pid_entries_unsubmitted,
                        subtitle = "Recorded, not submitted",
                        icon = icon("list", lib = "glyphicon"),
                        color = "yellow",
                        fill=TRUE)
                    },
                    if(missing_entries > 0){
                      infoBox(
                        width = 4,
                        title = all_visits[all_visits$visit_id == x, "visit_title"],
                        value = missing_entries,
                        subtitle = "Participants without entries",
                        icon = icon("question", lib = "font-awesome"),
                        color = "red",
                        fill=TRUE)
                    }
                  )
                )
              })
          )
        )
      )

      }


    output$summary = renderUI({render_summary()})

    observeEvent(input$update_summary, {
      output$summary = renderUI({render_summary()})
    })


  })
}

## To be copied in the UI
# mod_module_data_center_ui("module_data_center_1")

## To be copied in the server
# mod_module_data_center_server("module_data_center_1")
