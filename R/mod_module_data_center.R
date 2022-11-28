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
                         mod_module_documentation_summary_graph_ui(ns("module_documentation_summary_graph_1")),
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
                                            downloadButton(ns("downloadData"),
                                                           "Download",
                                                           icon = shiny::icon("download", verify_fa = FALSE)))
                ),
                tabPanel("Report editor", br(),
                         shinydashboard::box(title = "Summary report",
                                             status = "primary",
                                             collapsible = FALSE,
                                             collapsed = FALSE,
                                             width = 12,
                                             solidHeader = TRUE,
                                             div(
                                               mod_module_reports_editor_ui(ns("module_reports_editor_1"))
                                             )
                                             )
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


    # Start sub-modules ----

    ## module_documentation_summary_graph_1
    mod_module_documentation_summary_graph_server("module_documentation_summary_graph_1")

    ## module_reports_editor_1
    mod_module_reports_editor_server("module_reports_editor_1")

    # Render summary data ----
    render_summary = function() {

      #### Get data ----
      widget_data_input = load_widget_data(pool_config = golem::get_golem_options("pool_config"),
                                           production_mode = golem::get_golem_options("production_mode"))
      all_visits = widget_data_input$all_visits
      visits_list = all_visits$visit_id[!all_visits$inclusion_other_visit]
      total_inclusions = db_read_select(pool,
                                      tbl_id = "inclusion_dataset",
                                      count_rows = TRUE,
                                      pid_x = NULL,
                                      use.pid = FALSE,
                                      filter_deleted_rows = TRUE,
                                      filter_sumitted_rows = FALSE,
                                      select_cols = c("submitted_row", "date_modified"))
      total_inclusions = as.numeric(total_inclusions[[1]])

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
              "Here, numbers of participants with visits are summaized. ", strong("Repetitive visits"), " of the same type and individual are ", strong("omitted"), ".",
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


    # Download scientific dataset ----

    # Download data
    output$downloadData <- downloadHandler(
      filename = function(){
        paste0("database_export",".zip")

      },
      content = function(file){
        # use temp dir to avoid permission issues
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        files <- NULL;

        # loop through tables
        all_tables = sapply(RMariaDB::dbListTables(pool),
                            function(x) RMariaDB::dbReadTable(pool, x),
                            USE.NAMES = TRUE,
                            simplify = FALSE)

        for (i in 1:length(all_tables)){
          fileName = paste0(names(all_tables)[[i]],".csv")
          readr::write_csv(all_tables[[i]],fileName)
          files = c(fileName,files)
        }
        # create zip file
        zip(file,files)
      }
    )



  })
}

## To be copied in the UI
# mod_module_data_center_ui("module_data_center_1")

## To be copied in the server
# mod_module_data_center_server("module_data_center_1")
