#' module_editor_controls UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom utils zip
#' @importFrom readr write_csv
#' @importFrom RMariaDB dbReadTable dbListTables dbRemoveTable
mod_module_editor_controls_ui <- function(id) {
  ns = NS(id)
  shinydashboard::box(title = "Controls", status = "info", width = 12,
                      column(4,
                             shinydashboard::box(title = "Update & reload", status = "warning", collapsible = TRUE, collapsed = TRUE,width = 12,solidHeader = TRUE,
                                                 actionButton(ns("update_widgets_button"), "Update", icon("update"))
                             )
                      ),
                      column(4,
                             shinydashboard::box(title = "Export", status = "info", collapsible = TRUE, collapsed = TRUE,width = 12,solidHeader = TRUE,
                                                 actionButton(ns("download_widgets_button"), "Export", icon("export"))
                             )
                      ),
                      column(4,
                             shinydashboard::box(title = "Reset", status = "danger", collapsible = TRUE, collapsed = TRUE,width = 12,solidHeader = TRUE,
                                                 actionButton(ns("reset_widgets_button"), "Reset", icon("reset"))
                             )
                      )
  )
}


#' module_editor_controls Server Functions
#'
#' @noRd
mod_module_editor_controls_server <- function(id) {
  moduleServer(id, function(input, output, session) {


    # Requirements ----
    ns = session$ns
    pool = get_golem_options("pool")


    # Observers ----

    # Update dialogue
    observeEvent(input$update_widgets_button, {
      showModal(
        modalDialog(
          title = "Confirm update & reload",
          div(tags$head(tags$style(".modal-dialog{ width:400px}")),
              tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
              fluidPage(
                fluidRow(
                  actionButton(ns("update_widgets_button_confirm"), "Confirm"),
                  modalButton("Dismiss")
                )
              )
          ),
          easyClose = TRUE, footer = NULL
        )
      )
    })
    # Update widgets in data
    observeEvent(input$update_widgets_button_confirm, {
      app_data_internal <<- (make_widget_tables(pool = pool, ## Danger zone (<<-) !!!
                                                write_widget_tables = TRUE,
                                                remove_old_tables = TRUE))$app_data_internal
      removeModal()
      showNotification("Widgets updated", type = "message")
      session$reload()
    })


    # Download dialogue
    observeEvent(input$download_widgets_button, {

      # Export widget data from database to local folder
      dir.create(file.path("database_export"), showWarnings = FALSE)
      lapply(c("editor_table_visit", "editor_table_vars"), function(x) {
        tab_x = dbReadTable(pool, x)
        write_csv(tab_x, paste0("database_export/", x, ".csv"))
      })

      # Create widget files


      # Zip widget files and data base export
      dir.create(file.path("zip/database_export"), showWarnings = FALSE)
      make_widget_tables(pool = pool, write_widget_tables = TRUE, remove_old_tables = TRUE)
      zip(zipfile = 'zip/database_export', files = c('widgets/widgets.csv', "widgets/visits.csv", "widgets/panel_tabs.csv", "widgets/app_data_internal.RDS"))
      zip(zipfile = 'zip/database_export', files = c('database_export/editor_table_vars.csv', 'database_export/editor_table_visit.csv', 'database_export/app_data_internal.RDS'))

      showNotification("Ready for download!", type = "message")

      # Open modal for download
      showModal(
        modalDialog(
          title = "Confirm download",
          div(tags$head(tags$style(".modal-dialog{ width:400px}")),
              tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
              fluidPage(
                fluidRow(
                  downloadButton(ns("downloadData"), "Download"),
                  modalButton("Back")
                )
              )
          ),
          easyClose = TRUE, footer = NULL
        )
      )

    })

    # Download handler
    output$downloadData <- downloadHandler(

      filename = "database_export.zip",

      content <- function(file) {
        file.copy("zip/database_export.zip", file)
      },
      contentType = "application/zip"
    )



    # Reset dialogue ----
    observeEvent(input$reset_widgets_button, {
      showModal(
        modalDialog(
          title = "Confirm reset & reload",
          div(tags$head(tags$style(".modal-dialog{ width:400px}")),
              tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
              fluidPage(
                fluidRow(
                  actionButton(ns("reset_widgets_button_confirm"), "Confirm"),
                  modalButton("Dismiss")
                )
              )
          ),
          easyClose = TRUE, footer = NULL
        )
      )
    })
    observeEvent(input$reset_widgets_button_confirm, {
      tbl_ids = dbListTables(pool)
      if(length(tbl_ids) > 0){
        for (i in tbl_ids) {
          dbRemoveTable(pool, i)
        }
      }
      removeModal()
      showNotification("Database reset!", type = "message")
      session$reload()
    })

  })
}

## To be copied in the UI
# mod_module_editor_controls_ui("module_editor_controls_1")

## To be copied in the server
# mod_module_editor_controls_server("module_editor_controls_1")
