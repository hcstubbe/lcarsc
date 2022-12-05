#' module_reports_controls UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList fluidRow fluidRow div modalDialog showModal icon textInput actionButton modalButton observeEvent removeModal showNotification column moduleServer checkboxGroupInput
#' @importFrom shinydashboard box
#' @importFrom utils zip
#' @importFrom readr write_csv
#' @importFrom RMariaDB dbReadTable dbListTables dbRemoveTable
#' @importFrom golem get_golem_options
#' @importFrom shiny NS tagList
mod_module_reports_controls_ui <- function(id) {
  ns = NS(id)

  tagList(
    wellPanel(title = "Controls",
                        actionButton(ns("update_widgets_button"), "Update", icon("hammer", verify_fa = FALSE)),
                        downloadButton(ns("downloadData"), "Download", icon = icon("download", verify_fa = FALSE)),
                        actionButton(ns("uploadData"), "Upload", icon = icon("upload", verify_fa = FALSE))
    )
  )
}

#' module_reports_controls Server Functions
#'
#' @noRd
mod_module_reports_controls_server <- function(id) {
  moduleServer(id, function(input, output, session) {


    # Requirements ----
    ns = session$ns
    pool = get_golem_options("pool")
    pool_config = get_golem_options("pool_config")


    # Observers ----

    # Update dialogue
    observeEvent(input$update_widgets_button, {
      showModal(
        modalDialog(
          title = "Update report forms",
          div(tags$head(tags$style(".modal-dialog{ width:400px}")),
              tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
              fluidPage(
                fluidRow(
                  actionButton(ns("update_widgets_button_confirm"), "Update", icon("update", verify_fa = FALSE)),
                  modalButton("Dismiss", icon = icon("remove", verify_fa = FALSE))
                )
              )
          ),
          easyClose = TRUE, footer = NULL
        )
      )
    })
    # Update widgets in data
    observeEvent(input$update_widgets_button_confirm, {
      removeModal()
      showNotification("Reports updated", type = "message")
      session$reload()
    })

    # Upload data
    observeEvent(input$uploadData, {
      dbExecute(pool_config, "UPDATE start_config SET `tested_ecrf`='FALSE'")
      showModal(
        modalDialog(
          title = "Upload widget data",
          div(tags$head(tags$style(".modal-dialog{ width:400px}")),
              tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
              fluidPage(
                fluidRow(
                  h4("Upload report variables"),
                  fileInput(ns("vars_upload"), "Upload data file (CSV)",
                            multiple = FALSE,
                            accept = c(".csv")),
                  modalButton("Done", icon = icon("check", verify_fa = FALSE))
                )
              )
          ),
          easyClose = TRUE, footer = NULL
        )
      )
    })


    # Download data
    output$downloadData <- downloadHandler(
      filename = function(){
        paste0("report_editor_export",".zip")

      },
      content = function(file){
        # use temp dir to avoid permission issues
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        files <- NULL;

        # loop through tabs
        all_tables = sapply(c("report_editor_table_vars", "widgets_report_editor"),
                            function(x) RMariaDB::dbReadTable(pool, x),
                            USE.NAMES = TRUE,
                            simplify = FALSE)

        for (i in 1:length(all_tables)){
          fileName = paste0(names(all_tables)[[i]],".csv")
          write_csv(all_tables[[i]], fileName)
          files = c(fileName,files)
        }
        # create zip file
        zip(file,files)
      }
    )



    # Handle uploads ----
    observe({
      if (is.null(input$vars_upload)) return()
      input_csv_vars = read.csv(input$vars_upload$datapath)
      input_csv_vars$row_id = uuid::UUIDgenerate(use.time = FALSE, n = nrow(input_csv_vars))
      tryCatch(dbAppendTable(pool,
                             "report_editor_table_vars",
                             input_csv_vars),
               error = function(e) showNotification(paste0("Data not saved: check format! Original error message: ", e), type = "error", duration = NULL))
    })

  })
}

## To be copied in the UI
# mod_module_reports_controls_ui(ns("module_reports_controls_1"))

## To be copied in the server
# mod_module_reports_controls_server("module_reports_controls_1")
