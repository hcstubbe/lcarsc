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
                             shinydashboard::box(title = "Build & reload", status = "warning", collapsible = TRUE, collapsed = TRUE,width = 12,solidHeader = TRUE,
                                                 actionButton(ns("update_widgets_button"), "Build", icon("update", verify_fa = FALSE))
                             )
                      ),
                      column(4,
                             shinydashboard::box(title = "Export", status = "info", collapsible = TRUE, collapsed = TRUE,width = 12,solidHeader = TRUE,
                                                 downloadButton(ns("downloadData"), "Download", icon = shiny::icon("download", verify_fa = FALSE))
                             )
                      ),
                      column(4,
                             shinydashboard::box(title = "Reset", status = "danger", collapsible = TRUE, collapsed = TRUE,width = 12,solidHeader = TRUE,
                                                 actionButton(ns("reset_widgets_button"), "Reset", icon("reset", verify_fa = FALSE))
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
    pool_config = get_golem_options("pool_config")


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
      dir.create("tmp_widgetdata", showWarnings = F)
      saveRDS((make_widget_tables(pool = pool,
                                  pool_config = pool_config,
                                  write_widget_tables = TRUE))$widget_data_input,
              "tmp_widgetdata/tmp_widgetdata.RDS")
      removeModal()
      showNotification("Widgets updated", type = "message")
      session$reload()
    })


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

        # loop through tabs
        all_tables = sapply(RMariaDB::dbListTables(pool),
                            function(x) RMariaDB::dbReadTable(pool, x),
                            USE.NAMES = TRUE,
                            simplify = FALSE)

        for (i in 1:length(all_tables)){
          fileName = paste0(names(all_tables)[[i]],".csv")
          write.csv(all_tables[[i]],fileName)
          files = c(fileName,files)
        }
        # create zip file
        zip(file,files)
      }
    )



    # Reset dialogue ----
    # observeEvent(input$reset_widgets_button, {
    #   showModal(
    #     modalDialog(
    #       title = "Confirm reset & reload",
    #       div(tags$head(tags$style(".modal-dialog{ width:400px}")),
    #           tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
    #           fluidPage(
    #             fluidRow(
    #               actionButton(ns("reset_widgets_button_confirm"), "Confirm"),
    #               modalButton("Dismiss")
    #             )
    #           )
    #       ),
    #       easyClose = TRUE, footer = NULL
    #     )
    #   )
    # })
    # observeEvent(input$reset_widgets_button_confirm, {
    #   tbl_ids = dbListTables(pool)
    #   if(length(tbl_ids) > 0){
    #     for (i in tbl_ids) {
    #       dbRemoveTable(pool, i)
    #     }
    #   }
    #   removeModal()
    #   showNotification("Database reset!", type = "message")
    #   session$reload()
    # })

  })
}

## To be copied in the UI
# mod_module_editor_controls_ui("module_editor_controls_1")

## To be copied in the server
# mod_module_editor_controls_server("module_editor_controls_1")
