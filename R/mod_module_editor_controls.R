#' module_editor_controls UI Function
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
mod_module_editor_controls_ui <- function(id) {
  ns = NS(id)

  tagList(
    shinydashboard::box(title = "Controls", status = "info", width = 12,
                        actionButton(ns("update_widgets_button"), "Build", icon("hammer", verify_fa = FALSE)),
                        downloadButton(ns("downloadData"), "Download", icon = icon("download", verify_fa = FALSE)),
                        actionButton(ns("uploadData"), "Upload", icon = icon("upload", verify_fa = FALSE)),
                        actionButton(ns("delete_widgets_button"), "Delete", icon("trash", verify_fa = FALSE))
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
          title = "Build & reload",
          div(tags$head(tags$style(".modal-dialog{ width:400px}")),
              tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
              fluidPage(
                fluidRow(
                  actionButton(ns("update_widgets_button_confirm"), "Build", icon("hammer", verify_fa = FALSE)),
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
      dir.create("tmp_widgetdata", showWarnings = F)
      saveRDS((make_widget_tables(pool = pool,
                                  pool_config = pool_config))$widget_data_input,
              "tmp_widgetdata/tmp_widgetdata.RDS")
      removeModal()
      showNotification("Widgets updated", type = "message")
      session$reload()
    })

    # Upload data
    observeEvent(input$uploadData, {
      showModal(
        modalDialog(
          title = "Upload widget data",
          div(tags$head(tags$style(".modal-dialog{ width:400px}")),
              tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
              fluidPage(
                fluidRow(
                  h4("Upload visits"),
                  fileInput(ns("visits_upload"), "Upload data file (CSV)",
                            multiple = FALSE,
                            accept = c(".csv")),
                  h4("Upload variables"),
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
          write_csv(all_tables[[i]], fileName)
          files = c(fileName,files)
        }
        # create zip file
        zip(file,files)
      }
    )



    # delete dialogue ----
    observeEvent(input$delete_widgets_button, {
      showModal(
        modalDialog(
          title = "Delete",
          div(tags$head(tags$style(".modal-dialog{ width:400px}")),
              tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
              fluidPage(
                fluidRow(
                  actionButton(ns("delete_widgets_button_dialog"), "Select tables to delete", icon = icon("check-square-o",verify_fa = FALSE)),
                  modalButton("Dismiss", icon = icon("remove", verify_fa = FALSE))
                )
              )
          ),
          easyClose = FALSE, footer = NULL
        )
      )
    })

    # delete dialogue ----
    observeEvent(input$delete_widgets_button_dialog, {
      removeModal()
      showModal(
        modalDialog(
          title = "Delete & reload",
          div(tags$head(tags$style(".modal-dialog{ width:400px}")),
              tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
              fluidPage(
                fluidRow(
                  checkboxGroupInput(ns("tab_to_del_pool"), "Select data table ID for deleting", choices = dbListTables(pool)),
                  checkboxGroupInput(ns("tab_to_del_config"), "Select config table ID for deleting", choices = dbListTables(pool_config)),
                  actionButton(ns("delete_widgets_button_confirm"), "Confirm", icon = icon("trash",verify_fa = FALSE)),
                  actionButton(ns("delete_widgets_button_close"), "Close & relod", icon = icon("update",verify_fa = FALSE))
                )
              )
          ),
          easyClose = FALSE, footer = NULL
        )
      )
    })



    # Observe delete confirmation
    observeEvent(input$delete_widgets_button_confirm, {

      tab_to_del_pool = input$tab_to_del_pool
      if(!is.null(tab_to_del_pool)){
        for (i in tab_to_del_pool){
          if(i %in% dbListTables(pool)){
            dbRemoveTable(pool, i)
            showNotification(paste0("Data table", i, " deleted!"), type = "warning")
          } else {showNotification(paste0("Data table", i, " NOT found!"), type = "error")}
        }
      }


      tab_to_del_config = input$tab_to_del_config
      if(!is.null(tab_to_del_config)){
        for (i in tab_to_del_config){
          if(i %in% dbListTables(pool_config)){
            dbRemoveTable(pool_config, i)
            showNotification(paste0("Config table", i, " deleted!"), type = "warning")
          } else {showNotification(paste0("Config table", i, " NOT found!"), type = "error")}
        }
      }

    })


    # Function for closing the delete modal
    close_delete_modal = function(){
      removeModal()
      session$reload()
    }


    # Observe closing button of delete modal
    observeEvent(input$delete_widgets_button_close, {
      close_delete_modal()
    })

    # Handle uploads ----

    observe({
      if (is.null(input$visits_upload)) return()
      input_csv_visits = read.csv(input$visits_upload$datapath)
      tryCatch(dbAppendTable(pool,
                             "editor_table_visit",
                             input_csv_visits),
               error = function(e) showNotification("Data not saved: check format!", type = "error"))
    })

    observe({
      if (is.null(input$vars_upload)) return()
      input_csv_vars = read.csv(input$vars_upload$datapath)
      tryCatch(dbAppendTable(pool,
                             "editor_table_vars",
                             input_csv_vars),
               error = function(e) showNotification("Data not saved: check format!", type = "error"))
    })

  })
}

## To be copied in the UI
# mod_module_editor_controls_ui("module_editor_controls_1")

## To be copied in the server
# mod_module_editor_controls_server("module_editor_controls_1")
