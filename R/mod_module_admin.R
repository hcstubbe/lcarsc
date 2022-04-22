#' module_admin UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList h4 br
#' @importFrom golem get_golem_options
#' @importFrom RMariaDB dbReadTable
#' @importFrom shinyvalidate sv_equal sv_required InputValidator
#' @importFrom utils zip
#'
#'
mod_module_admin_ui <- function(id){
  ns <- NS(id)

  # Check if user is admin
  if(!get_golem_options("user_is_admin")){
    return(NULL)
  }else{
    tagList(
      fluidPage(
        fluidRow(
          shinydashboard::box(title = "Access database",
                              status = "info",
                              collapsible = TRUE,
                              collapsed = FALSE,
                              width = 4,
                              solidHeader = TRUE,
                              "Here, the data tables of this database can be downloaded. This includes the scientific datasets as well as widget data of the eCRF.",
                              br(),
                              br(),
                              downloadButton(ns("downloadData"), "Download", icon = shiny::icon("download", verify_fa = FALSE))

          )
        ),
        fluidRow(
          shinydashboard::box(title = "Danger zone",
                              status = "danger",
                              collapsible = TRUE,
                              collapsed = TRUE,
                              width = 4,
                              solidHeader = TRUE,
                              "This removes a record ", strong("irreversibly"),
                              " from all tables of this database. Once removed,  ",
                              strong("this cannot be undone!"),
                              br(),
                              br(),
                              textInput(ns("pid_to_delete"), label = "PID to remove from database"),
                              actionButton(ns("delete_pid_dialog"), label = "Delete record"),
                              br(),
                              br(),
                              "This reverses the server to editor mode!",
                              actionButton(ns("reverse_deploy"), label = "Reverse to editor mode!")

          )
        )
      )
    )
  }

}

#' module_admin Server Functions
#'
#' @noRd
mod_module_admin_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    pool = get_golem_options("pool")

    # Go back from production mode (for experimental version!)
    observeEvent(input$reverse_deploy, {
      RMariaDB::dbRemoveTable(conn = get_golem_options("pool_config"), name = "start_config")
      RMariaDB::dbCreateTable(conn = get_golem_options("pool_config"),
                                  name = "start_config",
                                  fields = data.frame(production_mode = "editor"))
      RMariaDB::dbAppendTable(conn = get_golem_options("pool_config"),
                                  name = "start_config",
                                  value = data.frame(production_mode = "editor"))
    
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


    # Deleting records
    if(get_golem_options("user_is_admin")){

      observeEvent(input$delete_pid_dialog, {
        iv$enable()
        if(iv$is_valid()){
          shiny::showModal(
            shiny::modalDialog(
              tags$head(tags$style(".modal-dialog{ width:50% }")),
              textInput(ns("reason_deleted"), label = "Pleas state the reason."),
              br(),
              textInput(ns("pid_to_delete_confirm"), label = "Confirm PID to remove from database - This cannot be undone!"),
              actionButton(ns("delete_pid"), label = "Delete reccord"),
              footer = actionButton(ns("cancel_delete"), label = "Cancel")
            )
          )
        }
      })

      observeEvent(input$delete_pid,{
        iv_confirm <- InputValidator$new()
        iv_confirm$add_rule("reason_deleted", sv_required())
        iv_confirm$add_rule("pid_to_delete_confirm", sv_required())
        iv_confirm$add_rule("pid_to_delete_confirm", sv_equal(input$pid_to_delete))
        iv_confirm$enable()
        if(iv_confirm$is_valid()){
          pid = input$pid_to_delete
          success_all = c()
          for( i in RMariaDB::dbListTables(pool) ) {
            db_cmd = paste0("DELETE FROM ", i, " WHERE (pid = '", pid, "');")
            success = tryCatch(RMariaDB::dbExecute(pool, db_cmd), error = function(x) FALSE)
            if(success != FALSE){
              shiny::showNotification(strong(paste0("PID '",
                                                    pid,
                                                    "' was deleted from ",
                                                    success,
                                                    " rows of ",
                                                    "table '",
                                                    i,
                                                    "'")),
                                      type = "error",
                                      duration = 10)
            }
            success_all = cbind(success_all, success)
          }


          if(any(success_all)){

            if(!RMariaDB::dbExistsTable(conn = pool, name = "entries_deleted_from_database")){
              sql_fields = c("date_deleted" = "TEXT" , "deleted_by" = "TEXT" , "pid_deleted" = "TEXT", "reason_deleted" = "TEXT")
              RMariaDB::dbCreateTable(conn = pool,
                                      name = "entries_deleted_from_database",
                                      fields = sql_fields)
            }

            deleted_entry = data.frame(date_deleted = as.character(Sys.Date()),
                                 deleted_by = get_current_user(),
                                 pid_deleted = pid,
                                 reason_deleted = input$reason_deleted)
            RMariaDB::dbAppendTable(conn = pool,
                                    name = "entries_deleted_from_database",
                                    value = deleted_entry)

          }


          if(all(success_all == FALSE)){
            shiny::showNotification(strong("No enries have been removed!"),
                                    type = "warning",
                                    duration = 10)
          }
          close_modal()
        }
      })

      observeEvent(input$cancel_delete,{
        close_modal()
      })



      # Input valdiation
      iv <- InputValidator$new()
      iv$add_rule("pid_to_delete", sv_required())


      close_modal = function() {
        removeModal()
        iv$disable()
        if(exists("iv_confirm")){
          iv_confirm$disable()
          rm(iv_confirm)
        }
      }

    }
  })
}

## To be copied in the UI
# mod_module_admin_ui("module_admin_1")

## To be copied in the server
# mod_module_admin_server("module_admin_1")
