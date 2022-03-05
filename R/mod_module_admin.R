#' module_admin UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom golem get_golem_options
#' @importFrom RMariaDB dbReadTable
#' @importFrom shinyvalidate sv_equal sv_required InputValidator
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
                              solidHeader = TRUE

          )
        ),
        fluidRow(
          shinydashboard::box(title = "Danger zone",
                              status = "danger",
                              collapsible = TRUE,
                              collapsed = TRUE,
                              width = 4,
                              solidHeader = TRUE,
                              textInput(ns("pid_to_delete"), label = "PID to remove from database"),
                              actionButton(ns("delete_pid_dialog"), label = "Delete record")

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


    if(get_golem_options("user_is_admin")){

      observeEvent(input$delete_pid_dialog, {
        iv$enable()
        if(iv$is_valid()){
          shiny::showModal(
            shiny::modalDialog(
              textInput(ns("pid_to_delete_confirm"), label = "Confirm PID to remove from database - This cannot be undone!"),
              actionButton(ns("delete_pid"), label = "Delete reccord"),
              footer = actionButton(ns("cancel_delete"), label = "Cancel")
            )
          )
        }
      })

      observeEvent(input$delete_pid,{
        iv_confirm <- InputValidator$new()
        iv_confirm$add_rule("pid_to_delete_confirm", sv_required())
        iv_confirm$add_rule("pid_to_delete_confirm", sv_equal(input$pid_to_delete))
        iv_confirm$enable()
        if(iv_confirm$is_valid()){
          pid = input$pid_to_delete
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
          }
          close_modal()
        }
      })

      observeEvent(input$cancel_delete,{
        close_modal()
      })

      # Add input valdiation
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
