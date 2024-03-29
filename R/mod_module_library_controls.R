#' module_library_controls UI Function
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
#' @importFrom readr write_csv read_csv
#' @importFrom RMariaDB dbReadTable dbListTables dbRemoveTable
#' @importFrom golem get_golem_options
#' @importFrom uuid UUIDgenerate

mod_module_library_controls_ui <- function(id) {
  ns = NS(id)

  tagList(
    div(actionButton(ns("addvars"), "Add selected variables", icon("plus", verify_fa = FALSE)),
        downloadButton(ns("downloadData"), "Download", icon = icon("download", verify_fa = FALSE)),
        actionButton(ns("uploadData"), "Upload", icon = icon("upload", verify_fa = FALSE)) #,
        # actionButton(ns("delete_widgets_button"), "Delete table(s)", icon("trash", verify_fa = FALSE))
    )
  )
}

#' module_library_controls Server Functions
#'
#' @noRd
mod_module_library_controls_server <- function(id, selected_row) {
  moduleServer(id, function(input, output, session) {

    # Requirements ----
    ns = session$ns
    pool = get_golem_options("pool")
    pool_config = get_golem_options("pool_config")



    # Observers ----

    # Add dialogue
    observeEvent(input$addvars, {

      # Remove tested status
      dbExecute(pool_config, "UPDATE start_config SET `tested_ecrf`='FALSE'")

      # Get inputIds to add to editor
      vars_table_sql <- dbReadTable(pool, "library_table_vars")
      sel_inputIds = vars_table_sql[vars_table_sql$row_id %in% selected_row(), "inputId"]

      visits_table = dbReadTable(pool, "editor_table_visit")[,c("visit_id_visits", "visit_title", "deleted_row")]
      visits_table = visits_table[visits_table$deleted_row == FALSE, ]
      visit_choices = visits_table$visit_id_visits
      names(visit_choices) = visits_table$visit_title

      if(length(visit_choices) > 0 & length(sel_inputIds) > 0){
        showModal(
          modalDialog(
            title = "Add",
            div(tags$head(tags$style(".modal-dialog{ width:400px}")),
                tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
                fluidPage(
                  fluidRow(
                    h4("Add the following variables to the editor: "),
                    paste(sel_inputIds, collapse = ", "),
                    br(),
                    br(),
                    selectInput(ns("visit_for_var"), label = "Select visit for varsiables", choices = visit_choices),
                    br(),
                    br(),
                    h4("Replace appear_if visit id?"),
                    checkboxInput(ns("replace_visit_id"), label = "Replace appear_if visit_id with selected visit_id"),
                    actionButton(ns("addvars_confirm"), "Add", icon("plus", verify_fa = FALSE)),
                    modalButton("Dismiss", icon = icon("remove", verify_fa = FALSE))
                  )
                )
            ),
            easyClose = FALSE, footer = NULL
          )
        )
      }else if(length(sel_inputIds) == 0){
        showModal(
          modalDialog(
            title = "Select variables!",
            div(tags$head(tags$style(".modal-dialog{ width:400px}")),
                tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
                fluidPage(
                  fluidRow(
                    h4("Select at least one variable to add to the editor!"),
                  )
                )
            ),
            easyClose = TRUE
          )
        )
      }else{
        showModal(
          modalDialog(
            title = "Visit required!",
            div(tags$head(tags$style(".modal-dialog{ width:400px}")),
                tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
                fluidPage(
                  fluidRow(
                    h4("Add at least one visit in the editor!"),
                  )
                )
            ),
            easyClose = TRUE
          )
        )
      }
    })
    # Add library data to editor
    observeEvent(input$addvars_confirm, {

      # Add data
      SQL_df <- db_read_select(pool = pool, tbl_id = "library_table_vars", pid_x = NULL, use.pid = FALSE, order.by = "order_of_var")
      SQL_df = SQL_df[SQL_df$row_id %in% selected_row() & SQL_df$deleted_row == FALSE, ]

      start_order = max(dbReadTable(pool, "editor_table_vars")$order_of_var)
      if(start_order < 0) {
        start_order = 0
        }
      SQL_df$visit_for_var = input$visit_for_var
      SQL_df$order_of_var = start_order + 1:nrow(SQL_df)
      if(input$replace_visit_id == TRUE){
        SQL_df$appear_if = gsub("xxx_dummyvisit_xxx", input$visit_for_var, SQL_df$appear_if)
      }
      SQL_df$row_id = uuid::UUIDgenerate(use.time = FALSE, n = nrow(SQL_df))

      tryCatch(dbAppendTable(pool,
                             "editor_table_vars",
                             SQL_df),
               error = function(e) showNotification(paste0("Data not saved: check format! Original error message: ", e), type = "error", duration = NULL))

      removeModal()
      showNotification("Widgets updated", type = "message")
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
                  h4("Upload variables"),
                  textInput(ns("origin_of_var"), "Origin of variables"),
                  fileInput(ns("vars_upload"), "Upload data file (CSV or JSON)",
                            multiple = FALSE,
                            accept = c(".csv", ".json")),
                  actionButton(ns("confirm_upload"), "Save upload", icon = icon("save", verify_fa = FALSE)),
                  modalButton("Done", icon = icon("check", verify_fa = FALSE))
                )
              )
          ),
          easyClose = TRUE, footer = NULL
        )
      )
    })



    # Handle uploads ----

    observeEvent(input$confirm_upload, {

      if ( is.null(input$vars_upload) ) {
        return()
        removeModal()
      }


      if(rev(strsplit(input$vars_upload$datapath, split = ".", fixed = TRUE)[[1]])[[1]] == "json"){

         input_csv_vars = tryCatch(json_parser(input$vars_upload$datapath),
                                  error = function(e) {
                                    showNotification(paste0("JSON file could not be converted: check format! Original error message: ", e), type = "error", duration = NULL)
                                    return(data.frame(1:1))
                                  })

      }else if(rev(strsplit(input$vars_upload$datapath, split = ".", fixed = TRUE)[[1]])[[1]] == "csv"){
        input_csv_vars = readr::read_csv(input$vars_upload$datapath)
      }else{
        input_csv_vars = data.frame(1:1)
      }

      visit_for_var_col = which(colnames(input_csv_vars) == "visit_for_var")

      if ( length(visit_for_var_col) > 0 ) {
        input_csv_vars = input_csv_vars[,-visit_for_var_col]
      }

      if ( input$origin_of_var == "" ) {
        input_csv_vars$origin_of_var[length(input_csv_vars$origin_of_var) == 0] = "Unkown"
      } else {
        input_csv_vars$origin_of_var = input$origin_of_var
      }

      input_csv_vars = input_csv_vars[input_csv_vars$deleted_row == FALSE,]
      input_csv_vars$row_id = uuid::UUIDgenerate(use.time = FALSE, n = nrow(input_csv_vars))


      upload_success = tryCatch(
        dbAppendTable(pool,
                      "library_table_vars",
                      input_csv_vars)
        ,
        error = function(e) NULL)


      if(!is.null(upload_success)){
        showNotification("Variables uploaded!", duration = 10, type = "message")
      }else{
        showNotification("Data not saved: check format!", type = "error")
      }

      removeModal()
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



    # # delete dialogue ----
    # observeEvent(input$delete_widgets_button, {
    #   showModal(
    #     modalDialog(
    #       title = "Delete",
    #       div(tags$head(tags$style(".modal-dialog{ width:400px}")),
    #           tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
    #           fluidPage(
    #             fluidRow(
    #               actionButton(ns("delete_widgets_button_dialog"), "Select tables to delete", icon = icon("check-square-o",verify_fa = FALSE)),
    #               modalButton("Dismiss", icon = icon("remove", verify_fa = FALSE))
    #             )
    #           )
    #       ),
    #       easyClose = FALSE, footer = NULL
    #     )
    #   )
    # })

    # # delete dialogue ----
    # observeEvent(input$delete_widgets_button_dialog, {
    #   removeModal()
    #   showModal(
    #     modalDialog(
    #       title = "Delete & reload",
    #       div(tags$head(tags$style(".modal-dialog{ width:400px}")),
    #           tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
    #           fluidPage(
    #             fluidRow(
    #               checkboxGroupInput(ns("tab_to_del_pool"), "Select data table ID for deleting", choices = dbListTables(pool)),
    #               checkboxGroupInput(ns("tab_to_del_config"), "Select config table ID for deleting", choices = dbListTables(pool_config)),
    #               actionButton(ns("delete_widgets_button_confirm"), "Confirm", icon = icon("trash",verify_fa = FALSE)),
    #               actionButton(ns("delete_widgets_button_close"), "Close & relod", icon = icon("update",verify_fa = FALSE))
    #             )
    #           )
    #       ),
    #       easyClose = FALSE, footer = NULL
    #     )
    #   )
    # })



    # # Observe delete confirmation
    # observeEvent(input$delete_widgets_button_confirm, {
    #
    #   tab_to_del_pool = input$tab_to_del_pool
    #   if(!is.null(tab_to_del_pool)){
    #     for (i in tab_to_del_pool){
    #       if(i %in% dbListTables(pool)){
    #         dbRemoveTable(pool, i)
    #         showNotification(paste0("Data table", i, " deleted!"), type = "warning")
    #       } else {showNotification(paste0("Data table", i, " NOT found!"), type = "error")}
    #     }
    #   }
    #
    #
    #   tab_to_del_config = input$tab_to_del_config
    #   if(!is.null(tab_to_del_config)){
    #     for (i in tab_to_del_config){
    #       if(i %in% dbListTables(pool_config)){
    #         dbRemoveTable(pool_config, i)
    #         showNotification(paste0("Config table", i, " deleted!"), type = "warning")
    #       } else {showNotification(paste0("Config table", i, " NOT found!"), type = "error")}
    #     }
    #   }
    #
    # })
    #
    #
    # # Function for closing the delete modal
    # close_delete_modal = function(){
    #   removeModal()
    #   session$reload()
    # }
    #
    #
    # # Observe closing button of delete modal
    # observeEvent(input$delete_widgets_button_close, {
    #   close_delete_modal()
    # })





  })
}

## To be copied in the UI
# mod_module_library_controls_ui("module_library_controls_1")

## To be copied in the server
# mod_module_library_controls_server("module_library_controls_1")
