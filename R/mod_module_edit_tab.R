#' module_edit_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import dplyr
#' @importFrom shiny NS tagList
#' @importFrom RMariaDB dbListTables dbAppendTable dbExecute dbCreateTable
#' @importFrom golem get_golem_options
#' @importFrom shinyvalidate InputValidator sv_required
#' @importFrom DT dataTableOutput
#' @importFrom uuid UUIDgenerate
mod_module_edit_tab_ui <- function(id) {
  ns = NS(id)
  tagList(

    fluidPage(
      fluidRow(
        actionButton(ns("add_button"), "Add", icon("plus")),
        actionButton(ns("edit_button"), "Edit", icon("edit")),
        actionButton(ns("delete_button"), "Delete", icon("trash-alt")),
        actionButton(ns("submit_button"), "Submit", icon("paper-plane"))
      ),
      br(),
      fluidRow(width="100%",
               dataTableOutput(ns("responses_table"), width = "100%")
      )
    )
  )
}

#' module_edit_tab Server Functions
#'
#' @noRd
mod_module_edit_tab_server<- function(id, var_group, tbl_id, widgets_table_global, widget_tab_selection, all_visits, all_tabs = NULL, rv_in = NULL, show_vals = NULL, simple = TRUE, modal_width = ".modal-dialog{ width:400px}", visit_id, create_new_pid = FALSE, add.copy.btn = FALSE, num_entries = 5, order.by) {



  moduleServer(id, function(input, output, session) {
    ns = session$ns

    pool = get_golem_options("pool")

    rv_uuid = reactiveValues()


    if(add.copy.btn == TRUE){
      insertUI(
        selector = paste("#", ns("submit_button"), sep = ""),
        where = "afterEnd",
        ui = actionButton(ns("copy_button"), "Copy", icon("copy"))
      )
    }

    ## Add widgets ----

    # Get required fields
    widgets_table = subset(widgets_table_global, (widget_tab == widget_tab_selection | widget_tab == "all"))
    fieldsAll = widgets_table$label
    names(fieldsAll) = widgets_table$inputId
    sql_tbl_vars = widgets_table$data_type
    names(sql_tbl_vars) = widgets_table$inputId
    visit_choices = all_visits$visit_id[!all_visits$inclusion_other_visit]
    names(visit_choices) = all_visits$visit_title[!all_visits$inclusion_other_visit]

    # Check if database exists and create if required
    if(!is.element(tbl_id, dbListTables(pool))){
      dbCreateTable(pool, tbl_id, fields = sql_tbl_vars)
    }


    # Prepare data base access
    responses_df <- reactive({

      #make reactive to
      input$submit
      input$submit_edit
      input$copy_button
      input$delete_button
      input$submit_data_confirm

      db_read_select(pool, tbl_id, pid = rv_in$pid(), use.pid = !create_new_pid, order.by = order.by)

    })




    # Form for data entry ----
    entry_form <- function(button_id, visit_id, submit = FALSE){
      ## Compile widget list
      if(submit == FALSE){
        if(simple == TRUE){
          widget_list = makeWidgetList_simple(widget_data = widgets_table[widgets_table$widget,], ns = ns, pid = rv_in$pid(), tbl_id = tbl_id)
        }else{
          widget_list = makeWidgetList_panels(widget_data = widgets_table[widgets_table$widget,], all_tabs = all_tabs, visit_id = visit_id, all_visits = all_visits, row_id = rv_in$row_id(), ns = ns, pid = rv_in$pid(), tbl_id = tbl_id)
        }
        showModal(
          modalDialog(
            div(id=(ns("entry_form")),
                tags$head(tags$style(modal_width)),
                tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
                fluidPage(
                  fluidRow(
                    widget_list,
                    actionButton(button_id, "Save"),
                    actionButton(ns("submit_cancel"), "Cancel")
                  )
                )
            ),
            easyClose = FALSE, footer = NULL
          )
        )
      }else{
        showModal(
          modalDialog(
            div(id=(ns("entry_form")),
                tags$head(tags$style(".modal-dialog{ width:400px}")),
                tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
                fluidPage(
                  fluidRow(
                    actionButton(button_id, "Submit"),
                    actionButton(ns("submit_cancel"), "Cancel")
                  )
                )
            ),
            easyClose = FALSE, footer = NULL
          )
        )
      }
    }

    widgets_only = widgets_table[widgets_table$widget,]
    widgets_ids = sapply(1:nrow(widgets_only), function(i) widgets_only$inputId[i])




    ## Gather input data ----
    formData <- reactive({
      data = sapply(names(fieldsAll), function(x) input[[x]], simplify = FALSE, USE.NAMES = TRUE)

      data = format_input_for_database(data = data, pid = rv_in$pid(), input_uuid = rv_uuid$uuid)

    })



    # Add data ----
    appendData <- function(data){
      dbAppendTable(pool,tbl_id, data)
    }

    observeEvent(input$add_button, priority = 20,{

      entry_form(ns("submit"), visit_id)

    })

    observeEvent(input$submit, priority = 20,{
      rv_uuid$uuid = UUIDgenerate()
      iv$enable()
      if (iv$is_valid()) {
        appendData(formData())
        close()
        showNotification("Data saved", type = "message")
        shinyjs::reset("entry_form")
      }


    })

    observeEvent(input$submit_cancel, priority = 20,{
      rv_uuid$uuid = UUIDgenerate()
      close()
      showNotification("Data not saved", type = "warning")
      shinyjs::reset("entry_form")


    })



    # Delete data ----
    deleteData <- reactive({
      SQL_df <- db_read_select(pool, tbl_id, pid = rv_in$pid(), use.pid = !create_new_pid, order.by = order.by)
      row_selection <- SQL_df[input$responses_table_rows_selected, "row_id"]

      # Set old row as 'deleted_row = TRUE'
      db_cmd = sprintf(paste("UPDATE", tbl_id, "SET deleted_row = TRUE WHERE row_id = '%s'"), row_selection)
      dbExecute(pool, db_cmd)

    })

    observeEvent(input$delete_button, priority = 20,{
      rv_uuid$uuid = UUIDgenerate()
      SQL_df <- db_read_select(pool, tbl_id, pid = rv_in$pid(), use.pid = !create_new_pid)
      row_submitted <- SQL_df[input$responses_table_rows_selected, "submitted_row"]
      if(length(row_submitted) < 1){
        row_submitted = FALSE
      }

      if(length(input$responses_table_rows_selected)>=1 & row_submitted == FALSE){
        deleteData()
      }

      showModal(

        if(length(input$responses_table_rows_selected) < 1 ){
          modalDialog(
            title = "Warning",
            paste("Please select row(s)." ),easyClose = TRUE
          )
        }else{
          if(row_submitted == TRUE & input$responses_table_rows_selected == 1 ){
            modalDialog(
              title = "Warning",
              paste("Submitted row(s) cannot be deleted." ),easyClose = TRUE
            )
          }
        })
    })



    # Copy data ----
    unique_id <- function(data){
      replicate(nrow(data), UUIDgenerate())
    }

    copyData <- reactive({

      SQL_df <- db_read_select(pool, tbl_id, pid = rv_in$pid(), use.pid = !create_new_pid, order.by = order.by)
      row_selection <- SQL_df[input$responses_table_rows_selected, "row_id"]
      SQL_df <- SQL_df %>% filter(row_id %in% row_selection)
      SQL_df$row_id <- unique_id(SQL_df)
      SQL_df$date_modified = as.character(date())
      SQL_df$submitted_row = FALSE
      if(create_new_pid){
        SQL_df$pid = randomIdGenerator(exisiting_IDs = loadData(pool, "inclusion_dataset")$pid)
      }
      dbAppendTable(pool,tbl_id, SQL_df)

    })

    observeEvent(input$copy_button, priority = 20,{

      if(length(input$responses_table_rows_selected)>=1 ){
        copyData()
      }

      showModal(
        if(length(input$responses_table_rows_selected) < 1 ){
          modalDialog(
            title = "Warning",
            paste("Please select row(s)." ),easyClose = TRUE
          )
        })

    })



    # Edit data ----

    ## Functions for updating fields
    updateSelectInputFromDatabase = updateSelectInput
    updateNumericInputCouter = updateNumericInput
    updateTextInputForChoicesFromVar = function(...){
      NULL
    }

    start_upper = function(x) {
      substr(x, 1, 1) = toupper(substr(x, 1, 1))
      x
    }
    select_true = grepl("select", widgets_table$type, TRUE, ignore.case = T) | grepl("radio", widgets_table$type, TRUE, ignore.case = T)
    widgets_x =  subset(widgets_table, select_true)
    if(nrow(widgets_x) > 0){
      field_updates_select = paste("update",
                                   start_upper(widgets_x[widgets_x$widget,"type"]),
                                   "(session,'",
                                   widgets_x[widgets_x$widget,"inputId"],
                                   "', selected = SQL_df[input$responses_table_rows_selected,'",
                                   widgets_x[widgets_x$widget,"inputId"],
                                   "'])",
                                   sep = "")
    }else{
      field_updates_select = NULL
    }

    widgets_x = subset(widgets_table, !select_true)
    if(nrow(widgets_x) > 0){
      field_updates_value = paste("update",
                                  start_upper(widgets_x[widgets_x$widget,"type"]),
                                  "(session,'",
                                  widgets_x[widgets_x$widget,"inputId"],
                                  "', value = SQL_df[input$responses_table_rows_selected,'",
                                  widgets_x[widgets_x$widget,"inputId"],
                                  "'])",
                                  sep = "")
    }else{
      field_updates_value = NULL
    }
    field_updates = c(field_updates_select, field_updates_value)




    ## Open edit dialogue
    observeEvent(input$edit_button, priority = 20,{
      SQL_df <- db_read_select(pool, tbl_id, pid = rv_in$pid(), use.pid = !create_new_pid, order.by = order.by)
      row_submitted <- SQL_df[input$responses_table_rows_selected, "submitted_row"]

      if(length(row_submitted) < 1){
        row_submitted = FALSE
      }

      showModal(
        if(length(input$responses_table_rows_selected) > 1 ){
          modalDialog(
            title = "Warning",
            paste("Please select only one row." ),easyClose = TRUE)
        } else if(length(input$responses_table_rows_selected) < 1){
          modalDialog(
            title = "Warning",
            paste("Please select a row." ),easyClose = TRUE)
        }else if(length(input$responses_table_rows_selected) == 1 & row_submitted == TRUE){
          modalDialog(
            title = "Warning",
            paste("Submitted row(s) cannot be edited"),easyClose = TRUE
          )
        }
      )

      if(length(input$responses_table_rows_selected) == 1  & row_submitted == FALSE){

        entry_form(ns("submit_edit"), visit_id)

        for(i in field_updates){
          eval(parse(text=i))
        }
      }

    })


    #### Update row ----
    observeEvent(input$submit_edit, priority = 20, {

      iv$enable()
      if (iv$is_valid()) {

        SQL_df <- db_read_select(pool, tbl_id, pid = rv_in$pid(), use.pid = !create_new_pid, order.by = order.by)
        row_selection <- SQL_df[input$responses_table_row_last_clicked, "row_id"]

        # Add new row
        rv_uuid$uuid = UUIDgenerate()
        appendData(formData())

        # Set old row as 'deleted_row = TRUE'
        db_cmd = sprintf(paste("UPDATE", tbl_id, "SET deleted_row = TRUE WHERE row_id = '%s'"), row_selection)
        dbExecute(pool, db_cmd)

        # Close modal
        close()
        showNotification("Data added", type = "message")
        shinyjs::reset("entry_form")
      }

    })


    # Submit data ----
    observeEvent(input$submit_button, priority = 20,{

      SQL_df <- db_read_select(pool, tbl_id, pid = rv_in$pid(), use.pid = !create_new_pid, order.by = order.by)
      row_submitted <- SQL_df[input$responses_table_rows_selected, "submitted_row"]
      if(length(row_submitted) < 1){
        row_submitted = FALSE
      }

      showModal(
        if(length(input$responses_table_rows_selected) > 1 ){
          modalDialog(
            title = "Warning",
            paste("Please select only one row." ),easyClose = TRUE)
        } else if(length(input$responses_table_rows_selected) < 1){
          modalDialog(
            title = "Warning",
            paste("Please select a row." ),easyClose = TRUE)
        }else if(length(input$responses_table_rows_selected) == 1 & row_submitted == TRUE){
          modalDialog(
            title = "Warning",
            paste("The seclected row(s) are submitted!"),easyClose = TRUE
          )
        }
      )

      if(row_submitted == FALSE & length(input$responses_table_rows_selected) == 1 ){
        entry_form(ns("submit_data_confirm"), submit = TRUE)
      }
    })

    observeEvent(input$submit_data_confirm, priority = 20,{


      SQL_df <- db_read_select(pool, tbl_id, pid = rv_in$pid(), use.pid = !create_new_pid, order.by = order.by)
      row_selection <- SQL_df[input$responses_table_row_last_clicked, "row_id"]

      # Set old row as 'deleted_row = TRUE'
      db_cmd = sprintf(paste("UPDATE", tbl_id, "SET submitted_row = TRUE WHERE row_id = '%s'"), row_selection)
      dbExecute(pool, db_cmd)
      removeModal()


    })

    # Render entry table --------
    output$responses_table <- DT::renderDataTable({
      table = responses_df()
      table = table[,show_vals]
      names(table) = names(show_vals)
      table <- datatable(table,
                         rownames = FALSE,
                         options = list(searching = FALSE, lengthChange = FALSE, pageLength = num_entries),
                         selection = c("single")
      )

    })

    ####-------- Observe mandatory fields --------####

    iv <- InputValidator$new()
    required_fields = widgets_table[widgets_table$widget & widgets_table$mandatory,]$inputId
    sapply(required_fields, function(x) iv$add_rule(x, sv_required()))

    close <- function() {
      removeModal()
      iv$disable()
    }


  })

}

## To be copied in the UI
# mod_module_edit_tab_ui("module_edit_tab_1")

## To be copied in the server
# mod_module_edit_tab_server("module_edit_tab_1")
