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
#' @importFrom shinyvalidate InputValidator sv_required sv_between sv_optional
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom uuid UUIDgenerate
mod_module_edit_tab_ui <- function(id) {
  ns = NS(id)
  tagList(

    fluidPage(
      fluidRow(
        actionButton(ns("add_button"), "New", icon("plus", verify_fa = FALSE)),
        actionButton(ns("edit_button"), "Edit", icon("edit", verify_fa = FALSE)),
        actionButton(ns("delete_button"), "Delete", icon("trash-alt", verify_fa = FALSE)),
        actionButton(ns("submit_button"), "Submit", icon("paper-plane", verify_fa = FALSE))
      ),
      br(),
      fluidRow(width="100%",
               DT::dataTableOutput(ns("responses_table"), width = "100%")
      )
    )
  )
}

#' module_edit_tab Server Functions
#'
#' @noRd
mod_module_edit_tab_server<- function(id,
                                      var_group,
                                      tbl_id,
                                      widgets_table_global,
                                      widget_tab_selection,
                                      all_visits,
                                      all_tabs = NULL,
                                      rv_in = NULL,
                                      show_vals = NULL,
                                      simple = TRUE,
                                      modal_width = ".modal-dialog{ width:400px}",
                                      visit_id,
                                      create_new_pid = FALSE,
                                      add.copy.btn = FALSE,
                                      num_entries = 5,
                                      order.by,
                                      preview = FALSE,
                                      select_multiple = FALSE,
                                      filter_origin = reactive({NULL}),
                                      search_field = FALSE,
                                      length_change = FALSE) {



  moduleServer(id, function(input, output, session) {
    ns = session$ns

    # Get the data base connection
    pool = get_golem_options("pool")

    # If the form is used for the preview, use local database
    prod_mod = get_production_mode(production_mode = get_golem_options("production_mode"),
                                   pool_config = get_golem_options("pool_config"))
    if(prod_mod == "editor" & preview == TRUE){
      pool = pool::dbPool(
        drv = RSQLite::SQLite(),
        dbname = "db_preview.sqlite3",
        host = "dbeditor",
        username = "user",
        password = "user"
      )
    }

    rv_uuid = reactiveValues()


    if(add.copy.btn == TRUE){
      insertUI(
        selector = paste("#", ns("submit_button"), sep = ""),
        where = "afterEnd",
        ui = actionButton(ns("copy_button"), "Copy", icon("copy", verify_fa = FALSE))
      )
    }

    ## Add widgets ----

    # Get required fields
    widgets_table = subset(widgets_table_global, (widget_tab == widget_tab_selection | widget_tab == "all"))
    fieldsAll = widgets_table$label
    names(fieldsAll) = widgets_table$inputId
    sql_tbl_vars = widgets_table$data_type
    names(sql_tbl_vars) = widgets_table$inputId
    visit_choices = all_visits$visit_id[!(all_visits$inclusion_other_visit == TRUE)]
    names(visit_choices) = all_visits$visit_title[!(all_visits$inclusion_other_visit == TRUE)]

    # Check if database exists and create if required
    if(!is.element(tbl_id, dbListTables(pool))){
      dbCreateTable(pool, tbl_id, fields = sql_tbl_vars)
    }



    # Render entry table --------
    if(select_multiple == FALSE){
      selection_tab = c("single")
    }else{
      selection_tab = c("multiple")
    }
    make_response_table = function(){
      table = db_read_select(pool, tbl_id, pid = rv_in$pid(), use.pid = !create_new_pid, order.by = order.by, filter_origin = filter_origin())
      table = table[,show_vals]
      names(table) = names(show_vals)
      table <- datatable(table,
                         rownames = FALSE,
                         options = list(searching = search_field, lengthChange = length_change, pageLength = num_entries),
                         selection = selection_tab
                         )
      table
      }

    output$responses_table <- DT::renderDataTable({
      make_response_table()
    })

    # Form for data entry ----
    entry_form <- function(button_id, visit_id, submit = FALSE, edit_entry = FALSE){
      ## Compile widget list

      if(submit == FALSE){
        if(simple == TRUE){
          widget_list = makeWidgetList_simple(widget_data = widgets_table[widgets_table$widget == TRUE,],
                                              ns = ns,
                                              pid = rv_in$pid(),
                                              tbl_id = tbl_id)
        }else{
          widget_list = makeWidgetList_panels(widget_data = widgets_table[widgets_table$widget == TRUE,],
                                              all_tabs = all_tabs,
                                              visit_id = visit_id,
                                              all_visits = all_visits,
                                              row_id = rv_in$row_id(),
                                              ns = ns,
                                              pid = rv_in$pid(),
                                              tbl_id = tbl_id)
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
                    if(edit_entry == TRUE){
                      actionButton(ns("edit_cancel"), "Cancel")
                    }else{
                      actionButton(ns("submit_cancel"), "Cancel")
                    }

                  )
                )
            ),
            easyClose = FALSE, footer = NULL
          )
        )
      }else{
        showModal(
          modalDialog(title = "Confirm submission",
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

    widgets_only = widgets_table[widgets_table$widget == TRUE,]
    widgets_ids = sapply(1:nrow(widgets_only), function(i) widgets_only$inputId[i])




    ## Gather input data ----
    formData <- reactive({
      input_data = sapply(names(fieldsAll),
                    function(x) input[[x]],
                    simplify = FALSE,
                    USE.NAMES = TRUE)
      input_data = format_input_for_database(input_data = input_data,
                                       pid = rv_in$pid(),
                                       input_uuid = rv_uuid$uuid,
                                       visit_id = visit_id,
                                       widgets_table = widgets_table,
                                       all_visits = all_visits,
                                       create_new_pid = create_new_pid)
      input_data
    })



    # Add data ----
    observeEvent(input$add_button, priority = 20,{

      entry_form(ns("submit"), visit_id)
      iv$enable()

    })

    observeEvent(input$submit, priority = 20,{
      rv_uuid$uuid = UUIDgenerate()

      if (iv$is_valid()) {
        dbAppendTable(pool, tbl_id, formData())
        close()
        showNotification("Data saved", type = "message")
        shinyjs::reset("entry_form")
        # Update response table
        output$responses_table <- DT::renderDataTable({
          make_response_table()
        })
      }


    })


    # Observe cancel buttons

    ## Cancel submit button
    observeEvent(input$submit_cancel, priority = 20,{
      close()
      showNotification("Data not saved", type = "warning")
      shinyjs::reset("entry_form")

      # Update response table
      output$responses_table <- DT::renderDataTable({
        make_response_table()
      })
    })

    ## Cancel edit button
    observeEvent(input$edit_cancel, priority = 20,{
      rv_uuid$uuid = UUIDgenerate()
      SQL_df <- db_read_select(pool, tbl_id, pid = rv_in$pid(), use.pid = !create_new_pid, order.by = order.by, filter_origin = filter_origin())
      row_selection <- SQL_df[input$responses_table_rows_selected, "row_id"]
      db_cmd = sprintf(paste("UPDATE", tbl_id, "SET locked_row = FALSE WHERE row_id = '%s'"), row_selection)
      dbExecute(pool, db_cmd)
      close()
      showNotification("Data not saved", type = "warning")
      shinyjs::reset("entry_form")

      # Update response table
      output$responses_table <- DT::renderDataTable({
        make_response_table()
      })
    })



    # Delete data ----

    ## Open edit dialogue
    observeEvent(input$delete_button, priority = 20,{
      SQL_df <- db_read_select(pool, tbl_id, pid = rv_in$pid(), use.pid = (create_new_pid == FALSE), order.by = order.by, filter_origin = filter_origin())
      row_submitted <- SQL_df[input$responses_table_rows_selected, "submitted_row"]
      SQL_df_selected = SQL_df[input$responses_table_rows_selected, ]
      row_selection <- SQL_df[input$responses_table_rows_selected, "row_id"]


      if(length(row_submitted) < 1){
        row_submitted = FALSE
      }

      showModal(

        if(length(input$responses_table_rows_selected) < 1 ){
          modalDialog(
            title = "Warning",
            paste("Please select a row." ),easyClose = TRUE
          )
        }else if(length(input$responses_table_rows_selected) > 1 ){
          modalDialog(
            title = "Warning",
            paste("Please select only one row." ),easyClose = TRUE
          )
        }else{
          if(row_submitted == TRUE & input$responses_table_rows_selected == 1 ){
            modalDialog(
              title = "Warning",
              paste("Submitted rows cannot be deleted." ),easyClose = TRUE
            )
          }
        })

      SQL_df_lock = check_lock(SQL_df[input$responses_table_rows_selected, c("editing_user", "locked_row")], session)

      if(length(input$responses_table_rows_selected) == 1 & all(row_submitted == FALSE) & all(SQL_df_lock$locked_row == FALSE)){

        # Set old row as 'deleted_row = TRUE'
        rv_uuid$uuid = UUIDgenerate()
        db_cmd = sprintf(paste("UPDATE", tbl_id, "SET deleted_row = TRUE WHERE row_id = '%s'"), row_selection)
        dbExecute(pool, db_cmd)

        # Update response table
        output$responses_table <- DT::renderDataTable({
          make_response_table()
        })
      }

    })





    # Copy data ----
    unique_id <- function(data){
      replicate(nrow(data), UUIDgenerate())
    }

    copyData <- reactive({

      SQL_df <- db_read_select(pool, tbl_id, pid = rv_in$pid(), use.pid = !create_new_pid, order.by = order.by, filter_origin = filter_origin())
      row_selection <- SQL_df[input$responses_table_rows_selected, "row_id"]
      SQL_df <- SQL_df %>% filter(row_id %in% row_selection)
      SQL_df$row_id <- unique_id(SQL_df)
      SQL_df$date_modified = as.character(date())
      SQL_df$submitted_row = FALSE
      SQL_df$locked_row = FALSE
      if(create_new_pid){
        SQL_df$pid = randomIdGenerator(exisiting_IDs = loadData(pool, "inclusion_dataset")$pid)
      }
      dbAppendTable(pool,tbl_id, SQL_df)

    })

    observeEvent(input$copy_button, priority = 20,{

      if(length(input$responses_table_rows_selected)>=1 ){
        copyData()

        # Update response table
        output$responses_table <- DT::renderDataTable({
          make_response_table()
        })
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

    ## Open edit dialogue
    observeEvent(input$edit_button, priority = 20,{
      SQL_df <- db_read_select(pool, tbl_id, pid = rv_in$pid(), use.pid = (create_new_pid == FALSE), order.by = order.by, filter_origin = filter_origin())
      row_submitted <- SQL_df[input$responses_table_rows_selected, "submitted_row"]
      SQL_df_selected = SQL_df[input$responses_table_rows_selected, ]
      row_selection <- SQL_df[input$responses_table_rows_selected, "row_id"]


      if(length(row_submitted) < 1){
        row_submitted = FALSE
      }

      iv$enable()
      showModal(
        if (length(input$responses_table_rows_selected) > 1 ) {
          modalDialog(
            title = "Warning",
            paste("Please select only one row." ),easyClose = TRUE)
        } else if (length(input$responses_table_rows_selected) < 1){
          modalDialog(
            title = "Warning",
            paste("Please select a row." ),easyClose = TRUE)
        } else if (length(input$responses_table_rows_selected) == 1 & row_submitted == TRUE) {
          modalDialog(
            title = "Warning",
            paste("Submitted row(s) cannot be edited"),easyClose = TRUE
          )
        }
      )

      SQL_df_lock = check_lock(SQL_df[input$responses_table_rows_selected, c("editing_user", "locked_row")], session)

      if(length(input$responses_table_rows_selected) == 1 & all(row_submitted == FALSE) & all(SQL_df_lock$locked_row == FALSE)){


        # Set current row as 'editing = current_user_name'
        db_cmd = sprintf(paste0("UPDATE ", tbl_id, " SET editing_user = '",  get_current_user(), "' WHERE row_id = '%s'"), row_selection)
        dbExecute(pool, db_cmd)
        db_cmd = sprintf(paste("UPDATE", tbl_id, "SET locked_row = TRUE WHERE row_id = '%s'"), row_selection)
        dbExecute(pool, db_cmd)

        entry_form(ns("submit_edit"), visit_id, edit_entry = TRUE)

        update_all_fields(session = session,
                          db_data = SQL_df_selected,
                          widget_data = widgets_table[widgets_table$widget == TRUE,])

      }

    })


    #### Update row ----
    observeEvent(input$submit_edit, priority = 20, {

      if (iv$is_valid()) {

        SQL_df <- db_read_select(pool, tbl_id, pid = rv_in$pid(), use.pid = !create_new_pid, order.by = order.by, filter_origin = filter_origin())
        row_selection <- SQL_df[input$responses_table_rows_selected, "row_id"]

        # Add new row
        rv_uuid$uuid = UUIDgenerate()
        dbAppendTable(pool, tbl_id, formData())

        # Set old row as 'deleted_row = TRUE' and 'locked_row = FALSE'
        db_cmd = sprintf(paste("UPDATE", tbl_id, "SET deleted_row = TRUE WHERE row_id = '%s'"), row_selection)
        dbExecute(pool, db_cmd)
        db_cmd = sprintf(paste("UPDATE", tbl_id, "SET locked_row = FALSE WHERE row_id = '%s'"), row_selection)
        dbExecute(pool, db_cmd)

        # Close modal
        close()
        showNotification("Data added", type = "message")
        shinyjs::reset("entry_form")

        # Update response table
        output$responses_table <- DT::renderDataTable({
          make_response_table()
        })
      }

    })


    # Force unlock data
    observeEvent(input$force_unlock, priority = 20,{

      SQL_df <- db_read_select(pool, tbl_id, pid = rv_in$pid(), use.pid = !create_new_pid, order.by = order.by, filter_origin = filter_origin())
      row_selection <- SQL_df[input$responses_table_rows_selected, "row_id"]

      db_cmd = sprintf(paste("UPDATE", tbl_id, "SET locked_row = FALSE WHERE row_id = '%s'"), row_selection)
      dbExecute(pool, db_cmd)

      close()

      showNotification("Entry unlocked!", type = "error")
      shinyjs::reset("entry_form")
    })

    # Submit data ----
    observeEvent(input$submit_button, priority = 20,{

      SQL_df <- db_read_select(pool, tbl_id, pid = rv_in$pid(), use.pid = !create_new_pid, order.by = order.by, filter_origin = filter_origin())
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

      SQL_df_lock = check_lock(SQL_df[input$responses_table_rows_selected, c("editing_user", "locked_row")], session)

      if(all(row_submitted == FALSE) & length(input$responses_table_rows_selected) == 1 & all(SQL_df_lock$locked_row == FALSE)){
        entry_form(ns("submit_data_confirm"), submit = TRUE)
      }
    })

    observeEvent(input$submit_data_confirm, priority = 20,{


      SQL_df <- db_read_select(pool, tbl_id, pid = rv_in$pid(), use.pid = !create_new_pid, order.by = order.by, filter_origin = filter_origin())
      row_selection <- SQL_df[input$responses_table_rows_selected, "row_id"]

      if(length(input$responses_table_rows_selected) == 1 ){

        db_cmd = sprintf(paste("UPDATE", tbl_id, "SET submitted_row = TRUE WHERE row_id = '%s'"), row_selection)
        dbExecute(pool, db_cmd)

      }


      close()

      # Update response table
      output$responses_table <- DT::renderDataTable({
        make_response_table()
      })

    })


    ####-------- Observe mandatory fields --------####

    iv <- InputValidator$new()

    required_fields = widgets_table[widgets_table$widget == TRUE & widgets_table$mandatory == TRUE,]$inputId
    sapply(required_fields, function(x) iv$add_rule(x, sv_required()))

    numeric_fields = widgets_table[widgets_table$widget == TRUE & widgets_table$type == "numericInput",]$inputId

    sapply(numeric_fields, function(x) {
      iv$add_rule(x, sv_between(left = widgets_table[widgets_table$inputId == x,]$min,
                                 right = widgets_table[widgets_table$inputId == x,]$max,
                                 allow_na = TRUE))
    })

    close <- function() {
      removeModal()
      iv$disable()
    }

    selected_row_id = reactive({
      SQL_df <- db_read_select(pool, tbl_id, pid = rv_in$pid(), use.pid = !create_new_pid, order.by = order.by, filter_origin = filter_origin())
      row_selection <- SQL_df[input$responses_table_rows_selected, "row_id"]
    })


    rv_out = selected_row_id

  })

}

## To be copied in the UI
# mod_module_edit_tab_ui("module_edit_tab_1")

## To be copied in the server
# mod_module_edit_tab_server("module_edit_tab_1")
