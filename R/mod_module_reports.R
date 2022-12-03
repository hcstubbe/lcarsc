#' module_reports UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
#' @importFrom golem get_golem_options
#'
#' @importFrom rmarkdown render
mod_module_reports_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    sliderInput(ns("slider"), "Slider", 1, 100, 50),
    downloadButton(ns("report"), "Generate report")
  )
}

#' module_reports Server Functions
#'
#' @noRd
mod_module_reports_server <- function(id, rv_in){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    pool = golem::get_golem_options("pool")

    # Create report templates
    report_data = db_read_select(pool = pool,
                                 tbl_id = "report_editor_table_vars",
                                 pid_x = NULL,
                                 filter_deleted_rows = TRUE,
                                 use.pid = FALSE,
                                 order.by = "order_of_var")
    report_ids = levels(as.factor(report_data$visit_for_var))

    filepath_list = sapply(report_ids, create_report_template,report_data = report_data)
    # saveRDS(list(report_data = report_data, report_ids = report_ids, filepath_list = filepath_list), "tests/tl.RDS")

    output$report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "report.html",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        # tempReport <- file.path(tempdir(), "report.Rmd")
        # x = file.copy("inst/app/www/report.Rmd", tempReport, overwrite = TRUE)
        # if(x == FALSE){stop("Report template not found!")}

        paramlist = list(db_read_select(pool = pool,
                                        tbl_id = "inclusion_dataset",
                                        pid_x = rv_in$pid()),
                         db_read_select(pool = pool,
                                        tbl_id = paste0("visit_table_", rv_in$selected_visit_id()),
                                        row_id = rv_in$selected_row_id())
                         )
        names(paramlist) = c("visit_table_vi", paste0("visit_table_", rv_in$selected_visit_id()))

        # paramlist = sapply(report_ids, function(x){
        #
        # })
        #
        # list("visit_table_v_bl" = )


        # Set up parameters to pass to Rmd document
        params <- list(paramlist = paramlist)

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(filepath_list["v_bl"], output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )


  })
}

## To be copied in the UI
# mod_module_reports_ui(ns("module_reports_1"))

## To be copied in the server
# mod_module_reports_server("module_reports_1")
