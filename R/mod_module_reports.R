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
#' @importFrom RMariaDB dbReadTable
mod_module_reports_ui <- function(id){
  ns <- NS(id)
  uiOutput(ns("report_ui"))
}

#' module_reports Server Functions
#'
#' @noRd
mod_module_reports_server <- function(id, rv_in, widgets_table_global){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    pool = golem::get_golem_options("pool")

    # Create report templates and save in temp folder
    report_data = db_read_select(pool = pool,
                                 tbl_id = "report_editor_table_vars",
                                 pid_x = NULL,
                                 filter_deleted_rows = TRUE,
                                 use.pid = FALSE,
                                 order.by = "order_of_var")
    report_ids = levels(as.factor(report_data$visit_for_var))

    filepath_list = sapply(report_ids, create_report_template,report_data = report_data)


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

        # Add currently selected visit and inclusion visit
        paramlist = list(db_read_select(pool = pool,
                                        tbl_id = "inclusion_dataset",
                                        pid_x = rv_in$pid()),
                         db_read_select(pool = pool,
                                        tbl_id = paste0("visit_table_", rv_in$selected_visit_id()),
                                        row_id = rv_in$selected_row_id())
                         )
        names(paramlist) = c("visit_table_vi", rv_in$selected_visit_id())

        # Add additional visits to paramlist
        query_visits = report_data[report_data$visit_for_var %in% rv_in$selected_visit_id(),
                                   "visit_id_for_query"]
        query_visits = query_visits[!is.na(query_visits) & !duplicated(query_visits)]
        query_visits = query_visits[!(query_visits %in% c("vi", rv_in$selected_visit_id()))]

        if( length(query_visits) > 0 ){
          names(query_visits) = query_visits
          paramlist = c(paramlist,
                        sapply(query_visits, function(x){
                          db_read_select(pool = pool,
                                         tbl_id = paste0("visit_table_", x),
                                         pid_x = rv_in$pid())
                        },
                        USE.NAMES = TRUE,
                        simplify = FALSE)
          )
        }
        names(paramlist) = paste0("visit_table_", names(paramlist))




        icd10_codes = tryCatch(RMariaDB::dbReadTable(pool,
                                       "reference_icd10_codes"),
                 error = function(e) {
                   showNotification(paste0("Reference ICD10 could not be loaded! Original error message: ",
                                           e),
                                    type = "error",
                                    duration = NULL)
                   return(data.frame(icd10 = NA, description = NA))
                 })

        paramlist$icd10_codes = icd10_codes


        atc_codes = tryCatch(RMariaDB::dbReadTable(pool,
                                                     "reference_atc_codes"),
                               error = function(e) {
                                 showNotification(paste0("Reference atc could not be loaded! Original error message: ",
                                                         e),
                                                  type = "error",
                                                  duration = NULL)
                                 return(data.frame(atc = NA, description = NA))
                               })

        paramlist$atc_codes = atc_codes



        # Set up parameters to pass to Rmd document
        params <- list(paramlist = paramlist,
                       report_makelist = report_makelist,
                       report_translate = report_translate,
                       report_icd10_add_descr = report_icd10_add_descr,
                       report_atc_add_descr = report_atc_add_descr,
                       widgets_table_global = widgets_table_global)


        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        fpath = filepath_list[rv_in$selected_visit_id()]
        if(length(fpath) > 0 & !is.na(fpath)){
          rmarkdown::render(fpath, output_file = file,
                            params = params,
                            envir = new.env(parent = globalenv())
          )
        }

      }
    )


    output$report_ui = renderUI({
      if(rv_in$selected_visit_id() %in% report_ids){

        div(shinydashboard::box(
          title = "REPORT",
          width = 12,
          status = "success",
          solidHeader = FALSE,
          collapsible = TRUE,
          collapsed = FALSE,
          fluidPage(
            downloadButton(ns("report"), "Generate report")
          )
        ))

      }else{
        NULL
      }
    })

  })
}

## To be copied in the UI
# mod_module_reports_ui(ns("module_reports_1"))

## To be copied in the server
# mod_module_reports_server("module_reports_1")
