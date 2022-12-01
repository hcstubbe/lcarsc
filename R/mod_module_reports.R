#' module_reports UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
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
mod_module_reports_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    pool = golem::get_golem_options("pool")

    output$report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "report.html",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        x = file.copy("inst/app/www/report.Rmd", tempReport, overwrite = TRUE)
        if(x == FALSE){stop("Report template not found!")}


        # Set up parameters to pass to Rmd document
        params <- list(n = create_report(pool))

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
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
