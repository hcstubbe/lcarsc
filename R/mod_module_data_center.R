#' module_data_center UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom DT dataTableOutput
#' @importFrom RMariaDB dbReadTable
#'
#'
mod_module_data_center_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(width="100%",
             DT::dataTableOutput(ns("responses_table"), width = "100%")
    )
  )
}

#' module_data_center Server Functions
#'
#' @noRd
mod_module_data_center_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    pool = get_golem_options("pool")

    output$responses_table <- DT::renderDataTable({
      input_table = dbReadTable(pool, "app_tbl")[,-c(1,2, 4:8)]
      input_table = t(as.matrix(input_table))
      input_table_df = data.frame(input_table[-1,])
      colnames(input_table_df) = input_table[1,-1]
      input_table_df <- datatable(input_table_df,
                                  rownames = TRUE,
                                  options = list(searching = TRUE, lengthChange = FALSE, pageLength = 80, scrollX = TRUE),
                                  selection = c("single")
      )

      input_table_df

    })


  })
}

## To be copied in the UI
# mod_module_data_center_ui("module_data_center_1")

## To be copied in the server
# mod_module_data_center_server("module_data_center_1")
