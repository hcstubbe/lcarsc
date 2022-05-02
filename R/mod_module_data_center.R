#' module_data_center UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinydashboard box
#' @import dplyr
#'
mod_module_data_center_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(type = "tabs",
                tabPanel("Overview", br(),
                         shinydashboard::box(title = "Overview",
                                             status = "primary",
                                             collapsible = FALSE,
                                             collapsed = FALSE,
                                             width = 12,
                                             solidHeader = TRUE,
                                             "Here are some summaries")
                ),
                tabPanel("Comparisons", br(),
                         shinydashboard::box(title = "Comparisons",
                                            status = "primary",
                                            collapsible = FALSE,
                                            collapsed = FALSE,
                                            width = 12,
                                            solidHeader = TRUE,
                                            "Here are some comparisons")
                )
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


  })
}

## To be copied in the UI
# mod_module_data_center_ui("module_data_center_1")

## To be copied in the server
# mod_module_data_center_server("module_data_center_1")
