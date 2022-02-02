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
#' @importFrom fmsb radarchart
#' @import dplyr
#'
mod_module_data_center_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(width="100%",
             plotOutput(ns("user_radarchart"))
    ),
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



    ## Render radar chart
    output$user_radarchart <- renderPlot({
      input_radar_df = dbReadTable(pool, "app_tbl")[,-c(1,2, 4:8)]

      rownames(input_radar_df) = input_radar_df$pid
      normal_vals = rbind(Min. = rep(0, ncol(input_radar_df)), Max. = rep(3, ncol(input_radar_df)))
      colnames(normal_vals) = colnames(input_radar_df)

      radar_df = normal_vals %>%
        rbind(input_radar_df) %>%
        select(ends_with("severity")) %>%
        rbind(min = rep(0, ncol(.))) %>%
        mutate(across(1:ncol(.), ~ ifelse(is.na(.x), 0, .x) )) %>%
        mutate(across(1:ncol(.), ~ gsub("[^[:digit:]]","", .x) )) %>%
        mutate(across(1:ncol(.), ~ as.integer(.x) ))

      radarchart(radar_df,
                 cglty = 1,       # Grid line type
                 cglcol = "gray", # Grid line color
                 pcol = 2:4,      # Color for each line
                 plwd = 2,        # Width for each line
                 plty = 1)        # Line type for each line

    })


    ## Render data table
    output$responses_table <- DT::renderDataTable({
      input_table = dbReadTable(pool, "app_tbl")[,-c(1,2, 4:8)]
      input_table = t(as.matrix(input_table))
      input_table_df = data.frame(input_table[-1,])
      colnames(input_table_df) = input_table[1,]
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
