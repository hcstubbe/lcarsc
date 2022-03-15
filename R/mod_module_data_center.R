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
  pool = get_golem_options("pool")
  tagList(
    shinydashboard::box(title = "Summary", status = "primary", collapsible = TRUE, collapsed = FALSE, width = 12,solidHeader = TRUE,
                        column(2,
                               selectInput(ns("pid_radar_selector"), "Select PID", choices = c("All", (dbReadTable(pool, "app_tbl"))$pid))),
                        column(5, plotOutput(ns("user_radarchart_severity"))),
                        column(5, plotOutput(ns("user_radarchart_promis")))


    ),
    shinydashboard::box(title = "Raw data", status = "primary", collapsible = TRUE, collapsed = TRUE, width = 12,solidHeader = TRUE,
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



    ## Render radar charts

    #### Symptom severity
    output$user_radarchart_severity <- renderPlot({

      input_radar_df = dbReadTable(pool, "app_tbl")[,-c(1,2, 4:8)]

      rownames(input_radar_df) = input_radar_df$pid
      if(input$pid_radar_selector != "All"){
        input_radar_df = input_radar_df %>% filter(pid == input$pid_radar_selector)
      }
      normal_vals = rbind(Min. = rep(0, ncol(input_radar_df)), Max. = rep(3, ncol(input_radar_df)))
      colnames(normal_vals) = colnames(input_radar_df)

      radar_df = normal_vals %>%
        rbind(input_radar_df) %>%
        select(ends_with("severity")) %>%
        mutate(across(1:ncol(.), ~ ifelse(is.na(.x), 0, .x) )) %>%
        mutate(across(1:ncol(.), ~ gsub("[^[:digit:]]","", .x) )) %>%
        mutate(across(1:ncol(.), ~ as.integer(.x) ))
      colnames(radar_df) = gsub("v0_|_severity","",colnames(radar_df))

      fmsb::radarchart(radar_df,
                       title = "Symptom severity",
                       cglty = 1,       # Grid line type
                       cglcol = "gray", # Grid line color
                       pcol = 2:4,      # Color for each line
                       plwd = 2,        # Width for each line
                       plty = 1)        # Line type for each line
    })


    #### PROMIS
    output$user_radarchart_promis <- renderPlot({
      input_radar_df = dbReadTable(pool, "app_tbl")[,-c(1,2, 4:8)]

      rownames(input_radar_df) = input_radar_df$pid
      if(input$pid_radar_selector != "All"){
        input_radar_df = input_radar_df %>% filter(pid == input$pid_radar_selector)
      }
      normal_vals = rbind(Min. = rep(0, ncol(input_radar_df)), Max. = rep(5, ncol(input_radar_df)))
      colnames(normal_vals) = colnames(input_radar_df)

      radar_df = normal_vals %>%
        rbind(input_radar_df) %>%
        select(contains("promis"))%>%
        mutate(across(1:ncol(.), ~ ifelse(is.na(.x), 0, .x) )) %>%
        mutate(across(1:ncol(.), ~ gsub("[^[:digit:]]","", .x) )) %>%
        mutate(across(1:ncol(.), ~ as.integer(.x) ))
      colnames(radar_df) = gsub("v0_promis_","",colnames(radar_df))

      fmsb::radarchart(radar_df,
                       title = "PROMIS",
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
