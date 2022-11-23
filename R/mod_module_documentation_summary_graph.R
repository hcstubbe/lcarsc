#' module_documentation_summary_graph UI Function
#'
#' @description A shiny module for summarizing and plotting visit data to create an overview of the study's progress.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList div renderUI fluidRow column plotOutput checkboxGroupInput
#' @importFrom ggplot2 ggplot geom_line geom_point theme_minimal labs aes scale_color_manual
#' @importFrom scales hue_pal
mod_module_documentation_summary_graph_ui <- function(id){
  ns <- shiny::NS(id)

  widget_data = load_widget_data(pool_config = golem::get_golem_options("pool_config"),
                                       production_mode = golem::get_golem_options("production_mode"))
  widget_choices = widget_data$all_visits$visit_id
  names(widget_choices) = widget_data$all_visits$visit_title

  widget_choices = widget_choices[!(widget_choices %in% c("vi", "samples"))]

  shiny::tagList(
    shinydashboard::box(title = "Development",
                        status = "primary",
                        collapsible = FALSE,
                        collapsed = FALSE,
                        width = 12,
                        solidHeader = TRUE,
                        shiny::fluidRow(
                          shiny::column(shiny::plotOutput(ns("summary_graph")), width = 7),
                          shiny::column(shiny::radioButtons(inputId = ns("selected_count"),
                                                            label = "Select count",
                                                            choices = c("Documented", "Submitted")),
                                        shiny::checkboxGroupInput(inputId = ns("selected_visits"),
                                                                  label = "Select visits",
                                                                  choices = widget_choices),
                                        width = 5)
                        ))

  )
}

#' module_documentation_summary_graph Server Functions
#'
#' @noRd
mod_module_documentation_summary_graph_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    pool = golem::get_golem_options("pool")
    widget_data = load_widget_data(pool_config = golem::get_golem_options("pool_config"),
                                   production_mode = golem::get_golem_options("production_mode"))
    all_visits = widget_data$all_visits
    all_visits[all_visits$visit_id == "vi","visit_id"] = "inclusion_dataset"

    # Define function for getting time data
    get_time_data = function(selected_visits){
      if(length(selected_visits > 0)){
        query_visits = c("inclusion_dataset",  paste0("visit_table_", selected_visits))
      }else{
        query_visits = c("inclusion_dataset")
      }

      inclusion_series = data.frame()
      for(i in query_visits){
        inclusion_series_i = db_read_select(pool,
                                          tbl_id = i,
                                          count_rows = FALSE,
                                          pid_x = NULL,
                                          use.pid = FALSE,
                                          filter_deleted_rows = TRUE,
                                          filter_sumitted_rows = FALSE,
                                          select_cols = c("submitted_row", "date_modified", "pid"))
        inclusion_series_i = inclusion_series_i[!duplicated(inclusion_series_i$pid),]
        if(nrow(inclusion_series_i) >0){
          inclusion_series_i$visit_title = all_visits[all_visits$visit_id == gsub("visit_table_", "", i), "visit_title"]
          inclusion_series_i$cumulative_count = 1:nrow(inclusion_series_i)
          inclusion_series_i$cumulative_submissions = cumsum(inclusion_series_i$submitted_row)
          inclusion_series = rbind(inclusion_series, inclusion_series_i)
          inclusion_series$visit_title = factor(inclusion_series$visit_title, levels = all_visits$visit_title)
        }
      }
      inclusion_series$date_modified = as.Date(inclusion_series$date_modified, format = "%a %b %d %H:%M:%S %Y")
      return(inclusion_series)
    }

    # Make colors
    factor_colors = scales::hue_pal()(length(all_visits$visit_title))

    # Render plot
    output$summary_graph = renderPlot({
        time_data = get_time_data(input$selected_visits)
        if(length(nrow(time_data)) != 0){
            if(input$selected_count == "Documented"){
              ggplot2::ggplot(data = time_data,
                              ggplot2::aes(x=date_modified,
                                           y=cumulative_count,
                                           group = visit_title,
                                           color = visit_title)) +
                ggplot2::geom_line() +
                ggplot2::theme_minimal() +
                ggplot2::labs(x = "Date", y = "Count [n]", color = "Visit") +
                ggplot2::scale_color_manual(values = factor_colors)
            }else if(input$selected_count == "Submitted"){
              ggplot2::ggplot(data = time_data,
                              ggplot2::aes(x=date_modified,
                                           y=cumulative_submissions,
                                           group = visit_title,
                                           color = visit_title)) +
                ggplot2::geom_line() +
                ggplot2::theme_minimal() +
                ggplot2::labs(x = "Date", y = "Count [n]", color = "Visit") +
                ggplot2::scale_color_manual(values = factor_colors)
            }
        }else{
          NULL
        }

    })

  })
}

## To be copied in the UI
# mod_module_documentation_summary_graph_ui(ns("module_documentation_summary_graph_1"))

## To be copied in the server
# mod_module_documentation_summary_graph_server("module_documentation_summary_graph_1")
