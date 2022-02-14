#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#' @importFrom pool dbPool
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  production_mode,
  database_driver = RMariaDB::MariaDB(),
  dbuser,
  dbpassword,
  dbhost,
  dbname,
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(production_mode = production_mode,
                      pool = pool::dbPool(drv = database_driver,
                                          user = dbuser,
                                          password = dbpassword,
                                          host = dbhost,
                                          db = dbname))
  )
}
