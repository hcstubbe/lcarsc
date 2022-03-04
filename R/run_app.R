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
#' @importFrom utils askYesNo
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  production_mode = NULL,
  ecrf_database_driver = RMariaDB::MariaDB(),
  ecrf_dbuser = "user",
  ecrf_dbpassword = "user",
  ecrf_dbhost = "dbeditor",
  ecrf_dbname = "mydbeditor",
  config_database_driver = RMariaDB::MariaDB(),
  config_dbuser = "user",
  config_dbpassword = "user",
  config_dbhost = "dbeditor",
  config_dbname = "mydbeditor",
  preview_mobile = FALSE,
  confirm_write_db = TRUE,
  # ecrf_database_driver = RSQLite::SQLite(), # RMariaDB::MariaDB()
  # ecrf_dbuser = "default_user",
  # ecrf_dbpassword = "default_password",
  # ecrf_dbhost = "db_ecrf_data",
  # ecrf_dbname = "db_ecrf_data.sqlite3",
  # config_database_driver = RSQLite::SQLite(), # RMariaDB::MariaDB()
  # config_dbuser = "config_user",
  # config_dbpassword = "config_password",
  # config_dbhost = "db_config",
  # config_dbname = "db_config.sqlite3",
  # preview_mobile = TRUE,
  ...
) {
  if(confirm_write_db == TRUE){
    x = utils::askYesNo(msg = "The package writes database files to the current working directory, if run locally. Continue?")
    if(x == FALSE){
      return(NULL)
    }
  }
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
                      preview_mobile = preview_mobile,
                      pool = pool::dbPool(drv = ecrf_database_driver,
                                          user = ecrf_dbuser,
                                          password = ecrf_dbpassword,
                                          host = ecrf_dbhost,
                                          db = ecrf_dbname),
                      pool_config = pool::dbPool(drv = config_database_driver,
                                               user = config_dbuser,
                                               password = config_dbpassword,
                                               host = config_dbhost,
                                               db = config_dbname)
    )
  )
}
