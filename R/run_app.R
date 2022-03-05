#' Run the Shiny Application
#'
#'
#' @param production_mode Set production mode before running the package (usually this should be NULL).
#' @param ecrf_database_driver Set the database driver.
#' @param ecrf_dbuser Username of the database.
#' @param config_dbpassword Password of the database.
#' @param config_dbhost Hostname of the database.
#' @param ecrf_dbname Name of the database.
#' @param config_database_driver Set the config database driver.
#' @param config_dbuser Username of the config database.
#' @param config_dbpassword  Password of the config database.
#' @param config_dbhost Hostname of the config database.
#' @param config_dbname Name of the config database.
#' @param preview_mobile TRUE if mobile app preview should be shown (this requires lcarsM)
#' @param confirm_write_db TRUE if user input should be requested before writing local database files.
#' @param start_as_admin If TRUE, the user will have admin privileges without checking the user group. If FALSE, the user will never have admin privileges regardless of user group. If null, the software checks against the database, if the user is an admin.
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#' @importFrom pool dbPool
#' @importFrom utils askYesNo
#' @importFrom golem get_golem_options
#' @importFrom RMariaDB dbReadTable
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
  confirm_write_db = FALSE,
  start_as_admin = NULL,
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
                      user_is_admin = user_is_admin(pool_config = pool::dbPool(drv = config_database_driver,
                                                                               user = config_dbuser,
                                                                               password = config_dbpassword,
                                                                               host = config_dbhost,
                                                                               db = config_dbname),
                                                    start_as_admin = start_as_admin),
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
