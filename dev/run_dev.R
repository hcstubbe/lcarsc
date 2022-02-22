# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Comment this if you don't want the app to be served on a random port
options(shiny.port = 3838)

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# Run the application using a local SQL database
# devtools::install_local("/home/rstudio/lcarsM", force = T)
# run_app(options = list(host = '0.0.0.0', port = 3838))

run_app( config_database_driver = RMariaDB::MariaDB(),
         config_dbuser = "user",
         config_dbpassword = "user",
         config_dbhost = "db",
         config_dbname = "mydb",
         ecrf_database_driver = RMariaDB::MariaDB(),
         ecrf_dbuser = "user",
         ecrf_dbpassword = "user",
         ecrf_dbhost = "db",
         ecrf_dbname = "mydb",
         options = list(host = '0.0.0.0', port = 3838))


# ecrf_database_driver = RSQLite::SQLite() # RMariaDB::MariaDB()
# ecrf_dbuser = "default_user"
# ecrf_dbpassword = "default_password"
# ecrf_dbhost = "db_ecrf_data"
# ecrf_dbname = "db_ecrf_data.sqlite3"
# pool = pool::dbPool(drv = ecrf_database_driver,
#                     user = ecrf_dbuser,
#                     password = ecrf_dbpassword,
#                     host = ecrf_dbhost,
#                     db = ecrf_dbname)
# x = list()
# for (i in RMariaDB::dbListTables(conn = pool)){
#   xi = RMariaDB::dbReadTable(conn = pool, name = i)
#   x = c(x , list(xi))
# }
# names(x) = RMariaDB::dbListTables(conn = pool)
# x
