# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Comment this if you don't want the app to be served on a random port
options(shiny.port = 3838)

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# # Run the application using mariaDB
# lcarsc::run_app(dbuser = 'user',
#                 dbpassword = 'user',
#                 dbhost = 'dbeditor',
#                 dbname = 'mydbeditor',
#                 options = list(host = '0.0.0.0',
#                                port = 3838))

# Run the application using a local SQL database
# devtools::install_local("/home/rstudio/lcarsM", force = T)
run_app(options = list(host = '0.0.0.0', port = 3838))

# pool = pool::dbPool(
#   drv = RSQLite::SQLite(),
#   dbname = "mydb.sqlite3",
#   host = "dbeditor",
#   username = "user",
#   password = "user"
# )

# reset_config = function(){
#   config_database_driver = RSQLite::SQLite() # RMariaDB::MariaDB()
#   config_dbuser = "config_user"
#   config_dbpassword = "config_password"
#   config_dbhost = "db_config"
#   config_dbname = "db_config.sqlite3"
#   pool_config = pool::dbPool(drv = config_database_driver,user = config_dbuser,password = config_dbpassword,host = config_dbhost,db = config_dbname)
#   RMariaDB::dbRemoveTable(pool_config, "start_config")
# }
# reset_config()
# #
# config_database_driver = RSQLite::SQLite() # RMariaDB::MariaDB()
# config_dbuser = "config_user"
# config_dbpassword = "config_password"
# config_dbhost = "db_config"
# config_dbname = "db_config.sqlite3"
# # pool_config = pool::dbPool(drv = config_database_driver,user = config_dbuser,password = config_dbpassword,host = config_dbhost,db = config_dbname)
# x = list()
# for (i in RMariaDB::dbListTables(conn = pool_config)){
#   xi = RMariaDB::dbReadTable(conn = pool_config, name = i)
#   x = c(x , list(xi))
# }
# names(x) = RMariaDB::dbListTables(conn = pool_config)
# x
#
#
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
