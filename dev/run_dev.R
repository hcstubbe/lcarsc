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
# lcarsc::run_app(production_mode = "editor", # if in production switch to "production", if in editing "editor"
#                 dbuser = 'user',
#                 dbpassword = 'user',
#                 dbhost = 'dbeditor',
#                 dbname = 'mydbeditor',
#                 options = list(host = '0.0.0.0',
#                                port = 3838))

# Run the application using a local SQL database
# devtools::install_local("/home/rstudio/lcarsM", force = T)
run_app(options = list(production_mode = "production",host = '0.0.0.0', port = 3838))

# pool = pool::dbPool(
#   drv = RSQLite::SQLite(),
#   dbname = "mydb.sqlite3",
#   host = "dbeditor",
#   username = "user",
#   password = "user"
# )
