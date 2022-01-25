# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Comment this if you don't want the app to be served on a random port
options(shiny.port = httpuv::randomPort())

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# # Run the application using mariaDB
run_app(dbuser = "user", dbpassword = "user", dbhost = "dbeditor", dbname = "mydbeditor")

# # Run the application using a local SQL database
# run_app(database_connection = pool::dbPool(RSQLite::SQLite(),
#                                            user = "user",
#                                            password = "user",
#                                            host = "db",
#                                            db = "mydb.sqlite3"))

