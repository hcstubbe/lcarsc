# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)
lcarsc::run_app(dbuser = 'user', dbpassword = 'user', dbhost = 'dbeditor', dbname = 'mydbeditor') # add parameters here (if any)
