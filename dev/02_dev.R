# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package("shiny")
usethis::use_package("golem")
usethis::use_package("htmltools")
usethis::use_package("dplyr")
usethis::use_package("pool")
usethis::use_package("RMariaDB")
usethis::use_package("shinydashboard")
usethis::use_package("shinyvalidate")
usethis::use_package("shinyjs")
usethis::use_package("uuid")
usethis::use_package("readr")
usethis::use_package("RSQLite")
usethis::use_package("DT")

## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = "module_launcher", with_test = TRUE) # Name of the module
golem::add_module(name = "module_new_pat", with_test = TRUE) # Name of the module
golem::add_module(name = "module_documentation", with_test = TRUE) # Name of the module
golem::add_module(name = "module_edit_tab", with_test = TRUE) # Name of the module
golem::add_module(name = "module_editor_controls", with_test = TRUE) # Name of the module
golem::add_module(name = "module_editor_launcher", with_test = TRUE) # Name of the module
golem::add_module(name = "module_data_center", with_test = TRUE) # Name of the module
golem::add_module(name = "module_preview", with_test = TRUE) # Name of the module
golem::add_module(name = "module_preview_mobile", with_test = TRUE) # Name of the module
golem::add_module(name = "module_deploy", with_test = TRUE) # Name of the module
golem::add_module(name = "module_settings", with_test = TRUE) # Name of the module
golem::add_module(name = "module_db_settings", with_test = TRUE) # Name of the module
golem::add_module(name = "module_admin", with_test = TRUE) # Name of the module
golem::add_module(name = "module_library", with_test = TRUE) # Name of the module
golem::add_module(name = "module_library_controls", with_test = TRUE) # Name of the module
golem::add_module(name = "module_documentation_summary", with_test = TRUE) # Name of the module
golem::add_module(name = "module_documentation_summary_graph", with_test = TRUE) # Name of the module
golem::add_module(name = "module_reports", with_test = TRUE) # Name of the module
golem::add_module(name = "module_reports_editor", with_test = TRUE) # Name of the module
golem::add_module(name = "module_reports_controls", with_test = TRUE) # Name of the module

## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct("db_read_select", with_test = TRUE)
golem::add_fct("define_widget", with_test = TRUE)
golem::add_fct("makeWidgetList_simple", with_test = TRUE)
golem::add_fct("makeWidgetList_panels", with_test = TRUE)
golem::add_fct("loadData", with_test = TRUE)
golem::add_fct("randomIdGenerator", with_test = TRUE)
golem::add_fct("format_input_for_database", with_test = TRUE)
golem::add_fct("make_widget_tables", with_test = TRUE)
golem::add_fct("load_widget_data", with_test = TRUE)
golem::add_fct("preview_mobile", with_test = TRUE)
golem::add_fct("get_production_mode", with_test = TRUE)
golem::add_fct("db_replace_tables", with_test = TRUE)
golem::add_fct("update_all_fields", with_test = TRUE)
golem::add_fct("user_is_admin", with_test = TRUE)
golem::add_fct("user_is_reviewer", with_test = TRUE)
golem::add_fct("get_current_user", with_test = TRUE)
golem::add_fct("check_lock", with_test = TRUE)
golem::add_fct("json_parser", with_test = TRUE)
golem::add_fct("create_summary", with_test = TRUE)
golem::add_fct("get_settings_data", with_test = TRUE)
golem::add_fct("create_report", with_test = TRUE)
golem::add_fct("report_makelist", with_test = TRUE)
golem::add_fct("report_translate", with_test = TRUE)


## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file("script")
golem::add_js_handler("handlers")
golem::add_css_file("custom")
golem::add_sass_file("custom")

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw(name = "app_labels", open = FALSE)
usethis::use_data_raw(name = "widgets", open = FALSE)

## Tests ----
## Add one line by test you want to create
usethis::use_test("app")

# Documentation

## Vignette ----
usethis::use_vignette("lcarsc")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action()
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_release()
usethis::use_github_action_check_standard()
usethis::use_github_action_check_full()
# Add action for PR
usethis::use_github_action_pr_commands()

# Travis CI
usethis::use_travis()
usethis::use_travis_badge()

# AppVeyor
usethis::use_appveyor()
usethis::use_appveyor_badge()

# Circle CI
usethis::use_circleci()
usethis::use_circleci_badge()

# Jenkins
usethis::use_jenkins()

# GitLab CI
usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
