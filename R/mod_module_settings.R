#' module_settings UI Function
#'
#' @description This modules holds the general server settings.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList fluidPage
mod_module_settings_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      wellPanel(
        shinydashboard::box(width = 12, status = "primary",title = "Study information",
                            textInput(ns("study_title"), label = "Title"),
                            textAreaInput(ns("study_introduction"), label = "Introduction")),
        column(6,
               shinydashboard::box(width = 12, status = "primary",title = "Contact 1",
                                   textInput(ns("contact1_name"), label = "Name of contact"),
                                   textInput(ns("contact1_institute"), label = "Institute"),
                                   textInput(ns("contact1_department"), label = "Department"),
                                   textInput(ns("contact1_street"), label = "Street"),
                                   textInput(ns("contact1_postal_code"), label = "Postal code"),
                                   textInput(ns("contact1_phone"), label = "Phone"),
                                   textInput(ns("contact1_fax"), label = "Fax"),
                                   textInput(ns("contact1_mail"), label = "E-mail")
               )
        ),
        column(6,
               shinydashboard::box(width = 12, status = "primary",title = "Contact 2",
                                   textInput(ns("contact2_name"), label = "Name of contact"),
                                   textInput(ns("contact2_institute"), label = "Institute"),
                                   textInput(ns("contact2_department"), label = "Department"),
                                   textInput(ns("contact2_street"), label = "Street"),
                                   textInput(ns("contact2_postal_code"), label = "Postal code"),
                                   textInput(ns("contact2_phone"), label = "Phone"),
                                   textInput(ns("contact2_fax"), label = "Fax"),
                                   textInput(ns("contact2_mail"), label = "E-mail")
                                   )
               )
        ),
      actionButton(inputId = ns("save_contact"), label = "Save")
      )
  )
}

#' module_settings Server Functions
#'
#' @noRd
mod_module_settings_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_module_settings_ui("module_settings_1")

## To be copied in the server
# mod_module_settings_server("module_settings_1")
