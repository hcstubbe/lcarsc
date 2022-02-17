#' Allow to preview a given app
#' Create the app UI
#'
#' @keywords Internal
#'
#' @param iframe iframe tag designed by \link{preview_mobile}.
#' @param device See \link{preview_mobile} input.
#' @param color See \link{preview_mobile} input.
#' @param landscape See \link{preview_mobile} input.
create_app_ui <- function(iframe, landscape = FALSE) {

  # should never change!
  devices_css_deps <- htmltools::htmlDependency(
    name = "marvel-devices-css",
    version = "1.0.0",
    src = c(file = system.file("marvel-devices-css-1.0.0", package = "shinyMobile")),
    stylesheet = "devices.min.css"
  )

  shiny::fluidPage(
    shiny::tagList(
      devices_css_deps,
      shiny::br()
    ),
    # container for preview app
    shiny::br(),
    shiny::fluidRow(
      align = "center",
      create_app_container(iframe, color = NULL, landscape = landscape)
    )
  )

}


create_app_container <- function(..., skin, color = NULL, landscape) {

  phoneCl <- "marvel-device"
  if (landscape) phoneCl <- paste0(phoneCl, " landscape")
  if (!is.null(color)) phoneCl <- paste(phoneCl, color)

  tag <- shiny::tags$div(
    class = paste0(phoneCl, " note8"),
    shiny::tags$div(class = "inner"),
    shiny::tags$div(class = "overflow", shiny::tags$div(class = "shadow")),
    shiny::tags$div(class = "speaker"),
    shiny::tags$div(class = "sensors"),
    shiny::tags$div(class = "more-sensors"),
    shiny::tags$div(class = "sleep"),
    shiny::tags$div(class = "volume"),
    shiny::tags$div(class = "camera"),
    shiny::tags$div(class = "screen", ..., height = "800px")
  )

  return(tag)
}
