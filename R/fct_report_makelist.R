#' report_makelist
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @importFrom shiny HTML
#'
#' @noRd
report_makelist = function(x, italic, bold){
  if(italic == TRUE & bold == FALSE){
    x = paste0("<i>", x, "</i>")
  }
  if(italic == FALSE & bold == TRUE){
    x = paste0("<b>", x, "</b>")
  }
  if(italic == TRUE & bold == TRUE){
    x = paste0("<b><i>", x, "</i></b>")
  }
  html_text = paste("<body><ul>",
                    paste(sapply(x, function(y){paste0("<li>", y, "</li>")}), collapse = ""),
                    "</ul></body>")
  html_output = shiny::HTML(html_text)
  return(html_output)}
