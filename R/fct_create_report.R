#' create_report
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @importFrom knitr spin
#'
#' @noRd
create_report = function(){

  r_code = "plot(cars)
            print(3)"

  knitr::spin(text = r_code)

}
