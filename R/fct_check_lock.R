#' check_lock
#'
#' @description Check wheter entry is locked by a user
#'
#' @return The return value, if any, from executing the function.
#'
#' @importFrom shiny showModal modalDialog icon actionButton modalButton
#'
#' @noRd

check_lock = function(SQL_df_lock, session, silent = FALSE){

  locked_row = SQL_df_lock$locked_row
  editing_user = SQL_df_lock$editing_user

  ns = session$ns
  if(is.null(locked_row)){
    locked_row = FALSE
  }
  if(any(is.na(locked_row))){
    locked_row = FALSE
  }
  if(any(locked_row == "")){
    locked_row = FALSE
  }
  if(any(locked_row == TRUE)){
    locked_row = TRUE
  }else{
    locked_row = FALSE
  }

  if(any(locked_row == TRUE) & silent == FALSE){
    showModal(
      modalDialog(
        title = "Warning!",
        paste0("This entry is locked by user ", editing_user, "!"),
        footer = div(actionButton(ns("force_unlock"), label = "Unlock!", icon = icon("unlock", verify_fa = FALSE)),
                     modalButton("Dismiss")),
        easyClose = FALSE
      ),session = session
    )
  }

  return(locked_row)
}
