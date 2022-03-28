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

  ns = session$ns
  if(any(is.null(SQL_df_lock$locked_row))){
    locked_row = FALSE
  }
  if(any(is.na(SQL_df_lock$locked_row))){
    locked_row = FALSE
  }
  if(any(SQL_df_lock$locked_row == "")){
    locked_row = FALSE
  }
  if(any(SQL_df_lock$locked_row == TRUE)){
    locked_row = TRUE
  }else{
    locked_row = FALSE
  }

  if(locked_row & silent == FALSE){
    showModal(
      modalDialog(
        title = "Warning!",
        paste0("This entry is locked by user ", SQL_df_lock$editing_user, "!"),
        footer = div(actionButton(ns("force_unlock"), label = "Unlock!", icon = icon("unlock", verify_fa = FALSE)),
                     modalButton("Dismiss")),
        easyClose = FALSE
      ),session = session
    )
  }

  SQL_df_lock$locked_row = locked_row

  return(SQL_df_lock$locked_row)
}
