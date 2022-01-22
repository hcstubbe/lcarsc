#' loadData
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
#' @importFrom RMariaDB dbListTables dbReadTable
loadData <- function(pool, tableName) {
  if(length(dbListTables(pool)) > 0){
    if(tableName %in% dbListTables(pool)){
      dat = dbReadTable(pool, tableName)
    }else{
      dat = NULL
    }
  }else{
    dat = NULL
  }

  return(dat)
}
