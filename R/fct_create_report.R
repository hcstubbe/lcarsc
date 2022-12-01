#' create_report
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @importFrom knitr spin
#'
#' @noRd
create_report = function(pool){


  # Get report
  report_data = db_read_select(pool = pool,
                               tbl_id = "report_editor_table_vars",
                               pid_x = NULL,
                               filter_deleted_rows = TRUE,
                               use.pid = FALSE,
                               order.by = "order_of_var")
  report_data = report_data %>% dplyr::select(which(colnames(.) == "inputId"):ncol(.))
  non_na = !is.na(report_data$visit_id_for_query)
  report_data$inputId_for_query[non_na] = paste0(report_data$visit_id_for_query[non_na], "_", report_data$inputId_for_query[non_na])



  # Function for making markdown line

  make_markdown_line = function(i, report_data) {

    code_line_i = NULL

    # Text
    if(report_data[i, "type"] == "Text"){
      code_line_i = report_data[i, "display_text"]
    }

    # Line break
    if(report_data[i, "type"] == "Line break"){
      num_breaks = report_data[i, "num_line_breaks"]
      if(is.na(num_breaks)){num_breaks = 1}
      code_line_i = rep("<br>", num_breaks)

      code_line_i = paste0("<lbr>", code_line_i, "<lbr>")
    }

    # Database value
    if(report_data[i, "type"] == "Database value"){
      code_line_i = paste0("visit_table_",
                           report_data[i, "visit_id_for_query"],
                           "$",
                           report_data[i, "inputId_for_query"])
      code_line_i = paste0('`r ', code_line_i, '`')
    }

    # Key/value
    if(report_data[i, "type"] == "Key/value"){
      var_i = paste0("visit_table_",
                           report_data[i, "visit_id_for_query"],
                           "$",
                           report_data[i, "inputId_for_query"])
      code_line_i = paste0('`r ', 'if(', var_i, ' == "' , report_data[i, "condition"], '"){"', report_data[i, "replace_value"], '"}else{NULL}`')
    }



    return(code_line_i)
  }

  # Create lines of code for R markdown file
  code_lines = sapply(1:nrow(report_data), make_markdown_line, report_data = report_data)


  code_lines = sapply(code_lines, paste, collapse = " ")
  code_lines = paste(code_lines, collapse = " ")
  code_lines = unlist(strsplit(code_lines, split = "<lbr>", perl = TRUE))

  code_lines = c(
    '---',
    'title: "Untitled"',
    'output: html_document',
    'date: "2022-12-01"',
    '---',
    '',
    '```{r setup, include=FALSE}',
    'knitr::opts_chunk$set(echo = TRUE)',
    '```',
    '',
    code_lines
  )

  # Write report rmd into temp dir
  tempReport <- file.path(tempdir(), "report.Rmd")
  fileConn<-file(tempReport)

  writeLines(code_lines, fileConn)

  close(fileConn)

  return(tempReport)
  # r_code = "plot(cars)
  #           print(3)"
  #
  # knitr::spin(text = r_code)

}
