#' create_report_template
#'
#' @description A fct function for creating the R markdown template.
#'
#' @return The return value, if any, from executing the function.
#'
#' @importFrom shiny HTML
#'
#' @noRd
create_report_template = function(report_data, report_id){

  # Get report data
  report_data = report_data %>% filter(visit_for_var == report_id) %>%
    dplyr::select(which(colnames(.) == "inputId"):ncol(.))
  non_na = !is.na(report_data$visit_id_for_query)
  report_data$inputId_for_query[non_na] = paste0(report_data$visit_id_for_query[non_na],
                                                 "_",
                                                 report_data$inputId_for_query[non_na])

  # Function for making markdown line
  make_markdown_line = function(i, report_data) {

    code_line_i = NULL


    # Format function
    bold_italic = function(code_line_i){
      if(sum(report_data[i, "bold"] == TRUE) == 1){
        code_line_i = paste0("*", code_line_i, "*")
      }
      if(sum(report_data[i, "italic"] == TRUE) == 1){
        code_line_i = paste0("**", code_line_i, "**")
      }
      if(sum(report_data[i, "bullet_point"] == TRUE) == 1){
        code_line_i = paste0("<lbr> * ", code_line_i, "<lbr>")
      }
      return(code_line_i)
    }


    # Format function for inline evaluated code
    bold_italic_db = function(i){
      x = list("", "")

      if(sum(report_data[i, "bold"] == TRUE) == 1){
        x = list("*", "*")
      }
      if(sum(report_data[i, "italic"] == TRUE) == 1){
        x = list("**", "**")
      }
      if(sum(report_data[i, "italic"] == TRUE) == 1 &
               sum(report_data[i, "bold"] == TRUE) == 1){
        x = list("***", "***")
      }

      return(x)
    }



    # Create markdown lines
    ## Title 1
    if(report_data[i, "type"] == "Title 1"){
      code_line_i = bold_italic(report_data[i, "display_text"])
      code_line_i = paste0("# ", code_line_i)
      code_line_i = paste0("<lbr> <lbr>", code_line_i, "<lbr> <lbr>")
    }

    ## Title 2
    if(report_data[i, "type"] == "Title 2"){
      code_line_i = bold_italic(report_data[i, "display_text"])
      code_line_i = paste0("## ", code_line_i)
      code_line_i = paste0("<lbr> <lbr>", code_line_i, "<lbr> <lbr>")
    }

    ## Text
    if(report_data[i, "type"] == "Text"){
      code_line_i = bold_italic(report_data[i, "display_text"])
    }

    ## Line break
    if(report_data[i, "type"] == "Line break"){
      num_breaks = report_data[i, "num_line_breaks"]
      if(is.na(num_breaks)){num_breaks = 1}
      code_line_i = rep("<br>", num_breaks)

      code_line_i = paste0("<lbr> <lbr>", code_line_i, "<lbr> <lbr>")
    }

    ## Database value
    if(report_data[i, "type"] == "Database value"){
      code_line_i = paste0("params$paramlist$visit_table_",
                           report_data[i, "visit_id_for_query"],
                           "$",
                           report_data[i, "inputId_for_query"])
      if(report_data[i, "include_translation"] == TRUE){
        code_line_i = paste0('params$report_translate(', code_line_i, ', "', report_data[i, "inputId_for_query"], '", ', 'params$widgets_table_global)')
      }
      if(report_data[i, "bullet_point"] == TRUE){
        code_line_i = paste0('`r params$report_makelist(', code_line_i, ', ', report_data[i, "italic"], ', ', report_data[i, "bold"], ')`')
      }else{
        code_line_i = paste0('`r paste0("', bold_italic_db(i)[[1]],'", ', code_line_i,', "', bold_italic_db(i)[[2]],'"', ')`')
      }

    }

    ## Key/value
    if(report_data[i, "type"] == "Value/replacement"){
      var_i = paste0("params$paramlist$visit_table_",
                           report_data[i, "visit_id_for_query"],
                           "$",
                           report_data[i, "inputId_for_query"])

      if(report_data[i, "bullet_point"] == TRUE){
        code_line_i = paste0('`r params$report_makelist(', code_line_i, ', ', report_data[i, "italic"], ', ', report_data[i, "bold"], ')`')
      }else{
        code_line_i = paste0('`r ',
                             'if(sum(',
                             var_i,
                             ' == "',
                             report_data[i, "value_replaced"],
                             '") == 1 ){paste0("',
                             bold_italic_db(i)[[1]],
                             '", "',
                             report_data[i, "display_text"],
                             '", "',
                             bold_italic_db(i)[[2]],
                             '"',
                             ')}else{NULL}`')
      }
    }


    return(code_line_i)
  }

  # Create lines of code for R markdown file
  code_lines = sapply(1:nrow(report_data), make_markdown_line, report_data = report_data)


  # Corrections
  code_lines = base::sapply(code_lines, paste, collapse = " ")
  code_lines = base::paste(code_lines, collapse = " ")
  code_lines = base::unlist(strsplit(code_lines, split = "<lbr>", perl = TRUE))
  code_lines = base::trimws(x = code_lines, which = "left")


  # Add header
  code_lines = c(
    '---',
    'title: "LCARS-C report"',
    'output: html_document',
    'date: "`r format(Sys.time())`"',
    'params:',
    '  paramlist: paramlist',
    '  report_translate: report_translate',
    '  report_makelist: report_makelist',
    '  widgets_table_global: widgets_table_global',
    '---',
    '',
    '```{r setup, include=FALSE}',
    'knitr::opts_chunk$set(echo = TRUE)',
    '```',
    '',
    '<br>',
    '<br>',
    '<br>',
    code_lines
  )

  # Write report rmd into temp dir
  tempReport <- file.path(tempdir(), paste0(report_id, "_report.Rmd"))
  fileConn<-file(tempReport)

  writeLines(code_lines, fileConn)

  close(fileConn)

  return(tempReport)
  # r_code = "plot(cars)
  #           print(3)"
  #
  # knitr::spin(text = r_code)

}
