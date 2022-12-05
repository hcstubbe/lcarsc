#' report_translate
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
# Translate choices if translation is available
report_translate = function(input_data, inputId, widgets_table_global){

  translate_i = function(x, inputId){

    choices = c("choice1",
                "choice2",
                "choice3",
                "choice4",
                "choice5",
                "choice6",
                "choice7",
                "choice8",
                "choice9",
                "choice10",
                "choice11",
                "choice12")

    choices_translations = c("choice1_translation",
                             "choice2_translation",
                             "choice3_translation",
                             "choice4_translation",
                             "choice5_translation",
                             "choice6_translation",
                             "choice7_translation",
                             "choice8_translation",
                             "choice9_translation",
                             "choice10_translation",
                             "choice11_translation",
                             "choice12_translation")

    widget_choices = as.character(widgets_table_global[widgets_table_global$inputId == inputId, choices])
    widget_translations = as.character(widgets_table_global[widgets_table_global$inputId == inputId, choices_translations])

    x_transl = widget_translations[widget_choices == as.character(x)]
    x_transl = x_transl[!is.na(x_transl) & x_transl != "" & !duplicated(x_transl)]

    if(sum(!is.na(x_transl) & x_transl != "") == 1){
      x = x_transl
    }

    return(x)

  }

  output = sapply(input_data, translate_i, inputId = inputId)
  return(output)

}
