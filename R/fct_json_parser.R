#' json_parser
#'
#' @description A function for importing fhir widgets
#'
#' @return The return value, if any, from executing the function.
#'
#' @importFrom dplyr filter add_row %>%
#' @importFrom jsonlite fromJSON
#'
#' @noRd
#'
#'
json_parser = function(json_file){
  fhir_json_data = jsonlite::fromJSON(json_file, simplifyVector = F)
  fhir_data = fhir_json_data$dataset[[1]]$concept[[1]]$concept
  names(fhir_data) = sapply(fhir_json_data$dataset[[1]]$concept[[1]]$concept,
                             function(x) x$shortName[[1]],
                             simplify = T)



  fhir_widgets = lapply(fhir_data, function(y) {

    dat_i = data.frame(
      inputId = sapply(y$concept,
                       function(x) {x$shortName}),
      data_class = sapply(y$concept,
                          function(x) {x$valueDomain[[1]]$type}),

      label = sapply(y$concept,
                     function(x) {
                       label = x$name[[1]]$`#text`
                       if(is.null(label)){
                         label = x$shortName
                       }else if(is.na(label)){
                         label = x$shortName
                       }
                       return(label)
                     }
      ),

      label_translation = sapply(y$concept,
                     function(x) {
                       label = x$name[[1]]$`#text`
                       if(is.null(label)){
                         label = x$shortName
                       }else if(is.na(label)){
                         label = x$shortName
                       }
                       return(label)
                     }
      ),

      description = sapply(y$concept,
                          function(x) {
                            label = x$desc[[1]]$`#text`
                            if(is.null(label)){
                              label = x$shortName
                            }else if(is.na(label)){
                              label = x$shortName
                            }
                            return(label)
                            }
                     ),

      unit = sapply(y$concept,
                    function(x) {
                      unit = x$valueDomain[[1]]$property[[1]][[1]]
                      if(is.null(unit)){
                        unit = NA
                      }else if(is.na(unit)){
                        unit = NA
                      }
                      return(unit)
                    }
      ),

      status_code = sapply(y$concept,
                         function(x) {
                           status_code = x$statusCode[1]
                           if(is.null(status_code)){
                             status_code = NA
                           }else if(is.na(status_code)){
                             status_code = NA
                           }
                           return(status_code)
                         }
      )


    )

    choices = t(sapply(y$concept,
                       function(x) {
                         x1 = sapply(x$valueDomain[[1]]$conceptList[[1]]$concept,function(x) {x$name[[1]] $`#text`})
                         x1 = unlist(x1)

                         if(length(x1) > 12){ # currently only 12 choices are supported!
                           x1 = x1[1:12]
                         }

                         x2 = c(x1, rep(NA, 12-length(x1)), x1, rep(NA, 12-length(x1)))
                         names(x2) = c(paste0("choice", 1:12), paste0("choice", 1:12, "translation"))
                         x2
                        },simplify = T))

    choices = data.frame(choices)

    if(all(is.na(choices))){
      choices = t(sapply(y$concept,
                          function(x) {
                            x1 = sapply(x$valueSet[[1]]$conceptList[[1]]$concept,function(x) {x$name[[1]]$`#text`})
                            x1 = unlist(x1)

                            if(length(x1) > 12){ # currently only 12 choices are supported!
                              x1 = x1[1:12]
                            }

                            x2 = c(x1, rep(NA, 12-length(x1)), x1, rep(NA, 12-length(x1)))
                            names(x2) = c(paste0("choice", 1:12), paste0("choice", 1:12, "translation"))
                            x2
                          },simplify = T))

      choices = data.frame(choices)
    }

    dat_i = cbind(choices, dat_i)

    return(dat_i)
  })

  fhir_widgets = dplyr::bind_rows(fhir_widgets, .id = "panel")

  fhir_widgets = dplyr::filter(fhir_widgets, status_code == "final")
  fhir_widgets$type = "textInput"
  if(any(fhir_widgets$data_class == "quantity")){
    fhir_widgets$type[fhir_widgets$data_class == "quantity"] = "numericInput"
  }
  if(any(fhir_widgets$data_class == "code")){
    fhir_widgets$type[fhir_widgets$data_class == "code"] = "checkboxInput"
  }
  if(any(fhir_widgets$data_class == "date")){
    fhir_widgets$type[fhir_widgets$data_class == "date"] = "dateInput"
  }
  fhir_widgets$data_type = "TEXT"
  fhir_widgets$r_class = "TEXT"
  fhir_widgets$origin_of_var = json_file
  fhir_widgets$order_of_var = NA
  fhir_widgets$include_translation = FALSE
  fhir_widgets$mandatory = FALSE
  fhir_widgets$panel_new = NA
  fhir_widgets$subgroup = "None"
  fhir_widgets$selected = NA
  fhir_widgets$conditional = FALSE
  fhir_widgets$appear_if = NA



      # fhir_widget_new = fhir_widgets[1,]
      # choice_cols = 2:13
      # choices = fhir_widgets[i,choice_cols]
      # if(any(!is.na(choices))){
      #   for ( j in choices[!is.na(choices)] ) {
      #     new_inputId = paste0(fhir_widgets[i, "inputId"], "_", j)
      #     new_label = paste0(fhir_widgets[i, "label"], ": ", j)
      #     new_translation = paste0(fhir_widgets[i, "label_translation"], ": ", j)
      #     fhir_widgets_j = fhir_widgets[i,]
      #     fhir_widgets_j$inputId = new_inputId
      #     fhir_widgets_j$label = new_label
      #     fhir_widget_new = rbind(fhir_widget_new, fhir_widgets_j)
      #   }
      #   fhir_widget_new = fhir_widget_new[-1,]
      #   fhir_widget_new$inputId = make.names(fhir_widget_new$inputId, unique = TRUE)
      # }
      #



  fhir_widgets_codes_booleans = fhir_widgets[1,]
  fhir_widget_list = list()
  fhir_widget_list_positions = c()
  for (i in 1:nrow(fhir_widgets) ) {

    if( fhir_widgets$data_class[i] == "code" ) {

      fhir_widget_new = fhir_widgets[1,]
      choice_cols = 2:13
      choices = fhir_widgets[i,choice_cols]
      choices_translation = fhir_widgets[i, choice_cols + 12 ]

      if(any(!is.na(choices))){

        for ( j in 1:ncol(choices) ) {
          if(!is.na(choices[,j])){
            new_inputId = paste0(fhir_widgets[i, "inputId"], "_", choices[,j])
            new_label = paste0(fhir_widgets[i, "label"], ": ", choices[,j])
            new_label_translation = paste0(fhir_widgets[i, "label_translation"], ": ", choices_translation[,j])
            fhir_widgets_j = fhir_widgets[i,]
            fhir_widgets_j$inputId = new_inputId
            fhir_widgets_j$label = new_label
            fhir_widgets_j$label_translation = new_label_translation
            fhir_widget_new = rbind(fhir_widget_new, fhir_widgets_j)
          }
        }
        fhir_widget_new = fhir_widget_new[-1,]
        fhir_widget_new$conditional = TRUE
        fhir_widget_new$appear_if = paste0("input.", "xxx_dummyvisit_xxx_", fhir_widgets[i, "inputId"])
        fhir_widget_new$inputId = make.names(fhir_widget_new$inputId, unique = TRUE)

        fhir_widget_list = c(fhir_widget_list, list(fhir_widget_new))

        fhir_widget_list_positions = c(fhir_widget_list_positions, i)

      }

      names(fhir_widget_list) = fhir_widget_list_positions

    }
  }

  if(length(fhir_widget_list) > 0){
    for (i in rev(names(fhir_widget_list))) {
      fhir_widgets = fhir_widgets %>% dplyr::add_row(fhir_widget_list[[i]], .before = as.numeric(i)+1)
    }
  }

  fhir_widgets$order_of_var = 1:nrow(fhir_widgets)
  fhir_widgets = fhir_widgets %>% select(!status_code & !data_class)
  fhir_widgets
}
