#' json_parser
#'
#' @description A function for importing fhir widgets
#'
#' @return The return value, if any, from executing the function.
#'
#' @importFrom dplyr filter
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
                       function(x) {x1 = sapply(x$valueDomain[[1]]$conceptList[[1]]$concept,function(x) {x$name[[1]] $`#text`})
                       x1 = unlist(x1)

                       if(length(x1) > 12){ # currently only 12 choices are supported!
                         x1 = x1[1:12]
                       }

                       x2 = c(x1, rep(NA, 12-length(x1)), x1, rep(NA, 12-length(x1)))
                       names(x2) = c(paste0("choice", 1:12), paste0("choice", 1:12, "translation"))
                       x2
                       },simplify = T))
    choices = data.frame(choices)

    dat_i = cbind(dat_i, choices)

    return(dat_i)
  })

  fhir_widgets = dplyr::bind_rows(fhir_widgets, .id = "panel")

  fhir_widgets = dplyr::filter(fhir_widgets, status_code == "final")



  fhir_widgets
}
