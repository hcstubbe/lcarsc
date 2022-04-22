#' json_parser
#'
#' @description A function for importing fhir widgets
#'
#' @return The return value, if any, from executing the function.
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



  dat_x = lapply(fhir_data, function(y) {

    dat_i = data.frame(
      inputId = sapply(y$concept,
                       function(x) {x$shortName}),
      data_class = sapply(y$concept,
                          function(x) {x$valueDomain[[1]]$type})
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

  dat_x = dplyr::bind_rows(dat_x, .id = "panel")


  fhir_data$anamnese__risikofaktoren$concept[[1]]$valueDomain[[1]]$type
  sapply(fhir_data$anamnese__risikofaktoren$concept[[1]]$valueDomain[[1]]$conceptList[[1]]$concept,
         function(x) {x$name[[1]] $`#text`})

  inputIds = sapply(fhir_data, function(x) {c(x$shortName, x$valueDomain[[1]]$type)})

  fhir_widgets = sapply(1:length(inputIds), function(x){
    y = data.frame(inputId = inputIds[[x]])
    y$panel = names(inputIds)[x]
    y
  }, simplify = F
  )

  fhir_widgets
}
