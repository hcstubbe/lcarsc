#' report_icd10_add_descr
#'
#' @description Finds and adds ICD10/ATC descriptions
#'
#' @return A character vector of ICD10/ATC descriptions
#'
#' @noRd
report_icd10_add_descr = function(codes, reference){

  codes = base::toupper(codes)

  decr_i = function(code_i, reference) {
    idx = grep(pattern = code_i, x = reference$icd10, ignore.case = TRUE)
    res = reference$description[idx][1]
    if(is.na(res)){
      res = code_i
    }else{
      res = paste0(res, " (", code_i, ")")
    }
    return(res)
  }

  descriptions_icd10 = sapply(codes, decr_i, reference = reference)

  return(descriptions_icd10)
}


report_atc_add_descr = function(codes, reference){

  codes = base::toupper(codes)

  decr_i = function(code_i, reference) {
    idx = grep(pattern = code_i, x = reference$atc, ignore.case = TRUE)
    res = reference$description[idx][1]
    if(is.na(res)){
      res = code_i
    }else{
      res = paste0(res, " (", code_i, ")")
    }
    return(res)
  }

  descriptions_atc = sapply(codes, decr_i, reference = reference)

  return(descriptions_atc)
}

