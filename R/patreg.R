
#' find_icd_codes_from_patreg
#'
#' @param patreg Patreg data.frame from SoS
#' @param icds_regex A regex with icd codes to match on
#'
#' @export
#'
#' @return Data frame from patreg matching any of the kva codes
find_icd_codes_from_patreg <- function(patreg, icds_regex) {
  icds <- NULL
  dia_cols <- dplyr::select(patreg, dplyr::contains("dia"))
  matched_list <- apply(dia_cols, 1, function(row) { # 1 = rowwise operation
    log_row <- grepl(icds_regex, row)
    matched_icds <- paste(unique(row[log_row]), collapse = ", ")
    return(data.frame(icds = matched_icds))
  })
  matched_vector <- unname(unlist(matched_list))
  n_match <- sum(matched_vector != "")
  perc_match <- scales::percent(mean(matched_vector != ""))
  print(glue::glue("Found match on icd [{icds_regex}] :  {n_match} ({perc_match}) rows")) #nolint
  patreg$icds <- matched_vector
  if (data.table::is.data.table(patreg)) {
    patreg_match <- patreg[icds != ""]
  }
  if (is.data.frame(patreg)) {
    patreg_match <- patreg[icds != "", ]
  }
  return(patreg_match)
}

#' find_kva_codes_from_patreg
#'
#' @param patreg Patreg data.frame from SoS
#' @param kva_regex A regex with kva codes to match on
#'
#' @export
#'
#' @return Data frame from patreg matching any of the kva codes
find_kva_codes_from_patreg <- function(patreg, kva_regex) {
  kvas <- NULL
  opd_cols <- dplyr::select(patreg, dplyr::contains("surgerycodes"))
  # surgerycodes is stored in one column, codes are seperated by " "
  opd_cols <- stringr::str_split(opd_cols$surgerycodes,
    pattern = " ", simplify = TRUE
  )

  matched_list <- apply(opd_cols, 1, function(row) { # 1 = rowwise operation
    log_row <- grepl(kva_regex, row)
    matched <- paste(unique(row[log_row]), collapse = ", ")
    return(data.frame(kva = matched))
  })
  matched_vector <- unname(unlist(matched_list))
  n_match <- sum(matched_vector != "")
  perc_match <- scales::percent(mean(matched_vector != ""))
  print(glue::glue("Found match on kva [{kva_regex}] : {n_match} ({perc_match}) rows")) #nolint
  patreg$kvas <- matched_vector
  if (data.table::is.data.table(patreg)) {
    patreg_match <- patreg[kvas != ""]
  }
  if (is.data.frame(patreg)) {
    patreg_match <- patreg[kvas != "", ]
  }
  return(patreg_match)
}
