#' cat_numeric: Categorization of numeric vector with logical defaults and formatting
#'
#' @param x Numeric vector
#' @param cutoffs Numeric vector with cutoffs
#' @param start Lower value to include in first level
#' @param stop Upper value to include in last level
#'
#' @export
#'
#' @examples
#' cat_numeric(1:10, cutoffs = c(2.5, 50, 7.5, 10), stop = 11)
#'
#' @returns A factor with categorized values
cat_numeric <- function(x, cutoffs = c(6, 18, 41, 66), start = -1, stop = 1000) { #nolint
    # add 0
    cutoffs <- c(start, cutoffs)
    cutoffs_start <- cutoffs[c(-1, -length(cutoffs))]
    cutoffs_stop <- cutoffs[c(-1, -2)]
    middle_lbls <- paste0(cutoffs_start, " < ", cutoffs_stop)
    start_lbl <- paste0("<", cutoffs[2])
    stop_lbl <- paste0(">=", cutoffs[length(cutoffs)])
    lbls <- c(start_lbl, middle_lbls, stop_lbl)
    cut(x, breaks = c(cutoffs, stop), labels = lbls)
}
#' convert_idate_to_date to lubridate::ymd in a data.frame
#'
#' @param data A data.frame
#'
#' @export
#' 
#' @returns A data.frame with all iDates converted
convert_idate_to_date <- function(data) {
    # Convert iDate to lubridate::date
    is_idate <- function(x) {
        return("IDate" %in% class(x))
    }
    any_idate <- any(unlist(lapply(data, is_idate)), na.rm = TRUE)
    if (any_idate) {
        data <- dplyr::mutate(data, dplyr::across(tidyselect::where(is_idate), lubridate::ymd)) #nolint
    }
    return(data)
}

#' convert_sas_dates_to_date: Convert all sas dates in a data.frame to lubridate::ymd
#'
#' @param data A dataframe
#'
#' @export
#'
#' @returns A data.frame with all sas dates converted
convert_sas_dates_to_date <- function(data) {
    is_sas_date <- function(x) {
        if ("integer" %in% class(x)) {
            it_is <- stats::median(stringr::str_length(x[!is.na(x)]), na.rm = TRUE) %in% 4:5 #nolint
            return(it_is)
        }
        return(FALSE)
    }
    sas_dates <- unlist(lapply(data, is_sas_date))
    if (any(sas_dates, na.rm = TRUE)) {
        sas_date_cols <- colnames(data)[sas_dates]
        sas_date_cols <- sas_date_cols[grepl("date|dat$", sas_date_cols)]
        print(glue::glue("Converting sas dates: {paste0(sas_date_cols, collapse = ', ')}")) #nolint
        for (sas_date_col in sas_date_cols) {
            converted_date <- as.Date(data[[sas_date_col]], origin = "1960-01-01") #nolint
            data[[sas_date_col]] <- converted_date
        }
    }
    return(data)
}

#' simplify_colnames: Helper to remove non-ascii and weird characters
#'
#' @param data Data.frame which colnames to change
#' @export
#' @examples
#' simplify_colnames(iris)
#' @returns A data.frame with simplified colnames
simplify_colnames <- function(data) {
    cn <- colnames(data)
    cn <- stringr::str_to_lower(cn)
    cn <- stringr::str_replace_all(cn, "\\u00e4", "a") #ä
    cn <- stringr::str_replace_all(cn, "\\u00e5", "a") #å
    cn <- stringr::str_replace_all(cn, "\\u00f6", "o") #ö
    cn <- stringr::str_replace_all(cn, "\\u2013", "-") #-
    cn <- stringr::str_replace_all(cn, " ", "_")
    cn <- stringr::str_replace_all(cn, "[-]+", "_")

    colnames(data) <- cn

    return(data)
}
