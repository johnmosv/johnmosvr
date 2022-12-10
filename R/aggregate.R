#' perc
#'
#' @param x A proportion
#' @param accuracy Use 0.1 for 1 decimal, 1 for 0 decimals, 0.01 for 2 etc.
#' @param ... Additional arguments passed to percent
#'
#' @export
#' @examples
#' perc(0.1)
#' @returns A character of form "10%"
perc <- function(x, accuracy = 0.1, ...) {
  scales::percent(x, accuracy = accuracy, ...)
}

#' n_prop
#'
#' @param n Nominator
#' @param denom Denominator
#' @param accuracy Which accuracy should be used
#'
#' @export
#'
#' @examples
#' n_prop(1, 10)
#' @returns A character of form n (perc%)
n_prop <- function(n, denom, accuracy = 1) {
  np <- paste0(n, " (", round(n / denom, accuracy + 2) * 100, "%)")

  return(np)
}
#' is_na
#'
#' @param x A vector
#' @export
#' @examples
#' is_na(c(rep(NA, 2), rep("", 2), "x"))
#' @returns A logical vector indicating if elements are NA or ""
is_na <- function(x) {
  if (is.character(x)) {
    x[x == ""] <- NA
  }
  return(is.na(x))
}
#' truethy
#'
#' @param x A logical, character or numeric vector. If character, use trueth to indicate TRUE value
#' @param trueth Optional, a string indicating the TRUE value.
#'
#' @export
#'
#' @examples
#' truethy(c(TRUE, TRUE, FALSE, NA))
#' @returns A logical vector, treating NA ans "" and 0 as FALSE.
truethy <- function(x, trueth = "Ja") {
  if (is.logical(x)) {
    x[is.na(x)] <- FALSE
    return(x == TRUE)
  }
  if (is.character(x)) {
    stopifnot(is.character(trueth))
    x[is.na(x)] <- ""
    return(x == trueth)
  }
  if (is.numeric(x)) {
    x[is.na(x)] <- 0
    return(x == 1)
  }
}

#' year_month
#'
#' @param date A date
#'
#' @export
#'
#' @examples
#' year_month(as.Date("2020-01-01")) 
#'
#' @returns A string of form YYYY-MM
year_month <- function(date) {
  if (!lubridate::is.Date(date)) {
    date <- lubridate::ymd(date)
  } else {
    date <- lubridate::ymd(date)
  }
  yr <- lubridate::year(date)
  mnth <- as.character(lubridate::month(date))
  mnth[stringr::str_length(mnth) == 1] <- stringr::str_c(
    "0", mnth[stringr::str_length(mnth) == 1]
  )
  return(paste0(yr, "-", mnth))
}
