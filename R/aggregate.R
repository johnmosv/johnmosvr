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
#' @param x A logical, character or numeric vector. If character, use trueth to indicate TRUE value #nolint
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
    if (!all(x %in% 0:1)) stop("none 0,1 values in numeric logical vector")
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

#' mean_sd
#'
#' @param x Numeric vector
#' @param remove_na Should NAs be removed?
#' @param digits The number of decimal places to include
#' @param ... Additional arguments passed to format
#'
#' @export
#'
#' @examples
#' mean_sd(1:10)
#'
#' @returns A string of form mean (sd)
mean_sd <- function(x, remove_na = TRUE, digits = 1, ...) {
  if (!is.numeric(x)) stop("x must be numeric")
  m <- format(round(mean(x, na.rm = remove_na), digits = digits), nsmall = digits)
  sdev <- format(round(stats::sd(x, na.rm = remove_na), digits = digits), nsmall = digits, ...)
  m_sd <- glue::glue("{m} ({sdev})")
  m <- sddev <- NULL
  return(m_sd)
}
#' median_iqr
#'
#' @param x Numeric vector
#' @param remove_na Should NAs be removed?
#' @param digits The number of decimal places to include
#' @param ... Additional arguments passed to format
#'
#' @export
#'
#' @examples
#' median_iqr(1:10)
#'
#' @returns A string of form median (iqr)
median_iqr <- function(x, remove_na = TRUE, digits = 1, ...) {
  if (!is.numeric(x)) stop("x must be numeric")
  m <- format(round(stats::median(x, na.rm = remove_na), digits = digits), nsmall = digits)
  quant_025_075 <- format(round(stats::quantile(x, na.rm = remove_na, probs = c(0.25, 0.75)), digits = digits), nsmall = digits, ...)
  m_iqr <- glue::glue("{m} ({quant_025_075[1]}-{quant_025_075[2]})")
  m <- quant_025_075 <- NULL
  return(m_iqr)
}

#' n_per
#'
#' @param x Numeric or logical vector of form 1/0 or TRUE/FALSE
#' @param remove_na Should NAs be removed?
#'
#' @export
#'
#' @examples
#' n_per(c(rep(1, 3), rep(0, 3)))
#'
#' @returns A string of form count (%)
n_per <- function(x, remove_na = FALSE) {
  # Remove na or set them to false
  if (is.numeric(x)) {
    # TODO make sure it is binary
    valid_values <- c(NA, 1, 0)
    unique_values <- unique(x)
    if (any(!unique_values %in% valid_values)) {
      stop(glue::glue("This is not a binary variable. Contains: {pcollapse(unique_values)}"))
    }
    x <- x == 1
  }

  if (!is.logical(x)) {
    stop("x must be logical or binary numeric variable (1/0)")
  }

  # Handle NA
  if (remove_na) {
    print(glue::glue("removing missing from variable {scales::percent(mean(is.na(x)))}"))
    x <- x[!is.na(x)]
  } else {
    # Set the missing to FALSE
    x[is.na(x)] <- FALSE
  }

  # Calculate prop and n
  count <- sum(x)
  prop <- mean(x)
  perc <- scales::percent(prop)

  res <- glue::glue("{count} ({perc})")
  count <- perc <- NULL

  return(res)
}
#' roundn
#'
#' @param x A float value to round and format
#' @param dig Number of digits to round to
#'
#' @export
#'
#' @examples
#' roundn(1.100100010, dig = 3)
#' @returns A rounded value of as string with the correct number of digits
roundn <- function(x, dig = 2) {
  format(round(x, digits = dig), nsmall = dig)
}
#' x_ci
#'
#' @param x A centric measure
#' @param cil Lower confidence value
#' @param ciu Upper confidence value
#' @param dig Number of digits to round to
#'
#' @examples
#' x_ci(1.0101, 0.500, 1.353, dig = 2)
#'
#' @returns A string for form x (cil-ciu)
#'
#' @export
x_ci <- function(x, cil, ciu, dig = 1) {
  paste0(roundn(x, dig), " (", roundn(cil, dig), "-", roundn(ciu, dig), ")")
}
