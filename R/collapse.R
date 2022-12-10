
#' pcollapse
#'
#' @param x A vector of elements to be combined
#' @param collapse Seperator used for collapsing the elements
#'
#' @export
#' @examples
#' pcollapse(letters[1:5])
#' @return A quoted combination of x seperated by sep
pcollapse <- function(x, collapse = " | ") {
    res <- paste0(x, collapse = collapse)
    res[res == "NA"] <- NA
    return(res)
}
#' split_collapse
#'
#' @param x A character of collapsed items
#' @param sep Seperator used for seperations
#' @export
#' @examples
#' split_collapse("a | b")
#' @return A seperated character vector
split_collapse <- function(x, sep = " | ") {
  unlist(stringr::str_split(x, " \\| "))
}
