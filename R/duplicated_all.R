#' duplicated_all
#'
#' @param x A vector of elements or a data.table
#'
#' @export
#' @examples
#' duplicated_all(c(rep("a", 3), "b", "c"))
#' @return A logical vector indicating ALL duplicated elements in x or rows if data.table
duplicated_all <- function(x) {

  if (data.table::is.data.table(x)) {
    duplicated(x, fromLast = TRUE) | duplicated(x)
  }
  duplicated(x) | rev(duplicated(rev(x)))
}

#' duplicated values
#'
#' @param x A vector 
#'
#' @export
#'
#' @examples
#' duplicated_values(c(rep("a", 3), "b", "c"))
#'
#' @returns A vector of duplicated items or a data.table with duplicated rows
duplicated_values <- function(x) {
  duplicated_items <- duplicated_all(x)
  return(x[duplicated_items])
}
