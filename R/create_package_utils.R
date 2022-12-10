#' create_r_and_test
#'
#' @param file_name Name of the file to crate
#' @param should_open Logical, should files be opened?
#'
#' @export
#'
#' @return None
create_r_and_test <- function(file_name, should_open = FALSE) {
  usethis::use_r(name = file_name, open = should_open)
  usethis::use_test(name = file_name, open = should_open)
}
