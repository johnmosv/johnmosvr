#' @title Factor case when
#' @description Use case when to create a factor
#' @param ... Arguments passed to `dplyr::case_when()`
#' @return A factor with the same order of levels as the cases in case when
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[stringr]{str_remove}}
#'  \code{\link[dplyr]{case_when}}
#' @rdname fct_case_when
#' @export
#' @importFrom stringr str_remove_all
#' @importFrom dplyr case_when

fct_case_when <- function(...) {
  args <- as.list(match.call())
  # extract RHS of formula as str
  levels <- sapply(
    # first is just function call
    args[-1],
    function(f) {
      # defaflt value is no call
      if (!is.call(f)) {
        return(f[1])
      }
      # This is the third element of the call
      call_3e <- f[3]
      # call to str
      dep <- deparse(call_3e)
      dep <- stringr::str_remove_all(dep, "\"")
      lvl_str <- stringr::str_remove_all(dep, "\\)$")
      lvl_str <- stringr::str_remove_all(lvl_str, "\\($")
      return(lvl_str)
    }
  )
  factor(dplyr::case_when(...), levels = levels)
}
