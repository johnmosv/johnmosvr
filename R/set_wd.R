#' set_wd: setwd to here::here()
#'
#' @returns None
#'
#' @export
set_wd <- function() {
  message(paste0("setting working directory to: ", here::here()))
  setwd(here::here())
}
