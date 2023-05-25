
#' @title Get confidence intervals for mean
#' @description Pass a numeric vector and get vector with confidence interval for the mean
#' @param x A numeric vector
#' @param level Confidence level, Default: 0.95
#' @return A vector with c(cil, ciu)
#' @details Uses lm and confint to get interval
#' @examples
#' ci_mean(1:10)
#' @rdname ci_mean
#' @export
ci_mean <- function(x, level = 0.95) {
  model <- stats::lm(x ~ 1)
  ci <- stats::confint(model, level = level)
  ci_vector <- c(ci)
  return(ci_vector)
}
