#' Create a Table One for descriptive statistics
#'
#' This function creates a "Table 1" for descriptive statistics, commonly used in
#' medical research papers. It's a wrapper around \code{tableone::CreateTableOne}
#' with an option to convert the result to a data frame.
#'
#' @param data A data frame containing the variables to be summarized
#' @param strata The name of the stratification variable as a character string
#' @param vars Character vector of variable names to be summarized
#' @param factorvars Character vector of variable names to be considered as categorical
#' @param overall Logical value indicating whether to include an overall summary column (default: TRUE)
#' @param test Logical value indicating whether to include p-values (default: FALSE)
#' @param convert Logical value indicating whether to convert the result to a data frame (default: TRUE)
#' @param ... Additional arguments passed to \code{tableone::CreateTableOne}
#'
#' @return If \code{convert = TRUE}, returns a data frame with formatted summary statistics.
#'         If \code{convert = FALSE}, returns an object of class "TableOne".
#'
#' @examples
#' # Create a Table One with stratification by cylinder count
#' tableone(mtcars, "cyl", "disp")
#'
#' # Return the raw TableOne object without conversion
#' tableone(mtcars, "cyl", "disp", convert = FALSE)
#'
#' @importFrom tableone CreateTableOne
#' @export
tableone <- function(data, strata, vars, factorvars, overall = TRUE, test = FALSE, convert = TRUE, ...) {
  tab <- tableone::CreateTableOne(
    data = data,
    strata = strata,
    vars = vars,
    factorVars = factorvars,
    addOverall = overall,
    test = test,
    ...
  )

  if (convert) tab <- convert_tableone(tab)

  return(tab)
}
