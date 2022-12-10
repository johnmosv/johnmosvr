#' find_variable in table
#'
#' @param df A dataframe to find the variable in
#' @param var_pattern The regex pattern to match variable
#' @param ignore_case Should case be ignored?
#'
#' @export
#' @examples find_variable_in_table(iris, "species")
#`
#'
#' @return A character vector with all matching column names
find_variable_in_table <- function(df, var_pattern, ignore_case = TRUE) {
    cn <- colnames(df)
    # ignore case
    if (ignore_case) {
        cn_lower <- stringr::str_to_lower(cn)
        var_pattern <- stringr::str_to_lower(var_pattern)
        return(cn[grepl(var_pattern, cn_lower)])
    }
    cn[grepl(var_pattern, cn)]
}
