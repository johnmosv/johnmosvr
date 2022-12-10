#' find_pattern_in_columns
#'
#' @param df A data.frame object
#' @param pattern Pattern to find in columns
#'
#' @export
#'
#' @examples find_pattern_in_columns(iris[1:10, ], "setosa")
#'
#' @return A logical vector indicating if pattern was found in row
find_pattern_in_columns <- function(df, pattern) {
    apply(df, 1, function(row_columns) {
        if (!is.character(row_columns)) row_columns <- as.character(row_columns)
        row_columns[is.na(row_columns)] <- ""
        any(stringr::str_detect(row_columns, pattern))
    })
}
