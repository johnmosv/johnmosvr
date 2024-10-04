#' Convert TableOne Object to Data Frame
#'
#' This function converts a `TableOne` object into a data frame.
#'
#' @param tabone A `TableOne` object to be converted.
#' @param ... Additional arguments passed to the `print` function.
#'
#' @return A data frame representation of the `TableOne` object.
#' @export
#'
#' @examples
#' \dontrun{
#' tabone <- CreateTableOne(vars = c("age", "sex"), data = mydata)
#' df <- convert_tableone(tabone)
#' }
convert_tableone <- function(tabone, ...) {
  mtx <- print(tabone, printToggle = FALSE, ...)
  df <- as.data.frame.matrix(mtx)
  df_rn <- data.frame(var = rownames(df))
  df <- bind_cols(df_rn, df)
  rownames(df) <- NULL
  return(df)
}
