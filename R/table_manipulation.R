
#' transpose_aggregate Transpose the way I want
#'
#' @param .data Table to be fransformed
#' @export
#'
#' @return A transposed table
transform_aggregate <- function(.data) {
  row_names_dt <- data.table::data.table(names = colnames(.data))
  table_trans <- data.table::transpose(.data)
  table_trans <- cbind(row_names_dt, table_trans)
  colnames(table_trans) <- as.character(table_trans[1])
  table_trans <- table_trans[-1]
  return(table_trans)
}
