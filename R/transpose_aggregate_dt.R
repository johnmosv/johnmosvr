
#' transpose_aggregate Transpose the way I want
#'
#' @param table1 Table to be fransformed
#' @export
#'
#' @return A transposed table
transpose_aggregated_dt <- function(table1) {
  # Good for transposing aggregated dt to table_dt
  row_names_dt <- data.table::data.table(names = colnames(table1))
  table_trans <- data.table::transpose(table1)
  table_trans <- cbind(row_names_dt, table_trans)
  colnames(table_trans) <- as.character(table_trans[1, ])
  table_trans <- table_trans[-1]
  return(table_trans)
}
