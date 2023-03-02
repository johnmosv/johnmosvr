
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

#' @title Transpose as wide dt
#' @description Format it like a proper long table
#' @param x A table with variables in columns and groups as rows
#' @return A table with variables as rows and groups as columns
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[dplyr]{pull}}, \code{\link[dplyr]{bind_cols}}
#'  \code{\link[tibble]{tibble}}
#' @rdname trans_dt
#' @export
#' @importFrom dplyr pull bind_cols
#' @importFrom tibble tibble

trans_dt <- function(x) {
  first_col <- dplyr::pull(x[, 1])
  if (is.logical(first_col) || is.numeric(first_col)) {
    first_char <- as.character(first_col)
    x[, 1] <- paste0(colnames(x)[1], "=", first_char)
  }
  tx <- t(x)
  if (ncol(tx) == 1) {
    tab <- tibble::tibble(var = rownames(tx), value = tx[, 1]) # nolint
  }
  if (ncol(tx) >= 2) {
    first_table <- tibble::tibble(var = rownames(tx))
    second_table <- as.data.frame(tx)
    tab <- dplyr::bind_cols(first_table, second_table)
    clnms <- as.character(unlist(tab[1, ]))
    colnames(tab) <- clnms
    tab <- tab[-1, ]
  }
  return(tab)
}
