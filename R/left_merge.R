#' left_merge
#'
#' @param x A data.frame lhs
#' @param y A data.frame rhs
#' @param by A character vector to join on
#' @param id_vars A character vector to join on. "lopnr" by default
#'
#' @export
#'
#' @return A left joined table where duplicated columns are removed.
left_merge <- function(x, y, by = NULL, id_vars = "lopnr") {
    nrow_x <- nrow(x)
    x <- tidyr::unite(x, "id_vars_combined", dplyr::all_of(id_vars), remove = FALSE) #nolint
    y <- tidyr::unite(y, "id_vars_combined", dplyr::all_of(id_vars), remove = FALSE) #nolint
    if (!is.null(by)) id_vars <- by
    if (any(duplicated(x$id_vars_combined))) warning("Duplicated in x")
    if (any(duplicated(y$id_vars_combined))) warning("Duplicated in y")
    x$id_vars_combined <- NULL
    y$id_vars_combined <- NULL

    if (any(colnames(y) %in% colnames(x))) {
        cols_already_in_x <- colnames(y)[colnames(y) %in% colnames(x)]
        # Remove id_vars
        cols_already_in_x <- cols_already_in_x[!cols_already_in_x %in% id_vars]
        cols_already_in_x_concat <- paste0(cols_already_in_x, collapse = ", ")
        if (length(cols_already_in_x) > 0) {
            warning(paste("Columns", cols_already_in_x_concat, "already in x. Removing from y")) #nolint
            y <- dplyr::select(y, -dplyr::all_of(cols_already_in_x))
        }

    }
    # merge.data.table does not give reliable results. switching to left_join
    # d <- merge.data.table(x, y, by = id_vars, all.x = TRUE, all.y = FALSE) #nolint
    d <- dplyr::left_join(x, y, by = id_vars)

    if (nrow(d) > nrow_x) {
        stop("Added rows in left merge. Please use merge instead if you want to add rows") #nolint
    }
    return(d)
}
