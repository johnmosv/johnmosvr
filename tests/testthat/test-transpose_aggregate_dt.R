test_that("transpose_aggreate_dt", {
  x <- data.table::as.data.table(iris)
  table_aggregate <- x[, .N, by = "Species"]
  n_columns <- ncol(transpose_aggregated_dt(table_aggregate))
  # the uniqe values of species + N
  expect_equal(n_columns, length(unique(iris$Species)) + 1)

})
