test_that("transpose_aggreated_dt", {
  table_aggregate <- data.table::data.table(species = letters[1:3], N = 1:3)
  n_columns <- ncol(transpose_aggregated_dt(table_aggregate))
  # When transposed the number of rows = ncol
  expect_equal(n_columns, nrow(table_aggregate))

})
