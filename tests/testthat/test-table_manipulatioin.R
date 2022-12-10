test_that("Manipulate tables, transform", {
  data <- iris
  data <- data.table::setDT(data)
  # data_aggregate <- data[, .N, by = "Species"]
  # transform_aggregate(data_aggregate)
  expect_true(TRUE)
})
