test_that("test that duplicated all works", {
  n_duplicated <- sum(duplicated_all(c(rep("a", 3), "x", "y")))
  expect_equal(n_duplicated, 3)
})

test_that("duplicated_values", {
  dup_values <- duplicated_values(c(rep("a", 3), "x", "y"))
  expect_equal(rep("a", 3), dup_values)
})
