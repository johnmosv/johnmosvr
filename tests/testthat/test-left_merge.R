test_that("left_merge", {
  # will add rows, not allowed in left merge
  expect_error(left_merge(iris, iris, id_vars = "Species"))
  y <- iris[!duplicated(iris$Species), ]
  x <- y
  # No columns shoul be added because already present in x
  expect_warning(left_merge(x, y, id_vars = "Species"))
})
