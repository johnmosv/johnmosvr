test_that("can create factor case when", {
  x <- 1:10
  f <- fct_case_when(
    x == 1 ~ "1",
    x %in% 2:9 ~ "2-9",
    x == 10 ~ "10",
    .default = NA
  )

  expect_true(is.factor(f))
  expect_equal(length(levels(f)), 3)
})
