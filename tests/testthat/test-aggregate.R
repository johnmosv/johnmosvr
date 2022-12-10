test_that("is_na can handle ''", {
  n_missing <- sum(is_na(c("", NA, "1")))
  expect_equal(n_missing, 2)
})

test_that("perc returns correct format", {
  perc_format <- perc(0.1, accuracy = 0.1)
  expect_equal(perc_format, "10.0%")
  perc_format <- perc(0.1, accuracy = 1)
  expect_equal(perc_format, "10%")
})

test_that("n_prop can return correct format", {
  expect_equal(n_prop(1, 10), "1 (10%)")
})

test_that("truethy can handle all input", {
  input_char <- c("Ja", "x", NA, "")
  expect_equal(truethy(input_char), c(TRUE, FALSE, FALSE, FALSE))
  expect_equal(truethy(c(TRUE, FALSE, NA)), c(TRUE, FALSE, FALSE))
  expect_equal(truethy(c(1, 0, NA)), c(TRUE, FALSE, FALSE))
})

test_that("year_month can handle dates", {
  expect_equal(year_month(as.Date("2020-01-01")), "2020-01")
  expect_equal(year_month("20200101"), "2020-01")
})
