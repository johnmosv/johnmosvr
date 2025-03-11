test_that("can create tableone", {
  data <- mtcars

  res_df <- tableone(
    data = mtcars,
    strata = "cyl",
    vars = "disp"
  )

  expect_true(class(res_df) == "data.frame")

  res_t1 <- tableone(
    mtcars, "cyl", "disp",
    convert = FALSE
  )
  expect_true(class(res_t1) == "TableOne")
})
