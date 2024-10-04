test_that("table_dt can handle stuff", {
  x <- mtcars
  table_dt(x, force_html = TRUE)
  # tableone is such a pain
  t1 <- tableone::CreateTableOne(vars = "mpg", data = x)
  table_dt(t1)
})

test_that("can convert tableone", {
  t1 <- tableone::CreateTableOne(vars = "mpg", data = mtcars)
  converted <- convert_tableone(t1)
  expect_equal(rownames(converted), paste(1:2))
})
