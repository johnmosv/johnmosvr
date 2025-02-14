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

test_that("can generate an html table", {
  # data.frame
  x <- mtcars
  html_table <- table_dt(x, force_html = TRUE, caption = "mtcars")
  html_table
  expect_equal(class(html_table), c("datatables", "htmlwidget"))

  # from grouped_df
  x |>
    dplyr::count(cyl) |>
    table_dt(force_html = TRUE)

  x |>
    janitor::tabyl(cyl) |>
    table_dt(force_html = TRUE)
})


test_that("can prepare dt data", {
  skip("just testing convertion for DT")
  prepare_dt_data <- function(data) {
    data <- dplyr::ungroup(data)
    if ("TableOne" %in% class(data)) {
      data <- convert_tableone(data)
    }
    col_names <- colnames(data)
    return(data)
  }

  data <- mtcars

  dp <- dplyr::count(mtcars, cyl)
  taby <- janitor::tabyl(mtcars, cyl)

  table_dt(dp, force_html = TRUE, caption = "dp", title_row_names = FALSE, title_col_names = FALSE)
  table_dt(taby, force_html = TRUE, caption = "dp", title_row_names = FALSE, title_col_names = FALSE)

  prepare_dt_data(mtcars)
  prepare_dt_data(dp)
  prepare_dt_data(taby)
})
