test_that("Can collapse string correctly", {
  combined_letters <- pcollapse(letters[1:5], collapse = "")
  expect_equal(combined_letters, "abcde")
})

test_that("Can collapse string correctly", {
  input_letters <- letters[1:5]
  combined_letters <- pcollapse(input_letters)
  uncombined_letters <- split_collapse(combined_letters)
  expect_equal(input_letters, uncombined_letters)
})
