test_that("can read files from onedrive", {
  # if it not not a file but a directory it will return a message
  expect_message(onedrive("projects"), info = "should return a message when")

  file <- onedrive("projects/adhd/R/dm_1/adhd_ulv.fst")
  expect_true(file.exists(path), info = "should return a file path")

  # does not exist
  expect_error(onedrive("projects/does_not_exist"), info = "should throw an error")
})
