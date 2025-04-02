test_that("install_packages works correctly", {
  # Test default packages installation
  got_instaled <- install_packages(quiet = TRUE)
  installed <- rownames(installed.packages())

  # Check if default packages are installed

  for (pkg in installed) {
    expect_true(pkg %in% installed,
      info = paste("Default package", pkg, "should be installed")
    )
  }

  # Test adding additional packages
  test_packages <- c("palmerpenguins")
  result <- install_packages(test_packages, quiet = TRUE)
  installed <- rownames(installed.packages())

  # Check if additional packages are installed
  expect_true("palmerpenguins" %in% installed, info = "Additional package should be installed")

  # Test handling of empty strings
  result <- install_packages(c("", "stats"), quiet = TRUE)
  expect_false("" %in% result,
    info = "Empty string should be removed from packages list"
  )
})
