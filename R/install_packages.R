#' Install Common R Packages
#'
#' This function checks if specified packages are installed and installs
#' missing packages from CRAN. It comes with a default set of commonly used packages
#' and any additional packages specified will be added to these defaults.
#'
#' @param packages Character vector of additional package names to check/install
#' @param quiet Logical indicating whether to suppress messages (default: FALSE)
#'
#' @return Invisible character vector of newly installed packages
#'
#' @examples
#' # Install default packages
#' install_packages()
#'
#' # Install default packages plus additional ones
#' install_packages(c("stringr", "purrr"))
#'
#' @export
install_packages <- function(packages = NULL, quiet = FALSE) {
  # First, ensure crayon is installed for colored output
  if (!requireNamespace("crayon", quietly = TRUE)) {
    install.packages("crayon", quiet = FALSE)
  }

  # Define default packages
  default_packages <- c(
    # Core
    "devtools",
    "Rcpp",
    "RcppArmadillo",

    # Tidyverse stuff
    "dplyr",
    "stringr",
    "purrr",
    "ggplot2",
    "tidyr",
    "lubridate",
    "data.table",
    "purrr",

    # tableone dependencies
    "survey",
    "tableone",

    # Stats stuff
    "emmeans", # requires openblas for linear algebra
    "performance", # test models
    "report", # report stats in line text
    "lme4", # mixed models
    "broom",
    "broom.mixed",

    # Language server requirements
    "callr",
    "collections",
    "fs",
    "jsonlite",
    "lintr",
    "parallel",
    "R6",
    "roxygen2",
    "stringi",
    "styler",
    "tools",
    "utils",
    "xml2", # removed extra space
    "xmlparsedata",
    "covr",
    "magrittr",
    "mockery",
    "pacman",
    "processx",
    "testthat",
    "withr",
    "rmarkdown",
    "languageserver"
  )



  # Combine default and additional packages
  if (!is.null(packages)) {
    all_packages <- unique(c(default_packages, packages))
  } else {
    all_packages <- default_packages
  }

  # Remove any empty strings
  all_packages <- all_packages[all_packages != ""]

  # Get installed packages
  installed_packages <- rownames(installed.packages())

  # Find which packages need to be installed
  packages_to_install <- all_packages[!all_packages %in% installed_packages]

  # Check each package and print status
  if (!quiet) {
    for (pkg in all_packages) {
      if (pkg %in% installed_packages) {
        message(crayon::green(paste0("✓ ", pkg, " is already installed")))
      } else {
        message(crayon::yellow(paste0("→ Installing ", pkg, "...")))
      }
    }
  }

  # Install missing packages
  if (length(packages_to_install) > 0) {
    for (package in packages_to_install) {
      message(crayon::yellow(paste0("→ Installing ", package, "...")))
      utils::install.packages(package, quiet = quiet)
      success <- package %in% rownames(installed.packages())
      if (success) {
        message(crayon::green(paste0("✓ ", package, " has been successfully installed")))
      }
    }

    if (!quiet) {
      message(crayon::green("\nAll packages have been successfully installed!"))
    }
  } else {
    if (!quiet) {
      message(crayon::green("\nAll required packages are already installed!"))
    }
  }

  # Return invisibly the list of newly installed packages
  invisible(packages_to_install)
}
