# Lightweight test runner used during migration (before the package is
# formally installed). Sources the R/ modules and runs the testthat files
# against the synthetic fixture. Once renv + the package build are in place,
# this is replaced by devtools::test().

suppressMessages({
  library(plm)
  library(dplyr)
  library(testthat)
})

env <- new.env()
for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) {
  sys.source(f, envir = env)
}
sys.source("tests/testthat/helper-fixture.R", envir = env)

test_files <- list.files("tests/testthat", pattern = "^test-.*[.]R$",
                         full.names = TRUE)

reporter <- testthat::SummaryReporter$new()
for (tf in test_files) {
  testthat::test_file(tf, env = env, reporter = reporter)
}
