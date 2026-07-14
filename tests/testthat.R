library(testthat)
library(OmicsLake)

old_root <- getOption("ol.root")
test_root <- file.path(tempdir(), paste0("omicslake-tests-", Sys.getpid()))
dir.create(test_root, recursive = TRUE, showWarnings = FALSE)
options(ol.root = test_root)

tryCatch(
  test_check("OmicsLake"),
  finally = {
    options(ol.root = old_root)
    unlink(test_root, recursive = TRUE, force = TRUE)
  }
)
