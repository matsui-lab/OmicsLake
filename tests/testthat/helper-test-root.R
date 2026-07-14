.omicslake_test_root <- file.path(
    tempdir(),
    paste0("omicslake-tests-", Sys.getpid())
)
dir.create(.omicslake_test_root, recursive = TRUE, showWarnings = FALSE)
options(ol.root = .omicslake_test_root)
