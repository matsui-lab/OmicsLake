test_that("ol_init creates project structure", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  result <- ol_init("test_project")
  
  expect_true(dir.exists(file.path(tmpdir, "test_project")))
  expect_equal(getOption("ol.project"), "test_project")
})

test_that("ol_init errors on empty project name", {
  expect_error(ol_init(NULL), "project must be a non-empty string")
  expect_error(ol_init(""), "project must be a non-empty string")
})

test_that("ol_init respects custom root", {
  tmpdir <- withr::local_tempdir()
  custom_root <- file.path(tmpdir, "custom")
  
  result <- ol_init("test_project", root = custom_root)
  expect_true(dir.exists(file.path(custom_root, "test_project")))
  expect_equal(getOption("ol.root"), normalizePath(custom_root, winslash="/", mustWork=FALSE))
})

test_that("ol_label creates tag", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_project")
  
  expect_silent(ol_label("v1.0"))
})

test_that("ol_label errors without initialized project", {
  old_proj <- getOption("ol.project")
  on.exit(options(ol.project = old_proj), add = TRUE)
  
  options(ol.project = NULL)
  expect_error(ol_label("tag"), "Call ol_init")
})
