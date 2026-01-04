test_that("ol_write stores table data", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_project")
  
  test_data <- data.frame(
    feature = c("gene1", "gene2", "gene3"),
    value = c(10, 20, 30)
  )
  
  expect_silent(ol_write("test_table", test_data))
})

test_that("ol_write supports different modes", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_project")
  
  test_data <- data.frame(x = 1:3)
  
  expect_silent(ol_write("test", test_data, mode = "create"))
  expect_silent(ol_write("test", test_data, mode = "overwrite"))
  expect_silent(ol_write("test", test_data, mode = "append"))
})

test_that("ol_write errors without initialized project", {
  old_proj <- getOption("ol.project")
  on.exit(options(ol.project = old_proj), add = TRUE)
  
  options(ol.project = NULL)
  expect_error(ol_write("test", data.frame(x=1)), "Call ol_init")
})

test_that("ol_save stores R objects", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_project")
  
  test_obj <- list(a = 1:5, b = "test")
  expect_silent(ol_save("test_object", test_obj))
})

test_that("ol_commit returns timestamp", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_project")
  
  result <- ol_commit("test commit", params = list(method = "test"))
  expect_match(result, "^\\d{8}-\\d{6}$")
})

test_that("ol_read retrieves table data", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_project")
  
  test_data <- data.frame(
    feature = c("gene1", "gene2"),
    value = c(10, 20)
  )
  
  ol_write("test_table", test_data)
  
  result <- ol_read("test_table")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
})

test_that("ol_read retrieves objects via ol_read_object fallback", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_project")
  
  test_obj <- list(a = 1:5, b = "test")
  ol_save("test_object", test_obj)
  
  result <- ol_read("test_object")
  expect_type(result, "list")
  expect_equal(result$a, 1:5)
})

test_that("ol_load is alias for ol_read", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_project")
  
  test_data <- data.frame(x = 1:3)
  ol_write("test", test_data)
  
  result1 <- ol_read("test")
  result2 <- ol_load("test")
  expect_equal(result1, result2)
})

test_that("ol_log returns snapshot information", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)

  options(ol.root = tmpdir)
  ol_init("test_project")

  result <- ol_log(name = NULL)
  expect_s3_class(result, "data.frame")
})

test_that("ol_drop_object removes objects", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)

  options(ol.root = tmpdir)
  ol_init("test_project")

  # Save an object
  test_obj <- list(a = 1:5, b = "test")
  ol_save("test_object", test_obj)

  # Verify it exists
  objs <- ol_list_objects()
  expect_true("test_object" %in% objs$name)

  # Drop the object
  ol_drop_object("test_object")

  # Verify it's gone
  objs <- ol_list_objects()
  expect_false("test_object" %in% objs$name)
})

test_that("ol_drop_object errors on non-existent object", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)

  options(ol.root = tmpdir)
  ol_init("test_project")

  expect_error(ol_drop_object("nonexistent"), "not found")
})
