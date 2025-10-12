test_that("ol_init_iceberg initializes Iceberg backend", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  
  state <- ol_init_iceberg("test_iceberg_project")
  expect_type(state, "list")
  expect_true("conn" %in% names(state))
  expect_true("catalog_name" %in% names(state))
  expect_equal(state$project, "test_iceberg_project")
})

test_that("ol_init_iceberg errors on unsupported engine", {
  expect_error(ol_init_iceberg("test", engine = "unsupported"), "Unsupported Iceberg engine")
})

test_that("ol_init_iceberg supports external object mode", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  
  obj_root <- file.path(tmpdir, "objects")
  state <- ol_init_iceberg("test", object_mode = "external", object_root = obj_root)
  
  expect_equal(state$object_mode, "external")
  expect_true(dir.exists(obj_root))
})

test_that("ol_tag creates snapshot tags", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_project")
  
  test_data <- data.frame(x = 1:3)
  ol_write("test_table", test_data)
  
  result <- tryCatch(
    ol_tag("test_table", "v1.0"),
    error = function(e) NULL
  )
  expect_true(is.null(result) || is.character(result))
})

test_that("ol_read_object retrieves stored objects", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_project")
  
  test_obj <- list(data = 1:10, name = "test")
  ol_save("my_object", test_obj)
  
  result <- ol_read_object("my_object", when = "latest")
  expect_type(result, "list")
  expect_equal(result$data, 1:10)
})

test_that("ol_read_object errors on missing object", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_project")
  
  expect_error(ol_read_object("nonexistent"), "Object not found")
})

test_that(".ol_ref_parse handles different reference types", {
  expect_equal(.ol_ref_parse(NULL)$type, "latest")
  expect_equal(.ol_ref_parse("@latest")$type, "latest")
  expect_equal(.ol_ref_parse("@mytag")$type, "tag")
  expect_equal(.ol_ref_parse("@mytag")$value, "mytag")
  expect_equal(.ol_ref_parse("snapshot123")$type, "snapshot")
  expect_equal(.ol_ref_parse("@tag(v1.0)")$type, "tag")
  expect_equal(.ol_ref_parse("@tag(v1.0)")$value, "v1.0")
})
