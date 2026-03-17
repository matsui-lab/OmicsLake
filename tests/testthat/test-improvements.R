test_that("ol_commit stores commit messages", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_commits")
  
  ol_write("test_table", data.frame(x = 1:5))
  commit_id <- ol_commit("test commit message", params = list(version = "1.0"))
  
  expect_match(commit_id, "^\\d{8}-\\d{6}$")
  
  history <- ol_log_commits()
  expect_true(nrow(history) > 0)
  expect_equal(history$note[[1]], "test commit message")
  expect_true(grepl("version", history$params_json[[1]]))
})

test_that("ol_label tags all tables", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_label")
  
  ol_write("table1", data.frame(x = 1:5))
  ol_write("table2", data.frame(y = 6:10))
  ol_label("v1")
  
  tags <- ol_list_tags()
  expect_true("table1" %in% tags$table_name)
  expect_true("table2" %in% tags$table_name)
  expect_true(all(tags$tag == "v1"))
})

test_that("ol_checkout restores project state", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_checkout")
  
  v1_data <- data.frame(x = 1:5)
  ol_write("data", v1_data)
  ol_label("v1")
  
  v2_data <- data.frame(x = 6:10)
  ol_write("data", v2_data, mode = "overwrite")
  
  ol_checkout("v1")
  restored <- ol_read("data")
  
  expect_equal(nrow(restored), 5)
  expect_equal(restored$x, 1:5)
})

test_that("ol_list_tables returns all tables", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_list")
  
  ol_write("table1", data.frame(x = 1:5))
  ol_write("table2", data.frame(y = 6:10))
  
  tables <- ol_list_tables()
  expect_true("table1" %in% tables$table_name)
  expect_true("table2" %in% tables$table_name)
  expect_false("__ol_refs" %in% tables$table_name)
})

test_that("ol_list_objects returns saved objects", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_objects")
  
  ol_save("obj1", list(a = 1))
  ol_save("obj2", list(b = 2))
  
  objects <- ol_list_objects()
  expect_true("obj1" %in% objects$name)
  expect_true("obj2" %in% objects$name)
})

test_that("ol_drop removes tables", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_drop")
  
  ol_write("temp_table", data.frame(x = 1:5))
  expect_true("temp_table" %in% ol_list_tables()$table_name)
  
  ol_drop("temp_table")
  expect_false("temp_table" %in% ol_list_tables()$table_name)
})

test_that("validation warns about special characters", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_validation")
  
  expect_warning(
    ol_write("table'name", data.frame(x = 1:5)),
    "special SQL characters"
  )
})
