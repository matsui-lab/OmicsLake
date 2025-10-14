test_that("ol_tag_object creates references correctly", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_obj_tag")
  
  ol_save("my_obj", list(version = 1))
  Sys.sleep(0.1)
  ol_save("my_obj", list(version = 2))
  Sys.sleep(0.1)
  ol_save("my_obj", list(version = 3))
  
  ol_tag_object("my_obj", "v2", when = "first")
  ol_tag_object("my_obj", "latest", when = "latest")
  
  state <- OmicsLake:::.ol_get_backend_state("test_obj_tag")
  refs <- DBI::dbGetQuery(state$conn, "SELECT * FROM ol.__ol_refs WHERE table_name LIKE '__object__%'")
  
  expect_true(nrow(refs) >= 2)
  expect_true("__object__my_obj" %in% refs$table_name)
})

test_that("ol_read_object retrieves objects by tag", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_obj_read")
  
  ol_save("test_obj", list(value = 100))
  Sys.sleep(0.1)
  ol_save("test_obj", list(value = 200))
  
  ol_tag_object("test_obj", "v1", when = "first")
  
  v1 <- ol_read_object("test_obj", ref = "@v1")
  latest <- ol_read_object("test_obj", ref = "@latest")
  
  expect_equal(v1$value, 100)
  expect_equal(latest$value, 200)
})

test_that("ol_label tags both tables and objects", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_label_all")
  
  ol_write("test_table", data.frame(x = 1:3))
  ol_save("test_object", list(data = "test"))
  
  ol_label("baseline")
  
  state <- OmicsLake:::.ol_get_backend_state("test_label_all")
  refs <- DBI::dbGetQuery(state$conn, "SELECT * FROM ol.__ol_refs WHERE tag = 'baseline'")
  
  expect_true("test_table" %in% refs$table_name)
  expect_true("__object__test_object" %in% refs$table_name)
})

test_that("ol_read_object backward compatibility with when parameter", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_compat")
  
  ol_save("obj", list(a = 1))
  
  result <- ol_read_object("obj", when = "latest")
  expect_equal(result$a, 1)
})

test_that("ol_tag_object validates inputs", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_validation")
  
  ol_save("test_obj", list(x = 1))
  
  expect_error(ol_tag_object("", "tag1"), "object name")
  expect_error(ol_tag_object("test_obj", ""), "tag")
  expect_error(ol_tag_object("nonexistent", "tag1"), "Object not found")
})

test_that("ol_read_object supports @first reference", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_first_ref")
  
  ol_save("obj", list(v = 1))
  Sys.sleep(0.1)
  ol_save("obj", list(v = 2))
  Sys.sleep(0.1)
  ol_save("obj", list(v = 3))
  
  first <- ol_read_object("obj", ref = "@first")
  latest <- ol_read_object("obj", ref = "@latest")
  
  expect_equal(first$v, 1)
  expect_equal(latest$v, 3)
})
