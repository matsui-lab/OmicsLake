test_that(".ol_root returns correct default path", {
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt))
  
  options(ol.root = NULL)
  expect_equal(.ol_root(), file.path(path.expand("~"), ".omicslake"))
})

test_that(".ol_root respects ol.root option", {
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt))
  
  options(ol.root = "/custom/path")
  expect_equal(.ol_root(), "/custom/path")
})

test_that(".ol_proj_root constructs correct path", {
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt))
  
  options(ol.root = "/test/root")
  expect_equal(.ol_proj_root("myproject"), "/test/root/myproject")
})

test_that(".ol_now_id generates timestamp format", {
  id <- .ol_now_id()
  expect_match(id, "^\\d{8}-\\d{6}$")
})

test_that(".ol_norm normalizes paths", {
  path <- .ol_norm("~/test")
  expect_true(startsWith(path, "/"))
})

test_that(".ol_assert_project stops on NULL project", {
  expect_error(.ol_assert_project(NULL, "test message"), "test message")
  expect_error(.ol_assert_project("", "test message"), "test message")
})

test_that(".ol_assert_project returns project when valid", {
  expect_equal(.ol_assert_project("myproject", "msg"), "myproject")
})

test_that(".ol_require stops when package missing", {
  expect_error(.ol_require(c("nonexistent_package_xyz")), "Please install")
})

test_that(".ol_require succeeds when package exists", {
  expect_silent(.ol_require("base"))
})
