# Tests for P0-1: Lazy semantics and SQL pushdown

test_that("get(collect=FALSE) returns lazy tbl for tables", {
  lake <- Lake$new("test_lazy_collect")

  df <- data.frame(a = 1:10, b = letters[1:10], stringsAsFactors = FALSE)
  lake$put("test_data", df)

  # collect=FALSE should return a lazy table

  lazy_result <- lake$get("test_data", collect = FALSE)

  # Check it's a lazy type (tbl_lazy, tbl_sql, tbl_dbi, etc.)
  expect_true(
    inherits(lazy_result, "tbl_lazy") ||
    inherits(lazy_result, "tbl_sql") ||
    inherits(lazy_result, "tbl_dbi") ||
    inherits(lazy_result, "tbl_duckdb_connection")
  )

  # Verify we can still collect it and get correct data
  collected <- dplyr::collect(lazy_result)
  expect_equal(nrow(collected), 10)

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_lazy_collect"), recursive = TRUE)
})

test_that("get(collect=TRUE) returns data.frame for tables", {
  lake <- Lake$new("test_eager_collect")

  df <- data.frame(a = 1:5, b = 6:10, stringsAsFactors = FALSE)
  lake$put("test_data", df)

  # collect=TRUE should return a data.frame
  result <- lake$get("test_data", collect = TRUE)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 5)
  expect_equal(names(result), c("a", "b"))

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_eager_collect"), recursive = TRUE)
})

test_that("where filter applies SQL pushdown with collect=FALSE", {
  lake <- Lake$new("test_pushdown_where")

  df <- data.frame(x = 1:100, y = rep(c("a", "b"), 50), stringsAsFactors = FALSE)
  lake$put("test_data", df)

  # Get lazy with filter
  lazy_filtered <- lake$get("test_data", where = ~ x > 50, collect = FALSE)

  # The query should have WHERE clause (SQL pushdown)
  # We can verify by checking the SQL or by collecting and verifying row count
  result <- dplyr::collect(lazy_filtered)
  expect_equal(nrow(result), 50)
  expect_true(all(result$x > 50))

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_pushdown_where"), recursive = TRUE)
})

test_that("select applies SQL pushdown with collect=FALSE", {
  lake <- Lake$new("test_pushdown_select")

  df <- data.frame(a = 1:10, b = 11:20, c = 21:30, stringsAsFactors = FALSE)
  lake$put("test_data", df)

  # Get lazy with select
  lazy_selected <- lake$get("test_data", select = c("a", "c"), collect = FALSE)

  # Collect and verify only selected columns
  result <- dplyr::collect(lazy_selected)
  expect_equal(names(result), c("a", "c"))
  expect_equal(ncol(result), 2)

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_pushdown_select"), recursive = TRUE)
})

test_that("where and select work together with collect=TRUE", {
  lake <- Lake$new("test_combined_filters")

  df <- data.frame(x = 1:20, y = letters[1:20], z = 100:119, stringsAsFactors = FALSE)
  lake$put("test_data", df)

  # Filter and select with immediate collection
  result <- lake$get("test_data", where = ~ x > 10, select = c("x", "z"), collect = TRUE)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 10)
  expect_equal(names(result), c("x", "z"))
  expect_true(all(result$x > 10))

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_combined_filters"), recursive = TRUE)
})

test_that("collect=FALSE warns for non-table objects", {
  lake <- Lake$new("test_object_lazy")

  obj <- list(x = 1:10, y = "test")
  lake$put("test_obj", obj)

  # Should warn that collect=FALSE is ignored for objects
  expect_warning(
    result <- lake$get("test_obj", collect = FALSE),
    "collect=FALSE is ignored"
  )

  # Should still return the object
  expect_equal(result$x, 1:10)
  expect_equal(result$y, "test")

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_object_lazy"), recursive = TRUE)
})

test_that("where/select warns for non-table objects", {
  lake <- Lake$new("test_object_filter")

  obj <- list(a = 1, b = 2)
  lake$put("test_obj", obj)

  # Should warn that where/select are ignored
  expect_warning(
    result <- lake$get("test_obj", where = ~ a > 0),
    "where/select parameters are ignored"
  )

  expect_equal(result$a, 1)

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_object_filter"), recursive = TRUE)
})

test_that("ref() returns lazy table with correct class", {
  lake <- Lake$new("test_ref_lazy")

  df <- data.frame(a = 1:5, stringsAsFactors = FALSE)
  lake$put("test_data", df)

  lazy_tbl <- lake$ref("test_data")

  # Should be a lake_tbl (which inherits from tbl_lazy)
  expect_true(inherits(lazy_tbl, "lake_tbl"))
  expect_true(
    inherits(lazy_tbl, "tbl_lazy") ||
    inherits(lazy_tbl, "tbl_sql") ||
    inherits(lazy_tbl, "tbl_dbi")
  )

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_ref_lazy"), recursive = TRUE)
})
