# Tests for the new Lake R6 API

test_that("Lake can be initialized", {
  lake <- Lake$new("test_lake_init")
  expect_s3_class(lake, "Lake")

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_lake_init"), recursive = TRUE)
})

test_that("Lake auto-generates project name when not provided", {
  lake <- Lake$new()
  expect_s3_class(lake, "Lake")

  # Project name should start with "lake_"
  # (accessing private field for testing)
  project <- lake$.__enclos_env__$private$.project
  expect_true(grepl("^lake_", project))

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", project), recursive = TRUE)
})

test_that("Lake$put and Lake$get work for data frames", {
  lake <- Lake$new("test_lake_io")

  df <- data.frame(a = 1:5, b = letters[1:5], stringsAsFactors = FALSE)
  lake$put("test_df", df)

  result <- lake$get("test_df")
  expect_equal(nrow(result), 5)
  expect_equal(names(result), c("a", "b"))
  expect_equal(result$a, 1:5)

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_lake_io"), recursive = TRUE)
})

test_that("Lake$put and Lake$get work for R objects", {
  lake <- Lake$new("test_lake_obj")

  obj <- list(x = 1:10, y = "hello", z = list(nested = TRUE))
  lake$put("test_obj", obj)

  result <- lake$get("test_obj")
  expect_equal(result$x, 1:10)
  expect_equal(result$y, "hello")
  expect_true(result$z$nested)

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_lake_obj"), recursive = TRUE)
})

test_that("Lake$get with formula filter works", {
  lake <- Lake$new("test_lake_filter")

  df <- data.frame(a = 1:10, b = letters[1:10], stringsAsFactors = FALSE)
  lake$put("test_data", df)

  # Filter using formula
  result <- lake$get("test_data", where = ~ a > 5)
  expect_equal(nrow(result), 5)
  expect_true(all(result$a > 5))

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_lake_filter"), recursive = TRUE)
})

test_that("Lake$get with select works", {
  lake <- Lake$new("test_lake_select")

  df <- data.frame(a = 1:5, b = 6:10, c = 11:15, stringsAsFactors = FALSE)
  lake$put("test_data", df)

  result <- lake$get("test_data", select = c("a", "c"))
  expect_equal(names(result), c("a", "c"))
  expect_equal(ncol(result), 2)

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_lake_select"), recursive = TRUE)
})

test_that("Lake$ref returns lazy table", {
  lake <- Lake$new("test_lake_ref")

  df <- data.frame(a = 1:5, b = letters[1:5], stringsAsFactors = FALSE)
  lake$put("test_data", df)

  lazy_tbl <- lake$ref("test_data")
  expect_true(inherits(lazy_tbl, "tbl_lazy") || inherits(lazy_tbl, "lake_tbl"))

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_lake_ref"), recursive = TRUE)
})

test_that("Lake$tables lists tables", {
  lake <- Lake$new("test_lake_tables")

  lake$put("table1", data.frame(x = 1:3))
  lake$put("table2", data.frame(y = 4:6))

  tables <- lake$tables()
  expect_true("table1" %in% tables$table_name)
  expect_true("table2" %in% tables$table_name)

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_lake_tables"), recursive = TRUE)
})

test_that("Lake$snap and Lake$restore work", {
  lake <- Lake$new("test_lake_version")

  # Initial data
  lake$put("data", data.frame(x = 1:3))
  lake$snap("v1")

  # Modify data
  lake$put("data", data.frame(x = 10:13))

  # Restore
  lake$restore("v1")
  result <- lake$get("data")
  expect_equal(result$x, 1:3)

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_lake_version"), recursive = TRUE)
})

test_that("Lake$tag works", {
  lake <- Lake$new("test_lake_tag")

  lake$put("data", data.frame(x = 1:3))
  lake$tag("data", "initial")

  lake$put("data", data.frame(x = 10:13))

  # Get tagged version
  result <- lake$get("data", ref = "@tag(initial)")
  expect_equal(result$x, 1:3)

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_lake_tag"), recursive = TRUE)
})

test_that("Lake$drop removes data", {
  lake <- Lake$new("test_lake_drop")

  lake$put("to_drop", data.frame(x = 1:3))
  expect_true("to_drop" %in% lake$tables()$table_name)

  lake$drop("to_drop")
  expect_false("to_drop" %in% lake$tables()$table_name)

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_lake_drop"), recursive = TRUE)
})

test_that("Lake bracket notation works", {
  lake <- Lake$new("test_lake_bracket")

  df <- data.frame(a = 1:10, b = letters[1:10], stringsAsFactors = FALSE)
  lake$put("data", df)

  # Read
  result <- lake["data"]
  expect_equal(nrow(result), 10)

  # Filter
  result <- lake["data", a > 5]
  expect_equal(nrow(result), 5)

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_lake_bracket"), recursive = TRUE)
})

test_that("Lake$tree returns lineage", {
  lake <- Lake$new("test_lake_lineage")

  lake$put("source", data.frame(x = 1:3))
  lake$put("derived", data.frame(y = 4:6), depends_on = "source")

  lineage <- lake$tree("derived")
  # Should have at least one row showing the dependency
  expect_true(is.data.frame(lineage))

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_lake_lineage"), recursive = TRUE)
})

test_that("Lake chaining works", {
  lake <- Lake$new("test_lake_chain")

  # Methods should return self for chaining
  result <- lake$
    put("data1", data.frame(x = 1:3))$
    put("data2", data.frame(y = 4:6))$
    snap("v1")

  expect_s3_class(result, "Lake")
  expect_true("data1" %in% lake$tables()$table_name)
  expect_true("data2" %in% lake$tables()$table_name)

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_lake_chain"), recursive = TRUE)
})
