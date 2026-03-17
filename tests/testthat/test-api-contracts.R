# Tests for API contracts: diff, auto-tracking scope, and edge cases

test_that("diff() uses ref1 and ref2 parameters", {
  lake <- Lake$new("test_diff_refs")

  # Create versions
  lake$put("evolving", data.frame(x = 1:3, y = 4:6))
  lake$tag("evolving", "v1")

  lake$put("evolving", data.frame(x = 1:5, y = 6:10, z = 11:15))
  lake$tag("evolving", "v2")

  # Compare specific versions
  comparison <- lake$diff("evolving", ref1 = "@tag(v2)", ref2 = "@tag(v1)")

  expect_equal(comparison$name, "evolving")
  expect_equal(comparison$ref1, "@tag(v2)")
  expect_equal(comparison$ref2, "@tag(v1)")
  expect_equal(comparison$ref1_rows, 5)
  expect_equal(comparison$ref2_rows, 3)
  expect_true("z" %in% comparison$cols_added)

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_diff_refs"), recursive = TRUE)
})

test_that("diff() handles default refs", {
  lake <- Lake$new("test_diff_default")

  lake$put("data", data.frame(a = 1:2))
  lake$tag("data", "first")

  lake$put("data", data.frame(a = 1:10))

  # Default: @latest vs @first (which would be the tagged version)
  # Note: @first refers to the actual first tag, not first put
  comparison <- lake$diff("data")

  expect_equal(comparison$ref1, "@latest")
  expect_equal(comparison$ref2, "@first")

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_diff_default"), recursive = TRUE)
})

test_that("get()->put() without tracking context has no auto-dependency", {
  lake <- Lake$new("test_no_auto_dep")

  lake$put("source", data.frame(x = 1:5))

  # Simple get/put without tracking context
  data <- lake$get("source")
  lake$put("derived", data.frame(y = data$x * 2))

  # Should have NO dependencies (this is the documented behavior)
  deps <- lake$deps("derived")
  expect_equal(nrow(deps), 0)

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_no_auto_dep"), recursive = TRUE)
})

test_that("save_as() captures dependencies from dplyr pipe", {
  lake <- Lake$new("test_save_as_deps")

  lake$put("input", data.frame(x = 1:10))

  # Use save_as() - should capture dependency
  lake$ref("input") |>
    dplyr::filter(x > 5) |>
    save_as("output", lake)

  deps <- lake$deps("output")
  expect_true(nrow(deps) > 0)
  expect_equal(deps$parent_name[1], "input")

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_save_as_deps"), recursive = TRUE)
})

test_that("explicit depends_on overrides auto-detection", {
  lake <- Lake$new("test_explicit_deps")

  lake$put("a", data.frame(x = 1))
  lake$put("b", data.frame(y = 2))
  lake$put("c", data.frame(z = 3))

  # Use explicit depends_on
  lake$put("result", data.frame(w = 4), depends_on = c("a", "b"))

  deps <- lake$deps("result")
  parent_names <- deps$parent_name

  expect_true("a" %in% parent_names)
  expect_true("b" %in% parent_names)
  expect_false("c" %in% parent_names)

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_explicit_deps"), recursive = TRUE)
})

test_that("join preserves lineage from both tables", {
  lake <- Lake$new("test_join_lineage")

  lake$put("left_data", data.frame(id = 1:3, x = 10:12))
  lake$put("right_data", data.frame(id = 1:3, y = 20:22))

  # Join using lake$ref()
  lake$ref("left_data") |>
    dplyr::left_join(lake$ref("right_data"), by = "id") |>
    save_as("joined", lake)

  deps <- lake$deps("joined")
  parent_names <- deps$parent_name

  expect_true("left_data" %in% parent_names)
  expect_true("right_data" %in% parent_names)

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_join_lineage"), recursive = TRUE)
})

test_that("depends_on with list format works", {
  lake <- Lake$new("test_list_deps")

  lake$put("src", data.frame(v = 1:3))
  lake$tag("src", "v1")

  # Use list format for versioned dependencies
  lake$put("dst", data.frame(w = 4:6),
           depends_on = list(list(name = "src", ref = "@tag(v1)")))

  deps <- lake$deps("dst")
  expect_equal(deps$parent_name[1], "src")

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_list_deps"), recursive = TRUE)
})

test_that("ref() with version parameter tracks correct version", {
  lake <- Lake$new("test_ref_version")

  lake$put("versioned", data.frame(x = 1:3))
  lake$tag("versioned", "old")

  lake$put("versioned", data.frame(x = 10:13))

  # Use ref with specific version
  tbl <- lake$ref("versioned", ref = "@tag(old)")

  # Should have the ref attribute
  expect_equal(attr(tbl, "lake_source_ref"), "@tag(old)")

  # Collect should give old data
  collected <- dplyr::collect(tbl)
  expect_equal(collected$x, 1:3)

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_ref_version"), recursive = TRUE)
})

test_that("is_lazy correctly identifies all lazy types", {
  lake <- Lake$new("test_is_lazy")

  lake$put("data", data.frame(x = 1:5))

  # Get various lazy references
  lazy_ref <- lake$ref("data")
  lazy_get <- lake$get("data", collect = FALSE)

  # Both should be considered lazy
  is_lazy_fn <- lake$.__enclos_env__$private$.is_lazy

  expect_true(is_lazy_fn(lazy_ref))
  expect_true(is_lazy_fn(lazy_get))

  # A regular data.frame should not be lazy
  df <- data.frame(a = 1)
  expect_false(is_lazy_fn(df))

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_is_lazy"), recursive = TRUE)
})
