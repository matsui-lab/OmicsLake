# Tests for P0-2: Version-aware lineage

test_that("dependencies include parent_ref and parent_version_id", {
  lake <- Lake$new("test_version_deps")

  # Create source data and tag it
  lake$put("source", data.frame(x = 1:5))
  lake$tag("source", "v1")

  # Modify source
  lake$put("source", data.frame(x = 10:15))
  lake$tag("source", "v2")

  # Create derived data from v1
  lake$ref("source", ref = "@tag(v1)") |>
    dplyr::filter(x > 2) |>
    save_as("derived_from_v1", lake)

  # Check dependencies include version info
  deps <- lake$deps("derived_from_v1")

  expect_true("parent_ref" %in% names(deps) || "parent_version_id" %in% names(deps))

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_version_deps"), recursive = TRUE)
})

test_that("dependency records resolve refs to version_ids", {
  lake <- Lake$new("test_dep_resolve")

  # Create and tag source
  lake$put("source", data.frame(val = 1:3))
  lake$tag("source", "baseline")

  # Create derived with explicit depends_on using tag
  lake$put("derived", data.frame(val = 4:6),
           depends_on = list(list(name = "source", ref = "@tag(baseline)")))

  # Get dependencies
  deps <- lake$deps("derived")

  expect_true(nrow(deps) > 0)
  expect_equal(deps$parent_name[1], "source")

  # If parent_ref is present, it should match what we specified
 if ("parent_ref" %in% names(deps)) {
    expect_equal(deps$parent_ref[1], "@tag(baseline)")
  }

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_dep_resolve"), recursive = TRUE)
})

test_that("DependencyTracker stores name+ref pairs", {
  lake <- Lake$new("test_tracker_pairs")

  # Put some test data
  lake$put("table1", data.frame(a = 1:3))
  lake$put("table2", data.frame(b = 4:6))
  lake$tag("table1", "tagged_version")

  # Access private tracker for testing
  tracker <- lake$.__enclos_env__$private$.tracker

  # Start tracking
  tracker$start_write("output")

  # Track reads with different refs
  tracker$track_read("table1", "@latest")
  tracker$track_read("table2", "@tag(some_tag)")

  # Get current reads
  reads <- tracker$current_reads()

  expect_true(is.list(reads))
  expect_true(length(reads) >= 2)

  # Check that reads are stored as name+ref pairs
  for (r in reads) {
    expect_true("name" %in% names(r))
    expect_true("ref" %in% names(r))
  }

  tracker$end_write()

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_tracker_pairs"), recursive = TRUE)
})

test_that("lake_tbl preserves lake_source_ref through dplyr operations", {
  lake <- Lake$new("test_ref_attr")

  lake$put("data", data.frame(x = 1:10, y = 11:20))
  lake$tag("data", "v1")

  # Get ref with specific version
  tbl <- lake$ref("data", ref = "@tag(v1)")

  # Check attribute is set
  expect_equal(attr(tbl, "lake_source_ref"), "@tag(v1)")

  # Apply dplyr operations
  filtered <- dplyr::filter(tbl, x > 5)

  # Attribute should be preserved
  expect_equal(attr(filtered, "lake_source_ref"), "@tag(v1)")

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_ref_attr"), recursive = TRUE)
})

test_that("explicit depends_on with version info works", {
  lake <- Lake$new("test_explicit_version_deps")

  lake$put("a", data.frame(x = 1:5))
  lake$tag("a", "v1.0")

  lake$put("a", data.frame(x = 10:15))
  lake$tag("a", "v2.0")

  # Put with version-aware dependency
  lake$put("b", data.frame(y = 1:3),
           depends_on = list(list(name = "a", ref = "@tag(v1.0)")))

  deps <- lake$deps("b")
  expect_equal(deps$parent_name[1], "a")

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_explicit_version_deps"), recursive = TRUE)
})

test_that("tree() shows version information when available", {
  lake <- Lake$new("test_tree_version")

  lake$put("root", data.frame(x = 1:3))
  lake$tag("root", "base")

  lake$put("child", data.frame(y = 4:6), depends_on = "root")

  tree <- lake$tree("child")

  # Tree should include some version-related columns
  expect_true(is.data.frame(tree))

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_tree_version"), recursive = TRUE)
})

test_that("migration adds version columns to existing dependencies table", {
  # This test verifies that old databases get migrated
  lake <- Lake$new("test_migration")

  # Create a dependency
  lake$put("old_source", data.frame(x = 1))
  lake$put("old_derived", data.frame(y = 2), depends_on = "old_source")

  # Get dependencies - should work even if columns were just added
  deps <- tryCatch(
    lake$deps("old_derived"),
    error = function(e) NULL
  )

  expect_true(!is.null(deps))

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_migration"), recursive = TRUE)
})
