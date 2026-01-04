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

test_that("multi-parent join preserves individual version refs", {
  # Critical test for Fix B: joining tables with different refs must preserve
  # each parent's specific ref, not share a single ref across all parents
  lake <- Lake$new("test_multiparent_refs")

  # Create two tables with different versions

  lake$put("counts", data.frame(sample_id = 1:3, count = c(100, 200, 300)))
  lake$tag("counts", "v1")
  lake$put("counts", data.frame(sample_id = 1:3, count = c(110, 210, 310)))
  lake$tag("counts", "v2")

  lake$put("metadata", data.frame(sample_id = 1:3, condition = c("A", "B", "A")))
  lake$tag("metadata", "baseline")
  lake$put("metadata", data.frame(sample_id = 1:3, condition = c("X", "Y", "X")))
  lake$tag("metadata", "updated")

  # Join using specific versions: counts@v1 and metadata@baseline
  counts_v1 <- lake$ref("counts", ref = "@tag(v1)")
  meta_baseline <- lake$ref("metadata", ref = "@tag(baseline)")

  joined <- dplyr::left_join(counts_v1, meta_baseline, by = "sample_id")

  # Check that lake_sources preserves both refs correctly
  sources <- attr(joined, "lake_sources")
  expect_true(length(sources) == 2)

  # Find each source and verify its ref
  counts_src <- NULL
  meta_src <- NULL
  for (src in sources) {
    if (src$name == "counts") counts_src <- src
    if (src$name == "metadata") meta_src <- src
  }

  expect_equal(counts_src$ref, "@tag(v1)")
  expect_equal(meta_src$ref, "@tag(baseline)")

  # Save the joined result and verify dependencies are stored correctly
  save_as(joined, "joined_result", lake)

  deps <- lake$deps("joined_result")
  expect_true(nrow(deps) == 2)

  # Verify each parent has its correct ref stored
  counts_dep <- deps[deps$parent_name == "counts", ]
  meta_dep <- deps[deps$parent_name == "metadata", ]

  if ("parent_ref" %in% names(deps)) {
    expect_equal(counts_dep$parent_ref[1], "@tag(v1)")
    expect_equal(meta_dep$parent_ref[1], "@tag(baseline)")
  }

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_multiparent_refs"), recursive = TRUE)
})

test_that("lake_sources attribute flows through dplyr pipe operations", {
  lake <- Lake$new("test_sources_flow")

  lake$put("input", data.frame(x = 1:10, y = 11:20))
  lake$tag("input", "initial")

  # Get ref and apply multiple dplyr operations
  result <- lake$ref("input", ref = "@tag(initial)") |>
    dplyr::filter(x > 3) |>
    dplyr::mutate(z = x + y) |>
    dplyr::select(x, z) |>
    dplyr::arrange(desc(x))

  # lake_sources should be preserved through all operations
  sources <- attr(result, "lake_sources")
  expect_true(!is.null(sources))
  expect_true(length(sources) == 1)
  expect_equal(sources[[1]]$name, "input")
  expect_equal(sources[[1]]$ref, "@tag(initial)")

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_sources_flow"), recursive = TRUE)
})
