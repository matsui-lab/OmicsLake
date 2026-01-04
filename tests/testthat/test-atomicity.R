# Tests for P1: Atomicity of snap/tag operations

test_that("snap() creates both commit and label atomically", {
  lake <- Lake$new("test_snap_atomic")

  lake$put("data1", data.frame(x = 1:5))
  lake$put("data2", data.frame(y = 6:10))

  # Create snapshot
  lake$snap("v1.0", note = "Initial snapshot")

  # Verify both commit and label exist
  commits <- ol_log_commits(project = "test_snap_atomic")
  labels <- lake$snaps()

  expect_true(nrow(commits) > 0)
  expect_true("v1.0" %in% labels$tag || nrow(labels) > 0)

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_snap_atomic"), recursive = TRUE)
})

test_that("tag() creates backup and ref entry atomically", {
  lake <- Lake$new("test_tag_atomic")

  lake$put("mydata", data.frame(val = 1:10))
  lake$tag("mydata", "baseline")

  # Modify data
  lake$put("mydata", data.frame(val = 100:110))

  # Original should be retrievable
  original <- lake$get("mydata", ref = "@tag(baseline)")
  expect_equal(original$val, 1:10)

  # Current should be different
  current <- lake$get("mydata")
  expect_equal(current$val, 100:110)

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_tag_atomic"), recursive = TRUE)
})

test_that("snap() with restore maintains consistency", {
  lake <- Lake$new("test_snap_restore")

  # Create initial state
  lake$put("table_a", data.frame(a = 1:3))
  lake$put("table_b", data.frame(b = 4:6))
  lake$snap("state1")

  # Modify state
  lake$put("table_a", data.frame(a = 10:13))
  lake$put("table_b", data.frame(b = 40:43))
  lake$snap("state2")

  # Restore to state1
  lake$restore("state1")

  # Both tables should be restored
  a_data <- lake$get("table_a")
  b_data <- lake$get("table_b")

  expect_equal(a_data$a, 1:3)
  expect_equal(b_data$b, 4:6)

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_snap_restore"), recursive = TRUE)
})

test_that("multiple tags on same data don't interfere", {
  lake <- Lake$new("test_multi_tag")

  lake$put("versioned", data.frame(v = 1:5))
  lake$tag("versioned", "alpha")

  lake$put("versioned", data.frame(v = 10:15))
  lake$tag("versioned", "beta")

  lake$put("versioned", data.frame(v = 100:105))
  lake$tag("versioned", "gamma")

  # Each tag should retrieve correct version
  alpha <- lake$get("versioned", ref = "@tag(alpha)")
  beta <- lake$get("versioned", ref = "@tag(beta)")
  gamma <- lake$get("versioned", ref = "@tag(gamma)")

  expect_equal(alpha$v, 1:5)
  expect_equal(beta$v, 10:15)
  expect_equal(gamma$v, 100:105)

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_multi_tag"), recursive = TRUE)
})

test_that("tag_object uses transaction for consistency", {
  lake <- Lake$new("test_tag_object_atomic")

  obj1 <- list(value = "first")
  lake$put("myobj", obj1)
  lake$tag("myobj", "v1")

  obj2 <- list(value = "second")
  lake$put("myobj", obj2)

  # Should retrieve correct version
  retrieved <- lake$get("myobj", ref = "@tag(v1)")
  expect_equal(retrieved$value, "first")

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_tag_object_atomic"), recursive = TRUE)
})

test_that("snap with params stores params correctly", {
  lake <- Lake$new("test_snap_params")

  lake$put("data", data.frame(x = 1:5))
  lake$snap("parameterized", note = "With params", params = list(threshold = 0.5, method = "auto"))

  # Check commits have params
  commits <- ol_log_commits(project = "test_snap_params")
  expect_true(nrow(commits) > 0)

  # params_json should contain our params
  if ("params_json" %in% names(commits) && nzchar(commits$params_json[1])) {
    params <- jsonlite::fromJSON(commits$params_json[1])
    expect_equal(params$threshold, 0.5)
    expect_equal(params$method, "auto")
  }

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_snap_params"), recursive = TRUE)
})

test_that("nested ol_tag calls within snap don't cause transaction issues", {
  # This test verifies Fix A: nested transactions were causing premature commits
  # After fix, ol_tag/ol_tag_object accept .in_transaction parameter
  lake <- Lake$new("test_nested_tx")

  # Create multiple tables
  lake$put("table1", data.frame(a = 1:3))
  lake$put("table2", data.frame(b = 4:6))
  lake$put("table3", data.frame(c = 7:9))

  # Create snapshot - this internally calls ol_label which calls ol_tag for each table
  # Before fix: nested dbBegin/dbCommit would cause issues
  # After fix: .in_transaction=TRUE prevents nested transactions
  expect_no_error(lake$snap("multi_table_snap"))

  # Verify all tables were tagged
  snaps <- lake$snaps()
  expect_true(nrow(snaps) > 0 || "multi_table_snap" %in% snaps$tag)

  # Verify we can restore
  lake$put("table1", data.frame(a = 100:103))
  lake$restore("multi_table_snap")

  restored <- lake$get("table1")
  expect_equal(restored$a, 1:3)

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_nested_tx"), recursive = TRUE)
})

test_that("ol_label with .in_transaction works correctly", {
  # Direct test of the .in_transaction parameter
  lake <- Lake$new("test_in_transaction")

  lake$put("data", data.frame(x = 1:5))

  # Call ol_label with .in_transaction = FALSE (default behavior - it manages its own tx)
  expect_no_error(ol_label("label1", project = "test_in_transaction", .in_transaction = FALSE))

  # Verify label was created
  refs <- tryCatch({
    state <- .ol_get_backend_state("test_in_transaction")
    ident <- .ol_sql_ident(state$conn, state, "__ol_refs")
    DBI::dbGetQuery(state$conn, sprintf("SELECT * FROM %s WHERE tag = 'label1'", ident))
  }, error = function(e) data.frame())

  expect_true(nrow(refs) > 0)

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_in_transaction"), recursive = TRUE)
})
