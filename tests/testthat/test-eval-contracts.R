# test-eval-contracts.R
# Evaluation Suite Contract Tests
# These tests verify the pass/fail criteria for paper/release readiness

test_that("W1-1/W1-2: SQL pushdown includes WHERE/SELECT", {
  skip_on_cran()

  lake <- Lake$new("test_pushdown_contracts")

  tryCatch({
    # Create test table
    df <- ol_eval_generate_table(1000, 10, seed = 42, type = "mixed")
    lake$put("test", df)

    # W1-1: get with where/select, collect=FALSE
    lazy_result <- lake$get("test",
                            where = ~ num1 > 50,
                            select = c("id", "num1"),
                            collect = FALSE)

    # Capture SQL
    sql <- tryCatch(
      dbplyr::sql_render(lazy_result),
      error = function(e) NA_character_
    )

    # Verify pushdown
    expect_true(!is.na(sql), info = "SQL should be capturable from lazy result")
    expect_true(ol_eval_check_pushdown(sql, c("WHERE")),
                info = "SQL should contain WHERE clause for pushdown")

    # W1-2: ref + dplyr pipeline
    lazy_pipeline <- lake$ref("test") |>
      dplyr::filter(num1 > 50) |>
      dplyr::group_by(cat1) |>
      dplyr::summarise(mean_val = mean(num1, na.rm = TRUE), .groups = "drop")

    sql2 <- tryCatch(
      dbplyr::sql_render(lazy_pipeline),
      error = function(e) NA_character_
    )

    expect_true(!is.na(sql2), info = "Pipeline SQL should be capturable")
    expect_true(ol_eval_check_pushdown(sql2, c("WHERE", "GROUP BY")),
                info = "Pipeline SQL should contain WHERE and GROUP BY")

  }, finally = {
    unlink(file.path(path.expand("~"), ".omicslake", "test_pushdown_contracts"),
           recursive = TRUE)
  })
})

test_that("W3: deps returns parent_ref and parent_version_id", {
  skip_on_cran()

  lake <- Lake$new("test_lineage_contracts")

  tryCatch({
    # Create parent data
    df_a <- data.frame(id = 1:100, value = rnorm(100))
    lake$put("parent", df_a)
    lake$tag("parent", "v1")

    # Create child with explicit lineage
    df_b <- lake$ref("parent@tag(v1)") |>
      dplyr::filter(value > 0) |>
      dplyr::collect()

    attr(df_b, "lake_sources") <- list(list(name = "parent", ref = "@tag(v1)"))
    lake$put("child", df_b)

    # Get dependencies
    deps <- lake$deps("child", direction = "up")

    # Verify structure
    expect_true(is.data.frame(deps), info = "deps should return a data frame")
    expect_true(nrow(deps) > 0, info = "child should have dependencies")
    expect_true("parent_name" %in% names(deps), info = "deps should have parent_name")
    expect_true("parent_ref" %in% names(deps),
                info = "deps should have parent_ref for version-aware lineage")
    expect_true("parent_version_id" %in% names(deps),
                info = "deps should have parent_version_id for version-aware lineage")

    # Verify values
    expect_equal(deps$parent_name[1], "parent")
    expect_equal(deps$parent_ref[1], "@tag(v1)")

  }, finally = {
    unlink(file.path(path.expand("~"), ".omicslake", "test_lineage_contracts"),
           recursive = TRUE)
  })
})

test_that("W3: multi-parent join preserves all dependencies", {
  skip_on_cran()

  lake <- Lake$new("test_multiparent_contracts")

  tryCatch({
    # Create multiple parents
    df_a <- data.frame(id = 1:100, value_a = rnorm(100))
    df_b <- data.frame(id = 1:100, value_b = runif(100))
    df_c <- data.frame(id = 1:100, value_c = rpois(100, 5))

    lake$put("table_a", df_a)
    lake$put("table_b", df_b)
    lake$put("table_c", df_c)

    lake$tag("table_a", "v1")
    lake$tag("table_b", "v1")
    lake$tag("table_c", "v1")

    # Create child from 3-parent join
    joined <- lake$ref("table_a@tag(v1)") |>
      dplyr::inner_join(lake$ref("table_b@tag(v1)"), by = "id") |>
      dplyr::inner_join(lake$ref("table_c@tag(v1)"), by = "id") |>
      dplyr::collect()

    attr(joined, "lake_sources") <- list(
      list(name = "table_a", ref = "@tag(v1)"),
      list(name = "table_b", ref = "@tag(v1)"),
      list(name = "table_c", ref = "@tag(v1)")
    )

    lake$put("joined_result", joined)

    # Verify all parents are tracked
    deps <- lake$deps("joined_result", direction = "up")

    expect_true(nrow(deps) >= 3,
                info = "Multi-parent join should have at least 3 dependencies")

    parent_names <- unique(deps$parent_name)
    expect_true("table_a" %in% parent_names, info = "table_a should be in dependencies")
    expect_true("table_b" %in% parent_names, info = "table_b should be in dependencies")
    expect_true("table_c" %in% parent_names, info = "table_c should be in dependencies")

    # Verify version refs are present
    expect_true(all(!is.na(deps$parent_ref)),
                info = "All parent_ref values should be non-NA")

  }, finally = {
    unlink(file.path(path.expand("~"), ".omicslake", "test_multiparent_contracts"),
           recursive = TRUE)
  })
})

test_that("W3: diff returns differences based on version refs", {
  skip_on_cran()

  lake <- Lake$new("test_diff_contracts")

  tryCatch({
    # Create and tag first version
    df_v1 <- data.frame(id = 1:100, value = rnorm(100, mean = 0))
    lake$put("data", df_v1)
    lake$tag("data", "v1")

    # Create and tag second version (different values)
    df_v2 <- data.frame(id = 1:100, value = rnorm(100, mean = 10))
    lake$put("data", df_v2)
    lake$tag("data", "v2")

    # Compare versions
    diff_result <- lake$diff("data", ref1 = "@tag(v2)", ref2 = "@tag(v1)")

    # Verify diff structure
    expect_true(inherits(diff_result, "lake_diff"),
                info = "diff should return lake_diff class")
    expect_equal(diff_result$ref1, "@tag(v2)")
    expect_equal(diff_result$ref2, "@tag(v1)")
    expect_equal(diff_result$ref1_rows, 100)
    expect_equal(diff_result$ref2_rows, 100)
    expect_equal(diff_result$row_diff, 0)

  }, finally = {
    unlink(file.path(path.expand("~"), ".omicslake", "test_diff_contracts"),
           recursive = TRUE)
  })
})

test_that("Config loading works with defaults and YAML", {
  # Test default config
  config <- ol_eval_load_config(NULL)

  expect_true(is.list(config))
  expect_true("project_root" %in% names(config))
  expect_true("seed" %in% names(config))
  expect_true("sizes" %in% names(config))
  expect_true("workloads" %in% names(config))
  expect_true("baselines" %in% names(config))
})

test_that("Data generators produce correct dimensions", {
  # Test table generator
  df <- ol_eval_generate_table(1000, 20, seed = 42, type = "numeric")

  expect_equal(nrow(df), 1000)
  expect_equal(ncol(df), 21)  # 20 cols + id
  expect_true("id" %in% names(df))

  # Test mixed type
  df_mixed <- ol_eval_generate_table(500, 10, seed = 42, type = "mixed")
  expect_equal(nrow(df_mixed), 500)
  expect_true(any(vapply(df_mixed, is.character, logical(1))))

  # Test dimension table
  dim_df <- ol_eval_generate_dim_table(100, seed = 42)
  expect_equal(nrow(dim_df), 100)
  expect_true("id" %in% names(dim_df))
  expect_true("category" %in% names(dim_df))

  # Test RNA-seq generator
  rnaseq <- ol_eval_generate_case_rnaseq(seed = 42, n_genes = 1000, n_samples = 6)

  expect_true(is.list(rnaseq))
  expect_true("counts" %in% names(rnaseq))
  expect_true("gene_info" %in% names(rnaseq))
  expect_true("sample_info" %in% names(rnaseq))
  expect_equal(nrow(rnaseq$counts), 1000)
  expect_equal(nrow(rnaseq$sample_info), 6)
})

test_that("Metrics measurement works correctly", {
  # Test basic measurement
  m <- ol_eval_measure({
    Sys.sleep(0.1)
    42
  })

  expect_true(is.list(m))
  expect_true("time_sec" %in% names(m))
  expect_true("result" %in% names(m))
  expect_true(m$time_sec >= 0.1)
  expect_equal(m$result, 42)

  # Test with memory measurement
  m2 <- ol_eval_measure({
    x <- rnorm(10000)
    sum(x)
  }, measure_memory = TRUE)

  expect_true("rss_mb" %in% names(m2))
})

test_that("Result record creation is correct", {
  record <- ol_eval_result(
    workload = "W0-1",
    variant = "omicslake",
    size = "small",
    cache = "warm",
    rep = 1,
    metrics = list(time_sec = 1.5, n_rows = 1000)
  )

  expect_true(is.list(record))
  expect_true("run_id" %in% names(record))
  expect_true("timestamp" %in% names(record))
  expect_equal(record$workload, "W0-1")
  expect_equal(record$variant, "omicslake")
  expect_equal(record$metrics$time_sec, 1.5)
  expect_true("env" %in% names(record))
})

test_that("Pushdown validation works", {
  # SQL with pushdown
  sql_good <- "SELECT id, value FROM table1 WHERE value > 5 GROUP BY id"
  expect_true(ol_eval_check_pushdown(sql_good, c("WHERE")))
  expect_true(ol_eval_check_pushdown(sql_good, c("WHERE", "GROUP BY")))
  expect_true(ol_eval_check_pushdown(sql_good, c("SELECT")))

  # SQL without pushdown
  sql_bad <- "SELECT * FROM table1"
  expect_false(ol_eval_check_pushdown(sql_bad, c("WHERE")))

  # Edge cases
  expect_false(ol_eval_check_pushdown(NULL, c("WHERE")))
  expect_false(ol_eval_check_pushdown(NA, c("WHERE")))
})

test_that("Lineage validation works", {
  # Valid deps
  deps_good <- data.frame(
    parent_name = c("a", "b"),
    parent_ref = c("@tag(v1)", "@latest"),
    parent_version_id = c("1", "2"),
    stringsAsFactors = FALSE
  )

  result <- ol_eval_check_lineage(deps_good, c("a", "b"), check_version_info = TRUE)
  expect_true(result$valid)
  expect_equal(length(result$missing), 0)

  # Missing parent
  result2 <- ol_eval_check_lineage(deps_good, c("a", "b", "c"))
  expect_false(result2$valid)
  expect_true("c" %in% result2$missing)

  # Empty deps
  result3 <- ol_eval_check_lineage(data.frame(), c("a"))
  expect_false(result3$valid)
})
