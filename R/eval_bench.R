#' @title OmicsLake Evaluation Benchmarks
#' @description Benchmark workloads W0-W2 for OmicsLake evaluation
#' @name eval_bench
NULL

# ============================================================================
# Main Benchmark Runner
# ============================================================================

#' Run all benchmarks
#'
#' @param config Configuration list from ol_eval_load_config()
#' @param output_file Path to JSONL output file
#' @return Invisible list of all results
#' @export
ol_eval_run_benchmarks <- function(config, output_file = NULL) {
  if (is.null(output_file)) {
    output_file <- file.path(config$outputs$results_dir, "benchmark_results.jsonl")
  }

  # Ensure output directory exists
  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

  # Set seed
  set.seed(config$seed)

  # Capture environment info once
  env_info <- ol_eval_env_info()

  all_results <- list()

  # W0: I/O and versioning
  if (isTRUE(config$workloads$W0_io)) {
    message("Running W0: I/O and versioning benchmarks...")
    w0_results <- .ol_eval_run_W0(config, env_info, output_file)
    all_results <- c(all_results, w0_results)
  }

  # W1: Query benchmarks
  if (isTRUE(config$workloads$W1_queries)) {
    message("Running W1: Query benchmarks...")
    w1_results <- .ol_eval_run_W1(config, env_info, output_file)
    all_results <- c(all_results, w1_results)
  }

  # W2: Lineage benchmarks
  if (isTRUE(config$workloads$W2_lineage)) {
    message("Running W2: Lineage benchmarks...")
    w2_results <- .ol_eval_run_W2(config, env_info, output_file)
    all_results <- c(all_results, w2_results)
  }

  message("Benchmarks complete. Results written to: ", output_file)
  invisible(all_results)
}

# ============================================================================
# W0: I/O and Versioning Operations
# ============================================================================

#' Run W0 workloads
#' @keywords internal
.ol_eval_run_W0 <- function(config, env_info, output_file) {
  results <- list()

  for (size_name in names(config$sizes)) {
    size_cfg <- config$sizes[[size_name]]

    message("  W0 size: ", size_name)

    # Create fresh lake for this size
    project_name <- paste0("eval_w0_", size_name, "_", Sys.getpid())
    lake <- Lake$new(project_name, root = config$project_root)

    tryCatch({
      # W0-1: put(table)
      results <- c(results, .ol_eval_W0_1_put_table(
        lake, size_cfg, size_name, config, env_info, output_file
      ))

      # W0-2: put(object)
      results <- c(results, .ol_eval_W0_2_put_object(
        lake, size_name, config, env_info, output_file
      ))

      # W0-3: tag/snap
      results <- c(results, .ol_eval_W0_3_tag_snap(
        lake, size_name, config, env_info, output_file
      ))

    }, finally = {
      # Cleanup
      .ol_eval_cleanup_project(config$project_root, project_name)
    })
  }

  results
}

#' W0-1: put(table) benchmark
#' @keywords internal
.ol_eval_W0_1_put_table <- function(lake, size_cfg, size_name, config, env_info, output_file) {
  n_reps <- config$reps$bench

  # Generate test data
  df <- ol_eval_generate_table(size_cfg$n_rows, size_cfg$n_cols,
                                seed = config$seed, type = "numeric")

  results <- list()

  for (rep in seq_len(n_reps)) {
    # Measure put operation
    table_name <- paste0("bench_table_", rep)

    m <- ol_eval_measure({
      lake$put(table_name, df)
    }, measure_memory = TRUE)

    # Get size
    project_size <- .ol_eval_get_project_size(config$project_root, lake)

    record <- ol_eval_result(
      workload = "W0-1",
      variant = "omicslake",
      size = size_name,
      cache = "na",
      rep = rep,
      metrics = list(
        time_sec = m$time_sec,
        rss_mb = m$rss_mb,
        bytes = project_size,
        n_rows = size_cfg$n_rows,
        n_cols = size_cfg$n_cols
      ),
      env = env_info
    )

    ol_eval_write_jsonl(record, output_file)
    results <- c(results, list(record))
  }

  results
}

#' W0-2: put(object) benchmark
#' @keywords internal
.ol_eval_W0_2_put_object <- function(lake, size_name, config, env_info, output_file) {
  n_reps <- config$reps$bench
  results <- list()

  for (rep in seq_len(n_reps)) {
    # Generate object
    obj <- ol_eval_generate_object(size = size_name, seed = config$seed + rep)
    obj_name <- paste0("bench_object_", rep)

    m <- ol_eval_measure({
      lake$put(obj_name, obj)
    }, measure_memory = TRUE)

    record <- ol_eval_result(
      workload = "W0-2",
      variant = "omicslake",
      size = size_name,
      cache = "na",
      rep = rep,
      metrics = list(
        time_sec = m$time_sec,
        rss_mb = m$rss_mb
      ),
      env = env_info
    )

    ol_eval_write_jsonl(record, output_file)
    results <- c(results, list(record))
  }

  results
}

#' W0-3: tag/snap benchmark
#' @keywords internal
.ol_eval_W0_3_tag_snap <- function(lake, size_name, config, env_info, output_file) {
  n_reps <- config$reps$bench
  results <- list()

  # Ensure we have a table to tag
  if (!"bench_table_1" %in% lake$tables()$table_name) {
    df <- ol_eval_generate_table(1000, 10, seed = config$seed)
    lake$put("bench_table_1", df)
  }

  for (rep in seq_len(n_reps)) {
    # Measure tag
    tag_name <- paste0("v", rep)

    size_before <- .ol_eval_get_project_size(config$project_root, lake)

    m_tag <- ol_eval_measure({
      lake$tag("bench_table_1", tag_name)
    })

    size_after_tag <- .ol_eval_get_project_size(config$project_root, lake)

    record_tag <- ol_eval_result(
      workload = "W0-3-tag",
      variant = "omicslake",
      size = size_name,
      cache = "na",
      rep = rep,
      metrics = list(
        time_sec = m_tag$time_sec,
        bytes_delta = size_after_tag - size_before
      ),
      env = env_info
    )

    ol_eval_write_jsonl(record_tag, output_file)

    # Measure snap
    snap_label <- paste0("S", rep)

    size_before_snap <- size_after_tag

    m_snap <- ol_eval_measure({
      lake$snap(snap_label, note = paste("Benchmark snapshot", rep))
    })

    size_after_snap <- .ol_eval_get_project_size(config$project_root, lake)

    record_snap <- ol_eval_result(
      workload = "W0-3-snap",
      variant = "omicslake",
      size = size_name,
      cache = "na",
      rep = rep,
      metrics = list(
        time_sec = m_snap$time_sec,
        bytes_delta = size_after_snap - size_before_snap
      ),
      env = env_info
    )

    ol_eval_write_jsonl(record_snap, output_file)
    results <- c(results, list(record_tag), list(record_snap))
  }

  results
}

# ============================================================================
# W1: Query Benchmarks (Lazy/Pushdown)
# ============================================================================

#' Run W1 workloads
#' @keywords internal
.ol_eval_run_W1 <- function(config, env_info, output_file) {
  results <- list()

  for (size_name in names(config$sizes)) {
    size_cfg <- config$sizes[[size_name]]

    message("  W1 size: ", size_name)

    # Create fresh lake
    project_name <- paste0("eval_w1_", size_name, "_", Sys.getpid())
    lake <- Lake$new(project_name, root = config$project_root)

    tryCatch({
      # Setup: put test table
      df <- ol_eval_generate_table(size_cfg$n_rows, size_cfg$n_cols,
                                    seed = config$seed, type = "mixed")
      lake$put("main", df)

      # Create dimension table for joins
      dim_df <- ol_eval_generate_dim_table(min(size_cfg$n_rows, 10000),
                                            seed = config$seed)
      lake$put("dim", dim_df)

      # Tag for version-aware tests
      lake$tag("main", "v1")
      lake$tag("dim", "v1")

      # W1-1: get with where/select and pushdown
      results <- c(results, .ol_eval_W1_1_get_pushdown(
        lake, size_name, config, env_info, output_file
      ))

      # W1-2: ref + dplyr pipeline
      results <- c(results, .ol_eval_W1_2_ref_dplyr(
        lake, size_name, config, env_info, output_file
      ))

      # W1-3: joins (2-parent and 3-parent)
      results <- c(results, .ol_eval_W1_3_joins(
        lake, size_name, config, env_info, output_file
      ))

      # Run baselines if configured
      if (isTRUE(config$baselines$B1_duckdb_dbplyr)) {
        results <- c(results, .ol_eval_W1_baseline_duckdb(
          lake, df, size_name, config, env_info, output_file
        ))
      }

    }, finally = {
      .ol_eval_cleanup_project(config$project_root, project_name)
    })
  }

  results
}

#' W1-1: get with where/select pushdown
#' @keywords internal
.ol_eval_W1_1_get_pushdown <- function(lake, size_name, config, env_info, output_file) {
  n_reps <- config$reps$bench
  results <- list()

  for (rep in seq_len(n_reps)) {
    # Get with collect=FALSE to test pushdown
    m <- ol_eval_measure({
      lazy_result <- lake$get("main",
                               where = ~ num1 > 50,
                               select = c("id", "num1", "cat1"),
                               collect = FALSE)
      # Capture SQL for evidence
      sql <- tryCatch(dbplyr::sql_render(lazy_result), error = function(e) NA_character_)
      # Then collect
      result <- dplyr::collect(lazy_result)
      list(result = result, sql = sql)
    })

    sql_evidence <- m$result$sql

    record <- ol_eval_result(
      workload = "W1-1",
      variant = "omicslake",
      size = size_name,
      cache = "warm",
      rep = rep,
      metrics = list(
        time_sec = m$time_sec,
        n_rows = nrow(m$result$result)
      ),
      evidence = list(
        sql = sql_evidence,
        pushdown_valid = ol_eval_check_pushdown(sql_evidence, c("WHERE", "SELECT"))
      ),
      env = env_info
    )

    ol_eval_write_jsonl(record, output_file)
    results <- c(results, list(record))
  }

  results
}

#' W1-2: ref + dplyr pipeline
#' @keywords internal
.ol_eval_W1_2_ref_dplyr <- function(lake, size_name, config, env_info, output_file) {
  n_reps <- config$reps$bench
  results <- list()

  for (rep in seq_len(n_reps)) {
    m <- ol_eval_measure({
      lazy_result <- lake$ref("main") |>
        dplyr::filter(num1 > 50) |>
        dplyr::group_by(cat1) |>
        dplyr::summarise(mean_val = mean(num1, na.rm = TRUE), .groups = "drop")

      sql <- tryCatch(dbplyr::sql_render(lazy_result), error = function(e) NA_character_)
      result <- dplyr::collect(lazy_result)
      list(result = result, sql = sql)
    })

    sql_evidence <- m$result$sql

    record <- ol_eval_result(
      workload = "W1-2",
      variant = "omicslake",
      size = size_name,
      cache = "warm",
      rep = rep,
      metrics = list(
        time_sec = m$time_sec,
        n_rows = nrow(m$result$result)
      ),
      evidence = list(
        sql = sql_evidence,
        pushdown_valid = ol_eval_check_pushdown(sql_evidence, c("WHERE", "GROUP BY"))
      ),
      env = env_info
    )

    ol_eval_write_jsonl(record, output_file)
    results <- c(results, list(record))
  }

  results
}

#' W1-3: Join benchmarks
#' @keywords internal
.ol_eval_W1_3_joins <- function(lake, size_name, config, env_info, output_file) {
  n_reps <- config$reps$heavy
  results <- list()

  # Create third table for 3-parent join
  anno <- ol_eval_generate_annotation(1000, seed = config$seed)
  lake$put("anno", anno)
  lake$tag("anno", "v1")

  for (rep in seq_len(n_reps)) {
    # 2-parent join
    m2 <- ol_eval_measure({
      result <- lake$ref("main@tag(v1)") |>
        dplyr::inner_join(lake$ref("dim@tag(v1)"), by = "id") |>
        dplyr::collect()
      result
    })

    record_2p <- ol_eval_result(
      workload = "W1-3-2parent",
      variant = "omicslake",
      size = size_name,
      cache = "warm",
      rep = rep,
      metrics = list(
        time_sec = m2$time_sec,
        n_rows = nrow(m2$result)
      ),
      env = env_info
    )

    ol_eval_write_jsonl(record_2p, output_file)

    # 3-parent join (main + dim + anno via category matching)
    m3 <- ol_eval_measure({
      result <- lake$ref("main@tag(v1)") |>
        dplyr::inner_join(lake$ref("dim@tag(v1)"), by = "id") |>
        dplyr::left_join(
          lake$ref("anno@tag(v1)") |> dplyr::select(category, term),
          by = "category"
        ) |>
        dplyr::collect()
      result
    })

    record_3p <- ol_eval_result(
      workload = "W1-3-3parent",
      variant = "omicslake",
      size = size_name,
      cache = "warm",
      rep = rep,
      metrics = list(
        time_sec = m3$time_sec,
        n_rows = nrow(m3$result)
      ),
      env = env_info
    )

    ol_eval_write_jsonl(record_3p, output_file)
    results <- c(results, list(record_2p), list(record_3p))
  }

  results
}

#' W1 Baseline: Raw DuckDB/dbplyr
#' @keywords internal
.ol_eval_W1_baseline_duckdb <- function(lake, df, size_name, config, env_info, output_file) {
  n_reps <- config$reps$bench
  results <- list()

  # Create temporary DuckDB connection
  temp_db <- tempfile(fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), temp_db)

  tryCatch({
    # Write data directly
    DBI::dbWriteTable(con, "main", df, overwrite = TRUE)

    tbl_main <- dplyr::tbl(con, "main")

    for (rep in seq_len(n_reps)) {
      # Same query as W1-2
      m <- ol_eval_measure({
        result <- tbl_main |>
          dplyr::filter(num1 > 50) |>
          dplyr::group_by(cat1) |>
          dplyr::summarise(mean_val = mean(num1, na.rm = TRUE), .groups = "drop") |>
          dplyr::collect()
        result
      })

      record <- ol_eval_result(
        workload = "W1-2",
        variant = "baseline_duckdb",
        size = size_name,
        cache = "warm",
        rep = rep,
        metrics = list(
          time_sec = m$time_sec,
          n_rows = nrow(m$result)
        ),
        env = env_info
      )

      ol_eval_write_jsonl(record, output_file)
      results <- c(results, list(record))
    }

  }, finally = {
    DBI::dbDisconnect(con, shutdown = TRUE)
    unlink(temp_db)
  })

  results
}

# ============================================================================
# W2: Lineage Operations
# ============================================================================

#' Run W2 workloads
#' @keywords internal
.ol_eval_run_W2 <- function(config, env_info, output_file) {
  results <- list()

  message("  Setting up lineage chain...")

  project_name <- paste0("eval_w2_", Sys.getpid())
  lake <- Lake$new(project_name, root = config$project_root)

  tryCatch({
    # Create lineage chain: a -> b -> c -> d
    df_a <- ol_eval_generate_table(10000, 10, seed = config$seed)
    lake$put("a", df_a)
    lake$tag("a", "v1")

    # b depends on a
    df_b <- lake$ref("a@tag(v1)") |>
      dplyr::filter(x1 > 0) |>
      dplyr::collect()
    attr(df_b, "lake_sources") <- list(list(name = "a", ref = "@tag(v1)"))
    lake$put("b", df_b)
    lake$tag("b", "v1")

    # c depends on b
    df_c <- lake$ref("b@tag(v1)") |>
      dplyr::mutate(derived = x1 * 2) |>
      dplyr::collect()
    attr(df_c, "lake_sources") <- list(list(name = "b", ref = "@tag(v1)"))
    lake$put("c", df_c)

    # d depends on a and c (multi-parent)
    df_d <- dplyr::inner_join(
      lake$ref("a@tag(v1)") |> dplyr::select(id, x1),
      lake$ref("c") |> dplyr::select(id, derived),
      by = "id"
    ) |> dplyr::collect()
    attr(df_d, "lake_sources") <- list(
      list(name = "a", ref = "@tag(v1)"),
      list(name = "c", ref = "@latest")
    )
    lake$put("d", df_d)

    # W2-1: deps/tree performance
    results <- c(results, .ol_eval_W2_1_deps_tree(
      lake, config, env_info, output_file
    ))

    # W2-2: impact (downstream) analysis
    results <- c(results, .ol_eval_W2_2_impact(
      lake, config, env_info, output_file
    ))

  }, finally = {
    .ol_eval_cleanup_project(config$project_root, project_name)
  })

  results
}

#' W2-1: deps/tree benchmark
#' @keywords internal
.ol_eval_W2_1_deps_tree <- function(lake, config, env_info, output_file) {
  n_reps <- config$reps$bench
  results <- list()

  for (rep in seq_len(n_reps)) {
    # deps (upstream)
    m_deps <- ol_eval_measure({
      lake$deps("d", direction = "up")
    })

    deps_result <- m_deps$result

    record_deps <- ol_eval_result(
      workload = "W2-1-deps",
      variant = "omicslake",
      size = "standard",
      cache = "na",
      rep = rep,
      metrics = list(
        time_sec = m_deps$time_sec,
        n_rows = if (is.data.frame(deps_result)) nrow(deps_result) else 0
      ),
      evidence = list(
        has_parent_ref = "parent_ref" %in% names(deps_result),
        has_parent_version_id = "parent_version_id" %in% names(deps_result)
      ),
      env = env_info
    )

    ol_eval_write_jsonl(record_deps, output_file)

    # tree with varying depth
    for (depth in c(1, 3, 5, 10)) {
      m_tree <- ol_eval_measure({
        lake$tree("d", direction = "up", depth = depth)
      })

      record_tree <- ol_eval_result(
        workload = paste0("W2-1-tree-d", depth),
        variant = "omicslake",
        size = "standard",
        cache = "na",
        rep = rep,
        metrics = list(
          time_sec = m_tree$time_sec,
          n_rows = if (is.data.frame(m_tree$result)) nrow(m_tree$result) else 0
        ),
        env = env_info
      )

      ol_eval_write_jsonl(record_tree, output_file)
      results <- c(results, list(record_tree))
    }

    results <- c(results, list(record_deps))
  }

  results
}

#' W2-2: impact analysis benchmark
#' @keywords internal
.ol_eval_W2_2_impact <- function(lake, config, env_info, output_file) {
  n_reps <- config$reps$bench
  results <- list()

  for (rep in seq_len(n_reps)) {
    m <- ol_eval_measure({
      lake$impact("a")
    })

    impact_result <- m$result

    record <- ol_eval_result(
      workload = "W2-2-impact",
      variant = "omicslake",
      size = "standard",
      cache = "na",
      rep = rep,
      metrics = list(
        time_sec = m$time_sec,
        n_rows = if (is.data.frame(impact_result)) nrow(impact_result) else 0
      ),
      env = env_info
    )

    ol_eval_write_jsonl(record, output_file)
    results <- c(results, list(record))
  }

  results
}

# ============================================================================
# Helper Functions
# ============================================================================

#' Get project size in bytes
#' @keywords internal
.ol_eval_get_project_size <- function(root, lake) {
  project_path <- file.path(path.expand(root), lake$.__enclos_env__$private$.project)
  if (!dir.exists(project_path)) {
    return(0)
  }
  sum(file.info(list.files(project_path, recursive = TRUE, full.names = TRUE))$size,
      na.rm = TRUE)
}

#' Cleanup evaluation project
#' @keywords internal
.ol_eval_cleanup_project <- function(root, project_name) {
  project_path <- file.path(path.expand(root), project_name)
  if (dir.exists(project_path)) {
    unlink(project_path, recursive = TRUE)
  }
}
