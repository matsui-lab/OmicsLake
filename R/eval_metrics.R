#' @title OmicsLake Evaluation Metrics
#' @description Functions for configuration loading, metrics collection, and result output
#' @name eval_metrics
NULL

# ============================================================================
# Configuration Management
# ============================================================================

#' Load evaluation configuration from YAML
#'
#' @param config_path Path to YAML config file, or NULL to use default
#' @return Named list with configuration values
#' @export
#' @examples
#' \dontrun{
#' config <- ol_eval_load_config("inst/eval/configs/eval_small.yml")
#' }
ol_eval_load_config <- function(config_path = NULL) {
  # Default config values
  defaults <- list(
    project_root = "~/.omicslake_eval",
    seed = 1,
    threads = 4,
    sizes = list(
      small = list(n_rows = 100000, n_cols = 20),
      medium = list(n_rows = 1000000, n_cols = 50),
      large = list(n_rows = 10000000, n_cols = 20)
    ),
    reps = list(bench = 10, heavy = 5),
    cache_mode = list(enable = TRUE, modes = c("cold", "warm")),
    workloads = list(
      W0_io = TRUE, W1_queries = TRUE, W2_lineage = TRUE, W3_case_study = TRUE
    ),
    baselines = list(B1_duckdb_dbplyr = TRUE, B2_file_based = TRUE),
    outputs = list(results_dir = "inst/eval/results", formats = c("jsonl", "csv")),
    case_study = list(n_genes = 20000, n_samples = 6),
    validation = list(check_pushdown = TRUE, check_lineage = TRUE, strict_mode = FALSE)
  )

  if (is.null(config_path)) {
    return(defaults)
  }

  if (!file.exists(config_path)) {
    stop("Config file not found: ", config_path, call. = FALSE)
  }

  # Load YAML
  if (!requireNamespace("yaml", quietly = TRUE)) {
    # Fallback to jsonlite if yaml not available
    warning("yaml package not available, using defaults", call. = FALSE)
    return(defaults)
  }

  user_config <- yaml::read_yaml(config_path)

  # Merge with defaults (user config overrides defaults)
  config <- .ol_merge_config(defaults, user_config)

  # Expand paths

  config$project_root <- path.expand(config$project_root)
  config$outputs$results_dir <- path.expand(config$outputs$results_dir)

  config
}

#' Merge two configuration lists (recursive)
#' @keywords internal
.ol_merge_config <- function(defaults, override) {
  if (is.null(override)) return(defaults)

  result <- defaults
  for (name in names(override)) {
    if (is.list(defaults[[name]]) && is.list(override[[name]])) {
      result[[name]] <- .ol_merge_config(defaults[[name]], override[[name]])
    } else {
      result[[name]] <- override[[name]]
    }
  }
  result
}

# ============================================================================
# Environment Information
# ============================================================================

#' Capture environment information for reproducibility
#'
#' @return Named list with environment details
#' @export
ol_eval_env_info <- function() {
  list(
    r_version = paste(R.version$major, R.version$minor, sep = "."),
    platform = R.version$platform,
    os = Sys.info()[["sysname"]],
    os_version = Sys.info()[["release"]],
    packages = list(
      OmicsLake = as.character(packageVersion("OmicsLake")),
      duckdb = as.character(packageVersion("duckdb")),
      dplyr = as.character(packageVersion("dplyr")),
      dbplyr = as.character(packageVersion("dbplyr")),
      arrow = as.character(packageVersion("arrow")),
      R6 = as.character(packageVersion("R6"))
    ),
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )
}

# ============================================================================
# Metrics Collection
# ============================================================================

#' Create a new evaluation result record
#'
#' @param workload Workload identifier (e.g., "W0-1", "W1-2")
#' @param variant Implementation variant (e.g., "omicslake", "baseline_duckdb")
#' @param size Data size used (e.g., "small", "medium", "large")
#' @param cache Cache mode (e.g., "cold", "warm", "na")
#' @param rep Repetition number
#' @param metrics Named list of metric values
#' @param evidence Optional named list with evidence (SQL, notes)
#' @param env Optional environment info (auto-captured if NULL)
#' @return A result record list
#' @export
ol_eval_result <- function(workload, variant, size, cache = "na", rep = 1,
                           metrics = list(), evidence = list(), env = NULL) {
  if (is.null(env)) {
    env <- ol_eval_env_info()
  }

  list(
    run_id = .ol_generate_uuid(),
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    workload = workload,
    variant = variant,
    size = size,
    cache = cache,
    rep = rep,
    metrics = metrics,
    env = env,
    evidence = evidence
  )
}

#' Generate a simple UUID
#' @keywords internal
.ol_generate_uuid <- function() {
  paste0(
    sprintf("%08x", sample.int(.Machine$integer.max, 1)),
    "-",
    sprintf("%04x", sample.int(65535, 1)),
    "-",
    sprintf("%04x", sample.int(65535, 1)),
    "-",
    sprintf("%04x", sample.int(65535, 1)),
    "-",
    sprintf("%012x", sample.int(.Machine$integer.max, 1))
  )
}

#' Measure execution time and optionally peak RSS
#'
#' @param expr Expression to evaluate
#' @param measure_memory Whether to attempt memory measurement
#' @return List with time_sec, result, and optionally rss_mb
#' @export
ol_eval_measure <- function(expr, measure_memory = FALSE) {
  expr_sub <- substitute(expr)

  # Memory before (if requested)
  mem_before <- if (measure_memory) {
    gc(verbose = FALSE, reset = TRUE)
    sum(gc()[, 2])  # Used memory in MB
  } else NULL

  # Time execution
  start_time <- Sys.time()
  result <- eval(expr_sub, envir = parent.frame())
  end_time <- Sys.time()

  time_sec <- as.numeric(difftime(end_time, start_time, units = "secs"))

  # Memory after
  mem_after <- if (measure_memory) {
    sum(gc()[, 2])
  } else NULL

  output <- list(
    time_sec = time_sec,
    result = result
  )

  if (measure_memory) {
    output$rss_mb <- max(0, mem_after - mem_before)
  }

  output
}

#' Measure with multiple repetitions
#'
#' @param expr Expression to evaluate
#' @param n Number of repetitions
#' @param warmup Number of warmup runs (not recorded)
#' @param measure_memory Whether to measure memory
#' @return List with times, median, iqr, min, max
#' @export
ol_eval_bench <- function(expr, n = 10, warmup = 1, measure_memory = FALSE) {
  expr_sub <- substitute(expr)

  # Warmup runs
  for (i in seq_len(warmup)) {
    eval(expr_sub, envir = parent.frame())
  }

  # Measured runs
  times <- numeric(n)
  memory <- if (measure_memory) numeric(n) else NULL

  for (i in seq_len(n)) {
    m <- ol_eval_measure(eval(expr_sub, envir = parent.frame()),
                         measure_memory = measure_memory)
    times[i] <- m$time_sec
    if (measure_memory) memory[i] <- m$rss_mb
  }

  result <- list(
    times = times,
    n = n,
    median = median(times),
    iqr = IQR(times),
    min = min(times),
    max = max(times)
  )

  if (measure_memory) {
    result$memory <- memory
    result$memory_median <- median(memory)
  }

  result
}

# ============================================================================
# Result Output
# ============================================================================

#' Write a result record to JSONL file
#'
#' @param record Result record from ol_eval_result()
#' @param file Path to JSONL output file
#' @param append Whether to append to existing file
#' @export
ol_eval_write_jsonl <- function(record, file, append = TRUE) {
  # Ensure directory exists
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)

  # Convert to JSON line
  json_line <- jsonlite::toJSON(record, auto_unbox = TRUE)

  # Write/append
  cat(json_line, "\n", file = file, append = append)

  invisible(record)
}

#' Read results from JSONL file
#'
#' @param file Path to JSONL file
#' @return List of result records
#' @export
ol_eval_read_jsonl <- function(file) {
  if (!file.exists(file)) {
    stop("Results file not found: ", file, call. = FALSE)
  }

  lines <- readLines(file, warn = FALSE)
  lines <- lines[nzchar(trimws(lines))]

  lapply(lines, jsonlite::fromJSON)
}

#' Aggregate results and write summary CSV
#'
#' @param jsonl_file Path to JSONL results file
#' @param csv_file Output CSV file path
#' @return Data frame with aggregated results
#' @export
ol_eval_aggregate_results <- function(jsonl_file, csv_file = NULL) {
  records <- ol_eval_read_jsonl(jsonl_file)

  if (length(records) == 0) {
    warning("No records found in ", jsonl_file, call. = FALSE)
    return(data.frame())
  }

  # Extract key fields and metrics
  rows <- lapply(records, function(r) {
    data.frame(
      workload = r$workload,
      variant = r$variant,
      size = r$size,
      cache = r$cache,
      rep = r$rep,
      time_sec = r$metrics$time_sec %||% NA_real_,
      rss_mb = r$metrics$rss_mb %||% NA_real_,
      bytes_delta = r$metrics$bytes_delta %||% NA_real_,
      n_rows = r$metrics$n_rows %||% NA_integer_,
      stringsAsFactors = FALSE
    )
  })

  df <- do.call(rbind, rows)

  # Aggregate by (workload, variant, size, cache)
  agg <- aggregate(
    time_sec ~ workload + variant + size + cache,
    data = df,
    FUN = function(x) {
      c(median = median(x, na.rm = TRUE),
        iqr = IQR(x, na.rm = TRUE),
        min = min(x, na.rm = TRUE),
        max = max(x, na.rm = TRUE),
        n = length(x))
    }
  )

  # Flatten the matrix column
  result <- cbind(
    agg[, c("workload", "variant", "size", "cache")],
    as.data.frame(agg$time_sec)
  )

  if (!is.null(csv_file)) {
    dir.create(dirname(csv_file), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(result, csv_file, row.names = FALSE)
  }

  result
}

#' Null coalescing operator
#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x

# ============================================================================
# Validation Helpers
# ============================================================================
#' Check if SQL contains pushdown evidence
#'
#' @param sql SQL query string
#' @param expected_clauses Character vector of expected clauses (e.g., "WHERE", "SELECT")
#' @return Logical indicating whether all expected clauses are present
#' @export
ol_eval_check_pushdown <- function(sql, expected_clauses = c("WHERE")) {
  if (is.null(sql) || !is.character(sql)) {
    return(FALSE)
  }

  sql_upper <- toupper(sql)
  all(vapply(toupper(expected_clauses), function(clause) {
    grepl(clause, sql_upper)
  }, logical(1)))
}

#' Validate lineage record completeness
#'
#' @param deps Data frame from lake$deps() or tree()
#' @param expected_parents Character vector of expected parent names
#' @param check_version_info Whether to verify parent_ref and parent_version_id
#' @return List with valid (logical), missing (character), and issues (character)
#' @export
ol_eval_check_lineage <- function(deps, expected_parents,
                                  check_version_info = TRUE) {
  if (is.null(deps) || nrow(deps) == 0) {
    return(list(
      valid = FALSE,
      missing = expected_parents,
      issues = "No dependencies found"
    ))

  }

  # Check parent names
  found_parents <- unique(deps$parent_name)
  missing <- setdiff(expected_parents, found_parents)

  issues <- character(0)

  if (length(missing) > 0) {
    issues <- c(issues, paste("Missing parents:", paste(missing, collapse = ", ")))
  }

  # Check version info if requested
  if (check_version_info) {
    if (!"parent_ref" %in% names(deps)) {
      issues <- c(issues, "Missing parent_ref column")
    } else if (any(is.na(deps$parent_ref))) {
      issues <- c(issues, "Some parent_ref values are NA")
    }

    if (!"parent_version_id" %in% names(deps)) {
      issues <- c(issues, "Missing parent_version_id column")
    }
  }

  list(
    valid = length(missing) == 0 && length(issues) == 0,
    missing = missing,
    issues = issues
  )
}
