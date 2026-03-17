#!/usr/bin/env Rscript
# ==============================================================================
# OmicsLake Actual Performance Benchmarks
# ==============================================================================
# Response to reviewer criticism: "Table 3 based on estimates is fatal"
#
# Requirements addressed:
# - Actual measurements (not estimates)
# - Same machine, same conditions
# - n >= 30 iterations
# - IQR and 95% run-interval reporting
# - Raw logs and scripts published
# - Extended comparison targets: arrow, fst, qs, data.table
#
# Author: OmicsLake Development Team
# Date: 2026-02
# ==============================================================================

# ==============================================================================
# CONFIGURATION
# ==============================================================================

CONFIG <- list(
  # Iteration settings (reviewer requirement: n >= 30)
  n_iterations = 30,
  n_warmup = 5,  # Excluded from analysis

  # Random seed for reproducibility
  seed = 42,

  # Data sizes
  import_size_mb = 100,
  aggregation_rows = 1e6,
  join_rows = 1e6,

  # Statistical thresholds
  alpha = 0.05,

  # Output directory
  output_dir = "results"
)

# ==============================================================================
# PACKAGE SETUP AND AVAILABILITY CHECK
# ==============================================================================

cat("==============================================================\n")
cat("  OmicsLake Actual Performance Benchmarks\n")
cat("  Reviewer Response: Actual measurements, n=30, with interval/IQR\n")
cat("==============================================================\n\n")

# Required packages
required_packages <- c("OmicsLake", "arrow", "duckdb", "dplyr", "DBI")
optional_packages <- c("fst", "qs", "data.table", "bench")

# Check required packages
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Required package '%s' is not installed", pkg), call. = FALSE)
  }
}

# Load required packages
suppressPackageStartupMessages({
  library(OmicsLake)
  library(arrow)
  library(duckdb)
  library(dplyr)
  library(DBI)
})

# Check optional packages and record availability
pkg_available <- list()
for (pkg in optional_packages) {
  pkg_available[[pkg]] <- requireNamespace(pkg, quietly = TRUE)
  if (pkg_available[[pkg]]) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  } else {
    cat(sprintf("Note: Optional package '%s' not available - skipping those benchmarks\n", pkg))
  }
}

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Initialize benchmark logging
init_benchmark_log <- function(output_dir) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  log_file <- file.path(output_dir, "benchmark_log.txt")

  # Write session info header
  sink(log_file)
  cat("=== OmicsLake Benchmark Log ===\n")
  cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n")
  cat("Seed:", CONFIG$seed, "\n")
  cat("Iterations:", CONFIG$n_iterations, "\n")
  cat("Warmup:", CONFIG$n_warmup, "\n\n")
  cat("=== Session Info ===\n")
  print(sessionInfo())
  cat("\n=== Package Versions ===\n")
  for (pkg in c(required_packages, optional_packages)) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      cat(sprintf("%s: %s\n", pkg, as.character(packageVersion(pkg))))
    }
  }
  cat("\n=== System Info ===\n")
  cat("Platform:", R.version$platform, "\n")
  cat("OS:", Sys.info()["sysname"], Sys.info()["release"], "\n")
  cat("CPU Cores:", parallel::detectCores(), "\n")
  cat("Memory: See R memory limit\n\n")
  sink()

  log_file
}

#' Append to benchmark log
log_benchmark <- function(log_file, message) {
  cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), message),
      file = log_file, append = TRUE)
}

#' Execute benchmark with warmup and timing
run_benchmark <- function(name, expr, n_iterations, n_warmup,
                          gc_before = TRUE, log_file = NULL,
                          envir = parent.frame()) {
  if (!is.null(log_file)) {
    log_benchmark(log_file, sprintf("Starting benchmark: %s", name))
  }

  # Warmup phase (discarded)
  for (i in seq_len(n_warmup)) {
    if (gc_before) gc(verbose = FALSE, full = TRUE)
    tryCatch(eval(expr, envir = envir), error = function(e) NULL)
  }

  # Measurement phase
  times <- numeric(n_iterations)
  for (i in seq_len(n_iterations)) {
    if (gc_before) gc(verbose = FALSE, full = TRUE)
    start_time <- Sys.time()
    ok <- tryCatch(
      {
        eval(expr, envir = envir)
        TRUE
      },
      error = function(e) FALSE
    )
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    times[i] <- if (isTRUE(ok)) elapsed else NA_real_

    # Progress indicator
    if (i %% 10 == 0) {
      cat(sprintf("  [%s] %d/%d completed\n", name, i, n_iterations))
    }
  }

  if (!is.null(log_file)) {
    log_benchmark(log_file, sprintf("Completed benchmark: %s (n=%d valid)",
                                     name, sum(!is.na(times))))
  }

  times
}

#' Calculate benchmark statistics (median, IQR, 95% run interval)
calc_stats <- function(times, name = "benchmark") {
  times <- times[!is.na(times)]
  n <- length(times)

  if (n == 0) {
    return(list(
      name = name,
      n = 0,
      median = NA,
      mean = NA,
      sd = NA,
      iqr = NA,
      q25 = NA,
      q75 = NA,
      ci_lower = NA,
      ci_upper = NA,
      min = NA,
      max = NA
    ))
  }

  # Calculate statistics
  med <- median(times)
  mn <- mean(times)
  std <- sd(times)
  iqr_val <- IQR(times)
  q25 <- quantile(times, 0.25)
  q75 <- quantile(times, 0.75)

  # Empirical 95% run interval (2.5th-97.5th percentiles)
  q025 <- quantile(times, 0.025)
  q975 <- quantile(times, 0.975)

  list(
    name = name,
    n = n,
    median = med,
    mean = mn,
    sd = std,
    iqr = iqr_val,
    q25 = q25,
    q75 = q75,
    # Keep legacy names for downstream compatibility.
    ci_lower = q025,
    ci_upper = q975,
    min = min(times),
    max = max(times),
    raw_times = times
  )
}

#' Calculate Cohen's d effect size
cohens_d <- function(x, y) {
  x <- x[!is.na(x)]
  y <- y[!is.na(y)]

  n1 <- length(x)
  n2 <- length(y)

  if (n1 < 2 || n2 < 2) return(list(d = NA, interpretation = "N/A"))

  m1 <- mean(x)
  m2 <- mean(y)
  v1 <- var(x)
  v2 <- var(y)

  # Pooled standard deviation
  pooled_sd <- sqrt(((n1 - 1) * v1 + (n2 - 1) * v2) / (n1 + n2 - 2))

  if (pooled_sd == 0) return(list(d = NA, interpretation = "N/A"))

  d <- (m1 - m2) / pooled_sd

  # Interpretation
  interpretation <- if (abs(d) < 0.2) "negligible"
                    else if (abs(d) < 0.5) "small"
                    else if (abs(d) < 0.8) "medium"
                    else "large"

  list(d = d, interpretation = interpretation)
}

#' Perform Wilcoxon signed-rank test
wilcox_compare <- function(x, y, paired = FALSE) {
  x <- x[!is.na(x)]
  y <- y[!is.na(y)]

  if (length(x) < 3 || length(y) < 3) {
    return(list(p_value = NA, significant = NA, method = "N/A"))
  }

  test <- tryCatch({
    wilcox.test(x, y, paired = paired, conf.int = TRUE)
  }, error = function(e) NULL)

  if (is.null(test)) {
    return(list(p_value = NA, significant = NA, method = "Wilcoxon"))
  }

  list(
    p_value = test$p.value,
    significant = test$p.value < CONFIG$alpha,
    method = "Wilcoxon"
  )
}

# ==============================================================================
# BENCHMARK 1: TABLE IMPORT (100MB)
# ==============================================================================

benchmark_import <- function(log_file) {
  cat("\n--- Benchmark 1: Table Import (100MB) ---\n")
  log_benchmark(log_file, "=== BENCHMARK 1: TABLE IMPORT ===")

  # Set working directory to paper directory
  paper_dir <- if (basename(getwd()) == "paper") {
    getwd()
  } else if (file.exists("inst/paper")) {
    "inst/paper"
  } else {
    stop("Cannot find paper directory")
  }

  # Generate test data if not exists
  data_dir <- file.path(paper_dir, "benchmark_datasets")
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
  }

  parquet_file <- file.path(data_dir, "dataset_100MB.parquet")
  rds_file <- file.path(data_dir, "rds_100MB.RDS")
  fst_file <- file.path(data_dir, "dataset_100MB.fst")
  qs_file <- file.path(data_dir, "dataset_100MB.qs")
  csv_file <- file.path(data_dir, "dataset_100MB.csv")

  # Generate data if needed
  if (!file.exists(parquet_file)) {
    cat("  Generating test data...\n")
    set.seed(CONFIG$seed)
    n_rows <- 1e6
    test_data <- data.frame(
      id = 1:n_rows,
      gene_id = paste0("gene_", sample(1:20000, n_rows, replace = TRUE)),
      sample_id = paste0("sample_", sample(1:100, n_rows, replace = TRUE)),
      expression = rnorm(n_rows, mean = 50, sd = 20),
      pvalue = runif(n_rows),
      log2fc = rnorm(n_rows, mean = 0, sd = 2),
      category = sample(c("up", "down", "unchanged"), n_rows, replace = TRUE),
      annotation = sample(c("protein_coding", "lncRNA", "miRNA"), n_rows, replace = TRUE),
      chromosome = paste0("chr", sample(1:22, n_rows, replace = TRUE)),
      start_pos = sample(1:250000000, n_rows, replace = TRUE),
      stringsAsFactors = FALSE
    )

    # Write in all formats
    arrow::write_parquet(test_data, parquet_file)
    saveRDS(test_data, rds_file)

    if (pkg_available$fst) {
      fst::write_fst(test_data, fst_file)
    }
    if (pkg_available$qs) {
      qs::qsave(test_data, qs_file)
    }

    # CSV for data.table
    if (pkg_available$data.table) {
      data.table::fwrite(test_data, csv_file)
    }

    rm(test_data)
    gc()
  }

  results <- list()

  # --- Read benchmarks ---
  cat("  Benchmarking READ operations...\n")

  # 1. arrow::read_parquet
  cat("  [1/6] arrow::read_parquet\n")
  results$parquet_read <- calc_stats(
    run_benchmark("parquet_read",
                  quote(df <- arrow::read_parquet(parquet_file)),
                  CONFIG$n_iterations, CONFIG$n_warmup, log_file = log_file),
    "arrow::read_parquet"
  )

  # 2. readRDS
  cat("  [2/6] readRDS\n")
  results$rds_read <- calc_stats(
    run_benchmark("rds_read",
                  quote(df <- readRDS(rds_file)),
                  CONFIG$n_iterations, CONFIG$n_warmup, log_file = log_file),
    "readRDS"
  )

  # 3. fst::read_fst
  if (pkg_available$fst && file.exists(fst_file)) {
    cat("  [3/6] fst::read_fst\n")
    results$fst_read <- calc_stats(
      run_benchmark("fst_read",
                    quote(df <- fst::read_fst(fst_file)),
                    CONFIG$n_iterations, CONFIG$n_warmup, log_file = log_file),
      "fst::read_fst"
    )
  }

  # 4. qs::qread
  if (pkg_available$qs && file.exists(qs_file)) {
    cat("  [4/6] qs::qread\n")
    results$qs_read <- calc_stats(
      run_benchmark("qs_read",
                    quote(df <- qs::qread(qs_file)),
                    CONFIG$n_iterations, CONFIG$n_warmup, log_file = log_file),
      "qs::qread"
    )
  }

  # 5. data.table::fread
  if (pkg_available$data.table && file.exists(csv_file)) {
    cat("  [5/6] data.table::fread\n")
    results$dt_read <- calc_stats(
      run_benchmark("dt_read",
                    quote(df <- data.table::fread(csv_file)),
                    CONFIG$n_iterations, CONFIG$n_warmup, log_file = log_file),
      "data.table::fread"
    )
  }

  # --- Write benchmarks ---
  cat("  Benchmarking WRITE operations...\n")

  # Load data for write tests
  test_data <- arrow::read_parquet(parquet_file)

  tmp_parquet <- tempfile(fileext = ".parquet")
  tmp_rds <- tempfile(fileext = ".rds")
  tmp_fst <- tempfile(fileext = ".fst")
  tmp_qs <- tempfile(fileext = ".qs")
  tmp_csv <- tempfile(fileext = ".csv")

  # 1. arrow::write_parquet
  cat("  [1/5] arrow::write_parquet\n")
  results$parquet_write <- calc_stats(
    run_benchmark("parquet_write",
                  quote(arrow::write_parquet(test_data, tmp_parquet)),
                  CONFIG$n_iterations, CONFIG$n_warmup, log_file = log_file),
    "arrow::write_parquet"
  )

  # 2. saveRDS
  cat("  [2/5] saveRDS\n")
  results$rds_write <- calc_stats(
    run_benchmark("rds_write",
                  quote(saveRDS(test_data, tmp_rds)),
                  CONFIG$n_iterations, CONFIG$n_warmup, log_file = log_file),
    "saveRDS"
  )

  # 3. fst::write_fst
  if (pkg_available$fst) {
    cat("  [3/5] fst::write_fst\n")
    results$fst_write <- calc_stats(
      run_benchmark("fst_write",
                    quote(fst::write_fst(test_data, tmp_fst)),
                    CONFIG$n_iterations, CONFIG$n_warmup, log_file = log_file),
      "fst::write_fst"
    )
  }

  # 4. qs::qsave
  if (pkg_available$qs) {
    cat("  [4/5] qs::qsave\n")
    results$qs_write <- calc_stats(
      run_benchmark("qs_write",
                    quote(qs::qsave(test_data, tmp_qs)),
                    CONFIG$n_iterations, CONFIG$n_warmup, log_file = log_file),
      "qs::qsave"
    )
  }

  # 5. data.table::fwrite
  if (pkg_available$data.table) {
    cat("  [5/5] data.table::fwrite\n")
    results$dt_write <- calc_stats(
      run_benchmark("dt_write",
                    quote(data.table::fwrite(test_data, tmp_csv)),
                    CONFIG$n_iterations, CONFIG$n_warmup, log_file = log_file),
      "data.table::fwrite"
    )
  }

  # Cleanup
  unlink(c(tmp_parquet, tmp_rds, tmp_fst, tmp_qs, tmp_csv))
  rm(test_data)
  gc()

  # Calculate file sizes
  results$file_sizes <- data.frame(
    format = c("Parquet", "RDS"),
    size_mb = c(
      file.size(parquet_file) / 1024^2,
      file.size(rds_file) / 1024^2
    ),
    stringsAsFactors = FALSE
  )

  if (pkg_available$fst && file.exists(fst_file)) {
    results$file_sizes <- rbind(results$file_sizes,
                                 data.frame(format = "FST",
                                           size_mb = file.size(fst_file) / 1024^2))
  }
  if (pkg_available$qs && file.exists(qs_file)) {
    results$file_sizes <- rbind(results$file_sizes,
                                 data.frame(format = "QS",
                                           size_mb = file.size(qs_file) / 1024^2))
  }
  if (pkg_available$data.table && file.exists(csv_file)) {
    results$file_sizes <- rbind(results$file_sizes,
                                 data.frame(format = "CSV",
                                           size_mb = file.size(csv_file) / 1024^2))
  }

  results
}

# ==============================================================================
# BENCHMARK 2: AGGREGATION (1M ROWS)
# ==============================================================================

benchmark_aggregation <- function(log_file) {
  cat("\n--- Benchmark 2: Aggregation (1M rows) ---\n")
  log_benchmark(log_file, "=== BENCHMARK 2: AGGREGATION ===")

  # Generate test data
  set.seed(CONFIG$seed)
  n_rows <- CONFIG$aggregation_rows

  cat("  Generating aggregation test data...\n")
  agg_data <- data.frame(
    gene_id = rep(1:1000, each = n_rows / 1000),
    sample1 = runif(n_rows, 0, 100),
    sample2 = runif(n_rows, 0, 100),
    sample3 = runif(n_rows, 0, 100),
    sample4 = runif(n_rows, 0, 100),
    sample5 = runif(n_rows, 0, 100),
    sample6 = runif(n_rows, 0, 100),
    stringsAsFactors = FALSE
  )

  # Initialize OmicsLake for DuckDB tests
  tmpdir <- tempfile(pattern = "agg_benchmark_")
  dir.create(tmpdir, recursive = TRUE)
  ol_init("agg_test", root = tmpdir)
  ol_write("agg_data", agg_data, mode = "overwrite")

  results <- list()

  # 1. dplyr (in-memory)
  cat("  [1/4] dplyr::summarise\n")
  results$dplyr <- calc_stats(
    run_benchmark("dplyr_agg",
                  quote({
                    result <- agg_data %>%
                      group_by(gene_id) %>%
                      summarise(
                        mean_s1 = mean(sample1),
                        mean_s2 = mean(sample2),
                        mean_s3 = mean(sample3),
                        mean_s4 = mean(sample4),
                        mean_s5 = mean(sample5),
                        mean_s6 = mean(sample6),
                        .groups = "drop"
                      )
                  }),
                  CONFIG$n_iterations, CONFIG$n_warmup, log_file = log_file),
    "dplyr::summarise"
  )

  # 2. OmicsLake/DuckDB via ol_aggregate
  cat("  [2/4] OmicsLake/DuckDB (ol_aggregate)\n")
  results$omicslake <- calc_stats(
    run_benchmark("ol_agg",
                  quote({
                    result <- ol_aggregate("agg_data",
                      group_by = "gene_id",
                      mean_s1 = list(func = "avg", col = "sample1"),
                      mean_s2 = list(func = "avg", col = "sample2"),
                      mean_s3 = list(func = "avg", col = "sample3"),
                      mean_s4 = list(func = "avg", col = "sample4"),
                      mean_s5 = list(func = "avg", col = "sample5"),
                      mean_s6 = list(func = "avg", col = "sample6")
                    )
                  }),
                  CONFIG$n_iterations, CONFIG$n_warmup, log_file = log_file),
    "OmicsLake::ol_aggregate"
  )

  # 3. data.table
  if (pkg_available$data.table) {
    cat("  [3/4] data.table aggregation\n")
    dt_agg <- data.table::as.data.table(agg_data)
    results$datatable <- calc_stats(
      run_benchmark("dt_agg",
                    quote({
                      result <- dt_agg[, .(
                        mean_s1 = mean(sample1),
                        mean_s2 = mean(sample2),
                        mean_s3 = mean(sample3),
                        mean_s4 = mean(sample4),
                        mean_s5 = mean(sample5),
                        mean_s6 = mean(sample6)
                      ), by = gene_id]
                    }),
                    CONFIG$n_iterations, CONFIG$n_warmup, log_file = log_file),
      "data.table"
    )
    rm(dt_agg)
  }

  # 4. base R aggregate
  cat("  [4/4] base::aggregate\n")
  results$base <- calc_stats(
    run_benchmark("base_agg",
                  quote({
                    result <- aggregate(
                      cbind(sample1, sample2, sample3, sample4, sample5, sample6) ~ gene_id,
                      data = agg_data,
                      FUN = mean
                    )
                  }),
                  CONFIG$n_iterations, CONFIG$n_warmup, log_file = log_file),
    "base::aggregate"
  )

  # Cleanup
  unlink(tmpdir, recursive = TRUE)
  rm(agg_data)
  gc()

  results
}

# ==============================================================================
# BENCHMARK 3: JOIN (1M x 1M)
# ==============================================================================

benchmark_join <- function(log_file) {
  cat("\n--- Benchmark 3: Join (1M x 1M rows) ---\n")
  log_benchmark(log_file, "=== BENCHMARK 3: JOIN ===")

  # Generate test data
  set.seed(CONFIG$seed)
  n_rows <- CONFIG$join_rows

  cat("  Generating join test data...\n")
  table1 <- data.frame(
    id = 1:n_rows,
    gene_name = paste0("gene_", 1:n_rows),
    expression = rnorm(n_rows, mean = 100, sd = 30),
    condition = sample(c("control", "treatment"), n_rows, replace = TRUE),
    stringsAsFactors = FALSE
  )

  table2 <- data.frame(
    id = sample(1:n_rows, n_rows, replace = FALSE),  # Shuffled
    annotation = sample(c("pathway_A", "pathway_B", "pathway_C", "pathway_D"), n_rows, replace = TRUE),
    go_term = paste0("GO:", sample(1000:9999, n_rows, replace = TRUE)),
    protein_length = sample(50:2000, n_rows, replace = TRUE),
    stringsAsFactors = FALSE
  )

  # Initialize OmicsLake for DuckDB tests
  tmpdir <- tempfile(pattern = "join_benchmark_")
  dir.create(tmpdir, recursive = TRUE)
  ol_init("join_test", root = tmpdir)
  ol_write("t1", table1, mode = "overwrite")
  ol_write("t2", table2, mode = "overwrite")

  results <- list()

  # 1. base::merge
  cat("  [1/4] base::merge\n")
  results$base_merge <- calc_stats(
    run_benchmark("base_merge",
                  quote(result <- base::merge(table1, table2, by = "id")),
                  CONFIG$n_iterations, CONFIG$n_warmup, log_file = log_file),
    "base::merge"
  )

  # 2. dplyr::inner_join
  cat("  [2/4] dplyr::inner_join\n")
  results$dplyr_join <- calc_stats(
    run_benchmark("dplyr_join",
                  quote(result <- dplyr::inner_join(table1, table2, by = "id")),
                  CONFIG$n_iterations, CONFIG$n_warmup, log_file = log_file),
    "dplyr::inner_join"
  )

  # 3. OmicsLake/DuckDB SQL join
  cat("  [3/4] OmicsLake/DuckDB SQL\n")
  results$duckdb <- calc_stats(
    run_benchmark("duckdb_join",
                  quote(result <- ol_query("SELECT * FROM t1 INNER JOIN t2 USING (id)")),
                  CONFIG$n_iterations, CONFIG$n_warmup, log_file = log_file),
    "OmicsLake::ol_query (DuckDB)"
  )

  # 4. data.table
  if (pkg_available$data.table) {
    cat("  [4/4] data.table join\n")
    dt1 <- data.table::as.data.table(table1)
    dt2 <- data.table::as.data.table(table2)
    data.table::setkey(dt1, id)
    data.table::setkey(dt2, id)

    results$datatable_join <- calc_stats(
      run_benchmark("dt_join",
                    quote(result <- dt1[dt2, nomatch = NULL]),
                    CONFIG$n_iterations, CONFIG$n_warmup, log_file = log_file),
      "data.table"
    )
    rm(dt1, dt2)
  }

  # Cleanup
  unlink(tmpdir, recursive = TRUE)
  rm(table1, table2)
  gc()

  results
}

# ==============================================================================
# BENCHMARK 4: SNAPSHOT CREATION
# ==============================================================================

benchmark_snapshot <- function(log_file) {
  cat("\n--- Benchmark 4: Snapshot Creation ---\n")
  log_benchmark(log_file, "=== BENCHMARK 4: SNAPSHOT ===")

  # Generate test data
  set.seed(CONFIG$seed)
  n_rows <- 1e6

  cat("  Generating snapshot test data...\n")
  test_data <- data.frame(
    x = rnorm(n_rows),
    y = rnorm(n_rows),
    z = rnorm(n_rows),
    stringsAsFactors = FALSE
  )

  # Setup for file copy baseline
  tmpdir_filecopy <- tempfile(pattern = "filecopy_")
  dir.create(tmpdir_filecopy, recursive = TRUE)
  source_rds <- file.path(tmpdir_filecopy, "source.rds")
  saveRDS(test_data, source_rds)

  # Setup for OmicsLake snapshot benchmark
  tmpdir_ol <- tempfile(pattern = "ol_snapshot_")
  dir.create(tmpdir_ol, recursive = TRUE)
  lake <- Lake$new("snapshot_test", root = tmpdir_ol)
  lake$put("snap_data", test_data)

  results <- list()

  # 1. File copy (baseline)
  cat("  [1/2] File copy (baseline)\n")
  results$file_copy <- calc_stats(
    run_benchmark("file_copy",
                  quote({
                    dest_rds <- tempfile(fileext = ".rds")
                    file.copy(source_rds, dest_rds, overwrite = TRUE)
                    unlink(dest_rds)
                  }),
                  CONFIG$n_iterations, CONFIG$n_warmup, log_file = log_file),
    "file.copy"
  )

  # 2. OmicsLake snapshot
  cat("  [2/2] OmicsLake lake$snap\n")
  snapshot_counter <- 0
  results$ol_snapshot <- calc_stats(
    run_benchmark("ol_snapshot",
                  quote({
                    snapshot_counter <<- snapshot_counter + 1
                    label <- sprintf("benchsnap%04d", snapshot_counter)
                    lake$snap(label)
                  }),
                  CONFIG$n_iterations, CONFIG$n_warmup, log_file = log_file),
    "OmicsLake::snap"
  )

  # Cleanup
  rm(lake)
  gc()
  unlink(tmpdir_filecopy, recursive = TRUE)
  unlink(tmpdir_ol, recursive = TRUE)
  rm(test_data)
  gc()

  results
}

# ==============================================================================
# STATISTICAL ANALYSIS
# ==============================================================================

perform_statistical_analysis <- function(results) {
  cat("\n--- Statistical Analysis ---\n")

  analysis <- list()
  all_p_values <- numeric()
  comparisons <- character()

  # Import comparisons (Parquet vs others)
  if (!is.null(results$import$parquet_read) && !is.null(results$import$rds_read)) {
    test <- wilcox_compare(results$import$parquet_read$raw_times,
                           results$import$rds_read$raw_times)
    effect <- cohens_d(results$import$rds_read$raw_times,
                       results$import$parquet_read$raw_times)
    analysis$import_parquet_vs_rds <- list(
      test = test,
      effect = effect
    )
    all_p_values <- c(all_p_values, test$p_value)
    comparisons <- c(comparisons, "Import: Parquet vs RDS")
  }

  # Aggregation comparisons
  if (!is.null(results$aggregation$omicslake) && !is.null(results$aggregation$dplyr)) {
    test <- wilcox_compare(results$aggregation$omicslake$raw_times,
                           results$aggregation$dplyr$raw_times)
    effect <- cohens_d(results$aggregation$dplyr$raw_times,
                       results$aggregation$omicslake$raw_times)
    analysis$agg_omicslake_vs_dplyr <- list(
      test = test,
      effect = effect
    )
    all_p_values <- c(all_p_values, test$p_value)
    comparisons <- c(comparisons, "Aggregation: OmicsLake vs dplyr")
  }

  # Join comparisons
  if (!is.null(results$join$duckdb) && !is.null(results$join$base_merge)) {
    test <- wilcox_compare(results$join$duckdb$raw_times,
                           results$join$base_merge$raw_times)
    effect <- cohens_d(results$join$base_merge$raw_times,
                       results$join$duckdb$raw_times)
    analysis$join_duckdb_vs_base <- list(
      test = test,
      effect = effect
    )
    all_p_values <- c(all_p_values, test$p_value)
    comparisons <- c(comparisons, "Join: DuckDB vs base::merge")
  }

  # Snapshot comparisons
  if (!is.null(results$snapshot$ol_snapshot) && !is.null(results$snapshot$file_copy)) {
    test <- wilcox_compare(results$snapshot$ol_snapshot$raw_times,
                           results$snapshot$file_copy$raw_times)
    effect <- cohens_d(results$snapshot$file_copy$raw_times,
                       results$snapshot$ol_snapshot$raw_times)
    analysis$snapshot_ol_vs_filecopy <- list(
      test = test,
      effect = effect
    )
    all_p_values <- c(all_p_values, test$p_value)
    comparisons <- c(comparisons, "Snapshot: OmicsLake vs file copy")
  }

  # Bonferroni correction
  n_tests <- length(all_p_values)
  if (n_tests > 0) {
    adjusted_p <- pmin(all_p_values * n_tests, 1.0)
    analysis$bonferroni <- data.frame(
      comparison = comparisons,
      p_value = all_p_values,
      p_adjusted = adjusted_p,
      significant = adjusted_p < CONFIG$alpha,
      stringsAsFactors = FALSE
    )
  }

  analysis
}

# ==============================================================================
# OUTPUT GENERATION
# ==============================================================================

generate_outputs <- function(results, analysis, output_dir, log_file) {
  cat("\n--- Generating Output Files ---\n")

  # 1. Raw benchmark results CSV
  raw_results <- data.frame()

  # Import results
  for (method in names(results$import)) {
    if (method != "file_sizes" && !is.null(results$import[[method]]$raw_times)) {
      for (i in seq_along(results$import[[method]]$raw_times)) {
        raw_results <- rbind(raw_results, data.frame(
          benchmark = "import",
          method = results$import[[method]]$name,
          iteration = i,
          time_sec = results$import[[method]]$raw_times[i],
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  # Aggregation results
  for (method in names(results$aggregation)) {
    if (!is.null(results$aggregation[[method]]$raw_times)) {
      for (i in seq_along(results$aggregation[[method]]$raw_times)) {
        raw_results <- rbind(raw_results, data.frame(
          benchmark = "aggregation",
          method = results$aggregation[[method]]$name,
          iteration = i,
          time_sec = results$aggregation[[method]]$raw_times[i],
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  # Join results
  for (method in names(results$join)) {
    if (!is.null(results$join[[method]]$raw_times)) {
      for (i in seq_along(results$join[[method]]$raw_times)) {
        raw_results <- rbind(raw_results, data.frame(
          benchmark = "join",
          method = results$join[[method]]$name,
          iteration = i,
          time_sec = results$join[[method]]$raw_times[i],
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  # Snapshot results
  for (method in names(results$snapshot)) {
    if (!is.null(results$snapshot[[method]]$raw_times)) {
      for (i in seq_along(results$snapshot[[method]]$raw_times)) {
        raw_results <- rbind(raw_results, data.frame(
          benchmark = "snapshot",
          method = results$snapshot[[method]]$name,
          iteration = i,
          time_sec = results$snapshot[[method]]$raw_times[i],
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  write.csv(raw_results, file.path(output_dir, "actual_benchmarks.csv"), row.names = FALSE)
  cat("  - actual_benchmarks.csv (raw data)\n")

  # 2. Summary statistics CSV
  summary_stats <- data.frame()

  add_stats <- function(benchmark, stats) {
    if (is.null(stats)) return(NULL)
    data.frame(
      benchmark = benchmark,
      method = stats$name,
      n = stats$n,
      median_sec = round(stats$median, 4),
      mean_sec = round(stats$mean, 4),
      sd_sec = round(stats$sd, 4),
      iqr_sec = round(stats$iqr, 4),
      ci_lower = round(stats$ci_lower, 4),
      ci_upper = round(stats$ci_upper, 4),
      min_sec = round(stats$min, 4),
      max_sec = round(stats$max, 4),
      stringsAsFactors = FALSE
    )
  }

  # Import
  for (method in names(results$import)) {
    if (method != "file_sizes") {
      summary_stats <- rbind(summary_stats, add_stats("import", results$import[[method]]))
    }
  }

  # Aggregation
  for (method in names(results$aggregation)) {
    summary_stats <- rbind(summary_stats, add_stats("aggregation", results$aggregation[[method]]))
  }

  # Join
  for (method in names(results$join)) {
    summary_stats <- rbind(summary_stats, add_stats("join", results$join[[method]]))
  }

  # Snapshot
  for (method in names(results$snapshot)) {
    summary_stats <- rbind(summary_stats, add_stats("snapshot", results$snapshot[[method]]))
  }

  write.csv(summary_stats, file.path(output_dir, "benchmark_summary.csv"), row.names = FALSE)
  cat("  - benchmark_summary.csv (summary statistics)\n")

  # 3. Table 3 Final (paper format)
  table3 <- data.frame(
    Task = character(),
    OmicsLake_Method = character(),
    Baseline_Method = character(),
    OmicsLake_Median = character(),
    Baseline_Median = character(),
    Speedup = character(),
    p_value = character(),
    Effect_Size = character(),
    stringsAsFactors = FALSE
  )

  # Import row
  if (!is.null(results$import$parquet_read) && !is.null(results$import$rds_read)) {
    speedup <- results$import$rds_read$median / results$import$parquet_read$median
    p_val <- if (!is.null(analysis$import_parquet_vs_rds))
               sprintf("%.2e", analysis$import_parquet_vs_rds$test$p_value) else "N/A"
    effect <- if (!is.null(analysis$import_parquet_vs_rds))
                sprintf("%.2f (%s)", analysis$import_parquet_vs_rds$effect$d,
                        analysis$import_parquet_vs_rds$effect$interpretation) else "N/A"
    table3 <- rbind(table3, data.frame(
      Task = "Table Import (100MB)",
      OmicsLake_Method = "arrow::read_parquet",
      Baseline_Method = "readRDS",
      OmicsLake_Median = sprintf("%.3f s", results$import$parquet_read$median),
      Baseline_Median = sprintf("%.3f s", results$import$rds_read$median),
      Speedup = sprintf("%.2fx", speedup),
      p_value = p_val,
      Effect_Size = effect,
      stringsAsFactors = FALSE
    ))
  }

  # Aggregation row
  if (!is.null(results$aggregation$omicslake) && !is.null(results$aggregation$dplyr)) {
    speedup <- results$aggregation$dplyr$median / results$aggregation$omicslake$median
    p_val <- if (!is.null(analysis$agg_omicslake_vs_dplyr))
               sprintf("%.2e", analysis$agg_omicslake_vs_dplyr$test$p_value) else "N/A"
    effect <- if (!is.null(analysis$agg_omicslake_vs_dplyr))
                sprintf("%.2f (%s)", analysis$agg_omicslake_vs_dplyr$effect$d,
                        analysis$agg_omicslake_vs_dplyr$effect$interpretation) else "N/A"
    table3 <- rbind(table3, data.frame(
      Task = "Aggregation (1M rows)",
      OmicsLake_Method = "ol_aggregate (DuckDB)",
      Baseline_Method = "dplyr::summarise",
      OmicsLake_Median = sprintf("%.3f s", results$aggregation$omicslake$median),
      Baseline_Median = sprintf("%.3f s", results$aggregation$dplyr$median),
      Speedup = sprintf("%.2fx", speedup),
      p_value = p_val,
      Effect_Size = effect,
      stringsAsFactors = FALSE
    ))
  }

  # Join row
  if (!is.null(results$join$duckdb) && !is.null(results$join$base_merge)) {
    speedup <- results$join$base_merge$median / results$join$duckdb$median
    p_val <- if (!is.null(analysis$join_duckdb_vs_base))
               sprintf("%.2e", analysis$join_duckdb_vs_base$test$p_value) else "N/A"
    effect <- if (!is.null(analysis$join_duckdb_vs_base))
                sprintf("%.2f (%s)", analysis$join_duckdb_vs_base$effect$d,
                        analysis$join_duckdb_vs_base$effect$interpretation) else "N/A"
    table3 <- rbind(table3, data.frame(
      Task = "Join (1M x 1M)",
      OmicsLake_Method = "ol_query (DuckDB SQL)",
      Baseline_Method = "base::merge",
      OmicsLake_Median = sprintf("%.3f s", results$join$duckdb$median),
      Baseline_Median = sprintf("%.3f s", results$join$base_merge$median),
      Speedup = sprintf("%.2fx", speedup),
      p_value = p_val,
      Effect_Size = effect,
      stringsAsFactors = FALSE
    ))
  }

  # Snapshot row
  if (!is.null(results$snapshot$ol_snapshot) && !is.null(results$snapshot$file_copy)) {
    speedup <- results$snapshot$file_copy$median / results$snapshot$ol_snapshot$median
    p_val <- if (!is.null(analysis$snapshot_ol_vs_filecopy))
               sprintf("%.2e", analysis$snapshot_ol_vs_filecopy$test$p_value) else "N/A"
    effect <- if (!is.null(analysis$snapshot_ol_vs_filecopy))
                sprintf("%.2f (%s)", analysis$snapshot_ol_vs_filecopy$effect$d,
                        analysis$snapshot_ol_vs_filecopy$effect$interpretation) else "N/A"
    table3 <- rbind(table3, data.frame(
      Task = "Snapshot Creation",
      OmicsLake_Method = "lake$snap (backup + label)",
      Baseline_Method = "file.copy",
      OmicsLake_Median = sprintf("%.3f s", results$snapshot$ol_snapshot$median),
      Baseline_Median = sprintf("%.3f s", results$snapshot$file_copy$median),
      Speedup = sprintf("%.2fx", speedup),
      p_value = p_val,
      Effect_Size = effect,
      stringsAsFactors = FALSE
    ))
  }

  write.csv(table3, file.path(output_dir, "Table3_final.csv"), row.names = FALSE)
  cat("  - Table3_final.csv (paper format)\n")

  # 4. Extended comparison table
  extended_table <- data.frame()

  # All import methods
  for (method in names(results$import)) {
    if (method != "file_sizes" && !is.null(results$import[[method]])) {
      extended_table <- rbind(extended_table, data.frame(
        Benchmark = "Import (Read)",
        Method = results$import[[method]]$name,
        Median_sec = round(results$import[[method]]$median, 4),
        IQR_sec = round(results$import[[method]]$iqr, 4),
        Interval_95 = sprintf("[%.4f, %.4f]",
                              results$import[[method]]$ci_lower,
                              results$import[[method]]$ci_upper),
        N = results$import[[method]]$n,
        stringsAsFactors = FALSE
      ))
    }
  }

  # All aggregation methods
  for (method in names(results$aggregation)) {
    if (!is.null(results$aggregation[[method]])) {
      extended_table <- rbind(extended_table, data.frame(
        Benchmark = "Aggregation",
        Method = results$aggregation[[method]]$name,
        Median_sec = round(results$aggregation[[method]]$median, 4),
        IQR_sec = round(results$aggregation[[method]]$iqr, 4),
        Interval_95 = sprintf("[%.4f, %.4f]",
                              results$aggregation[[method]]$ci_lower,
                              results$aggregation[[method]]$ci_upper),
        N = results$aggregation[[method]]$n,
        stringsAsFactors = FALSE
      ))
    }
  }

  # All join methods
  for (method in names(results$join)) {
    if (!is.null(results$join[[method]])) {
      extended_table <- rbind(extended_table, data.frame(
        Benchmark = "Join",
        Method = results$join[[method]]$name,
        Median_sec = round(results$join[[method]]$median, 4),
        IQR_sec = round(results$join[[method]]$iqr, 4),
        Interval_95 = sprintf("[%.4f, %.4f]",
                              results$join[[method]]$ci_lower,
                              results$join[[method]]$ci_upper),
        N = results$join[[method]]$n,
        stringsAsFactors = FALSE
      ))
    }
  }

  write.csv(extended_table, file.path(output_dir, "extended_comparison.csv"), row.names = FALSE)
  cat("  - extended_comparison.csv (all methods)\n")

  # 5. Statistical analysis CSV
  if (!is.null(analysis$bonferroni)) {
    write.csv(analysis$bonferroni, file.path(output_dir, "statistical_tests.csv"), row.names = FALSE)
    cat("  - statistical_tests.csv (Wilcoxon + Bonferroni)\n")
  }

  # 6. File sizes
  if (!is.null(results$import$file_sizes)) {
    write.csv(results$import$file_sizes, file.path(output_dir, "file_sizes.csv"), row.names = FALSE)
    cat("  - file_sizes.csv (storage comparison)\n")
  }

  log_benchmark(log_file, "Output generation complete")
}

# ==============================================================================
# MAIN EXECUTION
# ==============================================================================

main <- function() {
  # Set seed
  set.seed(CONFIG$seed)

  # Initialize output directory and logging
  if (!dir.exists(CONFIG$output_dir)) {
    dir.create(CONFIG$output_dir, recursive = TRUE)
  }

  log_file <- init_benchmark_log(CONFIG$output_dir)
  cat(sprintf("Logging to: %s\n", log_file))

  # Run all benchmarks
  results <- list()

  cat("\n========================================\n")
  cat("Running benchmarks with n=%d iterations\n", CONFIG$n_iterations)
  cat("Warmup: %d iterations (excluded)\n", CONFIG$n_warmup)
  cat("========================================\n")

  # Benchmark 1: Import
  results$import <- tryCatch({
    benchmark_import(log_file)
  }, error = function(e) {
    cat(sprintf("Error in import benchmark: %s\n", e$message))
    log_benchmark(log_file, sprintf("ERROR in import: %s", e$message))
    NULL
  })

  # Benchmark 2: Aggregation
  results$aggregation <- tryCatch({
    benchmark_aggregation(log_file)
  }, error = function(e) {
    cat(sprintf("Error in aggregation benchmark: %s\n", e$message))
    log_benchmark(log_file, sprintf("ERROR in aggregation: %s", e$message))
    NULL
  })

  # Benchmark 3: Join
  results$join <- tryCatch({
    benchmark_join(log_file)
  }, error = function(e) {
    cat(sprintf("Error in join benchmark: %s\n", e$message))
    log_benchmark(log_file, sprintf("ERROR in join: %s", e$message))
    NULL
  })

  # Benchmark 4: Snapshot
  results$snapshot <- tryCatch({
    benchmark_snapshot(log_file)
  }, error = function(e) {
    cat(sprintf("Error in snapshot benchmark: %s\n", e$message))
    log_benchmark(log_file, sprintf("ERROR in snapshot: %s", e$message))
    NULL
  })

  # Statistical analysis
  analysis <- perform_statistical_analysis(results)

  # Generate outputs
  generate_outputs(results, analysis, CONFIG$output_dir, log_file)

  # Print summary
  cat("\n==============================================================\n")
  cat("                 BENCHMARK SUMMARY                             \n")
  cat("==============================================================\n\n")

  cat("Configuration:\n")
  cat(sprintf("  - Iterations: %d (warmup: %d)\n", CONFIG$n_iterations, CONFIG$n_warmup))
  cat(sprintf("  - Seed: %d\n", CONFIG$seed))
  cat(sprintf("  - Alpha: %.2f\n", CONFIG$alpha))
  cat("\n")

  # Print Table 3 preview
  cat("Table 3 Preview (OmicsLake vs Baseline):\n")
  cat("----------------------------------------------------------\n")

  if (!is.null(results$import$parquet_read) && !is.null(results$import$rds_read)) {
    speedup <- results$import$rds_read$median / results$import$parquet_read$median
    cat(sprintf("  Import:      %.3fs vs %.3fs (%.1fx faster)\n",
                results$import$parquet_read$median,
                results$import$rds_read$median,
                speedup))
  }

  if (!is.null(results$aggregation$omicslake) && !is.null(results$aggregation$dplyr)) {
    speedup <- results$aggregation$dplyr$median / results$aggregation$omicslake$median
    cat(sprintf("  Aggregation: %.3fs vs %.3fs (%.1fx faster)\n",
                results$aggregation$omicslake$median,
                results$aggregation$dplyr$median,
                speedup))
  }

  if (!is.null(results$join$duckdb) && !is.null(results$join$base_merge)) {
    speedup <- results$join$base_merge$median / results$join$duckdb$median
    cat(sprintf("  Join:        %.3fs vs %.3fs (%.1fx faster)\n",
                results$join$duckdb$median,
                results$join$base_merge$median,
                speedup))
  }

  if (!is.null(results$snapshot$ol_snapshot) && !is.null(results$snapshot$file_copy)) {
    speedup <- results$snapshot$file_copy$median / results$snapshot$ol_snapshot$median
    cat(sprintf("  Snapshot:    %.3fs vs %.3fs (%.1fx faster)\n",
                results$snapshot$ol_snapshot$median,
                results$snapshot$file_copy$median,
                speedup))
  }

  cat("\n")
  cat("Output files saved to:", normalizePath(CONFIG$output_dir), "\n")
  cat("  - actual_benchmarks.csv      (raw iteration data)\n")
  cat("  - benchmark_summary.csv      (summary statistics)\n")
  cat("  - Table3_final.csv           (paper format)\n")
  cat("  - extended_comparison.csv    (all methods compared)\n")
  cat("  - statistical_tests.csv      (Wilcoxon + Bonferroni)\n")
  cat("  - benchmark_log.txt          (session info + timestamps)\n")
  cat("\n")

  cat("Statistical Evidence:\n")
  if (!is.null(analysis$bonferroni)) {
    cat("  Bonferroni-corrected comparisons:\n")
    for (i in seq_len(nrow(analysis$bonferroni))) {
      sig_mark <- if (analysis$bonferroni$significant[i]) " *" else ""
      cat(sprintf("    %s: p=%.2e (adj=%.2e)%s\n",
                  analysis$bonferroni$comparison[i],
                  analysis$bonferroni$p_value[i],
                  analysis$bonferroni$p_adjusted[i],
                  sig_mark))
    }
    cat("  * = significant after Bonferroni correction\n")
  }

  cat("\n=== Benchmark Complete ===\n")

  # Save complete results object
  saveRDS(list(results = results, analysis = analysis, config = CONFIG),
          file.path(CONFIG$output_dir, "benchmark_results.RDS"))

  log_benchmark(log_file, "=== BENCHMARK COMPLETE ===")

  invisible(list(results = results, analysis = analysis))
}

# Run if executed directly
if (!interactive()) {
  main()
} else {
  cat("Run main() to execute benchmarks\n")
}
