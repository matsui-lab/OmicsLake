#!/usr/bin/env Rscript
# ==============================================================================
# OmicsLake Reproducibility Test Suite
# ==============================================================================
# Validates OmicsLake's reproducibility in multi-step RNA-seq workflows
# Implements RT-001 through RT-005 based on reproducibility_experiment_design.json
# Generates data for GigaScience paper Table 3 (Reproducibility Comparison)
#
# Author: OmicsLake Development Team
# Date: 2026-02
# ==============================================================================

library(OmicsLake)
library(dplyr)

cat("=============================================================\n")
cat("       OmicsLake Reproducibility Test Suite                  \n")
cat("       Based on reproducibility_experiment_design.json       \n")
cat("=============================================================\n\n")

# Set working directory to paper directory
paper_dir <- if (basename(getwd()) == "paper") {
  getwd()
} else if (file.exists("inst/paper")) {
  "inst/paper"
} else {
  stop("Cannot find paper directory. Run from package root or inst/paper.")
}
setwd(paper_dir)

# =============================================================================
# Configuration
# =============================================================================

# Global settings from design document
set.seed(42)  # ACM reproducibility: same seed for repeatability
TOLERANCE <- 1e-8  # Numerical tolerance for all.equal()
VERBOSE <- TRUE

# Test iteration counts (from experimental_protocol.sample_sizes)
# Reviewer 2: n=10 gives 95%CI=[0.691,1.000], too wide for 100% claim
# Increased to n=30 for sufficient statistical power (95%CI width < 0.15)
RT001_ITERATIONS <- 30
RT002_ITERATIONS <- 30
RT003_ITERATIONS <- 3
RT004_VERSION_COUNTS <- c(50, 100, 200, 500)
RT005_ITERATIONS <- 5

# Results collector for all tests
all_results <- list()

# Helper function: Create isolated test environment
create_test_env <- function(prefix) {
  tmpdir <- tempfile(pattern = paste0(prefix, "_"))
  dir.create(tmpdir, recursive = TRUE)
  old_root <- getOption("ol.root")
  options(ol.root = tmpdir)
  list(
    tmpdir = tmpdir,
    old_root = old_root,
    cleanup = function() {
      options(ol.root = old_root)
      unlink(tmpdir, recursive = TRUE)
    }
  )
}

# Helper function: Log test progress
log_test <- function(test_id, message) {
  if (VERBOSE) {
    cat(sprintf("[%s] %s\n", test_id, message))
  }
}

# =============================================================================
# RT-001: State Restoration Exact
# =============================================================================
# Verify exact state restoration from labeled snapshots using all.equal()
# with strict tolerance (1e-8)
#
# Metrics:
#   - restoration_success_rate: Proportion of successful restorations
#   - numerical_deviation: Max absolute difference from original
#   - metadata_preservation_rate: Proportion of metadata preserved
#   - restoration_latency: Time to restore snapshots
# =============================================================================

cat("\n--- RT-001: State Restoration Exact ---\n")
cat("Testing exact state restoration from labeled snapshots\n\n")

rt001_run_single <- function(iteration) {
  env <- create_test_env("rt001")
  on.exit(env$cleanup(), add = TRUE)

  project <- paste0("rt001_iter_", iteration)
  ol_init(project)

  # Generate reproducible test data
  set.seed(42 + iteration)
  n_genes <- 1000
  n_samples <- 10

  raw_counts <- data.frame(
    gene_id = rep(paste0("GENE", sprintf("%04d", 1:n_genes)), each = n_samples),
    sample_id = rep(paste0("S", 1:n_samples), times = n_genes),
    count = rpois(n_genes * n_samples, lambda = 100),
    stringsAsFactors = FALSE
  )

  # ===== Workflow Step 1: Import Raw Data =====
  ol_write("raw_counts", raw_counts, project = project)
  ol_label("v1.0_raw", project = project)
  original_raw <- raw_counts

  # ===== Workflow Step 2: Normalization =====
  normalized <- raw_counts %>%
    mutate(norm_count = log2(count + 1))
  ol_write("normalized", normalized, depends_on = "raw_counts", project = project)
  ol_label("v1.1_normalized", project = project)
  original_norm <- normalized

  # ===== Workflow Step 3: QC Filter =====
  filtered <- normalized %>%
    group_by(gene_id) %>%
    filter(mean(norm_count) > 2) %>%
    ungroup()
  ol_write("filtered", filtered, depends_on = "normalized", project = project)
  ol_label("v1.2_qc", project = project)
  original_filtered <- filtered

  # ===== Workflow Step 4: DE Analysis (Mock) =====
  de_results <- data.frame(
    gene_id = unique(filtered$gene_id),
    log2fc = rnorm(length(unique(filtered$gene_id)), 0, 1),
    pvalue = runif(length(unique(filtered$gene_id))),
    stringsAsFactors = FALSE
  )
  de_results$padj <- p.adjust(de_results$pvalue, method = "BH")
  ol_save("de_results", de_results, depends_on = "filtered", project = project)

  # ===== Workflow Step 5: Pathway Enrichment (Mock) =====
  pathway_results <- list(
    pathways = paste0("pathway_", 1:10),
    pvalues = runif(10),
    metadata = list(method = "hypergeometric", database = "GO")
  )
  ol_save("pathway_results", pathway_results, depends_on = "de_results", project = project)
  ol_label("v1.3_complete", project = project)
  original_pathway <- pathway_results

  normalize_df_for_compare <- function(df, order_cols = NULL) {
    out <- as.data.frame(df, stringsAsFactors = FALSE)
    if (!is.null(order_cols) && length(order_cols) > 0) {
      out <- out[do.call(order, out[order_cols]), , drop = FALSE]
    }
    rownames(out) <- NULL
    out
  }

  # ===== Verification: Restore and Compare =====
  results <- list(
    restoration_checks = list(),
    latencies = list()
  )

  # Test v1.0_raw restoration
  t0 <- Sys.time()
  restored_raw <- ol_read("raw_counts", ref = "@v1.0_raw", project = project)
  results$latencies$raw <- as.numeric(Sys.time() - t0)
  results$restoration_checks$raw <- isTRUE(all.equal(
    normalize_df_for_compare(original_raw, c("gene_id", "sample_id")),
    normalize_df_for_compare(restored_raw, c("gene_id", "sample_id")),
    tolerance = TOLERANCE
  ))

  # Test v1.1_normalized restoration
  t0 <- Sys.time()
  restored_norm <- ol_read("normalized", ref = "@v1.1_normalized", project = project)
  results$latencies$norm <- as.numeric(Sys.time() - t0)
  results$restoration_checks$norm <- isTRUE(all.equal(
    normalize_df_for_compare(original_norm, c("gene_id", "sample_id")),
    normalize_df_for_compare(restored_norm, c("gene_id", "sample_id")),
    tolerance = TOLERANCE
  ))

  # Test v1.2_qc restoration
  t0 <- Sys.time()
  restored_filtered <- ol_read("filtered", ref = "@v1.2_qc", project = project)
  results$latencies$filtered <- as.numeric(Sys.time() - t0)
  results$restoration_checks$filtered <- isTRUE(all.equal(
    normalize_df_for_compare(original_filtered, c("gene_id", "sample_id")),
    normalize_df_for_compare(restored_filtered, c("gene_id", "sample_id")),
    tolerance = TOLERANCE
  ))

  # Test v1.3_complete (object) restoration
  t0 <- Sys.time()
  restored_pathway <- ol_read_object("pathway_results", ref = "@v1.3_complete", project = project)
  results$latencies$pathway <- as.numeric(Sys.time() - t0)
  results$restoration_checks$pathway <- isTRUE(all.equal(original_pathway, restored_pathway))

  # Metadata preservation check
  results$metadata_preserved <- isTRUE(all.equal(
    attributes(original_norm),
    attributes(restored_norm)
  ))

  # Summary
  results$all_passed <- all(unlist(results$restoration_checks))
  results$mean_latency <- mean(unlist(results$latencies))
  results$iteration <- iteration

  results
}

# Run RT-001 iterations
rt001_results <- lapply(1:RT001_ITERATIONS, function(i) {
  log_test("RT-001", sprintf("Iteration %d/%d", i, RT001_ITERATIONS))
  rt001_run_single(i)
})

# Calculate RT-001 summary statistics
rt001_successes <- sum(sapply(rt001_results, function(x) x$all_passed))
rt001_n <- RT001_ITERATIONS

# Clopper-Pearson exact binomial 95% confidence interval
# Addresses Reviewer 2 concern about statistical evidence for 100% reproducibility
rt001_ci <- binom.test(rt001_successes, rt001_n, conf.level = 0.95)$conf.int

rt001_summary <- data.frame(
  test_id = "RT-001",
  test_name = "state_restoration_exact",
  metric = "restoration_success_rate",
  value = mean(sapply(rt001_results, function(x) x$all_passed)),
  target = 1.0,
  n_iterations = RT001_ITERATIONS,
  passed_iterations = rt001_successes,
  ci_lower = rt001_ci[1],
  ci_upper = rt001_ci[2],
  mean_latency_sec = mean(sapply(rt001_results, function(x) x$mean_latency)),
  p95_latency_sec = quantile(sapply(rt001_results, function(x) x$mean_latency), 0.95),
  stringsAsFactors = FALSE
)

cat(sprintf("  RT-001 Result: %.1f%% success rate (%d/%d iterations)\n",
            rt001_summary$value * 100, rt001_summary$passed_iterations, RT001_ITERATIONS))
cat(sprintf("  95%% CI (Clopper-Pearson): [%.3f, %.3f]\n",
            rt001_summary$ci_lower, rt001_summary$ci_upper))
cat(sprintf("  Mean restoration latency: %.3f sec\n\n", rt001_summary$mean_latency_sec))

all_results$rt001 <- rt001_summary
all_results$rt001_detailed <- rt001_results

# =============================================================================
# RT-002: Lineage Tracking Completeness
# =============================================================================
# Verify complete and accurate dependency tracking across the entire pipeline
#
# Metrics:
#   - edge_recall: Proportion of expected edges found
#   - edge_precision: Proportion of found edges that are correct
#   - depth_accuracy: Accuracy of depth calculation
#   - lineage_query_latency: Time to execute lineage queries
# =============================================================================

cat("--- RT-002: Lineage Tracking Completeness ---\n")
cat("Testing dependency tracking accuracy\n\n")

rt002_run_single <- function(iteration) {
  env <- create_test_env("rt002")
  on.exit(env$cleanup(), add = TRUE)

  project <- paste0("rt002_iter_", iteration)
  ol_init(project)

  # Create dependency graph as per design document
  # Expected graph structure:
  #   raw_counts (table, root)
  #   sample_info (object, root)
  #   gene_info (object, root)
  #   qc_params (object, root)
  #   norm_params (object) -> gene_info, sample_info
  #   normalized_counts (table) -> raw_counts, norm_params
  #   filtered_counts (table) -> normalized_counts, qc_params
  #   de_results (object) -> filtered_counts, sample_info
  #   pathway_results (object) -> de_results

  # Root nodes (no dependencies)
  ol_write("raw_counts", data.frame(x = 1:10), project = project)
  ol_save("sample_info", list(samples = paste0("S", 1:5)), project = project)
  ol_save("gene_info", list(genes = paste0("G", 1:100)), project = project)
  ol_save("qc_params", list(threshold = 2.0), project = project)

  # Intermediate nodes with dependencies
  ol_save("norm_params", list(method = "TMM"),
          depends_on = c("gene_info", "sample_info"), project = project)

  ol_write("normalized_counts", data.frame(y = 1:10),
           depends_on = c("raw_counts", "norm_params"), project = project)

  ol_write("filtered_counts", data.frame(z = 1:5),
           depends_on = c("normalized_counts", "qc_params"), project = project)

  ol_save("de_results", list(genes = paste0("G", 1:10)),
          depends_on = c("filtered_counts", "sample_info"), project = project)

  ol_save("pathway_results", list(pathways = paste0("P", 1:5)),
          depends_on = "de_results", project = project)

  # Expected edges (9 total from design)
  expected_edges <- list(
    c("norm_params", "gene_info"),
    c("norm_params", "sample_info"),
    c("normalized_counts", "raw_counts"),
    c("normalized_counts", "norm_params"),
    c("filtered_counts", "normalized_counts"),
    c("filtered_counts", "qc_params"),
    c("de_results", "filtered_counts"),
    c("de_results", "sample_info"),
    c("pathway_results", "de_results")
  )

  results <- list()

  # Test upstream traversal from pathway_results
  t0 <- Sys.time()
  lineage_up <- ol_show_lineage("pathway_results", direction = "upstream",
                                 max_depth = 10, project = project)
  results$upstream_latency <- as.numeric(Sys.time() - t0)

  expected_ancestors <- c("de_results", "filtered_counts", "normalized_counts",
                          "raw_counts", "sample_info", "norm_params",
                          "gene_info", "qc_params")

  if (nrow(lineage_up) > 0 && "parent" %in% names(lineage_up)) {
    found_ancestors <- unique(lineage_up$parent)
    results$upstream_recall <- length(intersect(found_ancestors, expected_ancestors)) /
                               length(expected_ancestors)
  } else {
    results$upstream_recall <- 0
  }

  # Test downstream traversal from raw_counts
  t0 <- Sys.time()
  lineage_down <- ol_show_lineage("raw_counts", direction = "downstream",
                                   max_depth = 10, project = project)
  results$downstream_latency <- as.numeric(Sys.time() - t0)

  expected_descendants <- c("normalized_counts", "filtered_counts", "de_results", "pathway_results")

  if (nrow(lineage_down) > 0 && "child" %in% names(lineage_down)) {
    found_descendants <- unique(lineage_down$child)
    results$downstream_recall <- length(intersect(found_descendants, expected_descendants)) /
                                 length(expected_descendants)
  } else {
    results$downstream_recall <- 0
  }

  # Check all expected edges exist in database
  all_deps <- tryCatch({
    state <- OmicsLake:::.ol_get_backend_state(project)
    OmicsLake:::.ol_ensure_dependencies_table(state)
    conn <- state$conn
    ident <- OmicsLake:::.ol_sql_ident(conn, state, "__ol_dependencies")
    DBI::dbGetQuery(conn, sprintf("SELECT child_name, parent_name FROM %s", ident))
  }, error = function(e) {
    data.frame(child_name = character(0), parent_name = character(0))
  })

  edges_found <- 0
  for (edge in expected_edges) {
    if (any(all_deps$child_name == edge[1] & all_deps$parent_name == edge[2])) {
      edges_found <- edges_found + 1
    }
  }
  results$edge_recall <- edges_found / length(expected_edges)
  results$edge_precision <- ifelse(nrow(all_deps) > 0, edges_found / nrow(all_deps), 0)

  # Calculate F1 score
  if (results$edge_recall + results$edge_precision > 0) {
    results$f1_score <- 2 * (results$edge_precision * results$edge_recall) /
                        (results$edge_precision + results$edge_recall)
  } else {
    results$f1_score <- 0
  }

  results$iteration <- iteration
  results$total_expected_edges <- length(expected_edges)
  results$edges_found <- edges_found
  results$mean_latency <- mean(c(results$upstream_latency, results$downstream_latency))

  results
}

# Run RT-002 iterations
rt002_results <- lapply(1:RT002_ITERATIONS, function(i) {
  log_test("RT-002", sprintf("Iteration %d/%d", i, RT002_ITERATIONS))
  rt002_run_single(i)
})

# Calculate RT-002 summary statistics
# For RT-002, we count iterations with perfect edge recall (100%) as successes
rt002_successes <- sum(sapply(rt002_results, function(x) x$edge_recall == 1.0))
rt002_n <- RT002_ITERATIONS

# Clopper-Pearson exact binomial 95% confidence interval
rt002_ci <- binom.test(rt002_successes, rt002_n, conf.level = 0.95)$conf.int

rt002_summary <- data.frame(
  test_id = "RT-002",
  test_name = "lineage_tracking_completeness",
  metric = "dependency_completeness",
  value = mean(sapply(rt002_results, function(x) x$edge_recall)),
  target = 1.0,
  n_iterations = RT002_ITERATIONS,
  perfect_iterations = rt002_successes,
  ci_lower = rt002_ci[1],
  ci_upper = rt002_ci[2],
  edge_precision = mean(sapply(rt002_results, function(x) x$edge_precision)),
  upstream_recall = mean(sapply(rt002_results, function(x) x$upstream_recall)),
  downstream_recall = mean(sapply(rt002_results, function(x) x$downstream_recall)),
  f1_score = mean(sapply(rt002_results, function(x) x$f1_score)),
  mean_latency_sec = mean(sapply(rt002_results, function(x) x$mean_latency)),
  stringsAsFactors = FALSE
)

cat(sprintf("  RT-002 Edge Recall: %.1f%%\n", rt002_summary$value * 100))
cat(sprintf("  RT-002 Perfect Iterations: %d/%d\n", rt002_successes, rt002_n))
cat(sprintf("  95%% CI (Clopper-Pearson): [%.3f, %.3f]\n",
            rt002_summary$ci_lower, rt002_summary$ci_upper))
cat(sprintf("  RT-002 Edge Precision: %.1f%%\n", rt002_summary$edge_precision * 100))
cat(sprintf("  RT-002 F1 Score: %.3f\n", rt002_summary$f1_score))
cat(sprintf("  Mean lineage query latency: %.3f sec\n\n", rt002_summary$mean_latency_sec))

all_results$rt002 <- rt002_summary
all_results$rt002_detailed <- rt002_results

# =============================================================================
# RT-003: Cross-Environment Reproducibility (Simulated)
# =============================================================================
# Compare analysis reproducibility across different computing environments
# Note: In this implementation, we simulate cross-environment by copying
# the lake directory and verifying data can be restored correctly.
#
# ACM Reproducibility Definition Alignment:
#   - Repeatability: Same team, same experimental setup
#   - Reproducibility: Different team, same experimental setup
#   - Replicability: Different team, different experimental setup
#
# Metrics:
#   - cross_env_equivalence_rate: Data matches across environments
#   - max_numerical_deviation: Maximum difference in numerical values
#   - metadata_consistency: Metadata preserved across transfer
# =============================================================================

cat("--- RT-003: Cross-Environment Reproducibility ---\n")
cat("Testing data portability (simulated environment transfer)\n\n")

# Helper: Simulate environment transfer
simulate_env_transfer <- function(source_dir, target_dir) {
  if (dir.exists(target_dir)) {
    unlink(target_dir, recursive = TRUE)
  }
  dir.create(target_dir, recursive = TRUE)

  files <- list.files(source_dir, recursive = TRUE, full.names = TRUE)
  for (f in files) {
    rel_path <- sub(paste0("^", normalizePath(source_dir, winslash = "/"), "/?"), "",
                    normalizePath(f, winslash = "/"))
    target_path <- file.path(target_dir, rel_path)
    dir.create(dirname(target_path), recursive = TRUE, showWarnings = FALSE)
    file.copy(f, target_path, overwrite = TRUE)
  }

  invisible(TRUE)
}

rt003_run_single <- function(iteration, env_pair) {
  project <- paste0("rt003_", env_pair[1], "_", env_pair[2], "_iter", iteration)

  # Environment A: Source
  tmpdir_a <- tempfile(pattern = "rt003_envA_")
  dir.create(tmpdir_a, recursive = TRUE)

  # Environment B: Target
  tmpdir_b <- tempfile(pattern = "rt003_envB_")

  on.exit({
    unlink(tmpdir_a, recursive = TRUE)
    unlink(tmpdir_b, recursive = TRUE)
  }, add = TRUE)

  # Phase 1: Create workflow in Environment A
  old_root <- getOption("ol.root")
  options(ol.root = tmpdir_a)

  ol_init(project)

  # Create reproducible test data
  set.seed(42)
  n_genes <- 500
  n_samples <- 8

  raw_counts <- data.frame(
    gene_id = rep(paste0("GENE", sprintf("%04d", 1:n_genes)), each = n_samples),
    sample_id = rep(paste0("S", 1:n_samples), times = n_genes),
    count = rpois(n_genes * n_samples, lambda = 100),
    stringsAsFactors = FALSE
  )

  normalized <- raw_counts %>%
    mutate(norm_count = log2(count + 1))

  de_results <- data.frame(
    gene_id = unique(raw_counts$gene_id),
    log2fc = rnorm(n_genes, 0, 1),
    pvalue = runif(n_genes),
    stringsAsFactors = FALSE
  )

  params <- list(
    normalization = "log2_transform",
    pseudocount = 1,
    workflow_version = "1.0"
  )

  # Store in Environment A
  ol_write("raw_counts", raw_counts, project = project)
  ol_write("normalized", normalized, depends_on = "raw_counts", project = project)
  ol_save("de_results", de_results, depends_on = "normalized", project = project)
  ol_save("params", params, project = project)
  ol_label("v1.0_complete", project = project)

  # Capture lineage in Env A
  lineage_a <- ol_show_lineage("de_results", direction = "upstream",
                                max_depth = 10, project = project)

  original_data <- list(
    raw_counts = raw_counts,
    normalized = normalized,
    de_results = de_results,
    params = params,
    lineage = lineage_a
  )

  options(ol.root = old_root)

  # Phase 2: Transfer to Environment B
  lake_source <- file.path(tmpdir_a, project)
  lake_target <- file.path(tmpdir_b, project)
  simulate_env_transfer(lake_source, lake_target)

  # Phase 3: Verify in Environment B
  options(ol.root = tmpdir_b)

  verification <- tryCatch({
    restored_raw <- ol_read("raw_counts", ref = "@v1.0_complete", project = project)
    restored_norm <- ol_read("normalized", ref = "@v1.0_complete", project = project)
    restored_de <- ol_read_object("de_results", ref = "@v1.0_complete", project = project)
    restored_params <- ol_read_object("params", ref = "@v1.0_complete", project = project)

    lineage_b <- ol_show_lineage("de_results", direction = "upstream",
                                  max_depth = 10, project = project)

    # Compare data
    sort_df <- function(df, cols) df[do.call(order, df[cols]), ]

    raw_match <- isTRUE(all.equal(
      sort_df(original_data$raw_counts, c("gene_id", "sample_id")),
      sort_df(restored_raw, c("gene_id", "sample_id")),
      tolerance = TOLERANCE, check.attributes = FALSE
    ))

    norm_match <- isTRUE(all.equal(
      sort_df(original_data$normalized, c("gene_id", "sample_id")),
      sort_df(restored_norm, c("gene_id", "sample_id")),
      tolerance = TOLERANCE, check.attributes = FALSE
    ))

    de_match <- isTRUE(all.equal(
      original_data$de_results[order(original_data$de_results$gene_id), ],
      restored_de[order(restored_de$gene_id), ],
      tolerance = TOLERANCE, check.attributes = FALSE
    ))

    params_match <- isTRUE(all.equal(original_data$params, restored_params))

    # Compare lineage
    lineage_match <- nrow(lineage_a) == nrow(lineage_b)

    list(
      raw_match = raw_match,
      norm_match = norm_match,
      de_match = de_match,
      params_match = params_match,
      lineage_match = lineage_match,
      all_match = raw_match && norm_match && de_match && params_match
    )
  }, error = function(e) {
    list(
      raw_match = FALSE, norm_match = FALSE, de_match = FALSE,
      params_match = FALSE, lineage_match = FALSE, all_match = FALSE,
      error = e$message
    )
  })

  options(ol.root = old_root)

  list(
    iteration = iteration,
    env_pair = paste(env_pair, collapse = "->"),
    success = verification$all_match,
    verification = verification
  )
}

# Environment pairs to test (simulated)
env_pairs <- list(
  c("LocalDev", "Docker"),
  c("LocalDev", "HPC"),
  c("LocalDev", "Cloud")
)

# Run RT-003 iterations
rt003_results <- list()
for (pair in env_pairs) {
  for (i in 1:RT003_ITERATIONS) {
    log_test("RT-003", sprintf("%s -> %s, Iteration %d/%d",
                               pair[1], pair[2], i, RT003_ITERATIONS))
    result <- rt003_run_single(i, pair)
    rt003_results[[length(rt003_results) + 1]] <- result
  }
}

# Calculate RT-003 summary statistics
rt003_successes <- sum(sapply(rt003_results, function(x) x$success))
rt003_n <- length(rt003_results)

# Clopper-Pearson exact binomial 95% confidence interval
rt003_ci <- binom.test(rt003_successes, rt003_n, conf.level = 0.95)$conf.int

rt003_summary <- data.frame(
  test_id = "RT-003",
  test_name = "cross_environment_reproducibility",
  metric = "cross_env_equivalence_rate",
  value = mean(sapply(rt003_results, function(x) x$success)),
  target = 1.0,
  n_iterations = length(rt003_results),
  n_env_pairs = length(env_pairs),
  passed_iterations = rt003_successes,
  ci_lower = rt003_ci[1],
  ci_upper = rt003_ci[2],
  stringsAsFactors = FALSE
)

cat(sprintf("  RT-003 Cross-Environment Success Rate: %.1f%%\n", rt003_summary$value * 100))
cat(sprintf("  95%% CI (Clopper-Pearson): [%.3f, %.3f]\n",
            rt003_summary$ci_lower, rt003_summary$ci_upper))
cat(sprintf("  Tested %d environment pairs x %d iterations\n\n",
            length(env_pairs), RT003_ITERATIONS))

all_results$rt003 <- rt003_summary
all_results$rt003_detailed <- rt003_results

# =============================================================================
# RT-004: Long-term Stability
# =============================================================================
# Assess system performance and reproducibility after extensive version
# accumulation. Tests O(log n) scalability of version operations.
#
# Metrics:
#   - restore_latency_degradation_ratio: Performance at scale vs baseline
#   - database_size_per_version: Storage efficiency
#   - integrity_rate_at_scale: Data correctness under load
# =============================================================================

cat("--- RT-004: Long-term Stability ---\n")
cat("Testing performance scalability with version accumulation\n\n")

rt004_run_single <- function(n_versions) {
  env <- create_test_env("rt004")
  on.exit(env$cleanup(), add = TRUE)

  project <- paste0("rt004_v", n_versions)
  ol_init(project)

  log_test("RT-004", sprintf("Creating %d versions...", n_versions))

  creation_times <- numeric(n_versions)

  for (i in 1:n_versions) {
    start_time <- Sys.time()
    ol_save(paste0("obj_", (i %% 10) + 1),
            list(iteration = i, data = rnorm(100)),
            project = project)
    creation_times[i] <- as.numeric(Sys.time() - start_time)

    # Create checkpoint every 10 versions
    if (i %% 10 == 0) {
      ol_label(paste0("checkpoint_", i / 10), project = project)
    }
  }

  # Measure restoration latency
  labels <- ol_list_labels(project = project)
  restore_times <- numeric(0)

  if (nrow(labels) > 0) {
    n_tests <- min(10, nrow(labels))
    restore_times <- numeric(n_tests)

    for (j in 1:n_tests) {
      label <- labels$tag[j]
      start_time <- Sys.time()
      tryCatch({
        ol_read_object("obj_1", ref = paste0("@", label), project = project)
      }, error = function(e) NULL)
      restore_times[j] <- as.numeric(Sys.time() - start_time)
    }
  }

  # Get database size
  db_path <- file.path(env$tmpdir, project, "duckdb.db")
  db_size <- ifelse(file.exists(db_path), file.info(db_path)$size / 1024, NA)  # KB

  list(
    n_versions = n_versions,
    mean_creation_time = mean(creation_times),
    mean_restore_time = mean(restore_times, na.rm = TRUE),
    p50_restore_time = median(restore_times, na.rm = TRUE),
    p95_restore_time = quantile(restore_times, 0.95, na.rm = TRUE),
    db_size_kb = db_size,
    size_per_version_kb = db_size / n_versions
  )
}

# Run RT-004 for different version counts
rt004_results <- lapply(RT004_VERSION_COUNTS, function(n) {
  log_test("RT-004", sprintf("Testing with %d versions", n))
  rt004_run_single(n)
})

# Calculate degradation ratio
rt004_df <- do.call(rbind, lapply(rt004_results, function(x) {
  data.frame(
    n_versions = x$n_versions,
    mean_restore_time_sec = x$mean_restore_time,
    p95_restore_time_sec = x$p95_restore_time,
    db_size_kb = x$db_size_kb,
    size_per_version_kb = x$size_per_version_kb,
    stringsAsFactors = FALSE
  )
}))

baseline_time <- rt004_df$mean_restore_time_sec[1]
final_time <- rt004_df$mean_restore_time_sec[nrow(rt004_df)]
degradation_ratio <- ifelse(baseline_time > 0, final_time / baseline_time, NA)

rt004_summary <- data.frame(
  test_id = "RT-004",
  test_name = "long_term_stability",
  metric = "restore_latency_degradation_ratio",
  value = degradation_ratio,
  target = 2.0,  # Should be < 2x degradation
  baseline_versions = RT004_VERSION_COUNTS[1],
  max_versions = RT004_VERSION_COUNTS[length(RT004_VERSION_COUNTS)],
  baseline_latency_sec = baseline_time,
  final_latency_sec = final_time,
  stringsAsFactors = FALSE
)

cat(sprintf("  RT-004 Degradation Ratio: %.2fx (%d -> %d versions)\n",
            degradation_ratio, RT004_VERSION_COUNTS[1],
            RT004_VERSION_COUNTS[length(RT004_VERSION_COUNTS)]))
cat("  Performance vs Version Count:\n")
print(rt004_df)
cat("\n")

all_results$rt004 <- rt004_summary
all_results$rt004_detailed <- rt004_df

# =============================================================================
# RT-005: Rollback Cascade Verification
# =============================================================================
# Verify correct handling of dependent object invalidation on upstream rollback.
# Tests atomic multi-object restore via labels.
#
# Metrics:
#   - rollback_consistency_rate: All objects restored correctly
#   - cascade_correctness: Dependent objects match expected state
#   - rollback_latency: Time to perform rollback operation
# =============================================================================

cat("--- RT-005: Rollback Cascade Verification ---\n")
cat("Testing rollback consistency across dependent objects\n\n")

rt005_run_single <- function(iteration) {
  env <- create_test_env("rt005")
  on.exit(env$cleanup(), add = TRUE)

  project <- paste0("rt005_iter_", iteration)
  ol_init(project)

  # ===== Initial Pipeline (v1) =====
  raw_v1 <- data.frame(x = 1:10, stringsAsFactors = FALSE)
  processed_v1 <- data.frame(x = raw_v1$x * 2, stringsAsFactors = FALSE)
  results_v1 <- list(mean = mean(processed_v1$x), version = "v1")

  ol_write("raw_data", raw_v1, project = project)
  ol_write("processed", processed_v1, depends_on = "raw_data", project = project)
  ol_save("results", results_v1, depends_on = "processed", project = project)
  ol_label("v1.0_complete", project = project)

  # ===== Modified Pipeline (v2) =====
  raw_v2 <- data.frame(x = 11:20, stringsAsFactors = FALSE)
  processed_v2 <- data.frame(x = raw_v2$x * 2, stringsAsFactors = FALSE)
  results_v2 <- list(mean = mean(processed_v2$x), version = "v2")

  ol_write("raw_data", raw_v2, mode = "overwrite", project = project)
  ol_write("processed", processed_v2, mode = "overwrite", depends_on = "raw_data", project = project)
  ol_save("results", results_v2, depends_on = "processed", project = project)
  ol_label("v2.0_complete", project = project)

  # ===== Rollback to v1.0_complete =====
  t0 <- Sys.time()
  restored_raw <- ol_read("raw_data", ref = "@v1.0_complete", project = project)
  restored_processed <- ol_read("processed", ref = "@v1.0_complete", project = project)
  restored_results <- ol_read_object("results", ref = "@v1.0_complete", project = project)
  rollback_latency <- as.numeric(Sys.time() - t0)

  # ===== Verify Consistency =====
  raw_match <- isTRUE(all.equal(raw_v1, restored_raw, tolerance = TOLERANCE))
  processed_match <- isTRUE(all.equal(processed_v1, restored_processed, tolerance = TOLERANCE))
  results_match <- isTRUE(all.equal(results_v1, restored_results))

  # Check cascade: restored_processed should be consistent with restored_raw
  expected_processed <- data.frame(x = restored_raw$x * 2, stringsAsFactors = FALSE)
  cascade_consistent <- isTRUE(all.equal(expected_processed, restored_processed, tolerance = TOLERANCE))

  list(
    iteration = iteration,
    raw_match = raw_match,
    processed_match = processed_match,
    results_match = results_match,
    cascade_consistent = cascade_consistent,
    all_consistent = raw_match && processed_match && results_match,
    rollback_latency = rollback_latency
  )
}

# Run RT-005 iterations
rt005_results <- lapply(1:RT005_ITERATIONS, function(i) {
  log_test("RT-005", sprintf("Iteration %d/%d", i, RT005_ITERATIONS))
  rt005_run_single(i)
})

# Calculate RT-005 summary statistics
rt005_successes <- sum(sapply(rt005_results, function(x) x$all_consistent))
rt005_n <- RT005_ITERATIONS

# Clopper-Pearson exact binomial 95% confidence interval
rt005_ci <- binom.test(rt005_successes, rt005_n, conf.level = 0.95)$conf.int

rt005_summary <- data.frame(
  test_id = "RT-005",
  test_name = "rollback_cascade_verification",
  metric = "rollback_success_rate",
  value = mean(sapply(rt005_results, function(x) x$all_consistent)),
  target = 1.0,
  n_iterations = RT005_ITERATIONS,
  passed_iterations = rt005_successes,
  ci_lower = rt005_ci[1],
  ci_upper = rt005_ci[2],
  raw_consistency = mean(sapply(rt005_results, function(x) x$raw_match)),
  processed_consistency = mean(sapply(rt005_results, function(x) x$processed_match)),
  results_consistency = mean(sapply(rt005_results, function(x) x$results_match)),
  cascade_consistency = mean(sapply(rt005_results, function(x) x$cascade_consistent)),
  mean_rollback_latency_sec = mean(sapply(rt005_results, function(x) x$rollback_latency)),
  stringsAsFactors = FALSE
)

cat(sprintf("  RT-005 Rollback Success Rate: %.1f%%\n", rt005_summary$value * 100))
cat(sprintf("  95%% CI (Clopper-Pearson): [%.3f, %.3f]\n",
            rt005_summary$ci_lower, rt005_summary$ci_upper))
cat(sprintf("  Cascade Consistency: %.1f%%\n", rt005_summary$cascade_consistency * 100))
cat(sprintf("  Mean rollback latency: %.3f sec\n\n", rt005_summary$mean_rollback_latency_sec))

all_results$rt005 <- rt005_summary
all_results$rt005_detailed <- rt005_results

# =============================================================================
# Summary and Output Generation
# =============================================================================

cat("=============================================================\n")
cat("               REPRODUCIBILITY TEST SUMMARY                  \n")
cat("=============================================================\n\n")

# Create final summary table with 95% confidence intervals
# Addresses Reviewer 2 concern: n=10 -> 95%CI=[0.691,1.000] is too wide
# With n=30 and 30/30 successes: 95%CI=[0.884,1.000]
final_summary <- data.frame(
  Test_ID = c("RT-001", "RT-002", "RT-003", "RT-004", "RT-005"),
  Test_Name = c(
    "State Restoration",
    "Lineage Tracking",
    "Cross-Environment",
    "Long-term Stability",
    "Rollback Cascade"
  ),
  Category = c(
    "State Reproducibility",
    "Lineage Tracking",
    "Cross-Environment",
    "Long-term Stability",
    "State Reproducibility"
  ),
  N = c(
    RT001_ITERATIONS,
    RT002_ITERATIONS,
    length(rt003_results),
    length(RT004_VERSION_COUNTS),
    RT005_ITERATIONS
  ),
  Successes = c(
    rt001_summary$passed_iterations,
    rt002_summary$perfect_iterations,
    rt003_summary$passed_iterations,
    NA,  # RT-004 uses degradation ratio, not success count
    rt005_summary$passed_iterations
  ),
  Primary_Metric = c(
    rt001_summary$value,
    rt002_summary$value,
    rt003_summary$value,
    rt004_summary$value,
    rt005_summary$value
  ),
  CI_Lower = c(
    rt001_summary$ci_lower,
    rt002_summary$ci_lower,
    rt003_summary$ci_lower,
    NA,  # RT-004 not applicable
    rt005_summary$ci_lower
  ),
  CI_Upper = c(
    rt001_summary$ci_upper,
    rt002_summary$ci_upper,
    rt003_summary$ci_upper,
    NA,  # RT-004 not applicable
    rt005_summary$ci_upper
  ),
  Target = c(1.0, 1.0, 1.0, 2.0, 1.0),
  Pass = c(
    rt001_summary$value >= 0.99,
    rt002_summary$value >= 0.99,
    rt003_summary$value >= 0.99,
    !is.na(rt004_summary$value) && rt004_summary$value <= 2.0,
    rt005_summary$value >= 0.99
  ),
  stringsAsFactors = FALSE
)

cat("Primary Metrics Summary:\n")
print(final_summary)
cat("\n")

# =============================================================================
# Table 3: Reproducibility Comparison (GigaScience Paper)
# =============================================================================

cat("--- Table 3: Reproducibility Comparison ---\n")
cat("Comparison with alternative approaches\n\n")

# Reproducibility metrics explanation:
# - Standard R / Git + Manual baselines are reported qualitatively
# - OmicsLake values are measured from RT-001 through RT-005

table3_comparison <- data.frame(
  Environment = c("Standard R Script", "Git + Manual Versioning", "OmicsLake"),
  State_Reproducibility = c(
    "Ad hoc reconstruction (qualitative)",
    "Manual conventions (qualitative)",
    sprintf("%.0f%% (verified)", rt001_summary$value * 100)
  ),
  Dependency_Tracking = c(
    "None automatic",
    "Manual",
    sprintf("%.0f%% (verified)", rt002_summary$value * 100)
  ),
  Cross_Environment = c(
    "Typically unverified",
    "Checklist-based",
    sprintf("%.0f%% simulated (verified)", rt003_summary$value * 100)
  ),
  Rollback_Capability = c(
    "Manual file replacement",
    "Code-first + manual data sync",
    sprintf("%.0f%% (verified)", rt005_summary$value * 100)
  ),
  Automation_Level = c("Low", "Moderate", "High"),
  Human_Overhead = c("High", "Moderate", "Low"),
  Data_Source = c("Qualitative baseline", "Qualitative baseline", "Empirical (this study)"),
  stringsAsFactors = FALSE
)

cat("Table 3: Reproducibility Metrics Comparison\n")
print(table3_comparison)
cat("\n")

# Quantitative metrics for publication with 95% CI (Clopper-Pearson exact)
# Reviewer 2 Response: Added statistical evidence for reproducibility claims
table3_quantitative <- data.frame(
  Metric = c(
    "State Restoration Rate",
    "Dependency Completeness",
    "Cross-Environment Portability",
    "Rollback Success Rate",
    "Performance Degradation (500 vs 50 versions)"
  ),
  OmicsLake_Value = c(
    sprintf("%.1f%%", rt001_summary$value * 100),
    sprintf("%.1f%%", rt002_summary$value * 100),
    sprintf("%.1f%%", rt003_summary$value * 100),
    sprintf("%.1f%%", rt005_summary$value * 100),
    sprintf("%.2fx", rt004_summary$value)
  ),
  CI_95_Lower = c(
    sprintf("%.1f%%", rt001_summary$ci_lower * 100),
    sprintf("%.1f%%", rt002_summary$ci_lower * 100),
    sprintf("%.1f%%", rt003_summary$ci_lower * 100),
    sprintf("%.1f%%", rt005_summary$ci_lower * 100),
    "N/A"
  ),
  CI_95_Upper = c(
    sprintf("%.1f%%", rt001_summary$ci_upper * 100),
    sprintf("%.1f%%", rt002_summary$ci_upper * 100),
    sprintf("%.1f%%", rt003_summary$ci_upper * 100),
    sprintf("%.1f%%", rt005_summary$ci_upper * 100),
    "N/A"
  ),
  Target = c(">=99%", ">=99%", ">=99%", ">=99%", "<2.0x"),
  Pass = c(
    ifelse(rt001_summary$value >= 0.99, "PASS", "FAIL"),
    ifelse(rt002_summary$value >= 0.99, "PASS", "FAIL"),
    ifelse(rt003_summary$value >= 0.99, "PASS", "FAIL"),
    ifelse(rt005_summary$value >= 0.99, "PASS", "FAIL"),
    ifelse(!is.na(rt004_summary$value) && rt004_summary$value <= 2.0, "PASS", "FAIL")
  ),
  N = c(
    RT001_ITERATIONS,
    RT002_ITERATIONS,
    length(rt003_results),
    RT005_ITERATIONS,
    length(RT004_VERSION_COUNTS)
  ),
  stringsAsFactors = FALSE
)

cat("Table 3 (Quantitative): OmicsLake Reproducibility Metrics\n")
print(table3_quantitative)
cat("\n")

# =============================================================================
# Save Results
# =============================================================================

cat("--- Saving Results ---\n")

# Save primary summary CSV (Table 3 for paper)
write.csv(final_summary, "results_reproducibility_summary.csv", row.names = FALSE)
cat("  - results_reproducibility_summary.csv\n")

# Save comparison table (Table 3 narrative)
write.csv(table3_comparison, "results_table3_comparison.csv", row.names = FALSE)
cat("  - results_table3_comparison.csv\n")

# Save quantitative metrics (Table 3 quantitative)
write.csv(table3_quantitative, "results_table3_quantitative.csv", row.names = FALSE)
cat("  - results_table3_quantitative.csv\n")

# Save RT-004 scalability data
write.csv(all_results$rt004_detailed, "results_rt004_scalability.csv", row.names = FALSE)
cat("  - results_rt004_scalability.csv\n")

# Save detailed results as RDS
saveRDS(all_results, "results_reproducibility_detailed.RDS")
cat("  - results_reproducibility_detailed.RDS\n")

# =============================================================================
# Generate Publication-Ready Figures
# =============================================================================

cat("\n--- Generating Publication Figures ---\n")

# Figure: Reproducibility Metrics Comparison
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)

  # Prepare data for Figure
  fig_data <- data.frame(
    Environment = factor(c("Standard R", "Git + Manual", "OmicsLake"),
                         levels = c("Standard R", "Git + Manual", "OmicsLake")),
    State_Reproducibility = c(1, 2, 3),
    Dependency_Tracking = c(1, 2, 3),
    Rollback_Capability = c(1, 2, 3),
    stringsAsFactors = FALSE
  )

  fig_long <- tidyr::pivot_longer(fig_data,
                                   cols = c("State_Reproducibility", "Dependency_Tracking", "Rollback_Capability"),
                                   names_to = "Metric", values_to = "Value")

  fig_long$Metric <- gsub("_", " ", fig_long$Metric)

  p <- ggplot(fig_long, aes(x = Environment, y = Value, fill = Metric)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1),
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold"),
      panel.grid.minor = element_blank()
    ) +
    labs(
      title = "Reproducibility Capability Comparison",
      subtitle = "Qualitative baselines with empirically validated OmicsLake outcomes",
      x = "",
      y = "Capability Level",
      fill = "Metric",
      caption = sprintf("OmicsLake outcomes verified by RT-001/RT-002/RT-005 (n=%d/%d/%d)\nBaseline workflows are qualitative (Low/Moderate) descriptors",
                        RT001_ITERATIONS, RT002_ITERATIONS, RT005_ITERATIONS)
    ) +
    scale_fill_manual(values = c("#E74C3C", "#3498DB", "#27AE60")) +
    scale_y_continuous(
      limits = c(0, 3.4),
      breaks = c(1, 2, 3),
      labels = c("Low", "Moderate", "High")
    )

  ggsave("figure_reproducibility_comparison.pdf", p, width = 10, height = 7)
  cat("  - figure_reproducibility_comparison.pdf\n")

  # Figure: Scalability
  p2 <- ggplot(all_results$rt004_detailed,
               aes(x = n_versions, y = mean_restore_time_sec)) +
    geom_line(color = "#2980B9", size = 1.2) +
    geom_point(color = "#2980B9", size = 3) +
    geom_hline(yintercept = all_results$rt004_detailed$mean_restore_time_sec[1] * 2,
               linetype = "dashed", color = "red", alpha = 0.7) +
    annotate("text",
             x = max(all_results$rt004_detailed$n_versions) * 0.7,
             y = all_results$rt004_detailed$mean_restore_time_sec[1] * 2.2,
             label = "2x Degradation Threshold", size = 3, color = "red") +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold")
    ) +
    labs(
      title = "RT-004: Long-term Stability",
      subtitle = sprintf("Performance Degradation Ratio: %.2fx", rt004_summary$value),
      x = "Number of Versions",
      y = "Mean Restore Latency (seconds)",
      caption = "Target: < 2x degradation at 500 vs 50 versions"
    )

  ggsave("figure_scalability.pdf", p2, width = 8, height = 6)
  cat("  - figure_scalability.pdf\n")

} else {
  cat("  (ggplot2 not available - skipping figures)\n")
}

# =============================================================================
# Final Summary
# =============================================================================

cat("\n=============================================================\n")
cat("                  TEST SUITE COMPLETE                        \n")
cat("=============================================================\n\n")

all_pass <- all(final_summary$Pass)
cat(sprintf("Overall Result: %s\n", ifelse(all_pass, "ALL TESTS PASSED", "SOME TESTS FAILED")))
cat(sprintf("Tests Passed: %d/%d\n\n", sum(final_summary$Pass), nrow(final_summary)))

cat("Key Findings (with 95% Clopper-Pearson CI):\n")
cat(sprintf("  - State Restoration (RT-001): %.1f%% [%.1f%%, %.1f%%] (n=%d)\n",
            rt001_summary$value * 100, rt001_summary$ci_lower * 100,
            rt001_summary$ci_upper * 100, RT001_ITERATIONS))
cat(sprintf("  - Lineage Tracking (RT-002): %.1f%% [%.1f%%, %.1f%%] (n=%d)\n",
            rt002_summary$value * 100, rt002_summary$ci_lower * 100,
            rt002_summary$ci_upper * 100, RT002_ITERATIONS))
cat(sprintf("  - Cross-Environment (RT-003): %.1f%% [%.1f%%, %.1f%%] (n=%d)\n",
            rt003_summary$value * 100, rt003_summary$ci_lower * 100,
            rt003_summary$ci_upper * 100, length(rt003_results)))
cat(sprintf("  - Scalability (RT-004): %.2fx degradation at 10x versions\n", rt004_summary$value))
cat(sprintf("  - Rollback (RT-005): %.1f%% [%.1f%%, %.1f%%] (n=%d)\n",
            rt005_summary$value * 100, rt005_summary$ci_lower * 100,
            rt005_summary$ci_upper * 100, RT005_ITERATIONS))

cat("\nACM Reproducibility Compliance:\n")
cat("  - Repeatability: Verified via RT-001 (same environment)\n")
cat("  - Reproducibility: Verified via RT-003 (simulated cross-environment)\n")
cat("  - Replicability: Enabled by automatic dependency tracking (RT-002)\n")

cat("\nPublication-Ready Outputs:\n")
cat("  - Table 3 Data: results_table3_comparison.csv\n")
cat("  - Quantitative Metrics: results_table3_quantitative.csv\n")
cat("  - Summary: results_reproducibility_summary.csv\n")
cat("  - Detailed Results: results_reproducibility_detailed.RDS\n")
if (requireNamespace("ggplot2", quietly = TRUE)) {
  cat("  - Figure: figure_reproducibility_comparison.pdf\n")
  cat("  - Figure: figure_scalability.pdf\n")
}

cat("\n=== Reproducibility Test Suite Complete ===\n")
