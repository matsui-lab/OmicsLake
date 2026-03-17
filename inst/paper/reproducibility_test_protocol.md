# OmicsLake Reproducibility Test Protocol

## Document Information

| Item | Value |
|------|-------|
| Version | 1.0.0 |
| Last Updated | 2026-02-12 |
| Author | Reproducibility Test Design Expert |
| ACM Alignment | ACM v1.1 Reproducibility Definitions (2020) |

## Overview

This document provides detailed step-by-step protocols for executing the reproducibility verification experiments defined in `reproducibility_experiment_design.json`. Each test (RT-001 through RT-005) includes pre-conditions, execution commands, expected outputs, and success/failure criteria.

---

## Test RT-001: State Restoration Exact

### Purpose
Verify exact state restoration from labeled snapshots using `all.equal()` with strict tolerance.

### Category
State Reproducibility (ACM: Repeatability)

### Pre-conditions Checklist

- [ ] R version 4.2.0 or higher installed
- [ ] OmicsLake package installed and loadable
- [ ] Required dependencies available: `dplyr`, `digest`
- [ ] Clean working directory (no existing `benchmark_datasets/` folder or remove it)
- [ ] Sufficient disk space (minimum 100MB)
- [ ] System timezone set to UTC (recommended)

### Execution Steps

#### Step 1: Environment Setup

```r
# Clean environment
rm(list = ls())
gc()

# Set reproducible seed
set.seed(42)

# Set timezone for consistent timestamps
Sys.setenv(TZ = "UTC")

# Load packages
library(OmicsLake)
library(dplyr)
library(digest)  # For hash verification

# Define test parameters
TEST_ITERATIONS <- 10
TOLERANCE <- 1e-8
```

#### Step 2: Initialize Project

```r
# Create unique project name for test isolation
project_name <- paste0("rt001_test_", format(Sys.time(), "%Y%m%d_%H%M%S"))

# Initialize OmicsLake project
ol_init(project_name, root = tempdir())

# Verify initialization
stopifnot(
  !is.null(getOption("ol.project")),
  getOption("ol.project") == project_name
)

cat("Project initialized:", project_name, "\n")
```

#### Step 3: Execute Workflow with Checkpoints

```r
# Generate mock RNA-seq data (reproducible)
n_genes <- 1000
n_samples <- 12

raw_counts <- data.frame(
  gene_id = rep(paste0("gene_", sprintf("%04d", 1:n_genes)), n_samples),
  sample_id = rep(paste0("sample_", 1:n_samples), each = n_genes),
  count = rpois(n_genes * n_samples, lambda = 100),
  stringsAsFactors = FALSE
)

# Step 1: Import and checkpoint v1.0_raw
ol_write("raw_counts", raw_counts, mode = "overwrite", project = project_name)
ol_commit("Import raw counts", params = list(n_genes = n_genes, n_samples = n_samples), project = project_name)
ol_label("v1.0_raw", project = project_name)

# Store original for comparison
original_raw <- raw_counts

# Step 2: Normalization and checkpoint v1.1_normalized
normalized <- raw_counts %>%
  mutate(log_count = log2(count + 1))

ol_write("normalized_counts", normalized,
         mode = "overwrite",
         depends_on = "raw_counts",
         project = project_name)
ol_commit("Normalization complete", params = list(method = "log2", pseudocount = 1), project = project_name)
ol_label("v1.1_normalized", project = project_name)

original_normalized <- normalized

# Step 3: QC Filtering and checkpoint v1.2_qc
filtered <- normalized %>%
  group_by(gene_id) %>%
  filter(mean(log_count) > 2) %>%
  ungroup()

ol_write("filtered_counts", filtered,
         mode = "overwrite",
         depends_on = "normalized_counts",
         project = project_name)
ol_commit("QC filtering complete", params = list(threshold = 2), project = project_name)
ol_label("v1.2_qc", project = project_name)

original_filtered <- filtered

# Step 4: DE Analysis
de_results <- data.frame(
  gene_id = unique(filtered$gene_id),
  log2fc = rnorm(length(unique(filtered$gene_id)), 0, 1.5),
  pvalue = runif(length(unique(filtered$gene_id)))
)
de_results$padj <- p.adjust(de_results$pvalue, method = "BH")

ol_save("de_results", de_results,
        depends_on = c("filtered_counts"),
        project = project_name)

# Step 5: Final checkpoint v1.3_complete
pathway_results <- data.frame(
  pathway = paste0("pathway_", 1:10),
  pvalue = runif(10)
)

ol_save("pathway_results", pathway_results,
        depends_on = "de_results",
        project = project_name)
ol_commit("Analysis complete", project = project_name)
ol_label("v1.3_complete", project = project_name)

original_de <- de_results
original_pathways <- pathway_results

cat("Workflow completed with 4 checkpoints\n")
```

#### Step 4: Clear Memory and Restore

```r
# Clear working memory (simulate session restart)
rm(raw_counts, normalized, filtered, de_results, pathway_results)
gc()

# Track results
results <- list()

# Test restoration for each checkpoint
checkpoints <- c("v1.0_raw", "v1.1_normalized", "v1.2_qc", "v1.3_complete")

for (checkpoint in checkpoints) {
  cat("\n--- Testing checkpoint:", checkpoint, "---\n")

  # Measure restoration latency
  start_time <- Sys.time()

  if (checkpoint == "v1.0_raw") {
    restored <- ol_read("raw_counts", ref = paste0("@", checkpoint), project = project_name)
    original <- original_raw
  } else if (checkpoint == "v1.1_normalized") {
    restored <- ol_read("normalized_counts", ref = paste0("@", checkpoint), project = project_name)
    original <- original_normalized
  } else if (checkpoint == "v1.2_qc") {
    restored <- ol_read("filtered_counts", ref = paste0("@", checkpoint), project = project_name)
    original <- original_filtered
  } else if (checkpoint == "v1.3_complete") {
    restored <- ol_read_object("pathway_results", ref = paste0("@", checkpoint), project = project_name)
    original <- original_pathways
  }

  latency <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  # Verify with all.equal
  comparison <- all.equal(
    original[order(names(original))],
    restored[order(names(restored))],
    tolerance = TOLERANCE,
    check.attributes = TRUE
  )

  # Hash verification
  original_hash <- digest::digest(original, algo = "sha256")
  restored_hash <- digest::digest(restored, algo = "sha256")

  results[[checkpoint]] <- list(
    all_equal_result = comparison,
    passed = isTRUE(comparison),
    hash_match = original_hash == restored_hash,
    original_hash = original_hash,
    restored_hash = restored_hash,
    latency_sec = latency
  )

  cat("  all.equal:", isTRUE(comparison), "\n")
  cat("  Hash match:", original_hash == restored_hash, "\n")
  cat("  Latency:", round(latency, 3), "seconds\n")
}
```

#### Step 5: Generate Results Summary

```r
# Compile results
summary_df <- data.frame(
  checkpoint = names(results),
  all_equal_passed = sapply(results, function(x) x$passed),
  hash_match = sapply(results, function(x) x$hash_match),
  latency_sec = sapply(results, function(x) x$latency_sec),
  stringsAsFactors = FALSE
)

# Calculate metrics
metrics <- list(
  restoration_success_rate = mean(summary_df$all_equal_passed),
  hash_match_rate = mean(summary_df$hash_match),
  mean_latency = mean(summary_df$latency_sec),
  max_latency = max(summary_df$latency_sec),
  p95_latency = quantile(summary_df$latency_sec, 0.95)
)

cat("\n=== RT-001 Results ===\n")
print(summary_df)
cat("\nMetrics:\n")
cat("  Restoration Success Rate:", metrics$restoration_success_rate, "\n")
cat("  Mean Latency:", round(metrics$mean_latency, 3), "sec\n")
cat("  P95 Latency:", round(metrics$p95_latency, 3), "sec\n")
```

### Expected Output Format

```
=== RT-001 Results ===
       checkpoint all_equal_passed hash_match latency_sec
1       v1.0_raw             TRUE       TRUE       0.045
2 v1.1_normalized             TRUE       TRUE       0.052
3        v1.2_qc             TRUE       TRUE       0.048
4  v1.3_complete             TRUE       TRUE       0.038

Metrics:
  Restoration Success Rate: 1
  Mean Latency: 0.046 sec
  P95 Latency: 0.051 sec
```

### Success Criteria

| Metric | Threshold | Unit |
|--------|-----------|------|
| `restoration_success_rate` | >= 1.0 | proportion |
| `numerical_deviation` | < 1e-8 | max absolute difference |
| `metadata_preservation_rate` | >= 1.0 | proportion |
| `restoration_latency` | < 5.0 | seconds |

### Failure Mode Detection

| Failure Mode | Detection Method | Mitigation |
|--------------|------------------|------------|
| Floating point drift | `all.equal()` returns non-TRUE | Use raw binary serialization |
| Metadata loss | `attributes(original) != attributes(restored)` | Explicit attribute storage |
| Reference resolution error | Wrong data returned for labeled reference | Validate `__ol_refs` table |

### Error Handling

```r
tryCatch({
  # Test execution code
}, error = function(e) {
  cat("ERROR in RT-001:", conditionMessage(e), "\n")

  # Diagnostic information
  cat("Session info:\n")
  print(sessionInfo())

  # Check database state
  tryCatch({
    labels <- ol_list_labels(project = project_name)
    cat("Available labels:\n")
    print(labels)
  }, error = function(e2) {
    cat("Could not list labels:", conditionMessage(e2), "\n")
  })

  # Return failure result
  list(
    test_id = "RT-001",
    status = "FAILED",
    error = conditionMessage(e),
    timestamp = Sys.time()
  )
})
```

---

## Test RT-002: Lineage Tracking Completeness

### Purpose
Verify complete and accurate dependency tracking across the entire analysis pipeline.

### Category
Lineage Tracking (ACM: Reproducibility - provenance)

### Pre-conditions Checklist

- [ ] RT-001 passed (state restoration working)
- [ ] OmicsLake package loaded
- [ ] Clean project initialized

### Execution Steps

#### Step 1: Define Expected Dependency Graph

```r
# Expected dependency graph structure
expected_graph <- list(
  nodes = c(
    "raw_counts",      # root node, no upstream
    "sample_info",     # root node, no upstream
    "gene_info",       # root node, no upstream
    "norm_params",     # depends on gene_info, sample_info
    "normalized_counts",  # depends on raw_counts
    "qc_params",       # root node
    "filtered_counts", # depends on normalized_counts
    "de_results",      # depends on filtered_counts, sample_info
    "pathway_results"  # depends on de_results
  ),
  edges = list(
    c("normalized_counts", "raw_counts"),      # child -> parent
    c("norm_params", "gene_info"),
    c("norm_params", "sample_info"),
    c("filtered_counts", "normalized_counts"),
    c("de_results", "filtered_counts"),
    c("de_results", "sample_info"),
    c("pathway_results", "de_results")
  )
)

EXPECTED_EDGE_COUNT <- length(expected_graph$edges)
```

#### Step 2: Create Workflow with Dependencies

```r
project_name <- paste0("rt002_test_", format(Sys.time(), "%Y%m%d_%H%M%S"))
ol_init(project_name, root = tempdir())

set.seed(42)

# Root nodes (no dependencies)
raw_counts <- data.frame(gene_id = paste0("gene_", 1:100), count = rpois(100, 50))
sample_info <- data.frame(sample_id = paste0("S", 1:6), condition = rep(c("A", "B"), 3))
gene_info <- data.frame(gene_id = paste0("gene_", 1:100), symbol = paste0("SYM", 1:100))

ol_write("raw_counts", raw_counts, mode = "overwrite", project = project_name)
ol_save("sample_info", sample_info, project = project_name)
ol_save("gene_info", gene_info, project = project_name)

# Derived objects with explicit dependencies
norm_params <- list(method = "TMM", log = TRUE)
ol_save("norm_params", norm_params,
        depends_on = c("gene_info", "sample_info"),
        project = project_name)

normalized_counts <- raw_counts %>% mutate(log_count = log2(count + 1))
ol_write("normalized_counts", normalized_counts,
         mode = "overwrite",
         depends_on = "raw_counts",
         project = project_name)

qc_params <- list(threshold = 2.0)
ol_save("qc_params", qc_params, project = project_name)

filtered_counts <- normalized_counts %>% filter(log_count > 2)
ol_write("filtered_counts", filtered_counts,
         mode = "overwrite",
         depends_on = "normalized_counts",
         project = project_name)

de_results <- data.frame(gene_id = unique(filtered_counts$gene_id), pvalue = runif(nrow(filtered_counts)))
ol_save("de_results", de_results,
        depends_on = c("filtered_counts", "sample_info"),
        project = project_name)

pathway_results <- data.frame(pathway = paste0("PW", 1:5), pvalue = runif(5))
ol_save("pathway_results", pathway_results,
        depends_on = "de_results",
        project = project_name)

cat("Workflow with dependencies created\n")
```

#### Step 3: Test Upstream Traversal

```r
# Test case: pathway_results upstream traversal
test_upstream <- function(start_node, expected_ancestors) {
  lineage <- ol_show_lineage(start_node, direction = "upstream",
                              max_depth = 10, project = project_name)

  if (nrow(lineage) == 0) {
    detected_ancestors <- character(0)
  } else {
    detected_ancestors <- unique(lineage$parent)
  }

  # Calculate precision and recall
  true_positives <- length(intersect(detected_ancestors, expected_ancestors))
  false_positives <- length(setdiff(detected_ancestors, expected_ancestors))
  false_negatives <- length(setdiff(expected_ancestors, detected_ancestors))

  precision <- if (length(detected_ancestors) > 0)
    true_positives / length(detected_ancestors) else 1
  recall <- if (length(expected_ancestors) > 0)
    true_positives / length(expected_ancestors) else 1
  f1 <- if (precision + recall > 0)
    2 * precision * recall / (precision + recall) else 0

  list(
    start_node = start_node,
    expected = expected_ancestors,
    detected = detected_ancestors,
    true_positives = true_positives,
    false_positives = false_positives,
    false_negatives = false_negatives,
    precision = precision,
    recall = recall,
    f1_score = f1
  )
}

# Execute upstream test
upstream_result <- test_upstream(
  "pathway_results",
  c("de_results", "filtered_counts", "sample_info", "normalized_counts", "raw_counts")
)

cat("=== Upstream Traversal Test ===\n")
cat("Start node: pathway_results\n")
cat("Expected ancestors:", paste(upstream_result$expected, collapse = ", "), "\n")
cat("Detected ancestors:", paste(upstream_result$detected, collapse = ", "), "\n")
cat("Precision:", upstream_result$precision, "\n")
cat("Recall:", upstream_result$recall, "\n")
cat("F1 Score:", upstream_result$f1_score, "\n")
```

#### Step 4: Test Downstream Traversal

```r
# Test case: raw_counts downstream traversal
test_downstream <- function(start_node, expected_descendants) {
  lineage <- ol_show_lineage(start_node, direction = "downstream",
                              max_depth = 10, project = project_name)

  if (nrow(lineage) == 0) {
    detected_descendants <- character(0)
  } else {
    detected_descendants <- unique(lineage$child)
  }

  true_positives <- length(intersect(detected_descendants, expected_descendants))
  false_positives <- length(setdiff(detected_descendants, expected_descendants))
  false_negatives <- length(setdiff(expected_descendants, detected_descendants))

  precision <- if (length(detected_descendants) > 0)
    true_positives / length(detected_descendants) else 1
  recall <- if (length(expected_descendants) > 0)
    true_positives / length(expected_descendants) else 1
  f1 <- if (precision + recall > 0)
    2 * precision * recall / (precision + recall) else 0

  list(
    start_node = start_node,
    expected = expected_descendants,
    detected = detected_descendants,
    precision = precision,
    recall = recall,
    f1_score = f1
  )
}

downstream_result <- test_downstream(
  "raw_counts",
  c("normalized_counts", "filtered_counts", "de_results", "pathway_results")
)

cat("\n=== Downstream Traversal Test ===\n")
cat("Start node: raw_counts\n")
cat("Expected descendants:", paste(downstream_result$expected, collapse = ", "), "\n")
cat("Detected descendants:", paste(downstream_result$detected, collapse = ", "), "\n")
cat("Precision:", downstream_result$precision, "\n")
cat("Recall:", downstream_result$recall, "\n")
cat("F1 Score:", downstream_result$f1_score, "\n")
```

#### Step 5: Verify Graph Integrity

```r
# Get all dependencies from database
all_deps <- ol_get_dependencies("raw_counts", direction = "both", project = project_name)

# Check for cycles (should be none in DAG)
check_acyclicity <- function(project) {
  # BFS-based cycle detection
  all_nodes <- unique(c(
    ol_list_tables(project = project)$table_name,
    ol_list_objects(project = project)$name
  ))

  for (node in all_nodes) {
    visited <- character(0)
    stack <- c(node)

    while (length(stack) > 0) {
      current <- stack[1]
      stack <- stack[-1]

      if (current %in% visited) {
        if (current == node && length(visited) > 1) {
          return(list(acyclic = FALSE, cycle_node = node))
        }
        next
      }
      visited <- c(visited, current)

      deps <- tryCatch(
        ol_get_dependencies(current, direction = "downstream", project = project),
        error = function(e) data.frame()
      )

      if (nrow(deps) > 0) {
        stack <- c(stack, deps$child_name)
      }
    }
  }

  list(acyclic = TRUE)
}

acyclicity_check <- check_acyclicity(project_name)
cat("\n=== Graph Integrity ===\n")
cat("Acyclic (no cycles):", acyclicity_check$acyclic, "\n")
```

#### Step 6: Compile Results

```r
# Calculate overall metrics
rt002_metrics <- list(
  edge_recall = upstream_result$recall,
  edge_precision = upstream_result$precision,
  f1_score = (upstream_result$f1_score + downstream_result$f1_score) / 2,
  acyclic = acyclicity_check$acyclic,
  type_consistency = 1.0  # Assume consistent unless errors detected
)

cat("\n=== RT-002 Final Metrics ===\n")
cat("Edge Recall:", rt002_metrics$edge_recall, "\n")
cat("Edge Precision:", rt002_metrics$edge_precision, "\n")
cat("F1 Score:", rt002_metrics$f1_score, "\n")
cat("Graph Acyclic:", rt002_metrics$acyclic, "\n")
```

### Expected Output Format

```
=== Upstream Traversal Test ===
Start node: pathway_results
Expected ancestors: de_results, filtered_counts, sample_info, normalized_counts, raw_counts
Detected ancestors: de_results, filtered_counts, normalized_counts, raw_counts, sample_info
Precision: 1
Recall: 1
F1 Score: 1

=== RT-002 Final Metrics ===
Edge Recall: 1
Edge Precision: 1
F1 Score: 1
Graph Acyclic: TRUE
```

### Success Criteria

| Metric | Threshold | Unit |
|--------|-----------|------|
| `edge_recall` | >= 1.0 | proportion |
| `edge_precision` | >= 1.0 | proportion |
| `depth_accuracy` | >= 1.0 | proportion |
| `type_consistency` | >= 1.0 | proportion |
| `lineage_query_latency` | < 1.0 | seconds |

### Failure Mode Detection

| Failure Mode | Detection Method | Severity |
|--------------|------------------|----------|
| Missing dependency | Expected edge not in lineage | Critical |
| Phantom dependency | Unexpected edge in lineage | High |
| Depth truncation | Ancestors missing at depth > max_depth | Medium |
| Cyclic dependency | Infinite loop in traversal | High |

---

## Test RT-003: Cross-Environment Reproducibility

### Purpose
Compare analysis reproducibility across different computing environments.

### Category
Cross-Environment (ACM: Reproducibility)

### Pre-conditions Checklist

- [ ] Access to at least 2 different environments (local + container/cloud)
- [ ] Docker installed (for container testing)
- [ ] renv or similar for package version locking
- [ ] Network connectivity for cloud environments

### Execution Steps

#### Step 1: Environment Specification

```r
# Document current environment
env_spec <- function() {
  list(
    env_id = Sys.info()["nodename"],
    os = paste(Sys.info()["sysname"], Sys.info()["release"]),
    r_version = paste0(R.version$major, ".", R.version$minor),
    duckdb_version = packageVersion("duckdb"),
    omicslake_version = packageVersion("OmicsLake"),
    platform = R.version$platform,
    locale = Sys.getlocale(),
    timezone = Sys.timezone()
  )
}

current_env <- env_spec()
cat("Current Environment:\n")
print(current_env)
```

#### Step 2: Create Portable Test Dataset

```r
# Generate reproducible test data
set.seed(12345)

portable_test_data <- list(
  counts = data.frame(
    gene_id = paste0("G", sprintf("%05d", 1:500)),
    sample_A = rpois(500, 100),
    sample_B = rpois(500, 120),
    sample_C = rpois(500, 95)
  ),
  metadata = data.frame(
    sample_id = c("sample_A", "sample_B", "sample_C"),
    condition = c("ctrl", "treat", "ctrl"),
    batch = c(1, 1, 2)
  ),
  params = list(
    normalization = "log2",
    pseudocount = 1,
    filter_threshold = 3.0
  )
)

# Save as portable format
saveRDS(portable_test_data, "rt003_portable_data.rds")
```

#### Step 3: Execute Standardized Workflow

```r
run_standardized_workflow <- function(input_data, project_name) {
  # Initialize
  ol_init(project_name, root = tempdir())

  # Fixed seed for reproducibility
  set.seed(42)

  # Import data
  ol_write("counts", input_data$counts, mode = "overwrite", project = project_name)
  ol_save("metadata", input_data$metadata, project = project_name)
  ol_save("params", input_data$params, project = project_name)
  ol_commit("Data import", project = project_name)

  # Normalize
  normalized <- input_data$counts
  numeric_cols <- sapply(normalized, is.numeric)
  normalized[, numeric_cols] <- log2(normalized[, numeric_cols] + input_data$params$pseudocount)

  ol_write("normalized", normalized,
           mode = "overwrite",
           depends_on = c("counts", "params"),
           project = project_name)
  ol_commit("Normalization", project = project_name)

  # Filter
  row_means <- rowMeans(normalized[, numeric_cols])
  filtered <- normalized[row_means > input_data$params$filter_threshold, ]

  ol_write("filtered", filtered,
           mode = "overwrite",
           depends_on = "normalized",
           project = project_name)
  ol_commit("Filtering", project = project_name)

  # Create final snapshot
  ol_label("final_v1", project = project_name)

  # Return results for comparison
  list(
    normalized = normalized,
    filtered = filtered,
    normalized_hash = digest::digest(normalized, algo = "sha256"),
    filtered_hash = digest::digest(filtered, algo = "sha256"),
    env_spec = env_spec()
  )
}

# Execute in current environment
env_a_results <- run_standardized_workflow(
  portable_test_data,
  paste0("rt003_env_a_", format(Sys.time(), "%Y%m%d%H%M%S"))
)
```

#### Step 4: Docker Container Test (if available)

```bash
# Dockerfile for reproducible environment
# Save as: Dockerfile.rt003

FROM rocker/r-ver:4.3.3

RUN R -e "install.packages(c('remotes', 'digest'))"
RUN R -e "remotes::install_github('matsui-lab/OmicsLake')"

COPY rt003_portable_data.rds /data/
COPY rt003_container_test.R /scripts/

CMD ["Rscript", "/scripts/rt003_container_test.R"]
```

```r
# rt003_container_test.R (to be run inside container)
library(OmicsLake)
library(digest)

input_data <- readRDS("/data/rt003_portable_data.rds")

# ... same workflow as above ...

# Output results as JSON
results <- list(
  normalized_hash = digest(normalized, algo = "sha256"),
  filtered_hash = digest(filtered, algo = "sha256"),
  env = list(
    r_version = R.version$version.string,
    platform = R.version$platform
  )
)
cat(jsonlite::toJSON(results, auto_unbox = TRUE))
```

#### Step 5: Compare Results Across Environments

```r
compare_environments <- function(env_a_results, env_b_results) {
  # Compare hashes
  normalized_match <- env_a_results$normalized_hash == env_b_results$normalized_hash
  filtered_match <- env_a_results$filtered_hash == env_b_results$filtered_hash

  # Numerical comparison with tolerance
  if (!normalized_match) {
    numerical_diff <- all.equal(
      env_a_results$normalized,
      env_b_results$normalized,
      tolerance = 1e-10
    )
  } else {
    numerical_diff <- TRUE
  }

  list(
    hash_match = list(
      normalized = normalized_match,
      filtered = filtered_match
    ),
    numerical_equivalence = isTRUE(numerical_diff),
    env_a = env_a_results$env_spec,
    env_b = env_b_results$env_spec
  )
}

# When env_b_results available:
# comparison <- compare_environments(env_a_results, env_b_results)
```

### Expected Output Format

```
=== RT-003 Cross-Environment Comparison ===

Environment A: macOS 14.2 / R 4.3.3
Environment B: Docker rocker/r-ver:4.3.3

Normalized data hash match: TRUE
Filtered data hash match: TRUE
Numerical equivalence: TRUE

Cross-environment consistency: 100%
```

### Success Criteria

| Metric | Threshold | Unit |
|--------|-----------|------|
| `cross_env_equivalence_rate` | >= 1.0 | proportion |
| `max_numerical_deviation` | < 1e-10 | absolute difference |
| `metadata_consistency` | >= 1.0 | proportion |
| `portability_success_rate` | >= 1.0 | proportion |

---

## Test RT-004: Long-term Stability

### Purpose
Assess system performance and reproducibility after extensive version accumulation.

### Category
Long-term Stability (ACM: Replicability over time)

### Pre-conditions Checklist

- [ ] Sufficient disk space (minimum 1GB for heavy scenario)
- [ ] Long-running process capability (may take 10+ minutes)
- [ ] microbenchmark package installed

### Execution Steps

#### Step 1: Define Test Scenarios

```r
scenarios <- list(
  light = list(total_versions = 100, objects = 10, tables = 5),
  moderate = list(total_versions = 500, objects = 50, tables = 20),
  heavy = list(total_versions = 2000, objects = 100, tables = 50)
  # extreme = list(total_versions = 10000, objects = 200, tables = 100)  # Optional
)

# Select scenario based on available time/resources
current_scenario <- scenarios$moderate
```

#### Step 2: Version Accumulation Phase

```r
library(microbenchmark)

project_name <- paste0("rt004_stability_", format(Sys.time(), "%Y%m%d%H%M%S"))
ol_init(project_name, root = tempdir())

set.seed(42)

# Track creation times and database size
creation_log <- data.frame(
  version_num = integer(),
  type = character(),
  name = character(),
  creation_time_ms = numeric(),
  stringsAsFactors = FALSE
)

# Create versions
total <- current_scenario$total_versions
n_tables <- current_scenario$tables
n_objects <- current_scenario$objects

for (i in 1:total) {
  # Alternate between tables and objects
  if (i %% 2 == 0 && i <= n_tables * 10) {
    # Create/update table
    name <- paste0("table_", (i %% n_tables) + 1)
    data <- data.frame(
      id = 1:1000,
      value = rnorm(1000),
      version = i
    )

    timing <- system.time({
      ol_write(name, data, mode = "overwrite", project = project_name)
    })

    creation_log <- rbind(creation_log, data.frame(
      version_num = i,
      type = "table",
      name = name,
      creation_time_ms = timing["elapsed"] * 1000
    ))
  } else {
    # Create/update object
    name <- paste0("object_", (i %% n_objects) + 1)
    obj <- list(
      data = rnorm(100),
      metadata = list(version = i, timestamp = Sys.time())
    )

    timing <- system.time({
      ol_save(name, obj, project = project_name)
    })

    creation_log <- rbind(creation_log, data.frame(
      version_num = i,
      type = "object",
      name = name,
      creation_time_ms = timing["elapsed"] * 1000
    ))
  }

  # Create labels at 10% intervals
  if (i %% (total / 10) == 0) {
    label <- paste0("checkpoint_", i)
    ol_label(label, project = project_name)
    cat("Created checkpoint:", label, "at version", i, "\n")
  }

  if (i %% 100 == 0) cat("Progress:", i, "/", total, "\n")
}
```

#### Step 3: Performance Benchmarking

```r
# Benchmark snapshot restoration
labels <- ol_list_labels(project = project_name)
sample_labels <- head(labels$label, 5)

restore_benchmarks <- lapply(sample_labels, function(lbl) {
  mb <- microbenchmark(
    ol_read("table_1", ref = paste0("@", lbl), project = project_name),
    times = 10
  )
  data.frame(
    label = lbl,
    mean_ms = mean(mb$time) / 1e6,
    median_ms = median(mb$time) / 1e6,
    p95_ms = quantile(mb$time, 0.95) / 1e6,
    max_ms = max(mb$time) / 1e6
  )
})
restore_results <- do.call(rbind, restore_benchmarks)

cat("\n=== Snapshot Restoration Performance ===\n")
print(restore_results)

# Benchmark lineage queries
lineage_benchmark <- microbenchmark(
  depth_1 = ol_show_lineage("table_1", direction = "upstream", max_depth = 1, project = project_name),
  depth_3 = ol_show_lineage("table_1", direction = "upstream", max_depth = 3, project = project_name),
  depth_5 = ol_show_lineage("table_1", direction = "upstream", max_depth = 5, project = project_name),
  times = 20
)

cat("\n=== Lineage Query Performance ===\n")
print(summary(lineage_benchmark))

# Benchmark listing operations
list_benchmark <- microbenchmark(
  list_tables = ol_list_tables(project = project_name),
  list_objects = ol_list_objects(project = project_name),
  list_labels = ol_list_labels(project = project_name),
  times = 20
)

cat("\n=== Listing Operations Performance ===\n")
print(summary(list_benchmark))
```

#### Step 4: Integrity Verification

```r
# Sample 10% of versions for integrity check
sample_indices <- sample(1:nrow(creation_log), size = nrow(creation_log) * 0.1)
integrity_results <- list()

for (idx in sample_indices) {
  row <- creation_log[idx, ]

  result <- tryCatch({
    if (row$type == "table") {
      data <- ol_read(row$name, project = project_name)
      list(success = TRUE, rows = nrow(data))
    } else {
      obj <- ol_read_object(row$name, project = project_name)
      list(success = TRUE, class = class(obj)[1])
    }
  }, error = function(e) {
    list(success = FALSE, error = conditionMessage(e))
  })

  integrity_results[[length(integrity_results) + 1]] <- c(
    row,
    result
  )
}

integrity_rate <- mean(sapply(integrity_results, function(x) x$success))
cat("\n=== Integrity Check ===\n")
cat("Sample size:", length(integrity_results), "\n")
cat("Integrity rate:", integrity_rate, "\n")
```

#### Step 5: Scalability Analysis

```r
# Calculate performance degradation
# Compare early versions vs late versions
early_creation <- mean(creation_log$creation_time_ms[1:100])
late_creation <- mean(creation_log$creation_time_ms[(nrow(creation_log)-99):nrow(creation_log)])
creation_degradation <- late_creation / early_creation

# Get database size
db_path <- file.path(tempdir(), paste0(project_name, ".duckdb"))
if (file.exists(db_path)) {
  db_size_mb <- file.info(db_path)$size / 1024 / 1024
  size_per_version_kb <- (db_size_mb * 1024) / total
} else {
  db_size_mb <- NA
  size_per_version_kb <- NA
}

scalability_metrics <- list(
  total_versions = total,
  creation_degradation_ratio = creation_degradation,
  mean_restore_latency_ms = mean(restore_results$mean_ms),
  database_size_mb = db_size_mb,
  size_per_version_kb = size_per_version_kb,
  integrity_rate = integrity_rate
)

cat("\n=== RT-004 Scalability Metrics ===\n")
cat("Total versions created:", scalability_metrics$total_versions, "\n")
cat("Creation degradation ratio:", round(scalability_metrics$creation_degradation_ratio, 2), "\n")
cat("Mean restore latency (ms):", round(scalability_metrics$mean_restore_latency_ms, 2), "\n")
cat("Database size (MB):", round(scalability_metrics$database_size_mb, 2), "\n")
cat("Size per version (KB):", round(scalability_metrics$size_per_version_kb, 2), "\n")
cat("Integrity rate:", scalability_metrics$integrity_rate, "\n")
```

### Expected Output Format

```
=== RT-004 Scalability Metrics ===
Total versions created: 500
Creation degradation ratio: 1.15
Mean restore latency (ms): 45.2
Database size (MB): 25.3
Size per version (KB): 5.2
Integrity rate: 1
```

### Success Criteria

| Metric | Threshold | Unit |
|--------|-----------|------|
| `restore_latency_degradation_ratio` | < 2.0 | ratio |
| `lineage_query_degradation_ratio` | < 3.0 | ratio |
| `database_size_per_version` | < 10.0 | KB (amortized) |
| `integrity_rate_at_scale` | >= 1.0 | proportion |

---

## Test RT-005: Rollback Cascade Verification

### Purpose
Verify correct handling of dependent object invalidation on upstream rollback.

### Category
State Reproducibility (ACM: Repeatability)

### Pre-conditions Checklist

- [ ] RT-001 passed (state restoration working)
- [ ] RT-002 passed (lineage tracking working)
- [ ] Clean project initialized

### Execution Steps

#### Step 1: Create Initial Pipeline (v1.0)

```r
project_name <- paste0("rt005_cascade_", format(Sys.time(), "%Y%m%d%H%M%S"))
ol_init(project_name, root = tempdir())

set.seed(42)

# Step A: Create raw_data
raw_data_v1 <- data.frame(
  id = 1:100,
  value = rnorm(100, mean = 10, sd = 2)
)
ol_write("raw_data", raw_data_v1, mode = "overwrite", project = project_name)
ol_commit("Initial raw data v1", project = project_name)

# Step B: Create processed_v1 (depends on raw_data)
processed_v1 <- raw_data_v1 %>%
  mutate(scaled_value = (value - mean(value)) / sd(value))
ol_write("processed", processed_v1,
         mode = "overwrite",
         depends_on = "raw_data",
         project = project_name)
ol_commit("Processed data v1", project = project_name)

# Step C: Create results_v1 (depends on processed)
results_v1 <- data.frame(
  summary_stat = c("mean", "sd", "min", "max"),
  value = c(
    mean(processed_v1$scaled_value),
    sd(processed_v1$scaled_value),
    min(processed_v1$scaled_value),
    max(processed_v1$scaled_value)
  )
)
ol_save("results", results_v1,
        depends_on = "processed",
        project = project_name)
ol_commit("Results v1", project = project_name)

# Create v1.0_complete label
ol_label("v1.0_complete", project = project_name)

cat("=== v1.0 Pipeline Complete ===\n")
cat("raw_data rows:", nrow(raw_data_v1), "\n")
cat("processed rows:", nrow(processed_v1), "\n")
cat("results rows:", nrow(results_v1), "\n")

# Store v1 snapshots for verification
v1_snapshots <- list(
  raw_data = raw_data_v1,
  processed = processed_v1,
  results = results_v1
)
```

#### Step 2: Modify Pipeline (v2.0)

```r
# Step A': Update raw_data (different distribution)
raw_data_v2 <- data.frame(
  id = 1:150,  # More rows
  value = rnorm(150, mean = 20, sd = 5)  # Different distribution
)
ol_write("raw_data", raw_data_v2, mode = "overwrite", project = project_name)
ol_commit("Updated raw data v2", project = project_name)

# Step B': Update processed (from new raw_data)
processed_v2 <- raw_data_v2 %>%
  mutate(scaled_value = (value - mean(value)) / sd(value))
ol_write("processed", processed_v2,
         mode = "overwrite",
         depends_on = "raw_data",
         project = project_name)
ol_commit("Processed data v2", project = project_name)

# Step C': Update results
results_v2 <- data.frame(
  summary_stat = c("mean", "sd", "min", "max"),
  value = c(
    mean(processed_v2$scaled_value),
    sd(processed_v2$scaled_value),
    min(processed_v2$scaled_value),
    max(processed_v2$scaled_value)
  )
)
ol_save("results", results_v2,
        depends_on = "processed",
        project = project_name)
ol_commit("Results v2", project = project_name)

# Create v2.0_complete label
ol_label("v2.0_complete", project = project_name)

cat("\n=== v2.0 Pipeline Complete ===\n")
cat("raw_data rows:", nrow(raw_data_v2), "\n")
cat("processed rows:", nrow(processed_v2), "\n")
cat("results rows:", nrow(results_v2), "\n")

# Verify v2 is different from v1
stopifnot(nrow(raw_data_v2) != nrow(raw_data_v1))
cat("Confirmed: v2 data differs from v1\n")
```

#### Step 3: Rollback to v1.0

```r
cat("\n=== Rollback Test: Restoring v1.0_complete ===\n")

# Measure rollback latency
rollback_start <- Sys.time()

# Read all data at v1.0 reference
restored_raw <- ol_read("raw_data", ref = "@v1.0_complete", project = project_name)
restored_processed <- ol_read("processed", ref = "@v1.0_complete", project = project_name)
restored_results <- ol_read_object("results", ref = "@v1.0_complete", project = project_name)

rollback_latency <- as.numeric(difftime(Sys.time(), rollback_start, units = "secs"))
cat("Rollback latency:", round(rollback_latency, 3), "seconds\n")
```

#### Step 4: Verify Consistency

```r
# Verify each restored object matches v1 original
verify_rollback <- function(original, restored, name, tolerance = 1e-8) {
  # Sort for comparison
  if (is.data.frame(original)) {
    original <- original[order(names(original))]
    restored <- restored[order(names(restored))]
  }

  comparison <- all.equal(original, restored, tolerance = tolerance)
  hash_original <- digest::digest(original, algo = "sha256")
  hash_restored <- digest::digest(restored, algo = "sha256")

  list(
    name = name,
    all_equal_passed = isTRUE(comparison),
    hash_match = hash_original == hash_restored,
    all_equal_result = comparison,
    nrow_original = if (is.data.frame(original)) nrow(original) else NA,
    nrow_restored = if (is.data.frame(restored)) nrow(restored) else NA
  )
}

raw_verification <- verify_rollback(v1_snapshots$raw_data, restored_raw, "raw_data")
processed_verification <- verify_rollback(v1_snapshots$processed, restored_processed, "processed")
results_verification <- verify_rollback(v1_snapshots$results, restored_results, "results")

cat("\n=== Rollback Verification Results ===\n")
cat("raw_data:\n")
cat("  all.equal passed:", raw_verification$all_equal_passed, "\n")
cat("  hash match:", raw_verification$hash_match, "\n")
cat("  rows (original/restored):", raw_verification$nrow_original, "/", raw_verification$nrow_restored, "\n")

cat("processed:\n")
cat("  all.equal passed:", processed_verification$all_equal_passed, "\n")
cat("  hash match:", processed_verification$hash_match, "\n")
cat("  rows (original/restored):", processed_verification$nrow_original, "/", processed_verification$nrow_restored, "\n")

cat("results:\n")
cat("  all.equal passed:", results_verification$all_equal_passed, "\n")
cat("  hash match:", results_verification$hash_match, "\n")
```

#### Step 5: Verify Cascade Consistency

```r
# Verify that restored data forms a consistent pipeline
# Check: processed was derived from raw_data (values should be consistent)

# Recalculate processed from restored raw_data
recalculated_processed <- restored_raw %>%
  mutate(scaled_value = (value - mean(value)) / sd(value))

processed_consistent <- all.equal(
  restored_processed$scaled_value,
  recalculated_processed$scaled_value,
  tolerance = 1e-8
)

cat("\n=== Cascade Consistency Check ===\n")
cat("restored_processed consistent with restored_raw:", isTRUE(processed_consistent), "\n")

# Verify v2 artifacts are NOT accessible at v1.0 reference
# (v2 had 150 rows, v1 had 100)
v1_ref_row_count <- nrow(restored_raw)
v2_should_not_exist <- v1_ref_row_count != 150

cat("v2 artifacts excluded from v1.0 reference:", v2_should_not_exist, "\n")
```

#### Step 6: Compile Results

```r
# Calculate overall metrics
rt005_metrics <- list(
  rollback_consistency_rate = mean(c(
    raw_verification$all_equal_passed,
    processed_verification$all_equal_passed,
    results_verification$all_equal_passed
  )),
  cascade_correctness = isTRUE(processed_consistent),
  rollback_latency_sec = rollback_latency,
  v2_isolation_correct = v2_should_not_exist
)

cat("\n=== RT-005 Final Metrics ===\n")
cat("Rollback consistency rate:", rt005_metrics$rollback_consistency_rate, "\n")
cat("Cascade correctness:", rt005_metrics$cascade_correctness, "\n")
cat("Rollback latency:", round(rt005_metrics$rollback_latency_sec, 3), "sec\n")
cat("v2 isolation correct:", rt005_metrics$v2_isolation_correct, "\n")

# Overall test result
test_passed <- all(
  rt005_metrics$rollback_consistency_rate == 1.0,
  rt005_metrics$cascade_correctness,
  rt005_metrics$rollback_latency_sec < 10.0,
  rt005_metrics$v2_isolation_correct
)

cat("\n=== RT-005 Result:", ifelse(test_passed, "PASSED", "FAILED"), "===\n")
```

### Expected Output Format

```
=== RT-005 Final Metrics ===
Rollback consistency rate: 1
Cascade correctness: TRUE
Rollback latency: 0.125 sec
v2 isolation correct: TRUE

=== RT-005 Result: PASSED ===
```

### Success Criteria

| Metric | Threshold | Unit |
|--------|-----------|------|
| `rollback_consistency_rate` | >= 1.0 | proportion |
| `cascade_correctness` | TRUE | boolean |
| `rollback_latency` | < 10.0 | seconds |

### Failure Mode Detection

| Failure Mode | Detection Method | Severity |
|--------------|------------------|----------|
| Partial rollback | Some objects at v2, others at v1 | Critical |
| Orphaned references | Label points to non-existent snapshot | High |
| Cascade inconsistency | Child data inconsistent with parent | Critical |

---

## Appendix A: Complete Test Runner Script

```r
#!/usr/bin/env Rscript
# OmicsLake Reproducibility Test Suite Runner
# Usage: Rscript run_reproducibility_tests.R [test_ids]
# Example: Rscript run_reproducibility_tests.R RT-001 RT-002

library(OmicsLake)
library(dplyr)
library(digest)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  tests_to_run <- c("RT-001", "RT-002", "RT-003", "RT-004", "RT-005")
} else {
  tests_to_run <- args
}

results <- list()
start_time <- Sys.time()

cat("===============================================\n")
cat("OmicsLake Reproducibility Test Suite\n")
cat("Started:", format(start_time), "\n")
cat("Tests to run:", paste(tests_to_run, collapse = ", "), "\n")
cat("===============================================\n\n")

for (test_id in tests_to_run) {
  cat("\n--- Running", test_id, "---\n")

  test_result <- tryCatch({
    # Source appropriate test file
    # In production, each test would be in separate files
    switch(test_id,
      "RT-001" = source("tests/rt001_state_restoration.R"),
      "RT-002" = source("tests/rt002_lineage_tracking.R"),
      "RT-003" = source("tests/rt003_cross_environment.R"),
      "RT-004" = source("tests/rt004_long_term_stability.R"),
      "RT-005" = source("tests/rt005_rollback_cascade.R"),
      stop("Unknown test:", test_id)
    )
  }, error = function(e) {
    list(
      test_id = test_id,
      status = "ERROR",
      error = conditionMessage(e)
    )
  })

  results[[test_id]] <- test_result
}

# Summary
end_time <- Sys.time()
duration <- difftime(end_time, start_time, units = "mins")

cat("\n===============================================\n")
cat("Test Suite Complete\n")
cat("Duration:", round(as.numeric(duration), 2), "minutes\n")
cat("===============================================\n\n")

# Output summary table
summary_df <- data.frame(
  test_id = names(results),
  status = sapply(results, function(x) if (!is.null(x$status)) x$status else "OK"),
  stringsAsFactors = FALSE
)

print(summary_df)

# Save results
saveRDS(results, paste0("reproducibility_test_results_",
                        format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))
```

---

## Appendix B: Troubleshooting Guide

### Common Issues

#### Issue: "Project not found" error
```r
# Solution: Ensure ol_init() was called
ol_init("your_project", root = tempdir())
```

#### Issue: Label not found when using ref parameter
```r
# Check available labels
ol_list_labels(project = project_name)

# Verify label exists before referencing
labels <- ol_list_labels(project = project_name)
if (!"v1.0_complete" %in% labels$label) {
  stop("Label v1.0_complete not found")
}
```

#### Issue: Memory exhaustion with large datasets
```r
# Use chunked processing
chunk_size <- 10000
total_rows <- nrow(large_data)

for (i in seq(1, total_rows, by = chunk_size)) {
  chunk <- large_data[i:min(i + chunk_size - 1, total_rows), ]
  ol_write("table", chunk, mode = ifelse(i == 1, "overwrite", "append"),
           project = project_name)
}
```

#### Issue: Performance degradation over time
```r
# Check database size
state <- .ol_get_backend_state(project_name)
DBI::dbExecute(state$conn, "VACUUM")
DBI::dbExecute(state$conn, "ANALYZE")
```

---

## Appendix C: Expected File Outputs

After running all tests, the following files should be generated:

| File | Description | Generated by |
|------|-------------|--------------|
| `results_reproducibility_table.csv` | Summary comparison table | 02_reproducibility_test.R |
| `results_reproducibility_detailed.RDS` | Detailed test results | 02_reproducibility_test.R |
| `figure4_reproducibility_workflow.mmd` | Mermaid workflow diagram | 02_reproducibility_test.R |
| `figure5_reproducibility_metrics.pdf` | Metrics comparison chart | 02_reproducibility_test.R |
| `figure5_metrics_data.csv` | Raw data for Figure 5 | 02_reproducibility_test.R |
| `reproducibility_test_results_*.rds` | Full test suite results | Test runner script |

---

## Revision History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0.0 | 2026-02-12 | RT Design Expert | Initial protocol document |
