#!/usr/bin/env Rscript
# Performance Benchmark for OmicsLake Paper
# Compares OmicsLake I/O, aggregation, join, and snapshot operations
# against base R and SQLite workflows

library(OmicsLake)
library(arrow)
library(duckdb)
library(dplyr)
library(bench)

cat("=== OmicsLake Performance Benchmark ===\n\n")

# Initialize results list
results <- list()

# Set working directory to paper directory
setwd("inst/paper")

# Ensure benchmark datasets exist
if (!file.exists("benchmark_datasets/dataset_100MB.parquet")) {
  cat("Benchmark datasets not found. Generating...\n")
  source("00_generate_datasets.R")
}

cat("--- 1. Table Import Benchmark ---\n")
cat("Comparing Parquet (OmicsLake) vs RDS (base R) loading times\n\n")

bench_import <- mark(
  baseR = {
    df_base <- readRDS("benchmark_datasets/rds_100MB.RDS")
  },
  omicslake = {
    df_ol <- arrow::read_parquet("benchmark_datasets/dataset_100MB.parquet")
  },
  iterations = 10,
  check = FALSE
)

results$import <- bench_import
print(bench_import)
cat("\n")

cat("--- 2. Aggregation Benchmark ---\n")
cat("Comparing dplyr vs OmicsLake SQL aggregation on 1M rows\n\n")

# Create test data for aggregation
set.seed(42)
counts <- data.frame(
  gene_id = rep(1:1000, each = 1000),
  sample1 = runif(1e6, 0, 100),
  sample2 = runif(1e6, 0, 100),
  sample3 = runif(1e6, 0, 100),
  sample4 = runif(1e6, 0, 100),
  sample5 = runif(1e6, 0, 100),
  sample6 = runif(1e6, 0, 100),
  stringsAsFactors = FALSE
)

# Initialize OmicsLake project for aggregation test
ol_init("agg_benchmark", root = "benchmark_datasets")
ol_write("agg_test", counts, mode = "overwrite")

bench_agg <- mark(
  dplyr = {
    agg_result <- counts %>%
      group_by(gene_id) %>%
      summarise(
        mean_s1 = mean(sample1),
        mean_s2 = mean(sample2),
        mean_s3 = mean(sample3),
        mean_s4 = mean(sample4),
        mean_s5 = mean(sample5),
        mean_s6 = mean(sample6)
      )
  },
  omicslake = {
    agg_result_ol <- ol_aggregate("agg_test",
      group_by = "gene_id",
      mean_s1 = list(func = "avg", col = "sample1"),
      mean_s2 = list(func = "avg", col = "sample2"),
      mean_s3 = list(func = "avg", col = "sample3"),
      mean_s4 = list(func = "avg", col = "sample4"),
      mean_s5 = list(func = "avg", col = "sample5"),
      mean_s6 = list(func = "avg", col = "sample6")
    )
  },
  iterations = 5,
  check = FALSE
)

results$aggregation <- bench_agg
print(bench_agg)
cat("\n")

cat("--- 3. Join Benchmark ---\n")
cat("Comparing base merge vs DuckDB SQL join on 1M rows\n\n")

# Load join tables
table1 <- read.csv("benchmark_datasets/table1_1M.csv")
table2 <- read.csv("benchmark_datasets/table2_1M.csv")

# Initialize OmicsLake project for join test
ol_init("join_benchmark", root = "benchmark_datasets")
ol_write("join_t1", table1, mode = "overwrite")
ol_write("join_t2", table2, mode = "overwrite")

bench_join <- mark(
  base_merge = {
    join_result <- merge(table1, table2, by = "id")
  },
  duckdb_sql = {
    join_result_ol <- ol_query("SELECT * FROM join_t1 INNER JOIN join_t2 USING (id)")
  },
  iterations = 3,
  check = FALSE
)

results$join <- bench_join
print(bench_join)
cat("\n")

cat("--- 4. Version Snapshot Benchmark ---\n")
cat("Comparing file copy vs OmicsLake commit (metadata-based versioning)\n\n")

# Create test data for snapshot
set.seed(42)
temp_data <- data.frame(
  x = rnorm(1e6),
  y = rnorm(1e6),
  z = rnorm(1e6),
  stringsAsFactors = FALSE
)

# Save for file copy test
saveRDS(temp_data, "benchmark_datasets/temp_filecopy.RDS")

# Initialize OmicsLake project for snapshot test
ol_init("snapshot_benchmark", root = "benchmark_datasets")
ol_write("temp_ol", temp_data, mode = "overwrite")

bench_snapshot <- mark(
  file_copy = {
    file.copy("benchmark_datasets/temp_filecopy.RDS", 
              "benchmark_datasets/temp_filecopy_2.RDS", 
              overwrite = TRUE)
  },
  ol_commit = {
    commit_id <- ol_commit("Snapshot test", params = list(test = "benchmark"))
  },
  iterations = 5,
  check = FALSE
)

results$snapshot <- bench_snapshot
print(bench_snapshot)
cat("\n")

# Clean up temporary files
unlink("benchmark_datasets/temp_filecopy_2.RDS")

cat("--- 5. Storage Efficiency Benchmark ---\n")
cat("Comparing disk usage: RDS vs Parquet\n\n")

# Get file sizes
rds_size <- file.size("benchmark_datasets/rds_100MB.RDS")
parquet_size <- file.size("benchmark_datasets/dataset_100MB.parquet")

storage_comparison <- data.frame(
  Format = c("RDS (base R)", "Parquet (OmicsLake)"),
  Size_MB = c(rds_size / 1024^2, parquet_size / 1024^2),
  Compression_Ratio = c(1.0, rds_size / parquet_size),
  stringsAsFactors = FALSE
)

results$storage <- storage_comparison
print(storage_comparison)
cat("\n")

cat("--- Summary ---\n")
cat("Saving results to inst/paper/results_performance.RDS\n\n")

# Save all results
saveRDS(results, "results_performance.RDS")

# Create summary table
summary_table <- data.frame(
  Task = c("Table Import", "Aggregation", "Join", "Snapshot"),
  OmicsLake_Median = c(
    as.numeric(bench_import[bench_import$expression == "omicslake", "median"]),
    as.numeric(bench_agg[bench_agg$expression == "omicslake", "median"]),
    as.numeric(bench_join[bench_join$expression == "duckdb_sql", "median"]),
    as.numeric(bench_snapshot[bench_snapshot$expression == "ol_commit", "median"])
  ),
  Baseline_Median = c(
    as.numeric(bench_import[bench_import$expression == "baseR", "median"]),
    as.numeric(bench_agg[bench_agg$expression == "dplyr", "median"]),
    as.numeric(bench_join[bench_join$expression == "base_merge", "median"]),
    as.numeric(bench_snapshot[bench_snapshot$expression == "file_copy", "median"])
  ),
  stringsAsFactors = FALSE
)

summary_table$Speedup <- summary_table$Baseline_Median / summary_table$OmicsLake_Median
summary_table$Improvement_Percent <- (1 - summary_table$OmicsLake_Median / summary_table$Baseline_Median) * 100

cat("\nPerformance Summary:\n")
print(summary_table)

cat("\n=== Benchmark Complete ===\n")
cat("Results saved to: inst/paper/results_performance.RDS\n")
cat("\nExpected relative performance:\n")
cat("  - Table Import: ~3-4x faster\n")
cat("  - Aggregation: ~2-3x faster\n")
cat("  - Join: ~2-3x faster (with lower memory usage)\n")
cat("  - Snapshot: ~10-100x faster (metadata-based vs file copy)\n")
cat("  - Storage: ~40-60% compression vs RDS\n")
