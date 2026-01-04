#!/usr/bin/env Rscript
# OmicsLake Evaluation Suite - Benchmarks Only (W0-W2)
# Usage: Rscript run_benchmarks.R [--config path/to/config.yml]

suppressPackageStartupMessages({
  library(OmicsLake)
})

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)

config_path <- NULL
if (length(args) >= 2 && args[1] == "--config") {
  config_path <- args[2]
} else if (length(args) == 1) {
  config_path <- args[1]
}

# Load configuration
cat("=== OmicsLake Benchmarks (W0-W2) ===\n\n")

if (!is.null(config_path)) {
  cat("Loading config from:", config_path, "\n")
  config <- ol_eval_load_config(config_path)
} else {
  cat("Using default configuration\n")
  config <- ol_eval_load_config()
}

# Force disable case study
config$workloads$W3_case_study <- FALSE

cat("Project root:", config$project_root, "\n")
cat("Seed:", config$seed, "\n\n")

# Ensure output directory exists
dir.create(config$outputs$results_dir, recursive = TRUE, showWarnings = FALSE)

# Run benchmarks
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
output_file <- file.path(config$outputs$results_dir,
                         paste0("benchmark_", timestamp, ".jsonl"))

cat("Output file:", output_file, "\n\n")

results <- ol_eval_run_benchmarks(config, output_file = output_file)

# Quick summary
cat("\n--- Quick Summary ---\n")
records <- ol_eval_read_jsonl(output_file)
cat("Total measurements:", length(records), "\n")

# Aggregate
agg <- ol_eval_aggregate_results(output_file)
print(agg)

cat("\n=== Benchmarks Complete ===\n")
