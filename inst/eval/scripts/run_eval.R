#!/usr/bin/env Rscript
# OmicsLake Evaluation Suite - Main Runner
# Usage: Rscript run_eval.R [--config path/to/config.yml]

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
cat("=== OmicsLake Evaluation Suite ===\n\n")

if (!is.null(config_path)) {
  cat("Loading config from:", config_path, "\n")
  config <- ol_eval_load_config(config_path)
} else {
  cat("Using default configuration\n")
  config <- ol_eval_load_config()
}

cat("Project root:", config$project_root, "\n")
cat("Seed:", config$seed, "\n")
cat("Threads:", config$threads, "\n\n")

# Ensure output directory exists
dir.create(config$outputs$results_dir, recursive = TRUE, showWarnings = FALSE)

# Define output files
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
benchmark_file <- file.path(config$outputs$results_dir,
                            paste0("benchmark_", timestamp, ".jsonl"))
case_study_file <- file.path(config$outputs$results_dir,
                             paste0("case_study_", timestamp, ".jsonl"))

# Run benchmarks (W0-W2)
if (any(unlist(config$workloads[c("W0_io", "W1_queries", "W2_lineage")]))) {
  cat("--- Running Benchmarks (W0-W2) ---\n\n")
  tryCatch({
    ol_eval_run_benchmarks(config, output_file = benchmark_file)
  }, error = function(e) {
    cat("ERROR in benchmarks:", conditionMessage(e), "\n")
  })
  cat("\n")
}

# Run case study (W3)
if (isTRUE(config$workloads$W3_case_study)) {
  cat("--- Running Case Study (W3) ---\n\n")
  tryCatch({
    case_results <- ol_eval_run_case_study(config, output_file = case_study_file)

    # Generate case study report
    ol_eval_case_study_report(case_results,
                              file.path(config$outputs$results_dir, "case_study_report.md"))
  }, error = function(e) {
    cat("ERROR in case study:", conditionMessage(e), "\n")
  })
  cat("\n")
}

# Generate plots and summary
cat("--- Generating Reports ---\n\n")

if (file.exists(benchmark_file)) {
  tryCatch({
    # Aggregate to CSV
    csv_file <- file.path(config$outputs$results_dir,
                          paste0("summary_", timestamp, ".csv"))
    ol_eval_aggregate_results(benchmark_file, csv_file)
    cat("Summary CSV:", csv_file, "\n")

    # Generate plots
    ol_eval_plot_all(benchmark_file,
                     file.path(config$outputs$results_dir, "figures"),
                     format = "png")

    # Generate full report
    ol_eval_generate_report(benchmark_file, config$outputs$results_dir)

  }, error = function(e) {
    cat("ERROR generating reports:", conditionMessage(e), "\n")
  })
}

cat("\n=== Evaluation Complete ===\n")
cat("Results saved to:", config$outputs$results_dir, "\n")
