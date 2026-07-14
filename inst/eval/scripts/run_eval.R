#!/usr/bin/env Rscript
# OmicsLake Evaluation Suite - Main Runner
# Usage: Rscript run_eval.R [--config path] [--out dir] [--only W0,W1] [--no-baseline]

suppressPackageStartupMessages({
  library(OmicsLake)
})

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)

config_path <- NULL
output_dir <- NULL
only_workloads <- NULL
no_baseline <- FALSE
do_render <- FALSE

i <- 1
while (i <= length(args)) {
  if (args[i] == "--config" && i < length(args)) {
    config_path <- args[i + 1]
    i <- i + 2
  } else if (args[i] == "--out" && i < length(args)) {
    output_dir <- args[i + 1]
    i <- i + 2
  } else if (args[i] == "--only" && i < length(args)) {
    only_workloads <- strsplit(args[i + 1], ",")[[1]]
    i <- i + 2
  } else if (args[i] == "--no-baseline") {
    no_baseline <- TRUE
    i <- i + 1
  } else if (args[i] == "--render") {
    do_render <- TRUE
    i <- i + 1
  } else if (args[i] == "--help" || args[i] == "-h") {
    cat("Usage: Rscript run_eval.R [OPTIONS]\n\n")
    cat("Options:\n")
    cat("  --config PATH       Configuration file path\n")
    cat("  --out DIR           Output directory\n")
    cat("  --only W0,W1,W2,W3  Run only specified workloads\n")
    cat("  --no-baseline       Skip baseline comparisons\n")
    cat("  --render            Render case study Rmd to HTML\n")
    cat("  --help, -h          Show this help message\n")
    quit(status = 0)
  } else if (!grepl("^--", args[i])) {
    config_path <- args[i]
    i <- i + 1
  } else {
    i <- i + 1
  }
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

# Override output directory if specified
if (!is.null(output_dir)) {
  config$outputs$results_dir <- output_dir
}

# Apply --only filter
if (!is.null(only_workloads)) {
  config$workloads$W0_io <- "W0" %in% only_workloads
  config$workloads$W1_queries <- "W1" %in% only_workloads
  config$workloads$W2_lineage <- "W2" %in% only_workloads
  config$workloads$W3_case_study <- "W3" %in% only_workloads
}

# Apply --no-baseline
if (no_baseline) {
  config$baselines$B1_duckdb_dbplyr <- FALSE
  config$baselines$B2_file_based <- FALSE
}

cat("Project root:", config$project_root, "\n")
cat("Seed:", config$seed, "\n")
cat("Threads:", config$threads, "\n")
cat("Workloads:", paste(names(config$workloads)[unlist(config$workloads)], collapse = ", "), "\n")
cat("Baselines:", paste(names(config$baselines)[unlist(config$baselines)], collapse = ", "), "\n\n")

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

    # Render Rmd if requested
    if (do_render) {
      cat("\n--- Rendering Case Study Rmd ---\n")
      rmd_file <- system.file("eval/case_study/rnaseq_case_study.Rmd", package = "OmicsLake")

      if (file.exists(rmd_file) && requireNamespace("rmarkdown", quietly = TRUE)) {
        html_out <- file.path(config$outputs$results_dir, "rnaseq_case_study.html")
        tryCatch({
          rmarkdown::render(input = rmd_file, output_file = html_out, quiet = TRUE)
          cat("Rendered to:", html_out, "\n")
        }, error = function(e) {
          cat("Render failed:", conditionMessage(e), "\n")
        })
      }
    }

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
