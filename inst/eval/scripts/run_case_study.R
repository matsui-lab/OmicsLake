#!/usr/bin/env Rscript
# OmicsLake Evaluation Suite - Case Study Only (W3)
# Usage: Rscript run_case_study.R [--config path/to/config.yml]

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
cat("=== OmicsLake Case Study (W3) ===\n\n")

if (!is.null(config_path)) {
  cat("Loading config from:", config_path, "\n")
  config <- ol_eval_load_config(config_path)
} else {
  cat("Using default configuration\n")
  config <- ol_eval_load_config()
}

cat("Project root:", config$project_root, "\n")
cat("Seed:", config$seed, "\n")
cat("Genes:", config$case_study$n_genes, "\n")
cat("Samples:", config$case_study$n_samples, "\n\n")

# Ensure output directory exists
dir.create(config$outputs$results_dir, recursive = TRUE, showWarnings = FALSE)

# Run case study
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
output_file <- file.path(config$outputs$results_dir,
                         paste0("case_study_", timestamp, ".jsonl"))

cat("Output file:", output_file, "\n\n")

results <- ol_eval_run_case_study(config, output_file = output_file, verbose = TRUE)

# Generate report
report_file <- file.path(config$outputs$results_dir, "case_study_report.md")
ol_eval_case_study_report(results, report_file)

# Print validation summary
cat("\n--- Validation Summary ---\n")
cat("Lineage valid:", results$validation$lineage$valid, "\n")
cat("Has parent_ref:", results$validation$has_parent_ref %||% FALSE, "\n")
cat("Has parent_version_id:", results$validation$has_parent_version_id %||% FALSE, "\n")
cat("Diff valid:", results$validation$diff_valid %||% FALSE, "\n")

if (!results$validation$lineage$valid) {
  cat("\nIssues:\n")
  for (issue in results$validation$lineage$issues) {
    cat("  -", issue, "\n")
  }
}

cat("\n=== Case Study Complete ===\n")
cat("Report:", report_file, "\n")

# Return non-zero exit code if validation failed
if (!results$validation$lineage$valid) {
  quit(status = 1)
}
