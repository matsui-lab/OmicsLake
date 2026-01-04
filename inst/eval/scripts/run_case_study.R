#!/usr/bin/env Rscript
# OmicsLake Evaluation Suite - Case Study Only (W3)
# Usage: Rscript run_case_study.R [--config path] [--render] [--out dir]

suppressPackageStartupMessages({
  library(OmicsLake)
})

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)

config_path <- NULL
output_dir <- NULL
do_render <- FALSE

i <- 1
while (i <= length(args)) {
  if (args[i] == "--config" && i < length(args)) {
    config_path <- args[i + 1]
    i <- i + 2
  } else if (args[i] == "--out" && i < length(args)) {
    output_dir <- args[i + 1]
    i <- i + 2
  } else if (args[i] == "--render") {
    do_render <- TRUE
    i <- i + 1
  } else if (args[i] == "--help" || args[i] == "-h") {
    cat("Usage: Rscript run_case_study.R [OPTIONS]\n\n")
    cat("Options:\n")
    cat("  --config PATH    Configuration file path\n")
    cat("  --out DIR        Output directory\n")
    cat("  --render         Render Rmd to HTML after run\n")
    cat("  --help, -h       Show this help message\n")
    quit(status = 0)
  } else if (!grepl("^--", args[i])) {
    # Positional argument: treat as config path
    config_path <- args[i]
    i <- i + 1
  } else {
    i <- i + 1
  }
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

# Override output directory if specified
if (!is.null(output_dir)) {
  config$outputs$results_dir <- output_dir
}

cat("Project root:", config$project_root, "\n")
cat("Seed:", config$seed, "\n")
cat("Genes:", config$case_study$n_genes, "\n")
cat("Samples:", config$case_study$n_samples, "\n")
cat("Render Rmd:", do_render, "\n\n")

# Ensure output directory exists
dir.create(config$outputs$results_dir, recursive = TRUE, showWarnings = FALSE)

# Run case study
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
output_file <- file.path(config$outputs$results_dir,
                         paste0("case_study_", timestamp, ".jsonl"))

cat("Output file:", output_file, "\n\n")

results <- ol_eval_run_case_study(config, output_file = output_file, verbose = TRUE)

# Generate markdown report
report_file <- file.path(config$outputs$results_dir, "case_study_report.md")
ol_eval_case_study_report(results, report_file)

# Render Rmd if requested
if (do_render) {
  cat("\n--- Rendering Case Study Rmd ---\n")

  rmd_file <- system.file("eval/case_study/rnaseq_case_study.Rmd", package = "OmicsLake")

  if (file.exists(rmd_file)) {
    tryCatch({
      if (requireNamespace("rmarkdown", quietly = TRUE)) {
        html_out <- file.path(config$outputs$results_dir, "rnaseq_case_study.html")
        rmarkdown::render(
          input = rmd_file,
          output_file = html_out,
          output_format = "html_document",
          params = list(config_path = config_path),
          quiet = FALSE
        )
        cat("Rendered to:", html_out, "\n")
      } else {
        cat("rmarkdown package not available for rendering\n")
      }
    }, error = function(e) {
      cat("Render failed:", conditionMessage(e), "\n")
    })
  } else {
    cat("Rmd file not found at:", rmd_file, "\n")
  }
}

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
