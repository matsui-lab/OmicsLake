#!/usr/bin/env Rscript
# OmicsLake Evaluation Suite - Plot Results
# Usage: Rscript plot_results.R <results.jsonl> [--output dir] [--format png|pdf|svg]

suppressPackageStartupMessages({
  library(OmicsLake)
})

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  cat("Usage: Rscript plot_results.R <results.jsonl> [--output dir] [--format png|pdf|svg]\n")
  quit(status = 1)
}

results_file <- args[1]
output_dir <- "inst/eval/results/figures"
format <- "png"

# Parse optional arguments
i <- 2
while (i <= length(args)) {
  if (args[i] == "--output" && i < length(args)) {
    output_dir <- args[i + 1]
    i <- i + 2
  } else if (args[i] == "--format" && i < length(args)) {
    format <- args[i + 1]
    i <- i + 2
  } else {
    i <- i + 1
  }
}

# Validate inputs
if (!file.exists(results_file)) {
  cat("ERROR: Results file not found:", results_file, "\n")
  quit(status = 1)
}

if (!format %in% c("png", "pdf", "svg")) {
  cat("ERROR: Invalid format. Use png, pdf, or svg\n")
  quit(status = 1)
}

cat("=== OmicsLake Results Plotting ===\n\n")
cat("Input:", results_file, "\n")
cat("Output:", output_dir, "\n")
cat("Format:", format, "\n\n")

# Generate plots
plots <- ol_eval_plot_all(results_file, output_dir, format)

cat("\n--- Generated Plots ---\n")
for (name in names(plots)) {
  if (!is.null(plots[[name]])) {
    cat("  ", name, ":", plots[[name]], "\n")
  }
}

# Also generate summary CSV
csv_file <- file.path(dirname(output_dir), "summary_latest.csv")
ol_eval_aggregate_results(results_file, csv_file)
cat("\nSummary:", csv_file, "\n")

cat("\n=== Plotting Complete ===\n")
