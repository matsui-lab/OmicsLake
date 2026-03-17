#!/usr/bin/env Rscript
# === Generate All Publication Figures ===
# OmicsLake Paper - Master Figure Generation Script
#
# This script generates all figures for the GigaScience manuscript:
#   - Figure 1: Architecture Diagram (TikZ/ggplot2)
#   - Figure 2: Lineage Tracking Workflow
#   - Figure 3: Performance Benchmarks
#   - Figure 4: Reproducibility Workflow Comparison
#   - Figure 5: Reproducibility Capability Matrix
#   - Figure 6: Storage Efficiency
#   - Figure 7: Major Results Dashboard
#   - Supplementary Figure S1: Lineage Graph
#
# Usage:
#   cd inst/paper/figures
#   Rscript generate_all_figures.R
#
# Requirements:
#   - ggplot2 (required)
#   - dplyr, tidyr (required)
#   - scales (required)
#   - patchwork (for combined figures)
#   - igraph, ggraph (optional, for lineage graph)
#   - ggradar (optional, for radar chart)
#
# For TikZ figures (Figure 1, Figure 2):
#   - LaTeX installation with pdflatex (e.g., tinytex, TeX Live, MiKTeX)
#   - TikZ packages: shapes.geometric, arrows.meta, positioning, fit, backgrounds
#   - Install tinytex in R: tinytex::install_tinytex()
#
# Output:
#   - PDF files (publication quality, vector) -> output/
#   - PNG files (web/preview, 300 dpi) -> output/
#   - Mermaid files (for diagram editing)
#
# GigaScience Specifications:
#   - Resolution: 300 DPI minimum
#   - Format: PDF (vector) preferred, PNG for raster
#   - Color: CMYK compatible (RGB with standard palette)

cat("=== OmicsLake Paper: Figure Generation ===\n\n")

# === Check Required Packages ===
required_packages <- c("ggplot2", "dplyr", "tidyr", "scales")
optional_packages <- c("patchwork", "igraph", "ggraph", "ggradar", "gridExtra", "grid")

check_packages <- function(packages, type = "required") {
  missing <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]
  if (length(missing) > 0) {
    if (type == "required") {
      stop("Missing required packages: ", paste(missing, collapse = ", "),
           "\nInstall with: install.packages(c('", paste(missing, collapse = "', '"), "'))")
    } else {
      message("Optional packages not installed (some features may be skipped): ",
              paste(missing, collapse = ", "))
    }
  }
}

cat("Checking packages...\n")
check_packages(required_packages, "required")
check_packages(optional_packages, "optional")

# Load required packages
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(scales)
})

# === Set Working Directory ===
.get_script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = FALSE))
  }
  ofile <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(ofile) && nzchar(ofile)) {
    return(normalizePath(ofile, mustWork = FALSE))
  }
  NULL
}

script_path <- .get_script_path()
script_dir <- if (!is.null(script_path) && nzchar(script_path)) dirname(script_path) else getwd()
setwd(script_dir)
cat("Working directory:", getwd(), "\n\n")

# === Create Output Directory ===
output_dir <- file.path(script_dir, "output")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("Created output directory:", output_dir, "\n\n")
}
cat("Output directory:", output_dir, "\n\n")

# === Figure Generation Functions ===

generate_all <- function(output_dir = "output") {

  # Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  results <- list()

  # --- Figure 1: Architecture Diagram ---
  cat("--- Generating Figure 1: Architecture Diagram ---\n")
  tryCatch({
    source("figure1_architecture.R")

    # Generate main PDF (vector)
    results$figure1 <- generate_architecture_diagram(
      file.path(output_dir, "figure1_architecture.pdf"),
      width = 8,
      height = 7
    )

    # Also save PNG version at 300 DPI
    generate_architecture_diagram(
      file.path(output_dir, "figure1_architecture.png"),
      width = 8,
      height = 7,
      dpi = 300
    )

    # Try TikZ version if LaTeX is available
    if (Sys.which("pdflatex") != "") {
      cat("  [INFO] Attempting TikZ compilation...\n")
      tex_file <- "figure1_architecture_standalone.tex"
      if (file.exists(tex_file)) {
        compile_tikz_to_output(tex_file, output_dir, "figure1_architecture_tikz.pdf")
      }
    }

    cat("  [OK] Figure 1 generated\n\n")
  }, error = function(e) {
    cat("  [ERROR] Figure 1:", e$message, "\n\n")
    results$figure1 <- NULL
  })

  # --- Figure 2: Lineage Tracking Workflow ---
  cat("--- Generating Figure 2: Lineage Tracking Workflow ---\n")
  tryCatch({
    source("figure2_lineage_workflow.R")
    results$figure2 <- generate_figure2(output_dir)

    # Try TikZ version if LaTeX is available
    if (Sys.which("pdflatex") != "") {
      cat("  [INFO] Attempting TikZ compilation...\n")
      tex_file <- "figure2_lineage_workflow.tex"
      if (file.exists(tex_file)) {
        compile_tikz_to_output(tex_file, output_dir, "figure2_lineage_workflow_tikz.pdf")
      }
    }

    cat("  [OK] Figure 2 generated\n\n")
  }, error = function(e) {
    cat("  [ERROR] Figure 2:", e$message, "\n\n")
    results$figure2 <- NULL
  })

  # --- Figure 3: Performance Benchmarks ---
  cat("--- Generating Figure 3: Performance Benchmarks ---\n")
  tryCatch({
    source("figure3_performance_benchmarks.R")
    results$figure3 <- generate_figure3(output_dir)
    cat("  [OK] Figure 3 generated\n\n")
  }, error = function(e) {
    cat("  [ERROR] Figure 3:", e$message, "\n\n")
    results$figure3 <- NULL
  })

  # --- Figure 4: Reproducibility Workflow ---
  cat("--- Generating Figure 4: Reproducibility Workflow ---\n")
  tryCatch({
    source("figure4_reproducibility_workflow.R")
    results$figure4 <- generate_figure4(output_dir)
    cat("  [OK] Figure 4 generated\n\n")
  }, error = function(e) {
    cat("  [ERROR] Figure 4:", e$message, "\n\n")
    results$figure4 <- NULL
  })

  # --- Figure 5: Reproducibility Comparison ---
  cat("--- Generating Figure 5: Reproducibility Comparison ---\n")
  tryCatch({
    source("figure5_reproducibility_comparison.R")
    results$figure5 <- generate_figure5(output_dir)
    cat("  [OK] Figure 5 generated\n\n")
  }, error = function(e) {
    cat("  [ERROR] Figure 5:", e$message, "\n\n")
    results$figure5 <- NULL
  })

  # --- Figure 6: Storage Efficiency ---
  cat("--- Generating Figure 6: Storage Efficiency ---\n")
  tryCatch({
    source("figure6_storage_efficiency.R")
    results$figure6 <- generate_figure6(output_dir)
    cat("  [OK] Figure 6 generated\n\n")
  }, error = function(e) {
    cat("  [ERROR] Figure 6:", e$message, "\n\n")
    results$figure6 <- NULL
  })

  # --- Figure 7: Major Results Dashboard ---
  cat("--- Generating Figure 7: Major Results Dashboard ---\n")
  tryCatch({
    source("figure7_key_results_dashboard.R")
    results$figure7 <- generate_figure7(output_dir)
    cat("  [OK] Figure 7 generated\n\n")
  }, error = function(e) {
    cat("  [ERROR] Figure 7:", e$message, "\n\n")
    results$figure7 <- NULL
  })

  # --- Supplementary Figure S1: Lineage Graph ---
  cat("--- Generating Figure S1: Lineage Graph ---\n")
  tryCatch({
    source("figureS1_lineage_graph.R")
    results$figureS1 <- generate_figureS1(output_dir)
    cat("  [OK] Figure S1 generated\n\n")
  }, error = function(e) {
    cat("  [ERROR] Figure S1:", e$message, "\n\n")
    results$figureS1 <- NULL
  })

  return(results)
}

# === Print Figure Specifications ===
print_specifications <- function() {
  cat("=== Figure Specifications ===\n\n")

  cat("Color Palette (GigaScience Compliant):\n")
  cat("  - Primary Blue:  #1f77b4 (OmicsLake/Parquet)\n")
  cat("  - Primary Orange: #ff7f0e (Baseline/RDS)\n")
  cat("  - Primary Green:  #2ca02c (Success/OmicsLake advantage)\n")
  cat("  - Red:           #e74c3c (Standard R Script)\n")
  cat("  - Yellow/Orange: #f39c12 (Git + Manual)\n")
  cat("  - Gray:          #7f7f7f (Neutral/Commits)\n")
  cat("  - Purple:        #9467bd (Version labels)\n\n")

  cat("Typography:\n")
  cat("  - Font Family: sans-serif (Arial/Helvetica)\n")
  cat("  - Title: 13pt bold\n")
  cat("  - Subtitle: 11pt regular (gray40)\n")
  cat("  - Axis labels: 11pt bold\n")
  cat("  - Axis text: 10pt regular\n")
  cat("  - Annotations: 9-10pt\n\n")

  cat("Figure Dimensions:\n")
  cat("  - Figure 1 (Architecture): 10 x 8 inches\n")
  cat("  - Figure 2 (Lineage Workflow): 10 x 6 inches\n")
  cat("  - Figure 3 (Performance): 10 x 6 inches\n")
  cat("  - Figure 4 (Reproducibility Workflow): 12 x 8 inches\n")
  cat("  - Figure 5 (Reproducibility Capability Matrix): 11 x 7 inches\n")
  cat("  - Figure 6 (Storage): 12 x 10 inches (combined)\n")
  cat("  - Figure 7 (Major Results Dashboard): 15 x 10 inches\n")
  cat("  - Figure S1 (Lineage Graph): 14 x 6 inches\n\n")

  cat("Output Formats:\n")
  cat("  - PDF: Vector format for publication\n")
  cat("  - PNG: Raster format at 300 dpi for preview/web\n")
  cat("  - SVG: Scalable vector graphics for editing\n")
  cat("  - MMD: Mermaid diagrams for editable workflow figures\n")
  cat("  - TEX: TikZ source for LaTeX compilation\n\n")
}

# === Print Figure Captions ===
print_captions <- function() {
  cat("=== Figure Captions ===\n\n")

  cat("FIGURE 1: OmicsLake System Architecture\n")
  cat("-" ,rep("-", 50), "\n", sep = "")
  cat("Four-layer design of OmicsLake (API, transform/query, metadata, storage)\n")
  cat("showing how lineage-aware operations are backed by DuckDB, Arrow, and Parquet.\n\n")

  cat("FIGURE 2: Automatic Lineage Tracking in dplyr Pipelines\n")
  cat("-" ,rep("-", 50), "\n", sep = "")
  cat("Three-panel workflow illustrating automatic dependency propagation through\n")
  cat("dplyr operations and resulting lineage edges after `save_as()`.\n\n")

  cat("FIGURE 3: Performance Benchmarks\n")
  cat("-" ,rep("-", 50), "\n", sep = "")
  cat("Benchmark comparison of OmicsLake vs baseline methods for import, aggregation,\n")
  cat("join, and snapshot operations with median runtime and 95% run intervals (2.5th-97.5th percentiles).\n\n")

  cat("FIGURE 4: Reproducibility Workflow Comparison\n")
  cat("-" ,rep("-", 50), "\n", sep = "")
  cat("Conceptual comparison of three analysis workflows (standard scripts,\n")
  cat("Git+manual data versioning, OmicsLake) across a five-step pipeline.\n\n")

  cat("FIGURE 5: Reproducibility Comparison Across Analysis Environments\n")
  cat("-" ,rep("-", 50), "\n", sep = "")
  cat("Quantitative comparison of reproducibility metrics across three workflows,\n")
  cat("including step recovery, dependency tracking, accuracy, and operator overhead.\n\n")

  cat("FIGURE 6: Storage Efficiency Analysis\n")
  cat("-" ,rep("-", 50), "\n", sep = "")
  cat("Three-panel storage analysis comparing Parquet and RDS across dataset sizes\n")
  cat("and data types, highlighting consistent compression gains for Parquet.\n\n")

  cat("FIGURE 7: Major Results Dashboard\n")
  cat("-" ,rep("-", 50), "\n", sep = "")
  cat("Four-panel dashboard summarizing scorecard attainment, scope pass rates,\n")
  cat("and limit-boundary detection performance (ratio >= 1 indicates target met).\n\n")

  cat("SUPPLEMENTARY FIGURE S1: Data Lineage Graph\n")
  cat("-" ,rep("-", 50), "\n", sep = "")
  cat("DAG of an RNA-seq workflow with typed nodes and edges for data derivation,\n")
  cat("parameter provenance, commit lineage, and version labeling.\n\n")
}

# === Print TikZ Compilation Instructions ===
print_tikz_instructions <- function() {
  cat("=== TikZ Figure Compilation Instructions ===\n\n")

  cat("Figure 1 and Figure 2 have TikZ versions for highest quality output.\n")
  cat("These require a LaTeX installation to compile.\n\n")

  cat("Prerequisites:\n")
  cat("  1. Install LaTeX distribution:\n")
  cat("     - R: tinytex::install_tinytex()\n")
  cat("     - macOS: brew install --cask mactex-no-gui\n")
  cat("     - Ubuntu: sudo apt install texlive-full\n")
  cat("     - Windows: https://miktex.org/\n\n")


  cat("Manual Compilation:\n")
  cat("  cd inst/paper/figures\n")
  cat("  pdflatex figure1_architecture_standalone.tex\n")
  cat("  pdflatex figure2_lineage_workflow.tex\n")
  cat("  mv *.pdf output/\n\n")

  cat("From R:\n")
  cat("  tinytex::pdflatex('figure1_architecture_standalone.tex')\n")
  cat("  tinytex::pdflatex('figure2_lineage_workflow.tex')\n\n")

  cat("Output:\n")
  cat("  - figure1_architecture_standalone.pdf (TikZ vector)\n")
  cat("  - figure2_lineage_workflow.pdf (TikZ vector)\n\n")

  cat("Note: TikZ PDFs are recommended for publication due to perfect\n")
  cat("vector quality. The ggplot2 versions are provided as fallback.\n\n")
}

# === Submission Bundle Helpers ===
.submission_manifest_template <- function(profile = c("gigascience", "generic")) {
  profile <- match.arg(profile)

  if (profile == "gigascience") {
    return(data.frame(
      figure_id = c("Figure1", "Figure2", "Figure3", "Figure4",
                    "Figure5", "Figure6", "FigureS1"),
      source_file = c(
        "figure1_architecture.pdf",
        "figure2_lineage_workflow.pdf",
        "figure3_performance_benchmarks.pdf",
        "figure4_reproducibility_workflow.pdf",
        "figure5_reproducibility_grouped.pdf",
        "figure6_storage_combined.pdf",
        "figureS1_lineage_graph.pdf"
      ),
      submission_file = c(
        "Figure1_Architecture.pdf",
        "Figure2_LineageWorkflow.pdf",
        "Figure3_PerformanceBenchmarks.pdf",
        "Figure4_ReproducibilityWorkflow.pdf",
        "Figure5_ReproducibilityComparison.pdf",
        "Figure6_StorageEfficiency.pdf",
        "FigureS1_LineageGraph.pdf"
      ),
      required = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
      placement = c(
        "Main manuscript", "Main manuscript", "Main manuscript", "Main manuscript",
        "Main manuscript", "Main manuscript", "Supplementary"
      ),
      caption_short = c(
        "Four-layer architecture linking API, metadata lineage, DuckDB persistence, and Parquet export.",
        "Automatic lineage propagation in a dplyr pipeline from source tables to saved output.",
        "OmicsLake vs baseline runtime across import, aggregation, join, and snapshot operations.",
        "Conceptual workflow comparison: standard scripts, Git+manual data versioning, OmicsLake.",
        "Capability-level reproducibility matrix (qualitative baselines + observed OmicsLake RT outcomes).",
        "Parquet vs RDS storage comparison across scale and data-type scenarios.",
        "RNA-seq lineage DAG with typed nodes/edges for reproducibility and provenance tracing."
      ),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    figure_id = c("Figure1", "Figure2", "Figure3", "Figure4", "Figure5",
                  "Figure6", "Figure7", "FigureS1"),
    source_file = c(
      "figure1_architecture.pdf",
      "figure2_lineage_workflow.pdf",
      "figure3_performance_benchmarks.pdf",
      "figure4_reproducibility_workflow.pdf",
      "figure5_reproducibility_grouped.pdf",
      "figure6_storage_combined.pdf",
      "figure7_key_results_dashboard.pdf",
      "figureS1_lineage_graph.pdf"
    ),
    submission_file = c(
      "Figure1_Architecture.pdf",
      "Figure2_LineageWorkflow.pdf",
      "Figure3_PerformanceBenchmarks.pdf",
      "Figure4_ReproducibilityWorkflow.pdf",
      "Figure5_ReproducibilityComparison.pdf",
      "Figure6_StorageEfficiency.pdf",
      "Figure7_MajorResultsDashboard.pdf",
      "FigureS1_LineageGraph.pdf"
    ),
    required = rep(TRUE, 8),
    placement = rep("Main manuscript", 8),
    caption_short = c(
      "Four-layer architecture linking API, metadata lineage, DuckDB persistence, and Parquet export.",
      "Automatic lineage propagation in a dplyr pipeline from source tables to saved output.",
      "OmicsLake vs baseline runtime across import, aggregation, join, and snapshot operations.",
      "Conceptual workflow comparison: standard scripts, Git+manual data versioning, OmicsLake.",
      "Capability-level reproducibility matrix (qualitative baselines + observed OmicsLake RT outcomes).",
      "Parquet vs RDS storage comparison across scale and data-type scenarios.",
      "Scorecard dashboard of attainment ratios, scope pass rates, and limit-boundary detection.",
      "RNA-seq lineage DAG with typed nodes/edges for reproducibility and provenance tracing."
    ),
    stringsAsFactors = FALSE
  )
}

.submission_bundle_dir <- function(output_dir, profile) {
  if (identical(profile, "gigascience")) {
    return(file.path(output_dir, "submission_bundle_gigascience"))
  }
  file.path(output_dir, "submission_bundle")
}

write_submission_captions <- function(output_dir = "output",
                                      profile = c("gigascience", "generic"),
                                      filename = NULL) {
  profile <- match.arg(profile)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  if (is.null(filename)) {
    filename <- if (profile == "gigascience") "GIGASCIENCE_CAPTIONS_SHORT.md" else "CAPTIONS_SHORT.md"
  }

  captions <- .submission_manifest_template(profile = profile)
  header <- if (profile == "gigascience") {
    c(
      "# GigaScience Figure Captions (Short)",
      "",
      "Short legends for figure upload forms and submission metadata."
    )
  } else {
    c(
      "# OmicsLake Figure Captions (Short)",
      "",
      "Use these concise captions for figure submission forms or cover letters."
    )
  }

  lines <- c(header, "")
  for (i in seq_len(nrow(captions))) {
    req <- if (isTRUE(captions$required[[i]])) "Required" else "Optional"
    lines <- c(
      lines,
      paste0("## ", captions$figure_id[[i]]),
      paste0("- File: `", captions$submission_file[[i]], "`"),
      paste0("- Requirement: ", req, " (", captions$placement[[i]], ")"),
      paste0("- Caption: ", captions$caption_short[[i]]),
      ""
    )
  }
  out <- file.path(output_dir, filename)
  writeLines(lines, out)
  out
}

create_submission_bundle <- function(output_dir = "output",
                                     submission_dir = NULL,
                                     profile = c("gigascience", "generic")) {
  profile <- match.arg(profile)
  if (is.null(submission_dir)) {
    submission_dir <- .submission_bundle_dir(output_dir, profile)
  }
  if (!dir.exists(submission_dir)) {
    dir.create(submission_dir, recursive = TRUE)
  }

  manifest <- .submission_manifest_template(profile = profile)
  manifest$source_path <- file.path(output_dir, manifest$source_file)
  manifest$submission_path <- file.path(submission_dir, manifest$submission_file)
  manifest$status <- ifelse(manifest$required, "missing_required", "missing_optional")

  for (i in seq_len(nrow(manifest))) {
    if (file.exists(manifest$source_path[[i]])) {
      file.copy(manifest$source_path[[i]], manifest$submission_path[[i]], overwrite = TRUE)
      manifest$status[[i]] <- "copied"
    }
  }

  manifest_out <- manifest[, c(
    "figure_id", "source_file", "submission_file", "required",
    "placement", "status", "caption_short"
  )]
  write.csv(
    manifest_out,
    file = file.path(submission_dir, "submission_manifest.csv"),
    row.names = FALSE
  )

  captions_file <- write_submission_captions(
    output_dir = submission_dir,
    profile = profile
  )

  lines <- c(
    "# GigaScience Figure Package Summary",
    "",
    paste0("- Profile: `", profile, "`"),
    paste0("- Manifest: `", basename(file.path(submission_dir, "submission_manifest.csv")), "`"),
    paste0("- Captions: `", basename(captions_file), "`"),
    "",
    "## Status Summary",
    paste0("- Copied: ", sum(manifest$status == "copied")),
    paste0("- Missing required: ", sum(manifest$status == "missing_required")),
    paste0("- Missing optional: ", sum(manifest$status == "missing_optional"))
  )
  writeLines(lines, file.path(submission_dir, "SUBMISSION_PACKAGE.md"))

  manifest
}

# === TikZ Compilation Helper ===
compile_tikz_to_output <- function(tex_file, output_dir, output_name) {
  if (Sys.which("pdflatex") == "") {
    message("  [SKIP] pdflatex not found. Install LaTeX for TikZ figures.")
    return(NULL)
  }

  tex_base <- tools::file_path_sans_ext(tex_file)

  # Compile in current directory
  result <- system2(
    "pdflatex",
    args = c("-interaction=nonstopmode", "-halt-on-error", tex_file),
    stdout = FALSE,
    stderr = FALSE
  )

  if (result == 0) {
    # Move PDF to output directory
    pdf_file <- paste0(tex_base, ".pdf")
    if (file.exists(pdf_file)) {
      file.copy(pdf_file, file.path(output_dir, output_name), overwrite = TRUE)
      message("  [OK] TikZ figure compiled: ", output_name)
      # Clean up auxiliary files
      aux_files <- paste0(tex_base, c(".aux", ".log", ".pdf"))
      file.remove(aux_files[file.exists(aux_files)])
    }
  } else {
    message("  [WARN] TikZ compilation failed for: ", tex_file)
  }

  return(invisible(result == 0))
}

# === Main Execution ===
.generate_all_figures_is_direct_run <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (!length(file_arg)) {
    return(FALSE)
  }
  script <- sub("^--file=", "", file_arg[[1]])
  identical(basename(script), "generate_all_figures.R")
}

if (!interactive() && .generate_all_figures_is_direct_run()) {
  # Print specifications
  print_specifications()

  # Generate all figures
  cat("=== Generating Figures ===\n\n")
  results <- generate_all(output_dir)

  # Summary
  cat("=== Generation Summary ===\n")
  generated <- names(results)[!sapply(results, is.null)]
  failed <- names(results)[sapply(results, is.null)]

  if (length(generated) > 0) {
    cat("Successfully generated:", paste(generated, collapse = ", "), "\n")
  }
  if (length(failed) > 0) {
    cat("Failed to generate:", paste(failed, collapse = ", "), "\n")
  }

  cat("\nOutput files in:", normalizePath(output_dir), "\n\n")

  # List generated files
  output_files <- list.files(output_dir, pattern = "\\.(pdf|png|svg)$", full.names = TRUE)
  if (length(output_files) > 0) {
    cat("Generated files:\n")
    for (f in output_files) {
      cat("  -", basename(f), "\n")
    }
  }

  cat("\n")

  # Print captions
  print_captions()

  # Build submission bundle
  bundle_profile <- "gigascience"
  bundle <- create_submission_bundle(output_dir, profile = bundle_profile)
  copied_n <- sum(bundle$status == "copied")
  missing_req_n <- sum(bundle$status == "missing_required")
  missing_opt_n <- sum(bundle$status == "missing_optional")
  bundle_dir <- .submission_bundle_dir(output_dir, bundle_profile)
  cat("=== Submission Bundle ===\n")
  cat("Profile:", bundle_profile, "\n")
  cat("Bundle directory:", normalizePath(bundle_dir), "\n")
  cat("Copied:", copied_n,
      " | Missing required:", missing_req_n,
      " | Missing optional:", missing_opt_n, "\n")
  if (missing_req_n > 0) {
    cat("Missing required source files:\n")
    for (f in bundle$source_file[bundle$status == "missing_required"]) {
      cat("  -", f, "\n")
    }
  }
  if (missing_opt_n > 0) {
    cat("Missing optional source files:\n")
    for (f in bundle$source_file[bundle$status == "missing_optional"]) {
      cat("  -", f, "\n")
    }
  }
  cat("\n")

  # Print TikZ instructions
  print_tikz_instructions()

  cat("=== Figure Generation Complete ===\n")
} else if (interactive()) {
  cat("Running in interactive mode.\n")
  cat("Use generate_all('output') to generate all figures.\n")
  cat("Use create_submission_bundle('output', profile = 'gigascience') to prepare publication files.\n")
  cat("Use print_specifications() to view design specs.\n")
  cat("Use print_captions() to view figure captions.\n")
  cat("Use print_tikz_instructions() for TikZ compilation guide.\n")
}
