# ============================================================================
# Quick Script: Generate Figure 1 (Architecture Diagram)
# ============================================================================
#
# Run this script in R or RStudio to generate Figure 1.
#
# Usage (in R console):
#   source("run_figure1.R")
#
# Or from command line:
#   Rscript run_figure1.R
#   R CMD BATCH run_figure1.R
#
# ============================================================================

message("=== Generating Figure 1: OmicsLake Architecture Diagram ===\n")

# Get script directory
if (interactive()) {
  script_dir <- getwd()
} else {
  script_dir <- tryCatch({
    dirname(sys.frame(1)$ofile)
  }, error = function(e) getwd())
}

# Ensure we're in the figures directory
if (!file.exists(file.path(script_dir, "figure1_architecture.R"))) {
  # Try to find it
  possible_paths <- c(
    "inst/paper/figures",
    "../inst/paper/figures",
    "../../inst/paper/figures"
  )
  for (p in possible_paths) {
    if (file.exists(file.path(p, "figure1_architecture.R"))) {
      script_dir <- normalizePath(p)
      break
    }
  }
}

message("Working directory: ", script_dir, "\n")

# Source the main generation script
source(file.path(script_dir, "figure1_architecture.R"))

# Create output directory
output_dir <- file.path(script_dir, "output")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Generate the figures
message("Generating main architecture diagram (PDF)...")
p <- generate_architecture_diagram(
  file.path(output_dir, "figure1_architecture.pdf"),
  width = 8, height = 7
)

message("Generating PNG version (300 DPI)...")
generate_architecture_diagram(
  file.path(output_dir, "figure1_architecture.png"),
  width = 8, height = 7, dpi = 300
)

message("Generating high-res PNG (600 DPI)...")
generate_architecture_diagram(
  file.path(output_dir, "figure1_architecture_600dpi.png"),
  width = 8, height = 7, dpi = 600
)

message("Generating compact version...")
generate_architecture_diagram_compact(
  file.path(output_dir, "figure1_architecture_compact.pdf"),
  width = 5, height = 6
)

message("\n=== Figure generation complete! ===")
message("Output files:")
message("  - ", file.path(output_dir, "figure1_architecture.pdf"))
message("  - ", file.path(output_dir, "figure1_architecture.png"))
message("  - ", file.path(output_dir, "figure1_architecture_600dpi.png"))
message("  - ", file.path(output_dir, "figure1_architecture_compact.pdf"))
message("\nFor LaTeX manuscript, use:")
message('  \\includegraphics[width=0.95\\textwidth]{../figures/output/figure1_architecture.pdf}')
