# OmicsLake Figure Generation Guide

Complete guide for generating all publication figures for the GigaScience manuscript.

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [Quick Start](#quick-start)
3. [Detailed Setup](#detailed-setup)
4. [Figure Generation](#figure-generation)
5. [Individual Figures](#individual-figures)
6. [Troubleshooting](#troubleshooting)
7. [Figure Descriptions](#figure-descriptions)

---

## Prerequisites

### Required R Packages

These packages are **mandatory** for figure generation:

```r
# Install required packages
install.packages(c(
  "ggplot2",   # Core plotting
  "dplyr",     # Data manipulation
  "tidyr",     # Data reshaping
  "scales"     # Axis formatting
))
```

### Recommended R Packages

These packages enable additional features:

```r
# Install recommended packages
install.packages(c(
  "patchwork",   # Combining multiple plots (Figure 6, Figure 7)
  "gridExtra",   # Table grobs (Figure 5)
  "grid"         # Low-level graphics (Figures 2, 4)
))
```

### Optional R Packages

For enhanced visualizations:

```r
# For Figure S1 network visualization
install.packages(c("igraph", "ggraph"))

# For Figure 5 radar chart (optional)
# devtools::install_github("ricardo-bion/ggradar")
```

### LaTeX (for TikZ Figures)

Figures 1 and 2 have high-quality TikZ versions. LaTeX is optional but recommended.

**Option 1: TinyTeX (Recommended for R users)**
```r
install.packages("tinytex")
tinytex::install_tinytex()
```

**Option 2: Full TeX Distribution**
```bash
# macOS
brew install --cask mactex-no-gui

# Ubuntu/Debian
sudo apt install texlive-full

# Windows
# Download from https://miktex.org/
```

### PDF Device

All figure scripts now use base PDF output (`device = "pdf"`), so cairo is not required.

### Fonts

Scripts use the `sans` family (Helvetica/Arial). These are built-in on macOS.

```bash
# Linux (if fonts appear different)
sudo apt install fonts-liberation fonts-dejavu
```

---

## Quick Start

Generate all figures at once:

```bash
Rscript inst/paper/figures/generate_all_figures.R
```

Or from within R:

```r
setwd("inst/paper/figures")
source("generate_all_figures.R")
```

Output files will be saved to `inst/paper/figures/output/`.

---

## Detailed Setup

### Step 1: Verify Package Installation

```r
# Check required packages
required <- c("ggplot2", "dplyr", "tidyr", "scales")
missing <- required[!sapply(required, requireNamespace, quietly = TRUE)]
if (length(missing) > 0) {
  install.packages(missing)
}

# Check optional packages
optional <- c("patchwork", "gridExtra", "grid", "igraph", "ggraph")
missing_opt <- optional[!sapply(optional, requireNamespace, quietly = TRUE)]
if (length(missing_opt) > 0) {
  message("Optional packages not installed: ", paste(missing_opt, collapse = ", "))
  message("Install with: install.packages(c('", paste(missing_opt, collapse = "', '"), "'))")
}
```

### Step 2: Verify Output Directory

```r
# Create output directory if needed
output_dir <- "inst/paper/figures/output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
```

### Step 3: Verify LaTeX (Optional)

```r
# Check if pdflatex is available
if (Sys.which("pdflatex") == "") {
  message("pdflatex not found. TikZ figures will be skipped.")
  message("Install LaTeX with: tinytex::install_tinytex()")
} else {
  message("pdflatex found: ", Sys.which("pdflatex"))
}
```

---

## Figure Generation

### Generate All Figures

```r
# From project root
source("inst/paper/figures/generate_all_figures.R")
generate_all("inst/paper/figures/output")
```

### Create Submission Bundle

```r
source("inst/paper/figures/generate_all_figures.R")
create_submission_bundle("inst/paper/figures/output", profile = "gigascience")
```

This creates `inst/paper/figures/output/submission_bundle_gigascience/` with standardized
filenames, `submission_manifest.csv`, `GIGASCIENCE_CAPTIONS_SHORT.md`, and `SUBMISSION_PACKAGE.md`.

### Expected Output Files

After successful generation, `output/` should contain:

```
output/
  # Figure 1: Architecture
  figure1_architecture.pdf          # ggplot2 version
  figure1_architecture.png          # 300 DPI raster
  figure1_architecture_tikz.pdf     # TikZ version (if LaTeX available)

  # Figure 2: Lineage Workflow
  figure2_lineage_workflow.pdf
  figure2_lineage_workflow.png
  figure2_lineage_workflow.svg
  figure2_lineage_workflow_tikz.pdf # TikZ version (if LaTeX available)

  # Figure 3: Performance Benchmarks
  figure3_performance_benchmarks.pdf
  figure3_performance_benchmarks.png
  figure3_performance_normalized.pdf

  # Figure 4: Reproducibility Workflow
  figure4_reproducibility_workflow.pdf
  figure4_reproducibility_workflow.png
  figure4_reproducibility_workflow.svg

  # Figure 5: Reproducibility Comparison
  figure5_reproducibility_grouped.pdf   # Recommended
  figure5_reproducibility_grouped.png
  figure5_reproducibility_faceted.pdf
  figure5_reproducibility_faceted.png
  figure5_reproducibility_radar.pdf     # If ggradar installed

  # Figure 6: Storage Efficiency
  figure6_storage_simple.pdf
  figure6_storage_multidataset.pdf
  figure6_storage_combined.pdf          # Recommended
  figure6_storage_combined.png

  # Figure 7: Major Results Dashboard
  figure7_key_results_dashboard.pdf     # Recommended
  figure7_key_results_dashboard.png
  figure7A_common_axes.pdf
  figure7B_mode_axes.pdf
  figure7C_scope_pass_rates.pdf
  figure7D_limit_confusion.pdf

  # Supplementary Figure S1: Lineage Graph
  figureS1_lineage_graph.pdf
  figureS1_lineage_graph.png
  figureS1_lineage.mmd                  # Mermaid source
  figureS1_lineage_ggraph.pdf           # If igraph/ggraph installed

  # Submission bundle
  submission_bundle_gigascience/
    Figure1_Architecture.pdf
    Figure2_LineageWorkflow.pdf
    Figure3_PerformanceBenchmarks.pdf
    Figure4_ReproducibilityWorkflow.pdf
    Figure5_ReproducibilityComparison.pdf
    Figure6_StorageEfficiency.pdf
    FigureS1_LineageGraph.pdf
    FigureS2_MajorResultsDashboard.pdf  # optional
    GIGASCIENCE_CAPTIONS_SHORT.md
    submission_manifest.csv
    SUBMISSION_PACKAGE.md
```

---

## Individual Figures

### Figure 1: Architecture Diagram

**Description**: Four-layer architecture of OmicsLake showing User API, Query & Transform, Metadata, and Storage layers.

**Dimensions**: 10 x 8 inches

**Generate**:
```r
source("figure1_architecture.R")
generate_architecture_diagram("output/figure1_architecture.pdf")
```

**TikZ version** (higher quality):
```bash
pdflatex figure1_architecture_standalone.tex
mv figure1_architecture_standalone.pdf output/figure1_architecture_tikz.pdf
```

---

### Figure 2: Lineage Tracking Workflow

**Description**: Demonstrates automatic dependency tracking through dplyr pipelines with three panels: (A) Pipeline, (B) Data Sources, (C) Lineage Graph.

**Dimensions**: 10 x 6 inches

**Generate**:
```r
source("figure2_lineage_workflow.R")
generate_figure2("output")
```

**TikZ version** (higher quality):
```bash
pdflatex figure2_lineage_workflow.tex
mv figure2_lineage_workflow.pdf output/figure2_lineage_workflow_tikz.pdf
```

---

### Figure 3: Performance Benchmarks

**Description**: Bar chart comparing OmicsLake vs baseline methods across four operations: Table Import, Aggregation, Join, and Snapshot.

**Dimensions**: 10 x 6 inches

**Generate**:
```r
source("figure3_performance_benchmarks.R")
generate_figure3("output")
```

**Data Source**: Uses sample data by default. For actual results, run `01_performance_benchmark.R` first and save to `results_performance.RDS`.

---

### Figure 4: Reproducibility Workflow

**Description**: Visual comparison of three versioning approaches across a 5-step analysis pipeline.

**Dimensions**: 12 x 8 inches

**Generate**:
```r
source("figure4_reproducibility_workflow.R")
generate_figure4("output")
```

---

### Figure 5: Reproducibility Comparison

**Description**: Quantitative metrics comparison showing Steps Reproduced, Dependency Tracking, Reproduction Accuracy, and Human Overhead.

**Dimensions**: 12 x 7 inches (grouped), 10 x 8 inches (faceted)

**Generate**:
```r
source("figure5_reproducibility_comparison.R")
generate_figure5("output")
```

**Recommended version**: `figure5_reproducibility_grouped.pdf`

**Data Source**: Uses sample data by default. For actual results, save metrics to `figure5_metrics_data.csv`.

---

### Figure 6: Storage Efficiency

**Description**: Three-panel analysis comparing Parquet vs RDS storage: (A) Simple comparison, (B) Scaling across dataset sizes, (C) Compression by data type.

**Dimensions**: 12 x 10 inches (combined)

**Requirements**: `patchwork` package for combined layout

**Generate**:
```r
source("figure6_storage_efficiency.R")
generate_figure6("output")
```

**Recommended version**: `figure6_storage_combined.pdf`

---

### Figure 7: Major Results Dashboard

**Description**: Four-panel summary of core scorecard outcomes: common scope attainment, mode-specific attainment, scope-level pass rates, and limit-boundary confusion matrix.

**Dimensions**: 15 x 10 inches (dashboard)

**Requirements**: `patchwork` package for combined dashboard layout

**Generate**:
```r
source("figure7_key_results_dashboard.R")
generate_figure7("output")
```

**Recommended version**: `figure7_key_results_dashboard.pdf`

**Data Source**: `../results_unified_scorecard.csv`, `../results_unified_scope_summary.csv`, `../results_limit_confusion_matrix.csv`

---

### Supplementary Figure S1: Lineage Graph

**Description**: DAG visualization of data dependencies in RNA-seq workflow showing all node types and relationships.

**Dimensions**: 14 x 6 inches

**Generate**:
```r
source("figureS1_lineage_graph.R")
generate_figureS1("output")
```

**Optional**: Install `igraph` and `ggraph` for enhanced network layout.

---

## Troubleshooting

### Error: "Missing required packages"

```r
# Install all required packages
install.packages(c("ggplot2", "dplyr", "tidyr", "scales"))
```

### Error: "patchwork not found" (Figure 6)

```r
install.packages("patchwork")
```

### TikZ compilation fails

1. **Check LaTeX installation**:
   ```bash
   which pdflatex
   ```

2. **Install missing TikZ packages**:
   ```bash
   tlmgr install tikz pgf
   ```

3. **View error log**:
   ```bash
   cat figure1_architecture_standalone.log
   ```

4. **Common TikZ package requirements**:
   - shapes.geometric
   - arrows.meta
   - positioning
   - fit
   - backgrounds
   - calc

### Fonts look different

```bash
# Linux: Install Liberation fonts
sudo apt install fonts-liberation

# Rebuild font cache
fc-cache -fv
```

### Script fails with "sys.frame(1)$ofile" error

This happens when sourcing interactively. Use the function directly:

```r
# Instead of: source("figure3_performance_benchmarks.R")
# Do this:
source("figure3_performance_benchmarks.R")
generate_figure3("output")
```

### Output directory not created

```r
# Ensure output directory exists
dir.create("output", showWarnings = FALSE, recursive = TRUE)
```

### SVG output fails

```r
# Check if svglite is installed
install.packages("svglite")
```

### Low resolution PNG files

PNG files should be 300 DPI. If they appear low-res, check the `dpi` parameter:

```r
ggsave("output/figure.png", width = 10, height = 8, dpi = 300)
```

---

## Figure Descriptions

### Figure 1: OmicsLake System Architecture

Four-layer architecture: (1) User API Layer with Lake R6 class methods (put, get, ref, snap, tree, restore), (2) Query & Transform Layer with QueryBuilder, dplyr integration, and formula syntax, (3) Metadata Layer for lineage tracking and version history, (4) Storage Layer using DuckDB, Arrow, and Parquet. Bioconductor adapters enable bidirectional conversion for SE, Seurat, Matrix, and data.frame objects.

### Figure 2: Automatic Lineage Tracking in dplyr Pipelines

Panel A shows a dplyr pipeline with ref("counts") propagating lake_source attributes through operations. Panel B shows data sources in the lake. Panel C shows the resulting lineage graph queryable via lake$tree().

### Figure 3: Performance Benchmarks

Comparison of OmicsLake vs baseline methods: Table Import (Parquet vs RDS, ~3.7x faster), Aggregation (DuckDB vs dplyr, ~2.6x faster), Join (DuckDB vs base merge, ~2.5x faster), Snapshot (metadata-based vs file copy, ~92x faster).

### Figure 4: Reproducibility Workflow Comparison

Three approaches: (A) Standard R Script with manual file versioning (qualitatively lower reproducibility), (B) Git + Manual Versioning with data-code desync risk (qualitatively moderate reproducibility), (C) OmicsLake with automatic lineage and zero-copy snapshots (empirically observed 100% in RT-001).

### Figure 5: Reproducibility Comparison Across Environments

Capability matrix with qualitative baseline rows and empirically observed OmicsLake outcomes (RT-001, RT-002, RT-003, RT-005).

### Figure 6: Storage Efficiency Analysis

Panel A: Direct comparison for 100 MB dataset (55% reduction). Panel B: Scaling from 10 MB to 10 GB (consistent ~55% advantage). Panel C: Compression by data type (integers 65%, floats 52%, strings 38%, mixed 55%).

### Figure 7: Major Results Dashboard

Panel A: Target-attainment ratios for common reproducibility axes. Panel B: Mode-specific and limit-boundary attainment. Panel C: Scope-level pass rates. Panel D: Limit-boundary confusion matrix.

### Supplementary Figure S1: Data Lineage Graph

DAG of RNA-seq workflow: Import -> Normalize -> QC Filter -> DE Analysis -> Pathway. Node types: Data tables (blue), R objects (green), parameters (orange), commits (gray), version labels (purple).

---

## GigaScience Specifications

| Specification | Requirement | Implementation |
|--------------|-------------|----------------|
| Resolution | 300 DPI minimum | All PNG files at 300 DPI |
| Format | PDF preferred | Vector PDF with `device = "pdf"` |
| Color | RGB (converted to CMYK) | Standard palette used |
| Fonts | Embedded | Sans-serif (Arial/Helvetica) |

### Color Palette

| Color | Hex Code | Usage |
|-------|----------|-------|
| Blue | #1f77b4 | OmicsLake, Parquet, data tables |
| Orange | #ff7f0e | Baseline, RDS, parameters |
| Green | #2ca02c | Success, dplyr operations |
| Red | #e74c3c | Standard R Script |
| Yellow/Orange | #f39c12 | Git + Manual |
| Purple | #9467bd | Version labels, lake_tbl |
| Gray | #7f7f7f | Neutral, commits |

---

## Version Information

- **Scripts tested with**: R >= 4.0.0
- **Required ggplot2**: >= 3.4.0
- **Required dplyr**: >= 1.1.0
- **Last updated**: 2024

---

## Support

For issues with figure generation:

1. Check the troubleshooting section above
2. Verify all required packages are installed
3. Run individual figure scripts to isolate the problem
4. Check R console for detailed error messages

---

*This guide is part of the OmicsLake package documentation.*
