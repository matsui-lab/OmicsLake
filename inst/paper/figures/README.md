# OmicsLake Paper Figures

This directory contains all figure generation scripts for the GigaScience manuscript.

## Quick Start

Generate all figures at once:

```bash
Rscript inst/paper/figures/generate_all_figures.R
```

This command can be run from project root. Output files will be saved to `inst/paper/figures/output/`.

Prepare a submission-ready bundle (standardized filenames + short captions):

```bash
Rscript -e "source('inst/paper/figures/generate_all_figures.R'); create_submission_bundle('inst/paper/figures/output', profile='gigascience')"
```

## Figure List

| Figure | Description | Script | Format |
|--------|-------------|--------|--------|
| Figure 1 | Architecture Diagram | `figure1_architecture.R` | PDF, PNG, TikZ |
| Figure 2 | Lineage Workflow | `figure2_lineage_workflow.R` | PDF, PNG, TikZ |
| Figure 3 | Performance Benchmarks | `figure3_performance_benchmarks.R` | PDF, PNG |
| Figure 4 | Reproducibility Workflow | `figure4_reproducibility_workflow.R` | PDF, PNG, SVG |
| Figure 5 | Reproducibility Comparison | `figure5_reproducibility_comparison.R` | PDF, PNG |
| Figure 6 | Storage Efficiency | `figure6_storage_efficiency.R` | PDF, PNG |
| Figure 7 | Major Results Dashboard | `figure7_key_results_dashboard.R` | PDF, PNG |
| Figure S1 | Lineage Graph | `figureS1_lineage_graph.R` | PDF, PNG, Mermaid |

## Output Directory Structure

```
output/
  figure1_architecture.pdf          # ggplot2 version
  figure1_architecture.png          # 300 DPI raster
  figure1_architecture_tikz.pdf     # TikZ version (if LaTeX available)
  figure2_lineage_workflow.pdf
  figure2_lineage_workflow.png
  figure2_lineage_workflow_tikz.pdf # TikZ version (if LaTeX available)
  figure3_performance_benchmarks.pdf
  figure3_performance_benchmarks.png
  figure7_key_results_dashboard.pdf
  figure7_key_results_dashboard.png
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
  ...
```

## GigaScience Specifications

- **Resolution**: 300 DPI minimum
- **Format**: PDF (vector) preferred for line art, PNG for complex graphics
- **Color Space**: RGB (will be converted to CMYK by publisher)
- **Dimensions**: See individual scripts for size specifications

## TikZ Figure Compilation (Figure 1 & 2)

Figure 1 and Figure 2 have high-quality TikZ versions. These provide perfect vector output for publication.

### Prerequisites

Install a LaTeX distribution:

**Option 1: TinyTeX (Recommended for R users)**
```r
install.packages("tinytex")
tinytex::install_tinytex()
```

**Option 2: Full TeX Distribution**
- macOS: `brew install --cask mactex-no-gui`
- Ubuntu/Debian: `sudo apt install texlive-full`
- Windows: Download from https://miktex.org/

### Manual Compilation

```bash
cd inst/paper/figures

# Figure 1: Architecture
pdflatex figure1_architecture_standalone.tex
mv figure1_architecture_standalone.pdf output/figure1_architecture_tikz.pdf

# Figure 2: Lineage Workflow
pdflatex figure2_lineage_workflow.tex
mv figure2_lineage_workflow.pdf output/figure2_lineage_workflow_tikz.pdf

# Clean up auxiliary files
rm -f *.aux *.log
```

### Compilation from R

```r
# Using tinytex
tinytex::pdflatex("figure1_architecture_standalone.tex")
tinytex::pdflatex("figure2_lineage_workflow.tex")

# Move to output
file.copy("figure1_architecture_standalone.pdf", "output/figure1_architecture_tikz.pdf")
file.copy("figure2_lineage_workflow.pdf", "output/figure2_lineage_workflow_tikz.pdf")
```

### TikZ Source Files

| File | Description |
|------|-------------|
| `figure1_architecture_standalone.tex` | Self-contained LaTeX document |
| `figure1_architecture.tex` | For embedding in main manuscript |
| `figure2_lineage_workflow.tex` | Standalone TikZ figure |

## Required R Packages

```r
# Core (required)
install.packages(c("ggplot2", "dplyr", "tidyr", "scales"))

# Recommended
install.packages(c("patchwork", "gridExtra", "grid"))

# Optional (for Figure S1 network visualization)
install.packages(c("igraph", "ggraph"))

# Optional (for radar chart in Figure 5)
devtools::install_github("ricardo-bion/ggradar")
```

## Individual Figure Generation

Each script can be run independently:

```bash
# Single figure
Rscript figure3_performance_benchmarks.R

# From R
source("figure3_performance_benchmarks.R")
generate_figure3("output")
```

## Troubleshooting

### TikZ compilation fails
1. Ensure LaTeX is installed: `which pdflatex`
2. Check for missing packages: run `tlmgr install tikz`
3. View log file: `cat figure1_architecture_standalone.log`

### Fonts look different
The scripts use `sans` family (Helvetica/Arial). Install if missing:
- macOS: Built-in
- Linux: `sudo apt install fonts-liberation`

## Color Palette

GigaScience-compliant colors used throughout:

| Color | Hex | Usage |
|-------|-----|-------|
| Blue | `#1f77b4` | OmicsLake, Parquet, data tables |
| Orange | `#ff7f0e` | Baseline, RDS, parameters |
| Green | `#2ca02c` | Success, dplyr operations |
| Red | `#e74c3c` | Standard R Script |
| Yellow/Orange | `#f39c12` | Git + Manual |
| Purple | `#9467bd` | Version labels, lake_tbl |
| Gray | `#7f7f7f` | Neutral, commits |

## License

Figures are part of the OmicsLake package and distributed under MIT license.
