#!/bin/bash
# ============================================================================
# Generate Figure 1: OmicsLake Architecture Diagram
# ============================================================================
#
# Usage:
#   cd inst/paper/figures
#   ./generate_figure1.sh
#
# Or run directly:
#   bash /path/to/inst/paper/figures/generate_figure1.sh
#
# Outputs:
#   - output/figure1_architecture.pdf (publication quality, vector)
#   - output/figure1_architecture.png (300 DPI)
#   - output/figure1_architecture_600dpi.png (print quality)
#   - output/figure1_architecture_compact.pdf (single-column version)
#
# ============================================================================

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

echo "============================================"
echo "Figure 1: OmicsLake Architecture Diagram"
echo "============================================"
echo ""
echo "Working directory: $SCRIPT_DIR"
echo ""

# Create output directory if needed
mkdir -p output

# Run R script
echo "Running figure1_architecture.R..."
Rscript figure1_architecture.R

# Check output
echo ""
echo "Checking outputs..."
if [ -f "output/figure1_architecture.pdf" ]; then
    echo "[SUCCESS] Figure generation complete!"
    echo ""
    echo "Generated files:"
    ls -la output/figure1_architecture* 2>/dev/null || true
    echo ""
    echo "For LaTeX manuscript, use:"
    echo "  \\includegraphics[width=0.95\\textwidth]{../figures/output/figure1_architecture.pdf}"
else
    echo "[ERROR] Figure generation failed"
    echo ""
    echo "Please check:"
    echo "  1. R is installed (run: which Rscript)"
    echo "  2. ggplot2 is installed (run: R -e \"library(ggplot2)\")"
    echo "  3. Run manually in R:"
    echo "     source('figure1_architecture.R')"
    echo "     generate_architecture_diagram('output/figure1_architecture.pdf')"
    exit 1
fi
