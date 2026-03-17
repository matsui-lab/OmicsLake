#!/usr/bin/env Rscript
# === Figure 4: Reproducibility Workflow Diagram ===
# OmicsLake Paper - GigaScience Publication
#
# Illustrates the reproducibility workflow comparing three approaches:
#   - Standard R Script (ad-hoc versioning)
#   - Git + Manual Versioning
#   - OmicsLake (automatic lineage tracking)
#
# Specifications:
#   - Size: 12 x 8 inches
#   - Color Palette: GigaScience standard
#   - Font: Arial/Helvetica (sans-serif), 10-12pt

library(ggplot2)
library(dplyr)
library(grid)

# === GigaScience Color Palette ===
COLORS <- list(
  rscript    = "#e74c3c",   # Red - Standard R Script
  git        = "#f39c12",   # Orange - Git + Manual
  omicslake  = "#27ae60",   # Green - OmicsLake
  blue       = "#1f77b4",   # Blue - data
  gray       = "#7f7f7f",   # Gray - neutral
  lightred   = "#fadbd8",
  lightorange= "#fef5e7",
  lightgreen = "#d5f5e3"
)

# === Theme for GigaScience Publication ===
theme_gigascience <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      text = element_text(family = "sans"),
      plot.title = element_text(size = base_size + 2, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = base_size, hjust = 0.5, color = "gray40"),
      legend.position = "bottom",
      legend.title = element_text(size = base_size, face = "bold"),
      legend.text = element_text(size = base_size - 1),
      plot.margin = margin(t = 10, r = 15, b = 10, l = 10, unit = "pt"),
      panel.grid = element_blank()
    )
}

# === Create Figure 4 ===
create_figure4 <- function() {

  # Define workflow steps for each approach
  steps <- c("Import", "Normalize", "Filter", "DE Analysis", "Export")

  # === Build the plot ===
  p <- ggplot() +

    # === Background panels for each approach ===
    annotate("rect", xmin = 0.3, xmax = 5.7, ymin = 2.6, ymax = 3.5,
             fill = COLORS$lightred, alpha = 0.5, color = COLORS$rscript, linetype = "solid", linewidth = 1) +
    annotate("rect", xmin = 0.3, xmax = 5.7, ymin = 1.6, ymax = 2.5,
             fill = COLORS$lightorange, alpha = 0.5, color = COLORS$git, linetype = "solid", linewidth = 1) +
    annotate("rect", xmin = 0.3, xmax = 5.7, ymin = 0.6, ymax = 1.5,
             fill = COLORS$lightgreen, alpha = 0.5, color = COLORS$omicslake, linetype = "solid", linewidth = 1) +

    # === Approach labels ===
    annotate("text", x = 0.5, y = 3.4, label = "A. Standard R Script",
             hjust = 0, fontface = "bold", size = 3.5, color = COLORS$rscript) +
    annotate("text", x = 0.5, y = 2.4, label = "B. Git + Manual Versioning",
             hjust = 0, fontface = "bold", size = 3.5, color = COLORS$git) +
    annotate("text", x = 0.5, y = 1.4, label = "C. OmicsLake",
             hjust = 0, fontface = "bold", size = 3.5, color = COLORS$omicslake) +

    # === Workflow arrows (horizontal flow) ===
    # Standard R Script
    geom_segment(aes(x = 1.3, xend = 1.7, y = 3.05, yend = 3.05),
                 arrow = arrow(length = unit(0.1, "cm")), color = COLORS$gray) +
    geom_segment(aes(x = 2.3, xend = 2.7, y = 3.05, yend = 3.05),
                 arrow = arrow(length = unit(0.1, "cm")), color = COLORS$gray) +
    geom_segment(aes(x = 3.3, xend = 3.7, y = 3.05, yend = 3.05),
                 arrow = arrow(length = unit(0.1, "cm")), color = COLORS$gray) +
    geom_segment(aes(x = 4.3, xend = 4.7, y = 3.05, yend = 3.05),
                 arrow = arrow(length = unit(0.1, "cm")), color = COLORS$gray) +

    # Git + Manual
    geom_segment(aes(x = 1.3, xend = 1.7, y = 2.05, yend = 2.05),
                 arrow = arrow(length = unit(0.1, "cm")), color = COLORS$gray) +
    geom_segment(aes(x = 2.3, xend = 2.7, y = 2.05, yend = 2.05),
                 arrow = arrow(length = unit(0.1, "cm")), color = COLORS$gray) +
    geom_segment(aes(x = 3.3, xend = 3.7, y = 2.05, yend = 2.05),
                 arrow = arrow(length = unit(0.1, "cm")), color = COLORS$gray) +
    geom_segment(aes(x = 4.3, xend = 4.7, y = 2.05, yend = 2.05),
                 arrow = arrow(length = unit(0.1, "cm")), color = COLORS$gray) +

    # OmicsLake
    geom_segment(aes(x = 1.3, xend = 1.7, y = 1.05, yend = 1.05),
                 arrow = arrow(length = unit(0.1, "cm")), color = COLORS$omicslake, linewidth = 1.2) +
    geom_segment(aes(x = 2.3, xend = 2.7, y = 1.05, yend = 1.05),
                 arrow = arrow(length = unit(0.1, "cm")), color = COLORS$omicslake, linewidth = 1.2) +
    geom_segment(aes(x = 3.3, xend = 3.7, y = 1.05, yend = 1.05),
                 arrow = arrow(length = unit(0.1, "cm")), color = COLORS$omicslake, linewidth = 1.2) +
    geom_segment(aes(x = 4.3, xend = 4.7, y = 1.05, yend = 1.05),
                 arrow = arrow(length = unit(0.1, "cm")), color = COLORS$omicslake, linewidth = 1.2) +

    # === Step nodes (Standard R Script) ===
    geom_tile(aes(x = c(1, 2, 3, 4, 5), y = rep(3.05, 5)),
              width = 0.5, height = 0.25, fill = "white", color = COLORS$rscript) +
    annotate("text", x = 1:5, y = rep(3.05, 5), label = steps,
             size = 2.2, color = COLORS$rscript, fontface = "bold") +

    # Manual action indicators (Standard R Script)
    annotate("text", x = 1:5, y = rep(2.75, 5),
             label = c("file_v1.csv", "file_v2.csv", "file_v3.csv", "file_v4.csv", "file_final.csv"),
             size = 1.8, color = COLORS$rscript, fontface = "italic") +

    # === Step nodes (Git + Manual) ===
    geom_tile(aes(x = c(1, 2, 3, 4, 5), y = rep(2.05, 5)),
              width = 0.5, height = 0.25, fill = "white", color = COLORS$git) +
    annotate("text", x = 1:5, y = rep(2.05, 5), label = steps,
             size = 2.2, color = COLORS$git, fontface = "bold") +

    # Manual action indicators (Git + Manual)
    annotate("text", x = 1:5, y = rep(1.75, 5),
             label = c("commit", "commit+\ndata_v1/", "commit+\ndata_v2/", "commit+\ndata_v3/", "commit"),
             size = 1.5, color = COLORS$git, fontface = "italic", lineheight = 0.8) +

    # === Step nodes (OmicsLake) ===
    geom_tile(aes(x = c(1, 2, 3, 4, 5), y = rep(1.05, 5)),
              width = 0.5, height = 0.25, fill = COLORS$lightgreen, color = COLORS$omicslake, linewidth = 1.5) +
    annotate("text", x = 1:5, y = rep(1.05, 5), label = steps,
             size = 2.2, color = COLORS$omicslake, fontface = "bold") +

    # Auto tracking indicators (OmicsLake)
    annotate("text", x = 1:5, y = rep(0.75, 5),
             label = rep("auto-tracked", 5),
             size = 1.8, color = COLORS$omicslake, fontface = "italic") +

    # === Problem indicators ===
    annotate("label", x = 3, y = 3.35, label = "No dependency tracking",
             fill = COLORS$lightred, color = COLORS$rscript, size = 2,
             label.padding = unit(0.15, "lines"), fontface = "bold") +

    annotate("label", x = 3.5, y = 2.35, label = "Data-code desync risk",
             fill = COLORS$lightorange, color = COLORS$git, size = 2,
             label.padding = unit(0.15, "lines"), fontface = "bold") +

    # === Reproducibility outcome arrows ===
    geom_segment(aes(x = 5.3, xend = 5.8, y = 3.05, yend = 3.05),
                 arrow = arrow(length = unit(0.15, "cm")), color = COLORS$rscript, linewidth = 1) +
    geom_segment(aes(x = 5.3, xend = 5.8, y = 2.05, yend = 2.05),
                 arrow = arrow(length = unit(0.15, "cm")), color = COLORS$git, linewidth = 1) +
    geom_segment(aes(x = 5.3, xend = 5.8, y = 1.05, yend = 1.05),
                 arrow = arrow(length = unit(0.15, "cm")), color = COLORS$omicslake, linewidth = 1.5) +

    # Outcome boxes
    annotate("rect", xmin = 5.85, xmax = 6.6, ymin = 2.85, ymax = 3.25,
             fill = COLORS$lightred, color = COLORS$rscript, linewidth = 1) +
    annotate("text", x = 6.225, y = 3.05, label = "Lower\nReproducibility",
             size = 2.2, color = COLORS$rscript, fontface = "bold", lineheight = 0.9) +

    annotate("rect", xmin = 5.85, xmax = 6.6, ymin = 1.85, ymax = 2.25,
             fill = COLORS$lightorange, color = COLORS$git, linewidth = 1) +
    annotate("text", x = 6.225, y = 2.05, label = "Moderate\nReproducibility",
             size = 2.2, color = COLORS$git, fontface = "bold", lineheight = 0.9) +

    annotate("rect", xmin = 5.85, xmax = 6.6, ymin = 0.85, ymax = 1.25,
             fill = COLORS$lightgreen, color = COLORS$omicslake, linewidth = 2) +
    annotate("text", x = 6.225, y = 1.05, label = "Observed 100%\n(RT-001)",
             size = 2.5, color = COLORS$omicslake, fontface = "bold", lineheight = 0.9) +

    # === Legend / Key Features ===
    annotate("rect", xmin = 0.3, xmax = 6.7, ymin = 0.1, ymax = 0.55,
             fill = "gray95", color = "gray70", linetype = "dashed") +
    annotate("text", x = 0.5, y = 0.45, label = "OmicsLake Key Features:",
             hjust = 0, size = 2.5, fontface = "bold", color = "gray30") +
    annotate("text", x = 1.8, y = 0.28,
             label = "Automatic lineage | Label-based snapshots | Instant rollback | Complete reproducibility",
             hjust = 0, size = 2.2, color = COLORS$omicslake, fontface = "italic") +

    # === Coordinate and theme ===
    coord_fixed(ratio = 0.5, xlim = c(0, 7), ylim = c(0, 3.7)) +

    theme_gigascience() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank()
    ) +

    labs(
      title = "Reproducibility Workflow Comparison",
      subtitle = "Comparing data versioning approaches across a 5-step analysis pipeline"
    )

  return(p)
}

# === Generate and Save Figure ===
generate_figure4 <- function(output_dir = ".") {

  p <- create_figure4()

  # Save PDF (vector format for publication)
  ggsave(
    file.path(output_dir, "figure4_reproducibility_workflow.pdf"),
    p,
    width = 12, height = 8,
    device = "pdf"
  )

  # Save PNG (for preview/web)
  ggsave(
    file.path(output_dir, "figure4_reproducibility_workflow.png"),
    p,
    width = 12, height = 8,
    dpi = 300
  )

  # Save SVG (for editing)
  ggsave(
    file.path(output_dir, "figure4_reproducibility_workflow.svg"),
    p,
    width = 12, height = 8
  )

  message("Figure 4 saved to: ", output_dir)
  message("  - figure4_reproducibility_workflow.pdf (vector)")
  message("  - figure4_reproducibility_workflow.png (300 dpi)")
  message("  - figure4_reproducibility_workflow.svg (editable)")

  return(p)
}

# === Figure Caption ===
#' Figure 4: Reproducibility Workflow Comparison
#'
#' Comparison of three data versioning approaches across a typical 5-step
#' bioinformatics analysis pipeline (Import, Normalize, Filter, DE Analysis,
#' Export).
#'
#' (A) Standard R Script: Manual file versioning with ad-hoc naming conventions
#' (file_v1.csv, file_v2.csv, etc.). No dependency tracking between steps.
#' Reproducibility outcome is shown qualitatively (lower), limited by ability to
#' reconstruct exact analysis state from file names alone.
#'
#' (B) Git + Manual Versioning: Code tracked via Git commits, but data files
#' require manual versioning in separate directories (data_v1/, data_v2/, etc.).
#' Risk of data-code desynchronization when data changes are not committed
#' atomically with code changes. Reproducibility outcome is shown
#' qualitatively (moderate).
#'
#' (C) OmicsLake: Automatic lineage tracking through dplyr pipeline integration.
#' All data transformations are automatically recorded with label-based snapshots.
#' Instant rollback to any previous labeled state. RT-001 observed 30/30
#' successful restorations, with complete provenance tracking from any result
#' back to raw input data.
#'
#' Key innovation: OmicsLake eliminates manual versioning overhead while
#' providing superior reproducibility through automatic dependency capture.

.figure4_is_direct_run <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (!length(file_arg)) {
    return(FALSE)
  }
  script <- sub("^--file=", "", file_arg[[1]])
  identical(basename(script), "figure4_reproducibility_workflow.R")
}

# Run if executed directly
if (!interactive() && .figure4_is_direct_run()) {
  args <- commandArgs(trailingOnly = FALSE)
  script <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[[1]])
  script_dir <- dirname(normalizePath(script))
  setwd(script_dir)
  output_dir <- file.path(script_dir, "output")
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  generate_figure4(output_dir)
}
