#!/usr/bin/env Rscript
# === Figure 2: Lineage Tracking Workflow ===
# OmicsLake Paper - GigaScience Publication
#
# Illustrates automatic dependency tracking through dplyr pipelines:
#   - ref("counts") -> filter() -> join(ref("metadata")) -> summarize() -> save_as("summary")
#   - lake_tbl attribute propagation
#   - Resulting lineage graph
#
# Specifications:
#   - Size: 10 x 6 inches
#   - Color Palette: GigaScience standard
#   - Font: Arial/Helvetica (sans-serif), 10-12pt

library(ggplot2)
library(dplyr)
library(grid)

# === GigaScience Color Palette ===
COLORS <- list(
  blue       = "#1f77b4",   # Data tables (Parquet)
  green      = "#2ca02c",   # dplyr operations

  orange     = "#ff7f0e",   # ref() functions
  purple     = "#9467bd",   # lake_tbl attributes
  gray       = "#7f7f7f",   # Metadata/commits
  red        = "#d62728",   # Dependencies
  lightblue  = "#aec7e8",
  lightgreen = "#98df8a",
  lightorange= "#ffbb78"
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

# === Create Figure 2 ===
create_figure2 <- function() {

  # Node positions for Panel A: Pipeline
  pipeline_nodes <- data.frame(
    id = c("ref_counts", "filter", "ref_metadata", "join", "summarize", "save_as"),
    label = c('ref("counts")', "filter()", 'ref("metadata")', "left_join()", "summarize()", 'save_as("summary")'),
    x = c(1, 2.5, 1.5, 4, 5.5, 7),
    y = c(4.5, 4.5, 3.2, 3.8, 3.8, 3.8),
    type = c("ref", "op", "ref", "op", "op", "save"),
    stringsAsFactors = FALSE
  )

  # Attribute annotations
  attr_nodes <- data.frame(
    id = c("attr1", "attr2", "attr3", "attr4"),
    label = c(
      'lake_source:\n"counts"',
      'lake_source:\n"counts"',
      'lake_source:\n["counts", "metadata"]',
      'lake_source:\n["counts", "metadata"]'
    ),
    x = c(1, 2.5, 4, 5.5),
    y = c(5.2, 5.2, 4.6, 4.6),
    stringsAsFactors = FALSE
  )

  # Pipeline edges (data flow)
  pipeline_edges <- data.frame(
    from_x = c(1.4, 2.9, 2.0, 4.4, 5.9),
    from_y = c(4.5, 4.5, 3.2, 3.8, 3.8),
    to_x   = c(2.1, 3.6, 3.6, 5.1, 6.6),
    to_y   = c(4.5, 3.8, 3.8, 3.8, 3.8),
    type   = c("pipe", "pipe", "pipe", "pipe", "pipe"),
    stringsAsFactors = FALSE
  )

  # Attribute edges (dotted)
  attr_edges <- data.frame(
    from_x = c(1, 2.5, 4, 5.5),
    from_y = c(4.8, 4.8, 4.1, 4.1),
    to_x   = c(1, 2.5, 4, 5.5),
    to_y   = c(5.0, 5.0, 4.4, 4.4),
    stringsAsFactors = FALSE
  )

  # Panel B: Data Sources
  data_nodes <- data.frame(
    id = c("counts", "metadata", "summary"),
    label = c("counts", "metadata", "summary"),
    x = c(1.5, 3.5, 5.5),
    y = c(1.5, 1.5, 1.5),
    stringsAsFactors = FALSE
  )

  # Dependency edges for Panel B
  dep_edges <- data.frame(
    from_x = c(1.9, 3.9),
    from_y = c(1.5, 1.5),
    to_x   = c(5.1, 5.1),
    to_y   = c(1.5, 1.5),
    stringsAsFactors = FALSE
  )

  # Panel C: Lineage Graph
  lineage_nodes <- data.frame(
    id = c("ln_counts", "ln_metadata", "ln_summary"),
    label = c("counts", "metadata", "summary"),
    x = c(8, 10, 9),
    y = c(2.5, 2.5, 1.0),
    stringsAsFactors = FALSE
  )

  lineage_edges <- data.frame(
    from_x = c(8.2, 9.8),
    from_y = c(2.2, 2.2),
    to_x   = c(8.8, 9.2),
    to_y   = c(1.3, 1.3),
    stringsAsFactors = FALSE
  )

  # === Build the plot ===
  p <- ggplot() +

    # === Panel A: Pipeline ===

    # Panel A title
    annotate("text", x = 4, y = 5.9, label = "A. dplyr Pipeline with Automatic Dependency Tracking",
             hjust = 0.5, fontface = "bold", size = 4, color = "gray30") +

    # Panel A background
    annotate("rect", xmin = 0.2, xmax = 7.8, ymin = 2.8, ymax = 5.7,
             fill = COLORS$lightblue, alpha = 0.2, color = COLORS$blue, linetype = "dashed") +

    # Pipeline edges (green arrows)
    geom_segment(
      data = pipeline_edges,
      aes(x = from_x, y = from_y, xend = to_x, yend = to_y),
      arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
      color = COLORS$green,
      linewidth = 1.2
    ) +

    # Pipe labels
    annotate("text", x = 1.75, y = 4.65, label = "|>", size = 3, fontface = "bold", color = COLORS$green) +
    annotate("text", x = 4.75, y = 3.95, label = "|>", size = 3, fontface = "bold", color = COLORS$green) +
    annotate("text", x = 6.25, y = 3.95, label = "|>", size = 3, fontface = "bold", color = COLORS$green) +

    # Attribute edges (dotted purple)
    geom_segment(
      data = attr_edges,
      aes(x = from_x, y = from_y, xend = to_x, yend = to_y),
      linetype = "dotted",
      color = COLORS$purple,
      linewidth = 0.8
    ) +

    # ref() nodes (orange ellipses)
    geom_point(
      data = pipeline_nodes %>% filter(type == "ref"),
      aes(x = x, y = y),
      shape = 21, size = 12, fill = scales::alpha(COLORS$orange, 0.2),
      color = COLORS$orange, stroke = 1.5
    ) +
    geom_text(
      data = pipeline_nodes %>% filter(type == "ref"),
      aes(x = x, y = y, label = label),
      size = 2.5, fontface = "bold", color = COLORS$orange
    ) +

    # Operation nodes (green rectangles)
    geom_tile(
      data = pipeline_nodes %>% filter(type == "op"),
      aes(x = x, y = y),
      width = 1.2, height = 0.5, fill = scales::alpha(COLORS$green, 0.2),
      color = COLORS$green, linewidth = 1.2
    ) +
    geom_text(
      data = pipeline_nodes %>% filter(type == "op"),
      aes(x = x, y = y, label = label),
      size = 2.5, fontface = "bold", color = COLORS$green
    ) +

    # save_as node (blue rectangle, emphasized)
    geom_tile(
      data = pipeline_nodes %>% filter(type == "save"),
      aes(x = x, y = y),
      width = 1.5, height = 0.5, fill = scales::alpha(COLORS$blue, 0.3),
      color = COLORS$blue, linewidth = 2
    ) +
    geom_text(
      data = pipeline_nodes %>% filter(type == "save"),
      aes(x = x, y = y, label = label),
      size = 2.5, fontface = "bold", color = COLORS$blue
    ) +

    # Attribute annotations (purple)
    geom_label(
      data = attr_nodes,
      aes(x = x, y = y, label = label),
      size = 2, fill = scales::alpha(COLORS$purple, 0.1),
      color = COLORS$purple, linewidth = 0.5, fontface = "italic"
    ) +

    # === Panel B: Data Sources ===

    # Panel B title
    annotate("text", x = 3.5, y = 2.4, label = "B. Data Sources in Lake",
             hjust = 0.5, fontface = "bold", size = 4, color = "gray30") +

    # Panel B background
    annotate("rect", xmin = 0.2, xmax = 6.8, ymin = 0.8, ymax = 2.2,
             fill = COLORS$lightgreen, alpha = 0.2, color = COLORS$green, linetype = "dashed") +

    # Dependency edges (curved arrows)
    geom_curve(
      data = dep_edges,
      aes(x = from_x, y = from_y, xend = to_x, yend = to_y),
      arrow = arrow(length = unit(0.12, "cm"), type = "closed"),
      color = COLORS$gray, linewidth = 1, curvature = -0.3
    ) +

    # Data nodes (blue cylinders/rectangles)
    geom_tile(
      data = data_nodes,
      aes(x = x, y = y),
      width = 1.3, height = 0.6, fill = scales::alpha(COLORS$blue, 0.2),
      color = COLORS$blue, linewidth = 1.5
    ) +
    geom_text(
      data = data_nodes,
      aes(x = x, y = y, label = label),
      size = 3, fontface = "bold", color = COLORS$blue
    ) +

    # Edge labels
    annotate("text", x = 3.5, y = 1.85, label = "derived_from", size = 2,
             fontface = "italic", color = COLORS$gray) +

    # === Panel C: Lineage Graph ===

    # Panel C title
    annotate("text", x = 9, y = 3.3, label = "C. Resulting Lineage",
             hjust = 0.5, fontface = "bold", size = 4, color = "gray30") +

    # Panel C background
    annotate("rect", xmin = 7.2, xmax = 10.8, ymin = 0.4, ymax = 3.1,
             fill = COLORS$lightorange, alpha = 0.2, color = COLORS$orange, linetype = "dashed") +

    # Lineage edges
    geom_segment(
      data = lineage_edges,
      aes(x = from_x, y = from_y, xend = to_x, yend = to_y),
      arrow = arrow(length = unit(0.12, "cm"), type = "closed"),
      color = COLORS$gray, linewidth = 1
    ) +

    # Lineage nodes (circles)
    geom_point(
      data = lineage_nodes %>% filter(id != "ln_summary"),
      aes(x = x, y = y),
      shape = 21, size = 10, fill = COLORS$lightblue,
      color = COLORS$blue, stroke = 1.5
    ) +
    geom_point(
      data = lineage_nodes %>% filter(id == "ln_summary"),
      aes(x = x, y = y),
      shape = 21, size = 12, fill = COLORS$blue,
      color = "gray30", stroke = 2
    ) +
    geom_text(
      data = lineage_nodes,
      aes(x = x, y = y, label = label),
      size = 2.2, fontface = "bold",
      color = ifelse(lineage_nodes$id == "ln_summary", "white", COLORS$blue)
    ) +

    # Query annotation
    annotate("text", x = 9, y = 0.6, label = 'lake$tree("summary")',
             size = 2.5, fontface = "bold.italic", family = "mono", color = COLORS$gray) +

    # === Coordinate setup ===
    coord_fixed(ratio = 0.7, xlim = c(0, 11), ylim = c(0.3, 6.2)) +

    # Theme
    theme_gigascience() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank()
    ) +

    # Labels
    labs(
      title = "Automatic Lineage Tracking in dplyr Pipelines",
      subtitle = "Dependencies propagate through lake_tbl class via S3 method dispatch"
    )

  return(p)
}

# === Generate and Save Figure ===
generate_figure2 <- function(output_dir = ".") {

  p <- create_figure2()

  # Save PDF (vector format for publication)
  ggsave(
    file.path(output_dir, "figure2_lineage_workflow.pdf"),
    p,
    width = 10, height = 6,
    device = "pdf"
  )

  # Save PNG (for preview/web)
  ggsave(
    file.path(output_dir, "figure2_lineage_workflow.png"),
    p,
    width = 10, height = 6,
    dpi = 300
  )

  # Save SVG (for editing)
  ggsave(
    file.path(output_dir, "figure2_lineage_workflow.svg"),
    p,
    width = 10, height = 6
  )

  message("Figure 2 saved to: ", output_dir)
  message("  - figure2_lineage_workflow.pdf (vector)")
  message("  - figure2_lineage_workflow.png (300 dpi)")
  message("  - figure2_lineage_workflow.svg (editable)")

  return(p)
}

# === Figure Caption ===
#' Figure 2: Automatic Lineage Tracking in dplyr Pipelines
#'
#' (A) A typical OmicsLake workflow demonstrating automatic dependency tracking
#' through dplyr pipeline operations. The workflow begins with ref("counts")
#' which creates a lazy reference to the "counts" table in the lake with a
#' lake_source attribute. This attribute propagates through each dplyr operation
#' (filter, left_join, summarize) via S3 method dispatch on the lake_tbl class.
#' When ref("metadata") is joined, both sources are merged into the lake_source
#' attribute. The final save_as() call automatically records these dependencies
#' without requiring explicit specification.
#'
#' (B) The data sources stored in the lake. After execution, "summary" is
#' linked to both "counts" and "metadata" as its parent dependencies.
#'
#' (C) The resulting lineage graph, queryable via lake$tree("summary").
#' This directed acyclic graph (DAG) structure enables complete provenance
#' tracking for any derived dataset.
#'
#' Key innovation: Dependencies are captured automatically through R's S3
#' dispatch mechanism, requiring no modification to existing dplyr workflows.

.figure2_is_direct_run <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (!length(file_arg)) {
    return(FALSE)
  }
  script <- sub("^--file=", "", file_arg[[1]])
  identical(basename(script), "figure2_lineage_workflow.R")
}

# Run if executed directly
if (!interactive() && .figure2_is_direct_run()) {
  args <- commandArgs(trailingOnly = FALSE)
  script <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[[1]])
  script_dir <- dirname(normalizePath(script))
  setwd(script_dir)
  output_dir <- file.path(script_dir, "output")
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  generate_figure2(output_dir)
}
