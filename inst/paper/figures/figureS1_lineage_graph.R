#!/usr/bin/env Rscript
# === Supplementary Figure S1: Lineage Graph ===
# OmicsLake Paper - Data Visualization
# Visualizes data lineage and dependency tracking for a sample workflow
#
# Specifications:
#   - Size: 10 x 8 inches
#   - Color Palette: Node types
#     - Blue (#1f77b4): Data tables (Parquet)
#     - Green (#2ca02c): R objects
#     - Orange (#ff7f0e): Parameters/Config
#     - Gray (#7f7f7f): Commit nodes
#   - Font: Arial/Helvetica (sans-serif), 10-12pt
#   - Layout: Directed acyclic graph (DAG)

library(ggplot2)
library(dplyr)

# Check for network visualization packages
use_igraph <- requireNamespace("igraph", quietly = TRUE)
use_ggraph <- requireNamespace("ggraph", quietly = TRUE)

# === GigaScience Color Palette (Node Types) ===
COLORS_NODE <- c(
  "table" = "#1f77b4",     # Blue - Data tables
  "object" = "#2ca02c",    # Green - R objects
  "params" = "#ff7f0e",    # Orange - Parameters
  "commit" = "#7f7f7f",    # Gray - Commits
  "label" = "#9467bd"      # Purple - Version labels
)

# === Theme for GigaScience Publication ===
theme_gigascience <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      text = element_text(family = "sans"),
      plot.title = element_text(size = base_size + 2, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = base_size, hjust = 0.5, color = "gray40"),
      legend.position = "right",
      legend.title = element_text(size = base_size, face = "bold"),
      legend.text = element_text(size = base_size - 1),
      plot.margin = margin(t = 10, r = 15, b = 10, l = 10, unit = "pt")
    )
}

# === Define Lineage Graph Structure ===
create_lineage_data <- function() {

  # Node definitions for RNA-seq workflow
  nodes <- data.frame(
    id = c(
      # Step 1: Raw data
      "raw_counts", "gene_info", "sample_info", "commit_1",
      # Step 2: Normalization
      "norm_params", "normalized_counts", "commit_2",
      # Step 3: QC
      "qc_params", "filtered_counts", "commit_3",
      # Step 4: DE analysis
      "de_params", "de_results", "commit_4",
      # Step 5: Pathway
      "enrichment_params", "pathway_results", "commit_5",
      # Version label
      "v1.0_complete"
    ),
    label = c(
      "raw_counts", "gene_info", "sample_info", "Commit 1\n(Import)",
      "norm_params", "normalized_counts", "Commit 2\n(Normalize)",
      "qc_params", "filtered_counts", "Commit 3\n(QC)",
      "de_params", "de_results", "Commit 4\n(DE)",
      "enrichment_params", "pathway_results", "Commit 5\n(Pathway)",
      "v1.0_complete"
    ),
    type = c(
      "table", "object", "object", "commit",
      "params", "table", "commit",
      "params", "table", "commit",
      "params", "object", "commit",
      "params", "object", "commit",
      "label"
    ),
    step = c(
      1, 1, 1, 1,
      2, 2, 2,
      3, 3, 3,
      4, 4, 4,
      5, 5, 5,
      6
    ),
    stringsAsFactors = FALSE
  )

  # Edge definitions (dependencies)
  edges <- data.frame(
    from = c(
      # Step 1 -> Commit 1
      "raw_counts", "gene_info", "sample_info",
      # Step 1 -> Step 2 dependencies
      "raw_counts",
      "gene_info", "sample_info",
      # Step 2 -> Commit 2
      "normalized_counts", "norm_params",
      # Step 2 -> Step 3
      "normalized_counts",
      # Step 3 -> Commit 3
      "filtered_counts", "qc_params",
      # Step 3 -> Step 4
      "filtered_counts", "sample_info",
      # Step 4 -> Commit 4
      "de_results", "de_params",
      # Step 4 -> Step 5
      "de_results",
      # Step 5 -> Commit 5
      "pathway_results", "enrichment_params",
      # Commits -> Label
      "commit_5"
    ),
    to = c(
      "commit_1", "commit_1", "commit_1",
      "normalized_counts",
      "norm_params", "norm_params",
      "commit_2", "commit_2",
      "filtered_counts",
      "commit_3", "commit_3",
      "de_results", "de_results",
      "commit_4", "commit_4",
      "pathway_results",
      "commit_5", "commit_5",
      "v1.0_complete"
    ),
    relationship = c(
      rep("snapshot", 3),
      "derived_from",
      rep("config", 2),
      rep("snapshot", 2),
      "derived_from",
      rep("snapshot", 2),
      rep("derived_from", 2),
      rep("snapshot", 2),
      "derived_from",
      rep("snapshot", 2),
      "labeled"
    ),
    stringsAsFactors = FALSE
  )

  return(list(nodes = nodes, edges = edges))
}

# === Option A: Manual Layout with ggplot2 ===
create_figureS1_manual <- function() {

  lineage <- create_lineage_data()
  nodes <- lineage$nodes
  edges <- lineage$edges

  # Manual layout coordinates
  layout <- data.frame(
    id = nodes$id,
    x = c(
      # Step 1
      1, 1, 1, 2,
      # Step 2
      3, 3, 4,
      # Step 3
      5, 5, 6,
      # Step 4
      7, 7, 8,
      # Step 5
      9, 9, 10,
      # Label
      11
    ),
    y = c(
      # Step 1
      3, 2, 1, 2,
      # Step 2
      3, 2, 2.5,
      # Step 3
      3, 2, 2.5,
      # Step 4
      3, 2, 2.5,
      # Step 5
      3, 2, 2.5,
      # Label
      2.5
    ),
    stringsAsFactors = FALSE
  )

  nodes <- nodes %>%
    left_join(layout, by = "id")

  # Create edge coordinates
  edge_coords <- edges %>%
    left_join(nodes %>% select(id, x, y) %>% rename(from = id, x_from = x, y_from = y),
              by = "from") %>%
    left_join(nodes %>% select(id, x, y) %>% rename(to = id, x_to = x, y_to = y),
              by = "to")

  # Plot
  p <- ggplot() +

    # Draw edges
    geom_segment(
      data = edge_coords,
      aes(x = x_from, y = y_from, xend = x_to, yend = y_to),
      arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
      color = "gray50",
      linewidth = 0.5
    ) +

    # Draw nodes
    geom_point(
      data = nodes,
      aes(x = x, y = y, color = type),
      size = 8
    ) +

    # Node labels
    geom_text(
      data = nodes %>% filter(type != "commit"),
      aes(x = x, y = y - 0.4, label = label),
      size = 2.5,
      fontface = "bold"
    ) +

    # Commit labels (inside node)
    geom_text(
      data = nodes %>% filter(type == "commit"),
      aes(x = x, y = y, label = gsub("\n.*", "", label)),
      size = 2,
      color = "white",
      fontface = "bold"
    ) +

    # Step annotations
    annotate(
      "rect",
      xmin = c(0.5, 2.5, 4.5, 6.5, 8.5, 10.5),
      xmax = c(2.4, 4.4, 6.4, 8.4, 10.4, 11.5),
      ymin = 0.5, ymax = 3.5,
      fill = NA,
      color = "gray80",
      linetype = "dashed"
    ) +

    # Step labels
    annotate(
      "text",
      x = c(1.5, 3.5, 5.5, 7.5, 9.5, 11),
      y = 0.7,
      label = c("Import", "Normalize", "QC Filter", "DE Analysis", "Pathway", "Label"),
      size = 3,
      fontface = "italic",
      color = "gray50"
    ) +

    scale_color_manual(
      values = COLORS_NODE,
      labels = c(
        "table" = "Data Table (Parquet)",
        "object" = "R Object",
        "params" = "Parameters",
        "commit" = "Commit",
        "label" = "Version Label"
      )
    ) +

    labs(
      title = "Supplementary Figure S1: Data Lineage Graph",
      subtitle = "RNA-seq workflow dependency tracking in OmicsLake",
      x = NULL,
      y = NULL,
      color = "Node Type"
    ) +
    coord_fixed(ratio = 0.8) +
    theme_gigascience() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    )

  return(p)
}

# === Option B: Using igraph + ggraph ===
create_figureS1_ggraph <- function() {

  if (!use_igraph || !use_ggraph) {
    message("igraph and ggraph required for this visualization.")
    message("Install with: install.packages(c('igraph', 'ggraph'))")
    message("Falling back to manual layout.")
    return(create_figureS1_manual())
  }

  library(igraph)
  library(ggraph)

  lineage <- create_lineage_data()
  nodes <- lineage$nodes
  edges <- lineage$edges

  # Create igraph object
  g <- graph_from_data_frame(edges, vertices = nodes, directed = TRUE)

  # Use sugiyama layout for DAG
  p <- ggraph(g, layout = "sugiyama") +
    geom_edge_link(
      aes(linetype = relationship),
      arrow = arrow(length = unit(2, "mm"), type = "closed"),
      end_cap = circle(4, "mm"),
      color = "gray50"
    ) +
    geom_node_point(
      aes(color = type),
      size = 10
    ) +
    geom_node_text(
      aes(label = label),
      size = 2.5,
      repel = TRUE,
      fontface = "bold"
    ) +
    scale_color_manual(
      values = COLORS_NODE,
      labels = c(
        "table" = "Data Table (Parquet)",
        "object" = "R Object",
        "params" = "Parameters",
        "commit" = "Commit",
        "label" = "Version Label"
      )
    ) +
    scale_edge_linetype_manual(
      values = c(
        "derived_from" = "solid",
        "snapshot" = "dashed",
        "config" = "dotted",
        "labeled" = "solid"
      )
    ) +
    labs(
      title = "Supplementary Figure S1: Data Lineage Graph",
      subtitle = "RNA-seq workflow dependency tracking in OmicsLake",
      color = "Node Type",
      edge_linetype = "Relationship"
    ) +
    theme_void() +
    theme_gigascience() +
    theme(
      plot.margin = margin(20, 20, 20, 20)
    )

  return(p)
}

# === Option C: Mermaid Diagram Output ===
create_figureS1_mermaid <- function(output_file = "figureS1_lineage.mmd") {

  mermaid_code <- '---
title: "Supplementary Figure S1: Data Lineage Graph"
---
graph LR
  subgraph Step1["Step 1: Import"]
    raw_counts[(raw_counts)]
    gene_info([gene_info])
    sample_info([sample_info])
  end

  subgraph Step2["Step 2: Normalization"]
    norm_params{{norm_params}}
    normalized_counts[(normalized_counts)]
  end

  subgraph Step3["Step 3: QC"]
    qc_params{{qc_params}}
    filtered_counts[(filtered_counts)]
  end

  subgraph Step4["Step 4: DE Analysis"]
    de_params{{de_params}}
    de_results([de_results])
  end

  subgraph Step5["Step 5: Pathway"]
    enrichment_params{{enrichment_params}}
    pathway_results([pathway_results])
  end

  subgraph Version["Version Control"]
    commit1((Commit 1))
    commit2((Commit 2))
    commit3((Commit 3))
    commit4((Commit 4))
    commit5((Commit 5))
    v1_complete[["v1.0_complete"]]
  end

  %% Data dependencies
  raw_counts --> normalized_counts
  gene_info --> norm_params
  sample_info --> norm_params
  normalized_counts --> filtered_counts
  filtered_counts --> de_results
  sample_info --> de_results
  de_results --> pathway_results

  %% Commit snapshots
  raw_counts -.-> commit1
  gene_info -.-> commit1
  sample_info -.-> commit1
  normalized_counts -.-> commit2
  norm_params -.-> commit2
  filtered_counts -.-> commit3
  qc_params -.-> commit3
  de_results -.-> commit4
  de_params -.-> commit4
  pathway_results -.-> commit5
  enrichment_params -.-> commit5
  commit5 ==> v1_complete

  %% Styling
  style raw_counts fill:#1f77b4,stroke:#333,color:#fff
  style normalized_counts fill:#1f77b4,stroke:#333,color:#fff
  style filtered_counts fill:#1f77b4,stroke:#333,color:#fff
  style gene_info fill:#2ca02c,stroke:#333,color:#fff
  style sample_info fill:#2ca02c,stroke:#333,color:#fff
  style de_results fill:#2ca02c,stroke:#333,color:#fff
  style pathway_results fill:#2ca02c,stroke:#333,color:#fff
  style norm_params fill:#ff7f0e,stroke:#333,color:#fff
  style qc_params fill:#ff7f0e,stroke:#333,color:#fff
  style de_params fill:#ff7f0e,stroke:#333,color:#fff
  style enrichment_params fill:#ff7f0e,stroke:#333,color:#fff
  style commit1 fill:#7f7f7f,stroke:#333,color:#fff
  style commit2 fill:#7f7f7f,stroke:#333,color:#fff
  style commit3 fill:#7f7f7f,stroke:#333,color:#fff
  style commit4 fill:#7f7f7f,stroke:#333,color:#fff
  style commit5 fill:#7f7f7f,stroke:#333,color:#fff
  style v1_complete fill:#9467bd,stroke:#333,color:#fff
'

  writeLines(mermaid_code, output_file)
  message("Mermaid diagram saved to: ", output_file)
  message("Render with: mmdc -i ", output_file, " -o figureS1_lineage.svg")

  return(invisible(mermaid_code))
}

# === Generate and Save Figure ===
generate_figureS1 <- function(output_dir = ".") {

  # Create manual layout version
  p_manual <- create_figureS1_manual()

  # Save PDF
  ggsave(
    file.path(output_dir, "figureS1_lineage_graph.pdf"),
    p_manual,
    width = 14, height = 6,
    device = "pdf"
  )

  ggsave(
    file.path(output_dir, "figureS1_lineage_graph.png"),
    p_manual,
    width = 14, height = 6,
    dpi = 300
  )

  # Create Mermaid diagram
  create_figureS1_mermaid(file.path(output_dir, "figureS1_lineage.mmd"))

  # Try ggraph version if available
  if (use_igraph && use_ggraph) {
    p_ggraph <- create_figureS1_ggraph()
    ggsave(
      file.path(output_dir, "figureS1_lineage_ggraph.pdf"),
      p_ggraph,
      width = 12, height = 10,
      device = "pdf"
    )
  }

  message("Figure S1 saved to: ", output_dir)
  message("  - figureS1_lineage_graph.pdf (ggplot2 manual layout)")
  message("  - figureS1_lineage_graph.png")
  message("  - figureS1_lineage.mmd (Mermaid diagram)")

  return(list(manual = p_manual))
}

# === Figure Caption ===
#' Supplementary Figure S1: Data Lineage Graph for RNA-seq Workflow
#'
#' Directed acyclic graph (DAG) representation of data dependencies and
#' version control in a five-step RNA-seq analysis pipeline tracked by
#' OmicsLake. Node types: Data tables stored as Parquet files (blue
#' rectangles), R objects (green ovals), analysis parameters (orange diamonds),
#' commits (gray circles), and version labels (purple hexagons). Edge types:
#' Solid arrows indicate data derivation (depends_on relationship), dashed
#' arrows indicate commit snapshots, and thick arrows indicate version labeling.
#' The workflow proceeds from raw count import (Step 1) through normalization
#' (Step 2), quality control filtering (Step 3), differential expression
#' analysis (Step 4), and pathway enrichment (Step 5). Each step creates a
#' commit that captures the complete state of all modified data objects.
#' The final version label (v1.0_complete) enables instant restoration of
#' the entire analysis state using ol_read(..., ref = "@v1.0_complete").
#' This lineage tracking enables complete reproducibility: any intermediate
#' result can be traced back through its dependencies to the original input
#' data, and the exact parameters used at each step are preserved.

.figureS1_is_direct_run <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (!length(file_arg)) {
    return(FALSE)
  }
  script <- sub("^--file=", "", file_arg[[1]])
  identical(basename(script), "figureS1_lineage_graph.R")
}

# Run if executed directly
if (!interactive() && .figureS1_is_direct_run()) {
  args <- commandArgs(trailingOnly = FALSE)
  script <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[[1]])
  script_dir <- dirname(normalizePath(script))
  setwd(script_dir)
  output_dir <- file.path(script_dir, "output")
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  generate_figureS1(output_dir)
}
