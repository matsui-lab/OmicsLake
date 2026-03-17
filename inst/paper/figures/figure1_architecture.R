#' Generate Figure 1: OmicsLake Architecture Diagram
#'
#' This script generates a publication-quality architecture diagram for OmicsLake
#' using ggplot2, showing:
#' - Data flow: R -> Arrow -> DuckDB -> Parquet
#' - Metadata tables: __ol_objects, __ol_refs, __ol_dependencies
#' - Core API: put(), get(), snap(), restore(), tree()
#'
#' @author OmicsLake Team
#' @date 2024

# =============================================================================
# Main function: Generate publication-quality architecture diagram
# =============================================================================

#' Generate OmicsLake Architecture Diagram
#'
#' Creates a ggplot2-based architecture diagram suitable for publication in
#' GigaScience or similar journals. The diagram emphasizes:
#' - Clear data flow from R through Arrow to DuckDB/Parquet
#' - Metadata layer with three core tables
#' - User-facing API methods
#'
#' @param output_file Path to output file (PDF or PNG)
#' @param width Figure width in inches (default: 8)
#' @param height Figure height in inches (default: 7)
#' @param dpi Resolution for PNG output (default: 300)
#' @return Invisibly returns the ggplot object
#' @export
generate_architecture_diagram <- function(output_file = "figure1_architecture.pdf",
                                          width = 8,
                                          height = 7,
                                          dpi = 300) {

  # Check dependencies
  required_pkgs <- c("ggplot2", "grid")
  for (pkg in required_pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Package '%s' required. Install with: install.packages('%s')", pkg, pkg))
    }
  }

  suppressPackageStartupMessages({
    library(ggplot2)
    library(grid)
  })

  # ===========================================================================
  # Color palette (professional, colorblind-friendly)
  # ===========================================================================
  colors <- list(
    # Layer backgrounds
    api_layer = "#E3F2FD",        # Light blue
    lake_class = "#BBDEFB",       # Medium blue
    metadata_layer = "#FFF3E0",   # Light orange
    storage_layer = "#F3E5F5",    # Light purple
    bioc_layer = "#E0F2F1",       # Light teal

    # Component colors
    api_method = "#1976D2",       # Blue
    metadata_table = "#F57C00",   # Orange
    storage_tech = "#7B1FA2",     # Purple
    bioc_adapter = "#00796B",     # Teal

    # Arrow colors
    data_flow = "#2E7D32",        # Green
    metadata_flow = "#FF5722",    # Deep orange
    bidirectional = "#455A64",    # Blue grey

    # Text
    text_dark = "#212121",
    text_light = "#FFFFFF",
    border = "#757575"
  )

  # ===========================================================================
  # Define layout coordinates
  # ===========================================================================

  # Y coordinates for layers (bottom to top)
  y_storage <- 1.0
  y_metadata <- 3.2
  y_lake <- 5.4
  y_api <- 7.2

  # Layer heights
  layer_height <- 1.6
  api_box_height <- 0.6

  # X range
  x_min <- 0.5
  x_max <- 10.5
  x_center <- (x_min + x_max) / 2

  # ===========================================================================
  # Build data frames for plotting
  # ===========================================================================

  # --- Layer backgrounds ---
  layers <- data.frame(
    layer = c("User API", "Lake R6 Class", "Metadata Layer", "Storage Layer"),
    xmin = rep(x_min, 4),
    xmax = c(x_max, x_max, x_max, x_max),
    ymin = c(y_api, y_lake, y_metadata, y_storage),
    ymax = c(y_api + layer_height, y_lake + layer_height,
             y_metadata + layer_height, y_storage + layer_height),
    fill = c(colors$api_layer, colors$lake_class,
             colors$metadata_layer, colors$storage_layer),
    label_y = c(y_api + layer_height - 0.15,
                y_lake + layer_height - 0.15,
                y_metadata + layer_height - 0.15,
                y_storage + layer_height - 0.15),
    stringsAsFactors = FALSE
  )

  # --- API Methods (top layer) ---
  api_methods <- data.frame(
    name = c("put()", "get()", "snap()", "restore()", "tree()"),
    x = seq(2, 9, length.out = 5),
    y = rep(y_api + 0.8, 5),
    description = c("Store data", "Retrieve data", "Create snapshot",
                    "Restore state", "View lineage"),
    stringsAsFactors = FALSE
  )

  # --- Lake R6 Class (center) ---
  lake_components <- data.frame(
    name = c("Lake R6 Class", "DependencyTracker", "lake_tbl S3"),
    x = c(5.5, 3, 8),
    y = c(y_lake + 0.8, y_lake + 0.5, y_lake + 0.5),
    size = c(4.5, 3.5, 3.5),
    fontface = c("bold", "plain", "plain"),
    stringsAsFactors = FALSE
  )

  # --- Metadata Tables ---
  metadata_tables <- data.frame(
    name = c("__ol_objects", "__ol_refs", "__ol_dependencies"),
    x = c(2.5, 5.5, 8.5),
    y = rep(y_metadata + 0.8, 3),
    description = c("Object versions\n& hashes", "Tags &\nsnapshots",
                    "Lineage\nedges"),
    stringsAsFactors = FALSE
  )

  # --- Storage Layer ---
  storage_components <- data.frame(
    name = c("DuckDB", "Apache Arrow", "Parquet"),
    x = c(2.5, 5.5, 8.5),
    y = rep(y_storage + 0.8, 3),
    description = c("Query Engine\n(OLAP)", "Zero-Copy\nTransfer",
                    "Columnar\nStorage"),
    stringsAsFactors = FALSE
  )

  # --- Bioconductor Adapters (right side) ---
  bioc_panel <- data.frame(
    xmin = 11,
    xmax = 13,
    ymin = y_storage,
    ymax = y_api + layer_height
  )

  bioc_items <- data.frame(
    name = c("Summarized-\nExperiment", "Seurat*", "Matrix", "data.frame"),
    x = rep(12, 4),
    y = c(7.0, 5.8, 4.6, 3.4),
    stringsAsFactors = FALSE
  )

  # --- Data Flow Arrows ---
  # Main vertical flow
  arrows_vertical <- data.frame(
    x = rep(x_center, 3),
    xend = rep(x_center, 3),
    y = c(y_api, y_lake, y_metadata),
    yend = c(y_lake + layer_height, y_metadata + layer_height, y_storage + layer_height),
    type = rep("data_flow", 3),
    stringsAsFactors = FALSE
  )

  # Storage horizontal flow (bidirectional)
  arrows_storage <- data.frame(
    x = c(3.7, 6.7),
    xend = c(4.3, 7.3),
    y = rep(y_storage + 0.8, 2),
    yend = rep(y_storage + 0.8, 2),
    type = rep("bidirectional", 2),
    stringsAsFactors = FALSE
  )

  # API to Lake arrows
  arrows_api <- data.frame(
    x = api_methods$x,
    xend = api_methods$x,
    y = rep(y_api, 5),
    yend = rep(y_lake + layer_height + 0.05, 5),
    stringsAsFactors = FALSE
  )

  # ===========================================================================
  # Create the plot
  # ===========================================================================

  p <- ggplot() +

    # --- Layer backgrounds ---
    geom_rect(data = layers,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
              color = colors$border, linewidth = 0.4, alpha = 0.7) +
    scale_fill_identity() +

    # --- Layer labels ---
    geom_text(data = layers,
              aes(x = xmin + 0.2, y = label_y, label = layer),
              hjust = 0, vjust = 1, fontface = "bold",
              size = 3.2, color = colors$text_dark) +

    # --- API Methods ---
    geom_label(data = api_methods,
               aes(x = x, y = y, label = name),
               size = 3, fill = colors$api_method, color = colors$text_light,
               fontface = "bold",
               label.padding = unit(0.25, "lines"),
               linewidth = 0) +

    # --- Lake R6 Class box ---
    geom_rect(aes(xmin = 3, xmax = 8, ymin = y_lake + 0.2, ymax = y_lake + 1.4),
              fill = "#90CAF9", color = colors$api_method, linewidth = 0.8) +
    geom_text(aes(x = 5.5, y = y_lake + 0.8, label = "Lake R6 Class"),
              size = 4, fontface = "bold", color = colors$text_dark) +

    # Small boxes for DependencyTracker and lake_tbl
    geom_label(aes(x = 2.3, y = y_lake + 0.8, label = "Dependency\nTracker"),
               size = 2.3, fill = "#BBDEFB", color = colors$text_dark,
               label.padding = unit(0.2, "lines"), linewidth = 0.3,
               lineheight = 0.85) +
    geom_label(aes(x = 8.7, y = y_lake + 0.8, label = "lake_tbl\n(S3 class)"),
               size = 2.3, fill = "#BBDEFB", color = colors$text_dark,
               label.padding = unit(0.2, "lines"), linewidth = 0.3,
               lineheight = 0.85) +

    # --- Metadata Tables ---
    geom_label(data = metadata_tables,
               aes(x = x, y = y, label = name),
               size = 2.8, fill = colors$metadata_table, color = colors$text_light,
               fontface = "bold",
               label.padding = unit(0.25, "lines"),
               linewidth = 0) +
    # Table descriptions
    geom_text(data = metadata_tables,
              aes(x = x, y = y - 0.55, label = description),
              size = 2.2, color = colors$text_dark, lineheight = 0.85) +

    # --- Storage Components ---
    geom_label(data = storage_components,
               aes(x = x, y = y, label = name),
               size = 2.8, fill = colors$storage_tech, color = colors$text_light,
               fontface = "bold",
               label.padding = unit(0.25, "lines"),
               linewidth = 0) +
    # Storage descriptions
    geom_text(data = storage_components,
              aes(x = x, y = y - 0.55, label = description),
              size = 2.2, color = colors$text_dark, lineheight = 0.85) +

    # --- Data Flow Arrows (vertical) ---
    geom_segment(data = arrows_vertical,
                 aes(x = x, xend = xend, y = y - 0.05, yend = yend + 0.05),
                 arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
                 color = colors$data_flow, linewidth = 1.2) +

    # --- Storage bidirectional arrows ---
    geom_segment(data = arrows_storage,
                 aes(x = x, xend = xend, y = y, yend = yend),
                 arrow = arrow(length = unit(0.1, "cm"), ends = "both", type = "closed"),
                 color = colors$bidirectional, linewidth = 0.8) +

    # --- Bioconductor Panel ---
    geom_rect(data = bioc_panel,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = colors$bioc_layer, color = colors$border,
              linewidth = 0.4, alpha = 0.7) +
    geom_text(aes(x = 12, y = y_api + layer_height - 0.15, label = "Bioconductor\nAdapters"),
              size = 3, fontface = "bold", color = colors$text_dark,
              lineheight = 0.85, vjust = 1) +
    geom_label(data = bioc_items,
               aes(x = x, y = y, label = name),
               size = 2.3, fill = colors$bioc_adapter, color = colors$text_light,
               label.padding = unit(0.2, "lines"), linewidth = 0,
               lineheight = 0.85) +

    # Bioconductor connection (dashed lines)
    geom_segment(aes(x = x_max, xend = 11, y = y_lake + 0.8, yend = y_lake + 0.8),
                 linetype = "dashed", color = colors$bioc_adapter, linewidth = 0.6) +
    geom_segment(aes(x = x_max, xend = 11, y = y_storage + 0.8, yend = y_storage + 0.8),
                 linetype = "dashed", color = colors$bioc_adapter, linewidth = 0.6) +

    # --- Data flow labels ---
    annotate("text", x = x_center + 0.5, y = (y_api + y_lake + layer_height) / 2,
             label = "API calls", size = 2.5, color = colors$data_flow, angle = 0) +
    annotate("text", x = x_center + 0.5, y = (y_lake + y_metadata + layer_height) / 2,
             label = "Track lineage", size = 2.5, color = colors$data_flow, angle = 0) +
    annotate("text", x = x_center + 0.5, y = (y_metadata + y_storage + layer_height) / 2,
             label = "Read/Write", size = 2.5, color = colors$data_flow, angle = 0) +

    # Storage flow labels
    annotate("text", x = 4, y = y_storage + 0.4, label = "Zero-copy",
             size = 2.2, color = colors$bidirectional) +
    annotate("text", x = 7, y = y_storage + 0.4, label = "Columnar I/O",
             size = 2.2, color = colors$bidirectional) +

    # --- Footnote ---
    annotate("text", x = 12, y = 2.2, label = "*Planned",
             size = 2, color = colors$text_dark, fontface = "italic") +

    # --- Styling ---
    coord_fixed(ratio = 0.65, xlim = c(0, 14), ylim = c(0.3, 9.2)) +
    theme_void() +
    theme(
      text = element_text(family = "sans"),
      plot.margin = margin(5, 5, 5, 5, "pt"),
      plot.background = element_rect(fill = "white", color = NA)
    )

  # ===========================================================================
  # Save output
  # ===========================================================================

  if (!is.null(output_file)) {
    ext <- tolower(tools::file_ext(output_file))

    if (ext == "pdf") {
      ggsave(output_file, p, width = width, height = height, device = "pdf")
    } else if (ext %in% c("png", "jpg", "jpeg", "tiff")) {
      ggsave(output_file, p, width = width, height = height, dpi = dpi, device = ext)
    } else {
      ggsave(output_file, p, width = width, height = height)
    }

    message("Figure saved to: ", output_file)
  }

  invisible(p)
}


# =============================================================================
# Alternative: Simplified architecture diagram (single column)
# =============================================================================

#' Generate Simplified Architecture Diagram
#'
#' Creates a more compact, single-column architecture diagram suitable for
#' narrow journal layouts.
#'
#' @param output_file Path to output file
#' @param width Figure width in inches (default: 5)
#' @param height Figure height in inches (default: 6)
#' @return Invisibly returns the ggplot object
#' @export
generate_architecture_diagram_compact <- function(output_file = "figure1_architecture_compact.pdf",
                                                   width = 5,
                                                   height = 6) {

  suppressPackageStartupMessages(library(ggplot2))

  # Simplified color scheme
  colors <- list(
    api = "#1976D2",
    lake = "#2196F3",
    metadata = "#FF9800",
    storage = "#9C27B0",
    bg = "#FAFAFA",
    text = "#212121",
    arrow = "#4CAF50"
  )

  # Y positions
  y_pos <- c(api = 5.5, lake = 4.2, metadata = 2.8, storage = 1.4)

  # Create plot
  p <- ggplot() +

    # Background
    annotate("rect", xmin = 0.5, xmax = 5.5, ymin = 0.8, ymax = 6.2,
             fill = colors$bg, color = "grey70", linewidth = 0.3) +

    # Layer boxes
    geom_rect(aes(xmin = 1, xmax = 5, ymin = y_pos["api"] - 0.3, ymax = y_pos["api"] + 0.3),
              fill = colors$api, color = NA) +
    geom_rect(aes(xmin = 1, xmax = 5, ymin = y_pos["lake"] - 0.3, ymax = y_pos["lake"] + 0.3),
              fill = colors$lake, color = NA) +
    geom_rect(aes(xmin = 1, xmax = 5, ymin = y_pos["metadata"] - 0.3, ymax = y_pos["metadata"] + 0.3),
              fill = colors$metadata, color = NA) +
    geom_rect(aes(xmin = 1, xmax = 5, ymin = y_pos["storage"] - 0.3, ymax = y_pos["storage"] + 0.3),
              fill = colors$storage, color = NA) +

    # Labels
    annotate("text", x = 3, y = y_pos["api"],
             label = "User API: put() | get() | snap() | tree()",
             color = "white", size = 2.8, fontface = "bold") +
    annotate("text", x = 3, y = y_pos["lake"],
             label = "Lake R6 Class + DependencyTracker",
             color = "white", size = 2.8, fontface = "bold") +
    annotate("text", x = 3, y = y_pos["metadata"],
             label = "__ol_objects | __ol_refs | __ol_dependencies",
             color = "white", size = 2.5, fontface = "bold") +
    annotate("text", x = 3, y = y_pos["storage"],
             label = "DuckDB <-> Arrow <-> Parquet",
             color = "white", size = 2.8, fontface = "bold") +

    # Arrows
    geom_segment(aes(x = 3, xend = 3,
                     y = y_pos["api"] - 0.3, yend = y_pos["lake"] + 0.35),
                 arrow = arrow(length = unit(0.12, "cm"), type = "closed"),
                 color = colors$arrow, linewidth = 1) +
    geom_segment(aes(x = 3, xend = 3,
                     y = y_pos["lake"] - 0.3, yend = y_pos["metadata"] + 0.35),
                 arrow = arrow(length = unit(0.12, "cm"), type = "closed"),
                 color = colors$arrow, linewidth = 1) +
    geom_segment(aes(x = 3, xend = 3,
                     y = y_pos["metadata"] - 0.3, yend = y_pos["storage"] + 0.35),
                 arrow = arrow(length = unit(0.12, "cm"), type = "closed"),
                 color = colors$arrow, linewidth = 1) +

    coord_fixed(ratio = 0.8, xlim = c(0, 6), ylim = c(0.5, 6.5)) +
    theme_void() +
    theme(
      text = element_text(family = "sans"),
      plot.margin = margin(5, 5, 5, 5)
    )

  if (!is.null(output_file)) {
    ggsave(output_file, p, width = width, height = height)
    message("Compact figure saved to: ", output_file)
  }

  invisible(p)
}


# =============================================================================
# Main execution
# =============================================================================

.figure1_is_direct_run <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (!length(file_arg)) {
    return(FALSE)
  }
  script <- sub("^--file=", "", file_arg[[1]])
  identical(basename(script), "figure1_architecture.R")
}

if (!interactive() && .figure1_is_direct_run()) {
  message("========================================")
  message("Generating OmicsLake Architecture Diagram (Figure 1)")
  message("========================================\n")

  args <- commandArgs(trailingOnly = FALSE)
  script <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[[1]])
  script_dir <- dirname(normalizePath(script))

  output_dir <- file.path(script_dir, "output")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Generate main figure
  tryCatch({
    message("Generating main architecture diagram...")

    # PDF version
    pdf_file <- file.path(output_dir, "figure1_architecture.pdf")
    p <- generate_architecture_diagram(pdf_file, width = 8, height = 7)
    message("  [OK] ", pdf_file)

    # PNG version (300 DPI for submission)
    png_file <- file.path(output_dir, "figure1_architecture.png")
    generate_architecture_diagram(png_file, width = 8, height = 7, dpi = 300)
    message("  [OK] ", png_file)

    # High-res PNG (600 DPI for print)
    png_hires <- file.path(output_dir, "figure1_architecture_600dpi.png")
    generate_architecture_diagram(png_hires, width = 8, height = 7, dpi = 600)
    message("  [OK] ", png_hires)

    # Compact version
    message("\nGenerating compact version...")
    compact_pdf <- file.path(output_dir, "figure1_architecture_compact.pdf")
    generate_architecture_diagram_compact(compact_pdf)
    message("  [OK] ", compact_pdf)

    message("\n========================================")
    message("Figure generation complete!")
    message("Output files in: ", output_dir)
    message("========================================")

  }, error = function(e) {
    message("[ERROR] Failed to generate figure: ", e$message)
    message("\nTrying alternative methods...")

    # Try TikZ compilation as fallback
    tex_file <- file.path(script_dir, "figure1_architecture_standalone.tex")
    if (file.exists(tex_file) && Sys.which("pdflatex") != "") {
      message("Attempting TikZ compilation...")
      system2("pdflatex",
              args = c("-interaction=nonstopmode",
                       sprintf("-output-directory=%s", output_dir),
                       tex_file),
              stdout = FALSE, stderr = FALSE)
      message("TikZ compilation attempted. Check output directory.")
    }
  })
}

# =============================================================================
# Interactive usage example
# =============================================================================

#' @examples
#' \dontrun{
#' # Generate figure interactively
#' p <- generate_architecture_diagram()
#' print(p)
#'
#' # Save to specific location
#' generate_architecture_diagram("my_figure.pdf", width = 10, height = 8)
#'
#' # Generate compact version
#' generate_architecture_diagram_compact("compact.pdf")
#' }
