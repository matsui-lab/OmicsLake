#!/usr/bin/env Rscript
# === Figure 3: Performance Benchmarks ===
# OmicsLake Paper - Data Visualization
# Compares OmicsLake performance against baseline methods
#
# Specifications:
#   - Size: 10 x 6 inches (single column: 170mm / 6.7in)
#   - Color Palette: GigaScience compliant
#     - Blue (#1f77b4): OmicsLake/DuckDB
#     - Orange (#ff7f0e): Baseline (base R/dplyr)
#   - Font: Arial/Helvetica (sans-serif), 10-12pt
#   - Error bars: 95% run intervals (2.5th-97.5th percentiles)
#   - Speedup annotations: Above bars

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# === GigaScience Color Palette ===
COLORS <- list(
  omicslake = "#1f77b4",  # Blue
  baseline  = "#ff7f0e",  # Orange
  green     = "#2ca02c"   # Green (for additional categories)
)

# === Theme for GigaScience Publication ===
theme_gigascience <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      # Text settings
      text = element_text(family = "sans"),
      plot.title = element_text(size = base_size + 2, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = base_size, hjust = 0.5, color = "gray40"),
      axis.title = element_text(size = base_size, face = "bold"),
      axis.text = element_text(size = base_size - 1),
      axis.text.x = element_text(angle = 0, hjust = 0.5),

      # Legend settings
      legend.position = "bottom",
      legend.title = element_text(size = base_size, face = "bold"),
      legend.text = element_text(size = base_size - 1),
      legend.background = element_rect(fill = "white", color = NA),
      legend.key.size = unit(0.8, "lines"),

      # Panel settings
      panel.grid.major.y = element_line(color = "gray85", linewidth = 0.3),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "gray70", fill = NA, linewidth = 0.5),

      # Strip settings (for facets)
      strip.text = element_text(size = base_size, face = "bold"),
      strip.background = element_rect(fill = "gray95", color = "gray70"),

      # Plot margins
      plot.margin = margin(t = 10, r = 15, b = 10, l = 10, unit = "pt")
    )
}

# === Sample Benchmark Data ===
# Note: Replace with actual data from results_performance.RDS
# Expected structure from 01_performance_benchmark.R

# Function to load actual benchmark data if available
load_benchmark_data <- function(results_file = "../results_performance.RDS") {
  if (file.exists(results_file)) {
    results <- readRDS(results_file)
    return(results)
  } else {
    message("Using sample data. Run 01_performance_benchmark.R to generate actual results.")
    return(NULL)
  }
}

# Create benchmark data frame (sample data for demonstration)
# This will be replaced with actual results when available
create_benchmark_df <- function(results = NULL) {

  # Benchmark medians and run intervals from results/Table3_final.csv and
  # results/benchmark_summary.csv (n = 30 repeated runs)
  benchmark_data <- data.frame(
    Task = factor(c("Table Import", "Aggregation", "Join", "Snapshot"),
                  levels = c("Table Import", "Aggregation", "Join", "Snapshot")),

    # OmicsLake performance (seconds)
    OmicsLake_median = c(0.0143, 0.0050, 0.1049, 0.0329),
    OmicsLake_ci_lower = c(0.0139, 0.0049, 0.1009, 0.0313),
    OmicsLake_ci_upper = c(0.0200, 0.0057, 0.1091, 0.0376),

    # Baseline performance (seconds)
    Baseline_median = c(0.5230, 0.0314, 0.6538, 0.0075),
    Baseline_ci_lower = c(0.5205, 0.0304, 0.6340, 0.0041),
    Baseline_ci_upper = c(0.5290, 0.0328, 0.6832, 0.0097),

    # Method labels
    Baseline_Method = c("base R (RDS)", "dplyr", "base merge()", "file.copy()"),

    stringsAsFactors = FALSE
  )

  # Calculate speedup (keep full precision; format at annotation time)
  benchmark_data$Speedup <- benchmark_data$Baseline_median /
    benchmark_data$OmicsLake_median

  return(benchmark_data)
}

# === Convert to long format for ggplot2 ===
prepare_plot_data <- function(benchmark_data) {

  # Reshape to long format
  plot_data <- data.frame(
    Task = rep(benchmark_data$Task, 2),
    Method = factor(
      c(rep("OmicsLake", 4), rep("Baseline", 4)),
      levels = c("Baseline", "OmicsLake")
    ),
    Median = c(benchmark_data$OmicsLake_median, benchmark_data$Baseline_median),
    CI_Lower = c(benchmark_data$OmicsLake_ci_lower, benchmark_data$Baseline_ci_lower),
    CI_Upper = c(benchmark_data$OmicsLake_ci_upper, benchmark_data$Baseline_ci_upper),
    Speedup = c(benchmark_data$Speedup, rep(NA, 4)),
    stringsAsFactors = FALSE
  )

  return(plot_data)
}

# === Main Figure 3 Plot ===
create_figure3 <- function(benchmark_data = NULL) {

  if (is.null(benchmark_data)) {
    benchmark_data <- create_benchmark_df()
  }

  plot_data <- prepare_plot_data(benchmark_data)

  # Create speedup annotation data
  # Snapshot row uses "N/C" (not comparable) since snapshot and file-copy

  # proxy are not semantically identical (see Table 6 footnote b).
  annotation_data <- benchmark_data %>%
    mutate(
      y_pos = pmax(Baseline_ci_upper, OmicsLake_ci_upper) * 1.15,
      label = ifelse(Task == "Snapshot", "N/C",
                     ifelse(Speedup < 1,
                            sprintf("%.2fx", Speedup),
                            sprintf("%.1fx", Speedup)))
    )

  # Main plot
  p <- ggplot(plot_data, aes(x = Task, y = Median, fill = Method)) +

    # Bar plot
    geom_bar(stat = "identity", position = position_dodge(width = 0.8),
             width = 0.7, color = "gray30", linewidth = 0.3) +

    # Error bars (95% interval across repeated runs)
    geom_errorbar(
      aes(ymin = CI_Lower, ymax = CI_Upper),
      position = position_dodge(width = 0.8),
      width = 0.2,
      linewidth = 0.5,
      color = "gray30"
    ) +

    # Speedup annotations
    geom_text(
      data = annotation_data,
      aes(x = Task, y = y_pos, label = label),
      inherit.aes = FALSE,
      size = 3.5,
      fontface = "bold",
      color = COLORS$omicslake
    ) +

    # Color scale
    scale_fill_manual(
      values = c("Baseline" = COLORS$baseline, "OmicsLake" = COLORS$omicslake),
      labels = c("Baseline" = "Baseline (base R/dplyr)", "OmicsLake" = "OmicsLake")
    ) +

    # Y-axis formatting
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.2)),
      labels = function(x) paste0(x, "s")
    ) +

    # Labels
    labs(
      title = "Performance Benchmarks: OmicsLake vs Baseline Methods",
      subtitle = "n = 30 iterations; error bars indicate 95% run intervals (2.5th-97.5th percentiles)",
      x = NULL,
      y = "Execution Time (seconds)",
      fill = "Method"
    ) +

    # Theme
    theme_gigascience() +
    theme(
      legend.position = c(0.85, 0.85),
      legend.background = element_rect(fill = "white", color = "gray70", linewidth = 0.3)
    )

  return(p)
}

# === Alternative: Faceted Version (Normalized to Baseline) ===
create_figure3_normalized <- function(benchmark_data = NULL) {

  if (is.null(benchmark_data)) {
    benchmark_data <- create_benchmark_df()
  }

  # Calculate relative performance (baseline = 1.0)
  normalized_data <- benchmark_data %>%
    mutate(
      Baseline_norm = 1.0,
      Baseline_norm_lower = Baseline_ci_lower / Baseline_median,
      Baseline_norm_upper = Baseline_ci_upper / Baseline_median,
      OmicsLake_norm = OmicsLake_median / Baseline_median,
      OmicsLake_norm_lower = OmicsLake_ci_lower / Baseline_median,
      OmicsLake_norm_upper = OmicsLake_ci_upper / Baseline_median
    )

  plot_data <- data.frame(
    Task = rep(normalized_data$Task, 2),
    Method = factor(
      c(rep("OmicsLake", 4), rep("Baseline", 4)),
      levels = c("Baseline", "OmicsLake")
    ),
    Relative = c(normalized_data$OmicsLake_norm, rep(1.0, 4)),
    CI_Lower = c(normalized_data$OmicsLake_norm_lower,
                 normalized_data$Baseline_norm_lower),
    CI_Upper = c(normalized_data$OmicsLake_norm_upper,
                 normalized_data$Baseline_norm_upper)
  )

  p <- ggplot(plot_data, aes(x = Method, y = Relative, fill = Method)) +
    geom_bar(stat = "identity", width = 0.7, color = "gray30", linewidth = 0.3) +
    geom_errorbar(
      aes(ymin = CI_Lower, ymax = CI_Upper),
      width = 0.2, linewidth = 0.5, color = "gray30"
    ) +
    geom_hline(yintercept = 1.0, linetype = "dashed", color = "gray50") +
    facet_wrap(~ Task, nrow = 1) +
    scale_fill_manual(
      values = c("Baseline" = COLORS$baseline, "OmicsLake" = COLORS$omicslake)
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.1)),
      labels = scales::percent_format()
    ) +
    labs(
      title = "Relative Performance (Baseline = 100%)",
      subtitle = "Lower is better; error bars indicate 95% run intervals (2.5th-97.5th percentiles)",
      x = NULL,
      y = "Relative Execution Time"
    ) +
    theme_gigascience() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  return(p)
}

# === Generate and Save Figure ===
generate_figure3 <- function(output_dir = ".") {

  # Load actual data if available
  results <- load_benchmark_data()
  benchmark_data <- create_benchmark_df(results)

  # Create main figure
  p_main <- create_figure3(benchmark_data)

  # Create normalized version
  p_norm <- create_figure3_normalized(benchmark_data)

  # Save figures
  ggsave(
    file.path(output_dir, "figure3_performance_benchmarks.pdf"),
    p_main,
    width = 10, height = 6,
    device = "pdf"
  )

  ggsave(
    file.path(output_dir, "figure3_performance_benchmarks.png"),
    p_main,
    width = 10, height = 6,
    dpi = 300
  )

  ggsave(
    file.path(output_dir, "figure3_performance_normalized.pdf"),
    p_norm,
    width = 10, height = 5,
    device = "pdf"
  )

  message("Figure 3 saved to: ", output_dir)
  message("  - figure3_performance_benchmarks.pdf")
  message("  - figure3_performance_benchmarks.png")
  message("  - figure3_performance_normalized.pdf")

  return(list(main = p_main, normalized = p_norm))
}

# === Figure Caption ===
#' Figure 3: Performance Benchmarks
#'
#' Comparison of OmicsLake (blue) versus baseline methods (orange) across four
#' key operations: Table Import (Parquet vs RDS), Aggregation (DuckDB SQL vs
#' dplyr), Join (DuckDB SQL vs base merge), and Snapshot (label-based snapshot
#' operation vs file copy). Each bar represents the median execution time from
#' n = 30 iterations. Error bars indicate 95% run intervals (2.5th-97.5th
#' percentiles) across repeated runs.
#' Speedup factors are annotated above each benchmark pair. OmicsLake
#' demonstrates strong speedups for import, aggregation, and join operations,
#' while snapshot timing reflects semantic differences between in-database
#' snapshot creation and external file-copy workflows. Snapshot ratio should be
#' interpreted as an operation-cost indicator. All benchmarks performed on
#' AMD Ryzen 9 7950X,
#' 128 GB RAM, Ubuntu 24.04 LTS.

.figure3_is_direct_run <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (!length(file_arg)) {
    return(FALSE)
  }
  script <- sub("^--file=", "", file_arg[[1]])
  identical(basename(script), "figure3_performance_benchmarks.R")
}

# Run if executed directly
if (!interactive() && .figure3_is_direct_run()) {
  args <- commandArgs(trailingOnly = FALSE)
  script <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[[1]])
  script_dir <- dirname(normalizePath(script))
  setwd(script_dir)
  output_dir <- file.path(script_dir, "output")
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  generate_figure3(output_dir)
}
