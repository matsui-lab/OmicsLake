#' @title OmicsLake Evaluation Plotting
#' @description Functions for generating evaluation figures
#' @name eval_plot
NULL

#' Plot benchmark results
#'
#' Generates standard plots for OmicsLake evaluation results.
#'
#' @param results_file Path to JSONL results file or aggregated data frame
#' @param output_dir Directory for output plots
#' @param format Output format: "png", "pdf", or "svg"
#' @return Invisible list of plot file paths
#' @export
ol_eval_plot_all <- function(results_file, output_dir = NULL, format = "png") {
  if (is.null(output_dir)) {
    output_dir <- file.path("inst/eval/results", "figures")
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # Load results
  if (is.character(results_file)) {
    records <- ol_eval_read_jsonl(results_file)
    df <- .ol_eval_records_to_df(records)
  } else if (is.data.frame(results_file)) {
    df <- results_file
  } else {
    stop("results_file must be a file path or data frame", call. = FALSE)
  }

  plots <- list()

  # Figure 1: Overhead comparison
  plots$overhead <- ol_eval_plot_overhead(df, output_dir, format)

  # Figure 2: Storage growth
  plots$storage <- ol_eval_plot_storage(df, output_dir, format)

  # Figure 3: Pushdown effectiveness
  plots$pushdown <- ol_eval_plot_pushdown(df, output_dir, format)

  # Figure 4: Lineage scaling
  plots$lineage <- ol_eval_plot_lineage(df, output_dir, format)

  message("Plots saved to: ", output_dir)
  invisible(plots)
}

#' Convert JSONL records to data frame
#' @keywords internal
.ol_eval_records_to_df <- function(records) {
  rows <- lapply(records, function(r) {
    data.frame(
      workload = r$workload,
      variant = r$variant,
      size = r$size,
      cache = r$cache,
      rep = r$rep,
      time_sec = r$metrics$time_sec %||% NA_real_,
      rss_mb = r$metrics$rss_mb %||% NA_real_,
      bytes_delta = r$metrics$bytes_delta %||% NA_real_,
      n_rows = r$metrics$n_rows %||% NA_integer_,
      pushdown_valid = r$evidence$pushdown_valid %||% NA,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Plot overhead comparison (OmicsLake vs baseline)
#'
#' @param df Results data frame
#' @param output_dir Output directory
#' @param format Output format
#' @return Path to saved plot
#' @export
ol_eval_plot_overhead <- function(df, output_dir, format = "png") {
  # Filter to comparable workloads
  df_cmp <- df[df$workload %in% c("W0-1", "W1-1", "W1-2") &
                df$variant %in% c("omicslake", "baseline_duckdb"), ]

  if (nrow(df_cmp) == 0) {
    message("No comparable data for overhead plot")
    return(NULL)
  }

  # Aggregate by workload, variant, size
  agg <- aggregate(time_sec ~ workload + variant + size, data = df_cmp,
                   FUN = function(x) c(median = median(x), iqr = IQR(x)))
  agg <- cbind(agg[, c("workload", "variant", "size")], as.data.frame(agg$time_sec))

  # Create plot using base R graphics
  output_file <- file.path(output_dir, paste0("fig1_overhead.", format))

  if (format == "png") {
    grDevices::png(output_file, width = 800, height = 600)
  } else if (format == "pdf") {
    grDevices::pdf(output_file, width = 8, height = 6)
  } else if (format == "svg") {
    grDevices::svg(output_file, width = 8, height = 6)
  }

  # Grouped bar plot
  workloads <- unique(agg$workload)
  variants <- unique(agg$variant)
  sizes <- unique(agg$size)

  # Create matrix for barplot
  plot_data <- matrix(NA, nrow = length(variants), ncol = length(workloads))
  rownames(plot_data) <- variants
  colnames(plot_data) <- workloads

  for (w in workloads) {
    for (v in variants) {
      val <- agg[agg$workload == w & agg$variant == v, "median"]
      if (length(val) > 0) {
        plot_data[v, w] <- val[1]
      }
    }
  }

  colors <- c("omicslake" = "#4A90D9", "baseline_duckdb" = "#7B7B7B")

  graphics::par(mar = c(5, 4, 4, 8), xpd = TRUE)
  graphics::barplot(plot_data,
          beside = TRUE,
          col = colors[rownames(plot_data)],
          main = "Figure 1: Overhead Comparison",
          xlab = "Workload",
          ylab = "Time (seconds)",
          legend.text = TRUE,
          args.legend = list(x = "topright", inset = c(-0.15, 0)))

  grDevices::dev.off()

  message("  Saved: ", output_file)
  output_file
}

#' Plot storage growth with tags/snaps
#'
#' @param df Results data frame
#' @param output_dir Output directory
#' @param format Output format
#' @return Path to saved plot
#' @export
ol_eval_plot_storage <- function(df, output_dir, format = "png") {
  # Filter to storage-related workloads
  df_storage <- df[grepl("W0-3", df$workload), ]

  if (nrow(df_storage) == 0) {
    message("No storage data for plot")
    return(NULL)
  }

  # Aggregate bytes_delta
  agg <- aggregate(bytes_delta ~ workload + size, data = df_storage,
                   FUN = function(x) c(median = median(x, na.rm = TRUE),
                                       iqr = IQR(x, na.rm = TRUE)))
  agg <- cbind(agg[, c("workload", "size")], as.data.frame(agg$bytes_delta))

  output_file <- file.path(output_dir, paste0("fig2_storage.", format))

  if (format == "png") {
    grDevices::png(output_file, width = 800, height = 600)
  } else if (format == "pdf") {
    grDevices::pdf(output_file, width = 8, height = 6)
  } else if (format == "svg") {
    grDevices::svg(output_file, width = 8, height = 6)
  }

  # Bar plot of storage deltas
  workloads <- unique(agg$workload)
  vals <- agg$median / 1024  # Convert to KB

  colors <- c("W0-3-tag" = "#2ECC71", "W0-3-snap" = "#E74C3C")

  graphics::barplot(vals,
          names.arg = agg$workload,
          col = colors[agg$workload],
          main = "Figure 2: Storage Overhead per Operation",
          xlab = "Operation",
          ylab = "Size Delta (KB)",
          border = NA)

  grDevices::dev.off()

  message("  Saved: ", output_file)
  output_file
}

#' Plot pushdown effectiveness
#'
#' @param df Results data frame
#' @param output_dir Output directory
#' @param format Output format
#' @return Path to saved plot
#' @export
ol_eval_plot_pushdown <- function(df, output_dir, format = "png") {
  # Filter to query workloads
  df_query <- df[grepl("W1-1|W1-2", df$workload), ]

  if (nrow(df_query) == 0) {
    message("No query data for pushdown plot")
    return(NULL)
  }

  # Aggregate by workload, variant, size
  agg <- aggregate(time_sec ~ workload + variant + size, data = df_query,
                   FUN = function(x) c(median = median(x), n = length(x)))
  agg <- cbind(agg[, c("workload", "variant", "size")], as.data.frame(agg$time_sec))

  output_file <- file.path(output_dir, paste0("fig3_pushdown.", format))

  if (format == "png") {
    grDevices::png(output_file, width = 800, height = 600)
  } else if (format == "pdf") {
    grDevices::pdf(output_file, width = 8, height = 6)
  } else if (format == "svg") {
    grDevices::svg(output_file, width = 8, height = 6)
  }

  # Compare OmicsLake vs baseline for each size
  sizes <- unique(agg$size)

  graphics::par(mfrow = c(1, length(sizes)), mar = c(5, 4, 4, 2))

  for (sz in sizes) {
    sz_data <- agg[agg$size == sz, ]

    if (nrow(sz_data) > 0) {
      # Create comparison matrix
      variants <- unique(sz_data$variant)
      workloads <- unique(sz_data$workload)

      plot_data <- matrix(NA, nrow = length(variants), ncol = length(workloads))
      rownames(plot_data) <- variants
      colnames(plot_data) <- workloads

      for (w in workloads) {
        for (v in variants) {
          val <- sz_data[sz_data$workload == w & sz_data$variant == v, "median"]
          if (length(val) > 0) plot_data[v, w] <- val[1]
        }
      }

      colors <- c("omicslake" = "#4A90D9", "baseline_duckdb" = "#7B7B7B")

      graphics::barplot(plot_data,
              beside = TRUE,
              col = colors[rownames(plot_data)],
              main = paste("Size:", sz),
              xlab = "Workload",
              ylab = "Time (seconds)",
              legend.text = (sz == sizes[1]))
    }
  }

  grDevices::dev.off()

  message("  Saved: ", output_file)
  output_file
}

#' Plot lineage operation scaling
#'
#' @param df Results data frame
#' @param output_dir Output directory
#' @param format Output format
#' @return Path to saved plot
#' @export
ol_eval_plot_lineage <- function(df, output_dir, format = "png") {
  # Filter to lineage workloads
  df_lin <- df[grepl("W2-1-tree", df$workload), ]

  if (nrow(df_lin) == 0) {
    message("No lineage data for plot")
    return(NULL)
  }

  # Extract depth from workload name
  df_lin$depth <- as.numeric(gsub("W2-1-tree-d", "", df_lin$workload))

  # Aggregate by depth
  agg <- aggregate(time_sec ~ depth, data = df_lin,
                   FUN = function(x) c(median = median(x),
                                       min = min(x),
                                       max = max(x)))
  agg <- cbind(agg[, "depth", drop = FALSE], as.data.frame(agg$time_sec))
  agg <- agg[order(agg$depth), ]

  output_file <- file.path(output_dir, paste0("fig4_lineage.", format))

  if (format == "png") {
    grDevices::png(output_file, width = 800, height = 600)
  } else if (format == "pdf") {
    grDevices::pdf(output_file, width = 8, height = 6)
  } else if (format == "svg") {
    grDevices::svg(output_file, width = 8, height = 6)
  }

  # Line plot with error bars
  graphics::plot(agg$depth, agg$median,
       type = "b",
       pch = 19,
       col = "#4A90D9",
       main = "Figure 4: Lineage Tree Traversal Scaling",
       xlab = "Traversal Depth",
       ylab = "Time (seconds)",
       ylim = c(0, max(agg$max) * 1.1))

  # Add error bars
  graphics::arrows(agg$depth, agg$min, agg$depth, agg$max,
         angle = 90, code = 3, length = 0.05, col = "#4A90D9")

  grDevices::dev.off()

  message("  Saved: ", output_file)
  output_file
}

#' Generate complete evaluation report
#'
#' @param jsonl_file Path to JSONL results file
#' @param output_dir Output directory for report and figures
#' @param case_study_results Optional case study results
#' @return Invisible path to report file
#' @export
ol_eval_generate_report <- function(jsonl_file, output_dir = NULL,
                                    case_study_results = NULL) {
  if (is.null(output_dir)) {
    output_dir <- file.path("inst/eval/results")
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # Generate plots
  fig_dir <- file.path(output_dir, "figures")
  ol_eval_plot_all(jsonl_file, fig_dir, format = "png")

  # Aggregate results
  csv_file <- file.path(output_dir, "summary.csv")
  agg <- ol_eval_aggregate_results(jsonl_file, csv_file)

  # Generate markdown report
  report_file <- file.path(output_dir, "evaluation_report.md")

  lines <- c(
    "# OmicsLake Evaluation Report",
    "",
    paste("Generated:", Sys.time()),
    "",
    "## Summary Statistics",
    "",
    "```",
    utils::capture.output(print(agg)),
    "```",
    "",
    "## Figures",
    "",
    "### Figure 1: Overhead Comparison",
    "![Overhead](figures/fig1_overhead.png)",
    "",
    "### Figure 2: Storage Growth",
    "![Storage](figures/fig2_storage.png)",
    "",
    "### Figure 3: Pushdown Effectiveness",
    "![Pushdown](figures/fig3_pushdown.png)",
    "",
    "### Figure 4: Lineage Scaling",
    "![Lineage](figures/fig4_lineage.png)",
    ""
  )

  # Add case study if available
  if (!is.null(case_study_results)) {
    lines <- c(lines,
      "## Case Study",
      "",
      paste("See detailed report:", file.path(output_dir, "case_study_report.md")),
      ""
    )
  }

  writeLines(lines, report_file)
  message("Report saved to: ", report_file)

  invisible(report_file)
}

#' Null coalescing (internal)
#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x
