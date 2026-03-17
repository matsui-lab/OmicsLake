#!/usr/bin/env Rscript
# Figure 5: Reproducibility Capability Matrix
# Baseline workflows are shown qualitatively.
# OmicsLake cells include empirically observed RT outcomes from this study.

suppressPackageStartupMessages({
  library(ggplot2)
})

COLORS_ENV <- c(
  "Standard R Script" = "#e74c3c",
  "Git + Manual" = "#f39c12",
  "OmicsLake" = "#27ae60"
)

COLORS_LEVEL <- c(
  "Low" = "#fbe3df",
  "Moderate" = "#fdeccf",
  "High" = "#e1f3e5"
)

LABEL_COL <- c(
  "Low" = "#a93226",
  "Moderate" = "#8a6d1d",
  "High" = "#1f6f43"
)

theme_gigascience <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5, margin = margin(b = 10)),
      axis.title = element_blank(),
      axis.text.x = element_text(size = 10, face = "bold", angle = 20, hjust = 1),
      axis.text.y = element_text(size = 9),
      panel.grid = element_blank(),
      legend.position = "none",
      plot.margin = margin(t = 8, r = 8, b = 8, l = 8, unit = "pt")
    )
}

create_reproducibility_matrix <- function() {
  data.frame(
    Environment = c(
      "Standard R Script", "Git + Manual", "OmicsLake",
      "Standard R Script", "Git + Manual", "OmicsLake",
      "Standard R Script", "Git + Manual", "OmicsLake",
      "Standard R Script", "Git + Manual", "OmicsLake",
      "Standard R Script", "Git + Manual", "OmicsLake"
    ),
    Metric = c(
      rep("Intermediate-state restoration", 3),
      rep("Dependency capture", 3),
      rep("Rollback scope", 3),
      rep("Transfer verification", 3),
      rep("Operational overhead", 3)
    ),
    Level = c(
      "Low", "Moderate", "High",
      "Low", "Moderate", "High",
      "Low", "Moderate", "High",
      "Low", "Moderate", "High",
      "Low", "Moderate", "High"
    ),
    Label = c(
      "Ad hoc", "Manual conventions", "Observed 30/30\\n(RT-001)",
      "None automatic", "Manual", "Observed 100%\\n(RT-002)",
      "Manual file swap", "Code-first +\\nmanual data sync", "Observed cascade\\nrollback (RT-005)",
      "Typically\\nunverified", "Checklist-based", "Observed 9/9\\nsimulated (RT-003)",
      "High manual", "Medium manual", "Low incremental"
    ),
    stringsAsFactors = FALSE
  )
}

create_figure5 <- function(df = NULL) {
  if (is.null(df)) {
    df <- create_reproducibility_matrix()
  }

  df$Environment <- factor(
    df$Environment,
    levels = c("Standard R Script", "Git + Manual", "OmicsLake")
  )
  df$Metric <- factor(
    df$Metric,
    levels = rev(c(
      "Intermediate-state restoration",
      "Dependency capture",
      "Rollback scope",
      "Transfer verification",
      "Operational overhead"
    ))
  )
  df$Level <- factor(df$Level, levels = c("Low", "Moderate", "High"))

  ggplot(df, aes(x = Environment, y = Metric, fill = Level)) +
    geom_tile(color = "gray60", linewidth = 0.6) +
    geom_text(aes(label = Label, color = Level), size = 3, lineheight = 0.95) +
    scale_fill_manual(values = COLORS_LEVEL) +
    scale_color_manual(values = LABEL_COL) +
    labs(
      title = "Reproducibility Capability Matrix",
      subtitle = "Baseline workflows are qualitative; OmicsLake cells report observed RT outcomes"
    ) +
    theme_gigascience()
}

generate_figure5 <- function(output_dir = ".") {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  df <- create_reproducibility_matrix()
  p <- create_figure5(df)

  # Keep historical filename for downstream submission bundle scripts.
  ggsave(
    file.path(output_dir, "figure5_reproducibility_grouped.pdf"),
    p,
    width = 11,
    height = 7,
    device = "pdf"
  )

  ggsave(
    file.path(output_dir, "figure5_reproducibility_grouped.png"),
    p,
    width = 11,
    height = 7,
    dpi = 300
  )

  # Optional secondary export for review convenience.
  ggsave(
    file.path(output_dir, "figure5_reproducibility_faceted.pdf"),
    p,
    width = 11,
    height = 7,
    device = "pdf"
  )

  message("Figure 5 saved to: ", output_dir)
  message("  - figure5_reproducibility_grouped.pdf")
  message("  - figure5_reproducibility_grouped.png")

  invisible(list(grouped = p, data = df))
}

.figure5_is_direct_run <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (!length(file_arg)) {
    return(FALSE)
  }
  script <- sub("^--file=", "", file_arg[[1]])
  identical(basename(script), "figure5_reproducibility_comparison.R")
}

if (!interactive() && .figure5_is_direct_run()) {
  args <- commandArgs(trailingOnly = FALSE)
  script <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[[1]])
  script_dir <- dirname(normalizePath(script))
  setwd(script_dir)
  output_dir <- file.path(script_dir, "output")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  generate_figure5(output_dir)
}
