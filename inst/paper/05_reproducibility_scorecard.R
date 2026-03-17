#!/usr/bin/env Rscript
# ==============================================================================
# OmicsLake Unified Reproducibility Scorecard
# ==============================================================================
# Aggregates RT-001..RT-005 outputs and breakage-taxonomy outputs into
# a single machine-readable scorecard (common + mode-specific axes).
# ==============================================================================

cat("=============================================================\n")
cat(" OmicsLake Unified Reproducibility Scorecard\n")
cat("=============================================================\n\n")

paper_dir <- if (basename(getwd()) == "paper") {
  getwd()
} else if (file.exists("inst/paper")) {
  "inst/paper"
} else {
  stop("Cannot find paper directory. Run from package root or inst/paper.")
}
setwd(paper_dir)

required_files <- c(
  "results_reproducibility_summary.csv",
  "results_breakage_coverage_summary.csv",
  "results_breakage_mode_summary.csv"
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop(
    "Missing prerequisite result files:\n  - ",
    paste(missing_files, collapse = "\n  - "),
    "\nRun 02_reproducibility_test.R and 04_breakage_taxonomy_validation.R first.",
    call. = FALSE
  )
}

rt <- read.csv("results_reproducibility_summary.csv", stringsAsFactors = FALSE)
bx_cov <- read.csv("results_breakage_coverage_summary.csv", stringsAsFactors = FALSE)
bx_mode <- read.csv("results_breakage_mode_summary.csv", stringsAsFactors = FALSE)
limit_cov <- if (file.exists("results_limit_coverage_summary.csv")) {
  read.csv("results_limit_coverage_summary.csv", stringsAsFactors = FALSE)
} else {
  NULL
}

metric_value <- function(df, key) {
  idx <- which(df$metric == key)
  if (length(idx) < 1) {
    return(NA_real_)
  }
  suppressWarnings(as.numeric(df$value[idx[1]]))
}

rt_value <- function(id) {
  idx <- which(rt$Test_ID == id)
  if (length(idx) < 1) {
    return(NA_real_)
  }
  suppressWarnings(as.numeric(rt$Primary_Metric[idx[1]]))
}

mode_value <- function(mode_name, col_name) {
  idx <- which(bx_mode$mode == mode_name)
  if (length(idx) < 1 || !col_name %in% names(bx_mode)) {
    return(NA_real_)
  }
  suppressWarnings(as.numeric(bx_mode[idx[1], col_name]))
}

scorecard <- data.frame(
  axis_id = c(
    "CMN-001", "CMN-002", "CMN-003", "CMN-004", "CMN-005", "CMN-006", "CMN-007", "CMN-008",
    "AUTO-001", "AUTO-002", "GUIDE-001", "GUIDE-002", "GUIDE-003"
  ),
  scope = c(
    rep("common", 8),
    rep("auto_rollback", 2),
    rep("guided_manual", 3)
  ),
  metric = c(
    "state_restoration_rate",
    "lineage_tracking_rate",
    "cross_environment_rate",
    "long_term_degradation_ratio",
    "rollback_cascade_rate",
    "breakage_detection_rate",
    "breakage_visualized_rate",
    "breakage_safe_execution_rate",
    "auto_restore_rate_among_broken",
    "auto_restore_action_rate",
    "guided_detection_rate",
    "guided_actionable_proposal_rate",
    "guided_guardrail_rate"
  ),
  value = c(
    rt_value("RT-001"),
    rt_value("RT-002"),
    rt_value("RT-003"),
    rt_value("RT-004"),
    rt_value("RT-005"),
    metric_value(bx_cov, "detected_rate"),
    metric_value(bx_cov, "visualized_rate"),
    metric_value(bx_cov, "safe_execution_rate"),
    mode_value("auto_rollback", "restored_rate_among_broken"),
    metric_value(bx_cov, "auto_mode_restore_action_rate"),
    mode_value("guided_manual", "detected_rate"),
    mode_value("guided_manual", "actionable_proposal_rate"),
    mode_value("guided_manual", "guardrail_rate")
  ),
  target = c(
    0.99, 0.99, 0.99, 2.00, 0.99, 0.95, 0.95, 1.00, 0.95, 1.00, 0.95, 0.95, 1.00
  ),
  direction = c(
    "ge", "ge", "ge", "le", "ge", "ge", "ge", "ge", "ge", "ge", "ge", "ge", "ge"
  ),
  stringsAsFactors = FALSE
)

if (!is.null(limit_cov)) {
  limit_rows <- data.frame(
    axis_id = c("LIM-001", "LIM-002", "LIM-003", "LIM-004"),
    scope = rep("limit_boundary", 4),
    metric = c(
      "limit_false_positive_rate",
      "limit_false_negative_rate",
      "limit_auto_restore_success_rate_expected",
      "limit_known_limit_guardrail_rate"
    ),
    value = c(
      metric_value(limit_cov, "false_positive_rate"),
      metric_value(limit_cov, "false_negative_rate"),
      metric_value(limit_cov, "auto_restore_success_rate_expected"),
      metric_value(limit_cov, "known_limit_exposed_rate")
    ),
    target = c(0.10, 0.10, 0.90, 0.90),
    direction = c("le", "le", "ge", "ge"),
    stringsAsFactors = FALSE
  )
  scorecard <- rbind(scorecard, limit_rows)
}

scorecard$pass <- ifelse(
  is.na(scorecard$value),
  FALSE,
  ifelse(scorecard$direction == "ge", scorecard$value >= scorecard$target, scorecard$value <= scorecard$target)
)

scope_summary <- do.call(rbind, lapply(unique(scorecard$scope), function(sc) {
  sub <- scorecard[scorecard$scope == sc, , drop = FALSE]
  data.frame(
    scope = sc,
    metric_count = nrow(sub),
    pass_count = sum(sub$pass, na.rm = TRUE),
    pass_rate = if (nrow(sub) > 0) sum(sub$pass, na.rm = TRUE) / nrow(sub) else NA_real_,
    stringsAsFactors = FALSE
  )
}))

write.csv(scorecard, "results_unified_scorecard.csv", row.names = FALSE)
write.csv(scope_summary, "results_unified_scope_summary.csv", row.names = FALSE)

md <- c(
  "# Unified Reproducibility Scorecard",
  "",
  sprintf("Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  "",
  "## Scope Summary",
  "",
  "```",
  capture.output(print(scope_summary, row.names = FALSE)),
  "```",
  "",
  "## Detailed Metrics",
  "",
  "```",
  capture.output(print(scorecard, row.names = FALSE)),
  "```",
  ""
)
writeLines(md, "results_unified_scorecard.md")

cat("Outputs:\n")
cat("  - results_unified_scorecard.csv\n")
cat("  - results_unified_scope_summary.csv\n")
cat("  - results_unified_scorecard.md\n\n")
cat("Done.\n")
