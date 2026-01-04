#' @title OmicsLake Case Study (W3)
#' @description RNA-seq case study demonstrating reproducibility features
#' @name eval_case_study
NULL

#' Run the RNA-seq case study (W3)
#'
#' Demonstrates OmicsLake's reproducibility features through a minimal
#' RNA-seq workflow:
#' 1. Load raw counts and tag
#' 2. Normalize (v1) and tag
#' 3. Multi-table join with metadata
#' 4. Re-normalize (v2) with different parameters
#' 5. Compare versions with diff
#' 6. Show complete lineage
#'
#' @param config Configuration list from ol_eval_load_config()
#' @param output_file Path to JSONL output file for timing metrics
#' @param verbose Print progress messages
#' @return List with case study results
#' @export
#' @examples
#' \dontrun{
#' config <- ol_eval_load_config("inst/eval/configs/eval_small.yml")
#' results <- ol_eval_run_case_study(config)
#' }
ol_eval_run_case_study <- function(config, output_file = NULL, verbose = TRUE) {
  if (is.null(output_file)) {
    output_file <- file.path(config$outputs$results_dir, "case_study_results.jsonl")
  }

  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

  set.seed(config$seed)
  env_info <- ol_eval_env_info()

  # Create project for case study
  project_name <- paste0("eval_case_study_", Sys.getpid())

  if (verbose) message("W3 Case Study: RNA-seq Reproducibility")
  if (verbose) message("  Project: ", project_name)

  lake <- Lake$new(project_name, root = config$project_root)

  results <- list(
    project = project_name,
    steps = list(),
    lineage = NULL,
    diff = NULL,
    validation = list()
  )

  tryCatch({
    # ========================================================================
    # Step 1: Load raw counts
    # ========================================================================
    if (verbose) message("  Step 1: Loading raw counts...")

    rnaseq <- ol_eval_generate_case_rnaseq(
      seed = config$seed,
      n_genes = config$case_study$n_genes,
      n_samples = config$case_study$n_samples
    )

    m1 <- ol_eval_measure({
      lake$put("counts", rnaseq$counts)
      lake$put("gene_info", rnaseq$gene_info)
      lake$put("sample_info", rnaseq$sample_info)
      lake$tag("counts", "raw")
    })

    results$steps$load_counts <- list(
      time_sec = m1$time_sec,
      n_genes = nrow(rnaseq$counts),
      n_samples = ncol(rnaseq$counts) - 1  # Exclude gene_id column
    )

    .ol_eval_write_step(output_file, "W3-1-load", m1$time_sec, env_info)

    # ========================================================================
    # Step 2: Normalize v1 (simple log1p)
    # ========================================================================
    if (verbose) message("  Step 2: Normalization v1 (log1p)...")

    m2 <- ol_eval_measure({
      # Get counts and apply log1p normalization
      counts_raw <- lake$get("counts@tag(raw)")
      sample_cols <- setdiff(names(counts_raw), "gene_id")

      counts_norm <- counts_raw
      for (col in sample_cols) {
        counts_norm[[col]] <- log1p(counts_norm[[col]])
      }

      # Save with lineage tracking
      attr(counts_norm, "lake_sources") <- list(list(name = "counts", ref = "@tag(raw)"))
      lake$put("counts_norm", counts_norm)
      lake$tag("counts_norm", "norm_v1")
    })

    results$steps$normalize_v1 <- list(time_sec = m2$time_sec)

    .ol_eval_write_step(output_file, "W3-2-norm_v1", m2$time_sec, env_info)

    # ========================================================================
    # Step 3: Join with metadata and create summary
    # ========================================================================
    if (verbose) message("  Step 3: Multi-table join and aggregation...")

    m3 <- ol_eval_measure({
      # Join counts_norm with gene_info and sample_info
      # Create a "long format" summary

      counts_norm <- lake$ref("counts_norm@tag(norm_v1)")
      gene_info <- lake$ref("gene_info")
      sample_info <- lake$ref("sample_info")

      # This creates a multi-parent dependency
      # We'll compute mean expression per gene type per condition
      summary_result <- counts_norm |>
        dplyr::inner_join(gene_info |> dplyr::select(gene_id, gene_type), by = "gene_id") |>
        dplyr::collect()

      # Compute summary statistics
      sample_cols <- setdiff(names(summary_result), c("gene_id", "gene_type"))
      summary_result$mean_expr <- rowMeans(summary_result[, sample_cols], na.rm = TRUE)

      summary_agg <- summary_result |>
        dplyr::group_by(gene_type) |>
        dplyr::summarise(
          n_genes = dplyr::n(),
          mean_expression = mean(mean_expr, na.rm = TRUE),
          sd_expression = sd(mean_expr, na.rm = TRUE),
          .groups = "drop"
        )

      # Record multi-parent lineage
      attr(summary_agg, "lake_sources") <- list(
        list(name = "counts_norm", ref = "@tag(norm_v1)"),
        list(name = "gene_info", ref = "@latest")
      )

      lake$put("summary", summary_agg)
      summary_agg
    })

    results$steps$join_summarize <- list(
      time_sec = m3$time_sec,
      n_groups = nrow(m3$result)
    )

    .ol_eval_write_step(output_file, "W3-3-join", m3$time_sec, env_info)

    # ========================================================================
    # Step 4: Re-normalize v2 (log1p + scaling)
    # ========================================================================
    if (verbose) message("  Step 4: Normalization v2 (log1p + scaling)...")

    m4 <- ol_eval_measure({
      counts_raw <- lake$get("counts@tag(raw)")
      sample_cols <- setdiff(names(counts_raw), "gene_id")

      # Different normalization: log1p + scale to median
      counts_norm2 <- counts_raw
      for (col in sample_cols) {
        vals <- log1p(counts_norm2[[col]])
        med <- median(vals, na.rm = TRUE)
        if (med > 0) {
          vals <- vals / med
        }
        counts_norm2[[col]] <- vals
      }

      attr(counts_norm2, "lake_sources") <- list(list(name = "counts", ref = "@tag(raw)"))
      lake$put("counts_norm", counts_norm2)  # Overwrites previous version
      lake$tag("counts_norm", "norm_v2")

      counts_norm2
    })

    results$steps$normalize_v2 <- list(time_sec = m4$time_sec)

    .ol_eval_write_step(output_file, "W3-4-norm_v2", m4$time_sec, env_info)

    # Regenerate summary with v2 normalization
    m4b <- ol_eval_measure({
      counts_norm <- lake$ref("counts_norm@tag(norm_v2)")
      gene_info <- lake$ref("gene_info")

      summary_result <- counts_norm |>
        dplyr::inner_join(gene_info |> dplyr::select(gene_id, gene_type), by = "gene_id") |>
        dplyr::collect()

      sample_cols <- setdiff(names(summary_result), c("gene_id", "gene_type"))
      summary_result$mean_expr <- rowMeans(summary_result[, sample_cols], na.rm = TRUE)

      summary_agg <- summary_result |>
        dplyr::group_by(gene_type) |>
        dplyr::summarise(
          n_genes = dplyr::n(),
          mean_expression = mean(mean_expr, na.rm = TRUE),
          sd_expression = sd(mean_expr, na.rm = TRUE),
          .groups = "drop"
        )

      attr(summary_agg, "lake_sources") <- list(
        list(name = "counts_norm", ref = "@tag(norm_v2)"),
        list(name = "gene_info", ref = "@latest")
      )

      lake$put("summary", summary_agg)  # Overwrite
      lake$tag("summary", "v2")
    })

    # Tag first summary version retroactively (it's already overwritten, so we tag current as v1 proxy)
    # In practice, user would tag before overwriting

    .ol_eval_write_step(output_file, "W3-4b-summary_v2", m4b$time_sec, env_info)

    # ========================================================================
    # Step 5: Compare versions with diff
    # ========================================================================
    if (verbose) message("  Step 5: Comparing normalization versions...")

    m5 <- ol_eval_measure({
      # Compare counts_norm between v1 and v2
      diff_result <- lake$diff("counts_norm", ref1 = "@tag(norm_v2)", ref2 = "@tag(norm_v1)")
      diff_result
    })

    results$diff <- m5$result
    results$steps$diff <- list(time_sec = m5$time_sec)

    .ol_eval_write_step(output_file, "W3-5-diff", m5$time_sec, env_info)

    # ========================================================================
    # Step 6: Show complete lineage
    # ========================================================================
    if (verbose) message("  Step 6: Extracting lineage...")

    m6 <- ol_eval_measure({
      deps <- lake$deps("summary", direction = "up")
      tree <- lake$tree("summary", direction = "up", depth = 10)
      list(deps = deps, tree = tree)
    })

    results$lineage <- m6$result
    results$steps$lineage <- list(
      time_sec = m6$time_sec,
      deps_rows = nrow(m6$result$deps),
      tree_rows = nrow(m6$result$tree)
    )

    .ol_eval_write_step(output_file, "W3-6-lineage", m6$time_sec, env_info)

    # ========================================================================
    # Validation
    # ========================================================================
    if (verbose) message("  Validating results...")

    # Check lineage completeness
    deps <- results$lineage$deps
    expected_parents <- c("counts_norm", "gene_info")

    lineage_check <- ol_eval_check_lineage(deps, expected_parents, check_version_info = TRUE)
    results$validation$lineage <- lineage_check

    # Check version info in deps
    if (!is.null(deps) && nrow(deps) > 0) {
      results$validation$has_parent_ref <- "parent_ref" %in% names(deps)
      results$validation$has_parent_version_id <- "parent_version_id" %in% names(deps)

      # Show the actual lineage records
      if (verbose) {
        message("\n  Lineage for 'summary':")
        print(deps[, intersect(names(deps), c("parent_name", "parent_ref", "parent_version_id"))])
      }
    }

    # Check diff result
    if (!is.null(results$diff)) {
      results$validation$diff_valid <- inherits(results$diff, "lake_diff")
      results$validation$diff_refs <- list(
        ref1 = results$diff$ref1,
        ref2 = results$diff$ref2
      )
    }

    if (verbose) {
      message("\n  Validation Summary:")
      message("    Lineage valid: ", results$validation$lineage$valid)
      message("    Has parent_ref: ", results$validation$has_parent_ref %||% FALSE)
      message("    Has parent_version_id: ", results$validation$has_parent_version_id %||% FALSE)
      message("    Diff valid: ", results$validation$diff_valid %||% FALSE)
    }

  }, finally = {
    # Cleanup
    .ol_eval_cleanup_project(config$project_root, project_name)
  })

  if (verbose) message("\nCase study complete.")

  results
}

#' Write a case study step result to JSONL
#' @keywords internal
.ol_eval_write_step <- function(output_file, step_id, time_sec, env_info) {
  record <- ol_eval_result(
    workload = step_id,
    variant = "omicslake",
    size = "case_study",
    cache = "na",
    rep = 1,
    metrics = list(time_sec = time_sec),
    env = env_info
  )

  ol_eval_write_jsonl(record, output_file)
}

#' Generate case study report
#'
#' @param results Results from ol_eval_run_case_study()
#' @param output_file Output markdown file path
#' @return Invisible path to output file
#' @export
ol_eval_case_study_report <- function(results, output_file = NULL) {
  if (is.null(output_file)) {
    output_file <- file.path("inst/eval/results", "case_study_report.md")
  }

  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

  lines <- c(
    "# OmicsLake Case Study Report",
    "",
    paste("Generated:", Sys.time()),
    "",
    "## Overview",
    "",
    "This case study demonstrates OmicsLake's reproducibility features through",
    "a minimal RNA-seq workflow with version control and lineage tracking.",
    "",
    "## Steps",
    ""
  )

  # Add step timings
  for (step_name in names(results$steps)) {
    step <- results$steps[[step_name]]
    lines <- c(lines,
      paste0("### ", step_name),
      paste0("- Time: ", round(step$time_sec, 4), " seconds")
    )

    if (!is.null(step$n_genes)) {
      lines <- c(lines, paste0("- Genes: ", step$n_genes))
    }
    if (!is.null(step$n_groups)) {
      lines <- c(lines, paste0("- Groups: ", step$n_groups))
    }

    lines <- c(lines, "")
  }

  # Add lineage
  lines <- c(lines,
    "## Lineage",
    "",
    "### Dependencies for 'summary'",
    ""
  )

  if (!is.null(results$lineage$deps) && nrow(results$lineage$deps) > 0) {
    deps <- results$lineage$deps
    lines <- c(lines, "| Parent | Ref | Version ID |", "|--------|-----|------------|")
    for (i in seq_len(nrow(deps))) {
      lines <- c(lines, paste0("| ", deps$parent_name[i], " | ",
                               deps$parent_ref[i] %||% "NA", " | ",
                               deps$parent_version_id[i] %||% "NA", " |"))
    }
    lines <- c(lines, "")
  } else {
    lines <- c(lines, "*No dependencies found*", "")
  }

  # Add diff
  lines <- c(lines,
    "## Version Comparison",
    ""
  )

  if (!is.null(results$diff)) {
    lines <- c(lines,
      paste0("Comparing: ", results$diff$ref1, " vs ", results$diff$ref2),
      paste0("- Rows in ref1: ", results$diff$ref1_rows),
      paste0("- Rows in ref2: ", results$diff$ref2_rows),
      paste0("- Row difference: ", results$diff$row_diff %||% "N/A"),
      ""
    )
  }

  # Add validation
  lines <- c(lines,
    "## Validation",
    "",
    paste0("- Lineage valid: ", results$validation$lineage$valid),
    paste0("- Has parent_ref: ", results$validation$has_parent_ref %||% FALSE),
    paste0("- Has parent_version_id: ", results$validation$has_parent_version_id %||% FALSE),
    paste0("- Diff valid: ", results$validation$diff_valid %||% FALSE),
    ""
  )

  if (length(results$validation$lineage$issues) > 0) {
    lines <- c(lines,
      "### Issues",
      paste0("- ", results$validation$lineage$issues),
      ""
    )
  }

  writeLines(lines, output_file)
  message("Report written to: ", output_file)

  invisible(output_file)
}

#' Null coalescing (internal)
#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x
