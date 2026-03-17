#!/usr/bin/env Rscript
# ==============================================================================
# OmicsLake Reproducibility Breakage Taxonomy Validation
# ==============================================================================
# Goal:
#   1) classify common reproducibility breakages,
#   2) reproduce each breakage in a controlled workflow,
#   3) show conventional-process bottlenecks,
#   4) verify OmicsLake detection/visualization and safe resolution behavior.
# ==============================================================================

suppressPackageStartupMessages({
  library(jsonlite)
})

cat("=============================================================\n")
cat(" OmicsLake Reproducibility Breakage Taxonomy Validation\n")
cat("=============================================================\n\n")

paper_dir <- if (basename(getwd()) == "paper") {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
} else if (file.exists("inst/paper")) {
  normalizePath("inst/paper", winslash = "/", mustWork = TRUE)
} else {
  stop("Cannot find paper directory. Run from package root or inst/paper.")
}
setwd(paper_dir)

repo_root <- normalizePath(file.path(paper_dir, "..", ".."), winslash = "/", mustWork = TRUE)
if (file.exists(file.path(repo_root, "DESCRIPTION")) && requireNamespace("pkgload", quietly = TRUE)) {
  suppressPackageStartupMessages(pkgload::load_all(repo_root, quiet = TRUE))
} else {
  suppressPackageStartupMessages(library(OmicsLake))
}

if (!exists("lake_repair", where = asNamespace("OmicsLake"), mode = "function")) {
  stop(
    "lake_repair() is required for this validation. ",
    "Install/load an OmicsLake build that includes repair workflow support.",
    call. = FALSE
  )
}

taxonomy_path <- "reproducibility_breakage_taxonomy.json"
if (!file.exists(taxonomy_path)) {
  stop("Taxonomy file not found: ", taxonomy_path, call. = FALSE)
}

taxonomy <- jsonlite::fromJSON(taxonomy_path, simplifyVector = TRUE)
scenarios <- as.data.frame(taxonomy$scenarios, stringsAsFactors = FALSE)

obj_md5 <- function(x) {
  tf <- tempfile(pattern = "omicslake_breakage_", fileext = ".rds")
  on.exit(unlink(tf), add = TRUE)
  saveRDS(x, tf)
  as.character(unname(tools::md5sum(tf))[1])
}

run_git <- function(args) {
  if (Sys.which("git") == "") {
    return(list(status = 127L, output = "git not available"))
  }
  out <- suppressWarnings(system2("git", args, stdout = TRUE, stderr = TRUE))
  st <- attr(out, "status")
  if (is.null(st)) {
    st <- 0L
  }
  list(status = as.integer(st), output = out)
}

ensure_git_clean <- function(repo_path) {
  if (!is.character(repo_path) || length(repo_path) != 1 || !nzchar(repo_path)) {
    return(FALSE)
  }
  if (Sys.which("git") == "" || !dir.exists(repo_path)) {
    return(FALSE)
  }
  st <- run_git(c("-C", repo_path, "status", "--porcelain"))
  if (st$status != 0L) {
    return(FALSE)
  }
  if (length(st$output) == 0) {
    return(TRUE)
  }
  run_git(c("-C", repo_path, "add", "-A"))
  run_git(c("-C", repo_path, "commit", "-m", "autoclean for reproducibility test"))
  st2 <- run_git(c("-C", repo_path, "status", "--porcelain"))
  st2$status == 0L && length(st2$output) == 0
}

create_repro_context <- function(base_dir, mode = c("git_renv", "git_only", "none")) {
  mode <- match.arg(mode)
  ctx <- file.path(base_dir, paste0("repro_ctx_", mode))
  dir.create(ctx, recursive = TRUE, showWarnings = FALSE)

  if (identical(mode, "git_renv") || identical(mode, "none")) {
    writeLines(
      c(
        "{",
        '  "R": {"Version": "4.5.0"},',
        '  "Packages": {"dplyr": {"Version": "1.1.4"}}',
        "}"
      ),
      con = file.path(ctx, "renv.lock")
    )
  }

  git_ok <- FALSE
  if (!identical(mode, "none") && Sys.which("git") != "") {
    init_res <- run_git(c("init", ctx))
    if (init_res$status == 0L) {
      run_git(c("-C", ctx, "config", "user.email", "omicslake-test@example.com"))
      run_git(c("-C", ctx, "config", "user.name", "OmicsLake Test"))
      files <- list.files(ctx, all.files = FALSE, no.. = TRUE)
      if (length(files) > 0) {
        add_res <- run_git(c("-C", ctx, "add", files))
        if (add_res$status == 0L) {
          commit_res <- run_git(c("-C", ctx, "commit", "-m", "init repro context"))
          git_ok <- commit_res$status == 0L
        }
      } else {
        git_ok <- TRUE
      }
    }
  }

  list(path = ctx, git_ok = git_ok)
}

build_base_pipeline <- function(project_id) {
  use_lake(project_id)
  lk <- lake()

  counts <- data.frame(
    gene_id = paste0("gene_", sprintf("%03d", seq_len(10))),
    count = c(100, 110, 95, 160, 170, 140, 180, 90, 130, 150),
    stringsAsFactors = FALSE
  )
  metadata <- data.frame(
    gene_id = counts$gene_id,
    condition = rep(c("ctrl", "treat"), each = 5),
    stringsAsFactors = FALSE
  )
  analysis_params <- data.frame(threshold = 0, stringsAsFactors = FALSE)

  lk$put("counts", counts)
  lk$put("metadata", metadata)
  lk$put("analysis_params", analysis_params)

  normalized <- data.frame(
    gene_id = counts$gene_id,
    norm_count = log2(as.numeric(counts$count) + 1),
    stringsAsFactors = FALSE
  )
  lk$put("normalized", normalized, depends_on = "counts")

  score <- normalized$norm_count - stats::median(normalized$norm_count)
  de_results <- data.frame(
    gene_id = normalized$gene_id,
    score = round(score, 6),
    stringsAsFactors = FALSE
  )
  de_results <- de_results[de_results$score >= as.numeric(analysis_params$threshold[[1]]), , drop = FALSE]
  lk$put("de_results", de_results, depends_on = c("normalized", "analysis_params", "metadata"))
  lk$snap("v1")

  list(
    target = "de_results",
    baseline_hash = obj_md5(lk$get("de_results")),
    baseline_counts_hash = obj_md5(lk$get("counts"))
  )
}

recompute_outputs <- function(lk, threshold_override = NULL, schema_mode = c("default", "character")) {
  schema_mode <- match.arg(schema_mode)
  counts <- lk$get("counts")
  params <- lk$get("analysis_params")

  threshold <- if (is.null(threshold_override)) as.numeric(params$threshold[[1]]) else threshold_override
  raw_count <- counts$count
  if (schema_mode == "character") {
    parsed <- suppressWarnings(as.numeric(raw_count))
    parsed[is.na(parsed)] <- nchar(as.character(raw_count[is.na(parsed)]))
    raw_count <- parsed
  }

  normalized <- data.frame(
    gene_id = counts$gene_id,
    norm_count = log2(as.numeric(raw_count) + 1),
    stringsAsFactors = FALSE
  )
  lk$put("normalized", normalized, depends_on = "counts")

  score <- normalized$norm_count - stats::median(normalized$norm_count)
  de_results <- data.frame(
    gene_id = normalized$gene_id,
    score = round(score, 6),
    stringsAsFactors = FALSE
  )
  de_results <- de_results[de_results$score >= threshold, , drop = FALSE]
  lk$put("de_results", de_results, depends_on = c("normalized", "analysis_params", "metadata"))
}

scenario_inject <- function(id, lk, base_dir, clean_ctx) {
  if (id == "BX-001") {
    counts <- lk$get("counts")
    counts$count <- counts$count + seq_len(nrow(counts)) * 20
    lk$put("counts", counts)
    recompute_outputs(lk)
    return(list(note = "Input values replaced and downstream objects recomputed."))
  }

  if (id == "BX-002") {
    counts <- lk$get("counts")
    counts$count <- letters[seq_len(nrow(counts))]
    lk$put("counts", counts)
    recompute_outputs(lk, schema_mode = "character")
    return(list(note = "Schema/type drift injected into counts$count."))
  }

  if (id == "BX-003") {
    params <- lk$get("analysis_params")
    params$threshold <- 0.25
    lk$put("analysis_params", params)
    recompute_outputs(lk, threshold_override = as.numeric(params$threshold[[1]]))
    return(list(note = "Analysis parameter threshold changed and outputs recomputed."))
  }

  if (id == "BX-004") {
    lk$drop("de_results", force = TRUE)
    lk$put("de_results", data.frame(gene_id = character(0), score = numeric(0), stringsAsFactors = FALSE))
    return(list(note = "Critical artifact was removed/overwritten."))
  }

  if (id == "BX-005") {
    no_git_ctx <- create_repro_context(base_dir, mode = "none")
    options(ol.repro.path = no_git_ctx$path)
    return(list(note = "Repro context moved to non-git directory.", repro_ctx = no_git_ctx$path))
  }

  if (id == "BX-006") {
    git_only_ctx <- create_repro_context(base_dir, mode = "git_only")
    options(ol.repro.path = git_only_ctx$path)
    return(list(note = "Repro context moved to git-only directory without renv.lock.", repro_ctx = git_only_ctx$path))
  }

  stop("Unknown scenario id: ", id, call. = FALSE)
}

collapse_execution <- function(execution_df) {
  if (!is.data.frame(execution_df) || nrow(execution_df) == 0) {
    return("")
  }
  paste(
    paste0(execution_df$action_id, ":", execution_df$status),
    collapse = "; "
  )
}

safe_to_char <- function(x) {
  if (is.null(x) || length(x) < 1 || is.na(x[[1]])) {
    return(NA_character_)
  }
  as.character(x[[1]])
}

mean_or_na <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }
  mean(x, na.rm = TRUE)
}

median_or_na <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }
  stats::median(x, na.rm = TRUE)
}

scenario_details <- list()
result_rows <- vector("list", nrow(scenarios))

for (i in seq_len(nrow(scenarios))) {
  sc <- scenarios[i, , drop = FALSE]
  cat(sprintf("[%d/%d] %s - %s\n", i, nrow(scenarios), sc$id, sc$title))

  root_dir <- tempfile(pattern = paste0("bx_", tolower(sc$id), "_"))
  dir.create(root_dir, recursive = TRUE)
  project_id <- paste0("breakage_", tolower(gsub("-", "_", sc$id)))

  opt_names <- c(
    "ol.root",
    "ol.repro.path",
    "ol.repro.capture",
    "ol.repro.strict",
    "ol.repro.require_clean_git",
    "ol.snapshot.auto_validate",
    "ol.snapshot.validate.mode"
  )
  old_opts <- stats::setNames(lapply(opt_names, getOption), opt_names)

  if (exists("default", envir = OmicsLake:::.lake_env, inherits = FALSE)) {
    rm("default", envir = OmicsLake:::.lake_env)
  }

  clean_ctx <- create_repro_context(root_dir, mode = "git_renv")

  do.call(options, list(
    ol.root = root_dir,
    ol.repro.path = clean_ctx$path,
    ol.repro.capture = TRUE,
    ol.repro.strict = FALSE,
    ol.repro.require_clean_git = FALSE,
    ol.snapshot.auto_validate = TRUE,
    ol.snapshot.validate.mode = "off"
  ))

  base <- build_base_pipeline(project_id)
  lk <- lake()

  injection <- scenario_inject(sc$id, lk = lk, base_dir = root_dir, clean_ctx = clean_ctx)

  ensure_git_clean(getOption("ol.repro.path"))

  restore_label <- if (identical(sc$expected_resolution_mode, "auto_rollback")) "v1" else NULL

  pre_obj <- tryCatch(lk$get(base$target), error = function(e) NULL)
  pre_hash <- if (is.null(pre_obj)) NA_character_ else obj_md5(pre_obj)
  was_broken <- !is.na(pre_hash) && !identical(pre_hash, base$baseline_hash)

  t_diag_start <- unname(proc.time()[["elapsed"]])
  rep_diag <- lk$repair(
    target = base$target,
    restore_label = restore_label,
    auto = FALSE,
    verbose = FALSE
  )
  t_diag_sec <- unname(proc.time()[["elapsed"]]) - t_diag_start

  t_auto_start <- unname(proc.time()[["elapsed"]])
  rep_auto <- lk$repair(
    target = base$target,
    restore_label = restore_label,
    auto = TRUE,
    enable_strict = TRUE,
    verbose = FALSE
  )
  t_auto_sec <- unname(proc.time()[["elapsed"]]) - t_auto_start

  post_obj <- tryCatch(lk$get(base$target), error = function(e) NULL)
  post_hash <- if (is.null(post_obj)) NA_character_ else obj_md5(post_obj)

  recovered_to_baseline <- if (was_broken) identical(post_hash, base$baseline_hash) else NA

  detected <- FALSE
  if (is.data.frame(rep_diag$causes) && nrow(rep_diag$causes) > 0) {
    detected <- any(rep_diag$causes$source != "summary")
  }
  if (isTRUE(rep_diag$situation$doctor_failures[[1]] > 0)) {
    detected <- TRUE
  }
  if ("target_value_drift" %in% names(rep_diag$situation) &&
      !is.na(rep_diag$situation$target_value_drift[[1]]) &&
      isTRUE(rep_diag$situation$target_value_drift[[1]])) {
    detected <- TRUE
  }

  auto_failures_n <- if (is.data.frame(rep_auto$execution)) {
    sum(rep_auto$execution$status == "failed")
  } else {
    NA_integer_
  }

  proposals_df <- if (is.data.frame(rep_diag$proposals)) rep_diag$proposals else data.frame()
  execution_df <- if (is.data.frame(rep_auto$execution)) rep_auto$execution else data.frame()

  manual_actions_proposed <- if (nrow(proposals_df) > 0) {
    any(!proposals_df$auto_supported)
  } else {
    FALSE
  }
  manual_actions_n <- if (nrow(proposals_df) > 0) {
    sum(!proposals_df$auto_supported)
  } else {
    0L
  }
  manual_actions_actionable <- if (nrow(proposals_df) > 0) {
    any(!proposals_df$auto_supported & nzchar(as.character(proposals_df$command)))
  } else {
    FALSE
  }
  strict_guardrail_enabled <- if (nrow(execution_df) > 0) {
    any(execution_df$action_id == "enable_strict_mode" & execution_df$status == "ok")
  } else {
    FALSE
  }
  restore_action_ok <- if (nrow(execution_df) > 0) {
    any(execution_df$action_id == "restore_snapshot" & execution_df$status == "ok")
  } else {
    FALSE
  }

  baseline_detection_signal <- was_broken
  baseline_issue <- if (baseline_detection_signal) {
    "Output drift is visible, but root-cause tracing is manual across scripts/files."
  } else {
    "Issue is often unnoticed until audit/review because no automatic provenance check exists."
  }

  resolution_outcome <- if (isTRUE(was_broken) && identical(recovered_to_baseline, TRUE)) {
    "auto_restored"
  } else if (isTRUE(detected)) {
    "guided_manual_or_guardrail"
  } else {
    "not_resolved"
  }

  result_rows[[i]] <- data.frame(
    scenario_id = sc$id,
    category = sc$category,
    title = sc$title,
    expected_resolution_mode = sc$expected_resolution_mode,
    root_cause = sc$root_cause,
    conventional_bottleneck = sc$conventional_bottleneck,
    injection_note = safe_to_char(injection$note),
    baseline_detection_signal = baseline_detection_signal,
    baseline_issue = baseline_issue,
    omicslake_detected = isTRUE(detected),
    omicslake_visualized = is.list(rep_diag) &&
      all(c("situation", "causes", "proposals", "execution", "comparison") %in% names(rep_diag)),
    doctor_failures_pre = as.integer(rep_diag$situation$doctor_failures[[1]]),
    target_value_drift = if ("target_value_drift" %in% names(rep_diag$situation)) {
      as.logical(rep_diag$situation$target_value_drift[[1]])
    } else {
      NA
    },
    was_broken = was_broken,
    recovered_to_baseline = if (is.na(recovered_to_baseline)) NA else isTRUE(recovered_to_baseline),
    auto_failures_n = as.integer(auto_failures_n),
    safe_execution = !is.na(auto_failures_n) && auto_failures_n == 0L,
    diag_runtime_sec = as.numeric(t_diag_sec),
    auto_runtime_sec = as.numeric(t_auto_sec),
    manual_actions_proposed = manual_actions_proposed,
    manual_actions_n = as.integer(manual_actions_n),
    manual_actions_actionable = isTRUE(manual_actions_actionable),
    strict_guardrail_enabled = isTRUE(strict_guardrail_enabled),
    restore_action_ok = isTRUE(restore_action_ok),
    resolution_outcome = resolution_outcome,
    baseline_hash = base$baseline_hash,
    pre_hash = pre_hash,
    post_hash = post_hash,
    auto_execution = collapse_execution(rep_auto$execution),
    stringsAsFactors = FALSE
  )

  scenario_details[[sc$id]] <- list(
    taxonomy = sc,
    diag = rep_diag,
    auto = rep_auto,
    result = result_rows[[i]]
  )

  do.call(options, old_opts)
  if (exists("default", envir = OmicsLake:::.lake_env, inherits = FALSE)) {
    rm("default", envir = OmicsLake:::.lake_env)
  }
  unlink(root_dir, recursive = TRUE)
}

results_df <- do.call(rbind, result_rows)
rownames(results_df) <- NULL

auto_idx <- results_df$expected_resolution_mode == "auto_rollback"
guided_idx <- results_df$expected_resolution_mode == "guided_manual"

auto_df <- results_df[auto_idx, , drop = FALSE]
guided_df <- results_df[guided_idx, , drop = FALSE]

auto_broken_n <- sum(auto_df$was_broken, na.rm = TRUE)
auto_restored_n <- sum(auto_df$resolution_outcome == "auto_restored", na.rm = TRUE)

coverage_summary <- data.frame(
  metric = c(
    "scenario_count",
    "detected_count",
    "detected_rate",
    "visualized_rate",
    "safe_execution_rate",
    "median_diag_runtime_sec",
    "median_auto_runtime_sec",
    "auto_restored_count",
    "auto_restored_rate_among_broken",
    "auto_mode_scenario_count",
    "auto_mode_detected_rate",
    "auto_mode_restore_action_rate",
    "guided_mode_scenario_count",
    "guided_mode_detected_rate",
    "guided_mode_manual_proposal_rate",
    "guided_mode_actionable_proposal_rate",
    "guided_mode_guardrail_rate"
  ),
  value = c(
    nrow(results_df),
    sum(results_df$omicslake_detected, na.rm = TRUE),
    mean(results_df$omicslake_detected, na.rm = TRUE),
    mean(results_df$omicslake_visualized, na.rm = TRUE),
    mean(results_df$safe_execution, na.rm = TRUE),
    median_or_na(results_df$diag_runtime_sec),
    median_or_na(results_df$auto_runtime_sec),
    sum(results_df$resolution_outcome == "auto_restored", na.rm = TRUE),
    {
      if (auto_broken_n > 0) {
        auto_restored_n / auto_broken_n
      } else {
        NA_real_
      }
    },
    nrow(auto_df),
    mean_or_na(auto_df$omicslake_detected),
    mean_or_na(auto_df$restore_action_ok),
    nrow(guided_df),
    mean_or_na(guided_df$omicslake_detected),
    mean_or_na(guided_df$manual_actions_proposed),
    mean_or_na(guided_df$manual_actions_actionable),
    mean_or_na(guided_df$strict_guardrail_enabled)
  ),
  stringsAsFactors = FALSE
)

mode_levels <- unique(results_df$expected_resolution_mode)
mode_summary <- do.call(rbind, lapply(mode_levels, function(mode_name) {
  sub <- results_df[results_df$expected_resolution_mode == mode_name, , drop = FALSE]
  data.frame(
    mode = mode_name,
    scenario_count = nrow(sub),
    detected_rate = mean_or_na(sub$omicslake_detected),
    visualized_rate = mean_or_na(sub$omicslake_visualized),
    safe_execution_rate = mean_or_na(sub$safe_execution),
    median_diag_runtime_sec = median_or_na(sub$diag_runtime_sec),
    median_auto_runtime_sec = median_or_na(sub$auto_runtime_sec),
    restored_rate_among_broken = {
      broken <- sum(sub$was_broken, na.rm = TRUE)
      if (broken > 0) {
        sum(sub$resolution_outcome == "auto_restored", na.rm = TRUE) / broken
      } else {
        NA_real_
      }
    },
    manual_proposal_rate = mean_or_na(sub$manual_actions_proposed),
    actionable_proposal_rate = mean_or_na(sub$manual_actions_actionable),
    guardrail_rate = mean_or_na(sub$strict_guardrail_enabled),
    stringsAsFactors = FALSE
  )
}))

write.csv(results_df, "results_breakage_evaluation.csv", row.names = FALSE)
write.csv(coverage_summary, "results_breakage_coverage_summary.csv", row.names = FALSE)
write.csv(mode_summary, "results_breakage_mode_summary.csv", row.names = FALSE)

md_lines <- c(
  "# Reproducibility Breakage Taxonomy Validation",
  "",
  sprintf("Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  "",
  "This report classifies typical reproducibility breakages, reproduces each one, and evaluates OmicsLake's mechanical detection and safe resolution behavior.",
  "",
  "## Coverage Summary",
  ""
)
md_lines <- c(md_lines, "```")
md_lines <- c(md_lines, capture.output(print(coverage_summary, row.names = FALSE)))
md_lines <- c(md_lines, "```", "")
md_lines <- c(md_lines, "## Mode-specific Summary", "")
md_lines <- c(md_lines, "```")
md_lines <- c(md_lines, capture.output(print(mode_summary, row.names = FALSE)))
md_lines <- c(md_lines, "```", "")

for (id in names(scenario_details)) {
  d <- scenario_details[[id]]
  sc <- d$taxonomy
  rr <- d$result

  md_lines <- c(
    md_lines,
    paste0("## ", sc$id, ": ", sc$title),
    "",
    paste0("- Category: ", sc$category),
    paste0("- Root cause: ", sc$root_cause),
    paste0("- Conventional bottleneck: ", sc$conventional_bottleneck),
    paste0("- Injection: ", rr$injection_note),
    paste0("- OmicsLake detected: ", rr$omicslake_detected),
    paste0("- Resolution outcome: ", rr$resolution_outcome),
    paste0("- Auto execution safety (no failed actions): ", rr$safe_execution),
    ""
  )

  md_lines <- c(md_lines, "### 1) Situation", "", "```")
  md_lines <- c(md_lines, capture.output(print(d$diag$situation, row.names = FALSE)))
  md_lines <- c(md_lines, "```", "")

  md_lines <- c(md_lines, "### 2) Cause Identification", "", "```")
  md_lines <- c(md_lines, capture.output(print(d$diag$causes, row.names = FALSE)))
  md_lines <- c(md_lines, "```", "")

  md_lines <- c(md_lines, "### 3) Fix Proposals", "", "```")
  md_lines <- c(md_lines, capture.output(print(d$diag$proposals, row.names = FALSE)))
  md_lines <- c(md_lines, "```", "")

  md_lines <- c(md_lines, "### 4) Auto Execution", "", "```")
  md_lines <- c(md_lines, capture.output(print(d$auto$execution, row.names = FALSE)))
  md_lines <- c(md_lines, "```", "")

  md_lines <- c(md_lines, "### 5) Before/After Comparison", "", "```")
  md_lines <- c(md_lines, capture.output(print(d$auto$comparison, row.names = FALSE)))
  md_lines <- c(md_lines, "```", "")
}

writeLines(md_lines, con = "results_breakage_report.md")

cat("\nOutputs:\n")
cat("  - results_breakage_evaluation.csv\n")
cat("  - results_breakage_coverage_summary.csv\n")
cat("  - results_breakage_mode_summary.csv\n")
cat("  - results_breakage_report.md\n\n")
cat("Done.\n")
