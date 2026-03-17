#!/usr/bin/env Rscript
# ==============================================================================
# OmicsLake Reproducibility Limit/Boundary Stress Test
# ==============================================================================
# Goal:
#   1) evaluate known boundary conditions for fairness,
#   2) quantify false positives / false negatives,
#   3) measure restore behavior under missing preconditions,
#   4) expose attribution gaps for untracked external dependencies.
# ==============================================================================

suppressPackageStartupMessages({
  library(jsonlite)
})

cat("=============================================================\n")
cat(" OmicsLake Reproducibility Limit/Boundary Stress Test\n")
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

taxonomy_path <- "reproducibility_limit_taxonomy.json"
if (!file.exists(taxonomy_path)) {
  stop("Taxonomy file not found: ", taxonomy_path, call. = FALSE)
}

taxonomy <- jsonlite::fromJSON(taxonomy_path, simplifyVector = FALSE)
scenarios <- taxonomy$scenarios
if (!is.list(scenarios) || length(scenarios) < 1) {
  stop("No scenarios found in taxonomy file.", call. = FALSE)
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) < 1) y else x
}

as_chr1 <- function(x, default = NA_character_) {
  if (is.null(x) || length(x) < 1 || is.na(x[[1]]) || !nzchar(as.character(x[[1]]))) {
    return(default)
  }
  as.character(x[[1]])
}

as_lgl1 <- function(x, default = NA) {
  if (is.null(x) || length(x) < 1 || is.na(x[[1]])) {
    return(default)
  }
  isTRUE(as.logical(x[[1]]))
}

as_num1 <- function(x, default = NA_real_) {
  if (is.null(x) || length(x) < 1 || is.na(x[[1]])) {
    return(default)
  }
  suppressWarnings(as.numeric(x[[1]]))
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

obj_md5 <- function(x) {
  tf <- tempfile(pattern = "omicslake_limit_", fileext = ".rds")
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
  run_git(c("-C", repo_path, "commit", "-m", "autoclean for limit stress test"))
  st2 <- run_git(c("-C", repo_path, "status", "--porcelain"))
  st2$status == 0L && length(st2$output) == 0
}

create_repro_context <- function(base_dir) {
  ctx <- file.path(base_dir, "repro_ctx_git_renv")
  dir.create(ctx, recursive = TRUE, showWarnings = FALSE)
  writeLines(
    c(
      "{",
      '  "R": {"Version": "4.5.0"},',
      '  "Packages": {"dplyr": {"Version": "1.1.4"}}',
      "}"
    ),
    con = file.path(ctx, "renv.lock")
  )
  git_ok <- FALSE
  if (Sys.which("git") != "") {
    init_res <- run_git(c("init", ctx))
    if (init_res$status == 0L) {
      run_git(c("-C", ctx, "config", "user.email", "omicslake-test@example.com"))
      run_git(c("-C", ctx, "config", "user.name", "OmicsLake Test"))
      add_res <- run_git(c("-C", ctx, "add", "renv.lock"))
      if (add_res$status == 0L) {
        commit_res <- run_git(c("-C", ctx, "commit", "-m", "init repro context"))
        git_ok <- commit_res$status == 0L
      }
    }
  }
  list(path = ctx, git_ok = git_ok)
}

compute_outputs <- function(counts,
                            metadata,
                            analysis_params,
                            external_factor = 1,
                            jitter_sd = 0,
                            jitter_seed = NA_real_) {
  normalized <- data.frame(
    gene_id = counts$gene_id,
    norm_count = log2(as.numeric(counts$count) * external_factor + 1),
    stringsAsFactors = FALSE
  )
  score <- normalized$norm_count - stats::median(normalized$norm_count)
  if (!is.na(jitter_seed)) {
    set.seed(as.integer(jitter_seed))
  }
  if (!is.na(jitter_sd) && jitter_sd > 0) {
    score <- score + stats::rnorm(length(score), sd = jitter_sd)
  }
  de_results <- data.frame(
    gene_id = normalized$gene_id,
    score = round(score, 6),
    stringsAsFactors = FALSE
  )
  threshold <- as_num1(analysis_params$threshold[[1]], 0)
  de_results <- de_results[de_results$score >= threshold, , drop = FALSE]
  list(normalized = normalized, de_results = de_results)
}

read_external_factor <- function(path, default = 1) {
  if (!file.exists(path)) {
    return(default)
  }
  lines <- readLines(path, warn = FALSE)
  if (length(lines) < 1) {
    return(default)
  }
  out <- suppressWarnings(as.numeric(trimws(lines[[1]])))
  if (is.na(out)) default else out
}

build_base_pipeline <- function(project_id,
                                external_factor_file,
                                baseline_snapshot = TRUE) {
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

  ext_factor <- read_external_factor(external_factor_file, default = 1)
  out <- compute_outputs(
    counts = counts,
    metadata = metadata,
    analysis_params = analysis_params,
    external_factor = ext_factor,
    jitter_sd = 0
  )
  lk$put("normalized", out$normalized, depends_on = "counts")
  lk$put("de_results", out$de_results, depends_on = c("normalized", "analysis_params", "metadata"))

  if (isTRUE(baseline_snapshot)) {
    lk$snap("v1")
  }

  list(
    target = "de_results",
    baseline_object = lk$get("de_results"),
    baseline_hash = obj_md5(lk$get("de_results"))
  )
}

recompute_outputs <- function(lk,
                              external_factor = 1,
                              jitter_sd = 0,
                              jitter_seed = NA_real_) {
  counts <- lk$get("counts")
  metadata <- lk$get("metadata")
  params <- lk$get("analysis_params")
  out <- compute_outputs(
    counts = counts,
    metadata = metadata,
    analysis_params = params,
    external_factor = external_factor,
    jitter_sd = jitter_sd,
    jitter_seed = jitter_seed
  )
  lk$put("normalized", out$normalized, depends_on = "counts")
  lk$put("de_results", out$de_results, depends_on = c("normalized", "analysis_params", "metadata"))
}

semantic_equivalent <- function(scenario_id, x, y) {
  if (!is.data.frame(x) || !is.data.frame(y)) {
    return(FALSE)
  }
  if (!all(c("gene_id", "score") %in% names(x)) || !all(c("gene_id", "score") %in% names(y))) {
    return(FALSE)
  }
  x2 <- x[order(x$gene_id), c("gene_id", "score"), drop = FALSE]
  y2 <- y[order(y$gene_id), c("gene_id", "score"), drop = FALSE]
  rownames(x2) <- NULL
  rownames(y2) <- NULL

  if (identical(scenario_id, "LT-001")) {
    return(isTRUE(all.equal(x2, y2, check.attributes = FALSE)))
  }
  if (identical(scenario_id, "LT-002")) {
    if (nrow(x2) != nrow(y2) || !identical(x2$gene_id, y2$gene_id)) {
      return(FALSE)
    }
    max_abs <- max(abs(as.numeric(x2$score) - as.numeric(y2$score)))
    return(!is.na(max_abs) && max_abs <= 1e-8)
  }
  FALSE
}

scenario_inject <- function(scenario_id, lk, root_dir, external_factor_file) {
  if (identical(scenario_id, "LT-001")) {
    de <- lk$get("de_results")
    de <- de[rev(seq_len(nrow(de))), , drop = FALSE]
    lk$put("de_results", de, depends_on = c("normalized", "analysis_params", "metadata"))
    return(list(note = "Permuted row order only (same value set)."))
  }

  if (identical(scenario_id, "LT-002")) {
    de <- lk$get("de_results")
    de$score <- de$score + seq_len(nrow(de)) * 1e-12
    lk$put("de_results", de, depends_on = c("normalized", "analysis_params", "metadata"))
    return(list(note = "Injected sub-femtoscale numeric jitter."))
  }

  if (identical(scenario_id, "LT-003")) {
    writeLines("1.30", con = external_factor_file)
    ext_factor <- read_external_factor(external_factor_file, default = 1)
    recompute_outputs(lk, external_factor = ext_factor, jitter_sd = 0)
    return(list(
      note = "Changed external factor file and recomputed outputs (file is outside OmicsLake lineage).",
      external_factor = ext_factor,
      external_factor_file = external_factor_file
    ))
  }

  if (identical(scenario_id, "LT-004")) {
    counts <- lk$get("counts")
    counts$count <- counts$count + 25
    lk$put("counts", counts)
    recompute_outputs(lk, external_factor = 1, jitter_sd = 0)
    return(list(note = "Injected drift without having any baseline snapshot label."))
  }

  if (identical(scenario_id, "LT-005")) {
    recompute_outputs(lk, external_factor = 1, jitter_sd = 0.05, jitter_seed = NA_real_)
    return(list(note = "Recomputed with stochastic noise and no fixed seed."))
  }

  stop("Unknown scenario id: ", scenario_id, call. = FALSE)
}

collapse_execution <- function(execution_df) {
  if (!is.data.frame(execution_df) || nrow(execution_df) == 0) {
    return("")
  }
  paste(paste0(execution_df$action_id, ":", execution_df$status), collapse = "; ")
}

scenario_details <- list()
result_rows <- vector("list", length(scenarios))

for (i in seq_along(scenarios)) {
  sc <- scenarios[[i]]
  sc_id <- as_chr1(sc$id, default = paste0("SC-", i))
  sc_title <- as_chr1(sc$title, default = "")
  cat(sprintf("[%d/%d] %s - %s\n", i, length(scenarios), sc_id, sc_title))

  root_dir <- tempfile(pattern = paste0("lt_", tolower(sc_id), "_"))
  dir.create(root_dir, recursive = TRUE, showWarnings = FALSE)

  project_id <- paste0("limit_", tolower(gsub("-", "_", sc_id)))
  external_factor_file <- file.path(root_dir, "external_factor.txt")
  writeLines("1.00", con = external_factor_file)

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

  clean_ctx <- create_repro_context(root_dir)

  do.call(options, list(
    ol.root = root_dir,
    ol.repro.path = clean_ctx$path,
    ol.repro.capture = TRUE,
    ol.repro.strict = FALSE,
    ol.repro.require_clean_git = FALSE,
    ol.snapshot.auto_validate = TRUE,
    ol.snapshot.validate.mode = "off"
  ))

  baseline_snapshot <- as_lgl1(sc$baseline_snapshot, default = TRUE)
  base <- build_base_pipeline(
    project_id = project_id,
    external_factor_file = external_factor_file,
    baseline_snapshot = baseline_snapshot
  )
  lk <- lake()

  injection <- scenario_inject(
    scenario_id = sc_id,
    lk = lk,
    root_dir = root_dir,
    external_factor_file = external_factor_file
  )

  ensure_git_clean(getOption("ol.repro.path"))

  restore_label <- as_chr1(sc$restore_label, default = NA_character_)
  if (is.na(restore_label)) {
    restore_label <- NULL
  }

  pre_obj <- tryCatch(lk$get(base$target), error = function(e) NULL)
  pre_hash <- if (is.null(pre_obj)) NA_character_ else obj_md5(pre_obj)
  actual_restored_to_baseline <- NA

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
  if (!is.na(post_hash) && !is.na(base$baseline_hash)) {
    actual_restored_to_baseline <- identical(post_hash, base$baseline_hash)
  }

  detected <- FALSE
  if ("target_value_drift" %in% names(rep_diag$situation) &&
      !is.na(rep_diag$situation$target_value_drift[[1]]) &&
      isTRUE(rep_diag$situation$target_value_drift[[1]])) {
    detected <- TRUE
  }
  if (is.data.frame(rep_diag$causes) && nrow(rep_diag$causes) > 0) {
    target_sources <- c("target", "target_diff", "external_dependency", "snapshot_validation")
    detected <- detected || any(rep_diag$causes$source %in% target_sources)
  }

  execution_df <- if (is.data.frame(rep_auto$execution)) rep_auto$execution else data.frame()
  auto_failures_n <- if (nrow(execution_df) > 0) {
    sum(execution_df$status == "failed")
  } else {
    NA_integer_
  }
  restore_status <- NA_character_
  if (nrow(execution_df) > 0 && any(execution_df$action_id == "restore_snapshot")) {
    restore_status <- as_chr1(execution_df$status[execution_df$action_id == "restore_snapshot"][1], default = NA_character_)
  }

  ground_truth_breakage <- as_lgl1(sc$ground_truth_breakage, default = FALSE)
  expected_detection <- as_lgl1(sc$expected_detection, default = NA)
  expected_auto_restore <- as_lgl1(sc$expected_auto_restore, default = NA)
  known_limit <- as_lgl1(sc$known_limit, default = FALSE)
  limit_focus <- as_chr1(sc$limit_focus, default = "")

  sem_eq <- semantic_equivalent(sc_id, pre_obj, base$baseline_object)

  cause_text <- ""
  if (is.data.frame(rep_diag$causes) && nrow(rep_diag$causes) > 0) {
    cause_text <- paste(
      c(rep_diag$causes$item, rep_diag$causes$diagnosis, rep_diag$causes$evidence),
      collapse = " | "
    )
  }
  external_cause_identified <- is.data.frame(rep_diag$causes) &&
    nrow(rep_diag$causes) > 0 &&
    any(rep_diag$causes$source == "external_dependency")
  if (!isTRUE(external_cause_identified)) {
    external_marker <- as_chr1(injection$external_factor_file, default = basename(external_factor_file))
    ext_pattern <- paste0("external\\s+factor|", gsub("\\.", "\\\\.", basename(external_marker)))
    external_cause_identified <- grepl(ext_pattern, cause_text, ignore.case = TRUE)
  }

  false_positive <- !ground_truth_breakage && isTRUE(detected)
  false_negative <- ground_truth_breakage && !isTRUE(detected)

  auto_restore_success <- NA
  if (!is.na(expected_auto_restore) && isTRUE(expected_auto_restore)) {
    auto_restore_success <- isTRUE(actual_restored_to_baseline) && identical(restore_status, "ok")
  }

  limitation_exposed <- switch(
    limit_focus,
    false_positive_semantic_equivalence = false_positive,
    false_positive_numeric_tolerance = false_positive,
    cause_attribution_gap_external_dependency = isTRUE(detected) && !isTRUE(external_cause_identified),
    rollback_precondition_gap = !isTRUE(actual_restored_to_baseline),
    control_detectable_breakage = FALSE,
    NA
  )

  expected_detection_matched <- if (is.na(expected_detection)) NA else identical(isTRUE(detected), isTRUE(expected_detection))
  expected_auto_restore_matched <- if (is.na(expected_auto_restore)) {
    NA
  } else {
    identical(isTRUE(actual_restored_to_baseline), isTRUE(expected_auto_restore))
  }

  result_rows[[i]] <- data.frame(
    scenario_id = sc_id,
    category = as_chr1(sc$category, default = ""),
    title = sc_title,
    limit_focus = limit_focus,
    root_cause = as_chr1(sc$root_cause, default = ""),
    fairness_risk = as_chr1(sc$fairness_risk, default = ""),
    injection_note = as_chr1(injection$note, default = ""),
    ground_truth_breakage = ground_truth_breakage,
    semantic_equivalent = sem_eq,
    expected_detection = if (is.na(expected_detection)) NA else expected_detection,
    expected_auto_restore = if (is.na(expected_auto_restore)) NA else expected_auto_restore,
    expected_detection_matched = if (is.na(expected_detection_matched)) NA else expected_detection_matched,
    expected_auto_restore_matched = if (is.na(expected_auto_restore_matched)) NA else expected_auto_restore_matched,
    omicslake_detected = isTRUE(detected),
    false_positive = false_positive,
    false_negative = false_negative,
    known_limit = known_limit,
    limitation_exposed = if (is.na(limitation_exposed)) NA else isTRUE(limitation_exposed),
    external_cause_identified = isTRUE(external_cause_identified),
    doctor_failures_pre = as.integer(rep_diag$situation$doctor_failures[[1]]),
    target_value_drift = if ("target_value_drift" %in% names(rep_diag$situation)) {
      as_lgl1(rep_diag$situation$target_value_drift[[1]], default = NA)
    } else {
      NA
    },
    auto_failures_n = as.integer(auto_failures_n),
    safe_execution = !is.na(auto_failures_n) && auto_failures_n == 0L,
    restore_status = restore_status,
    restored_to_baseline = if (is.na(actual_restored_to_baseline)) NA else isTRUE(actual_restored_to_baseline),
    auto_restore_success = if (is.na(auto_restore_success)) NA else isTRUE(auto_restore_success),
    diag_runtime_sec = as.numeric(t_diag_sec),
    auto_runtime_sec = as.numeric(t_auto_sec),
    baseline_hash = base$baseline_hash,
    pre_hash = pre_hash,
    post_hash = post_hash,
    auto_execution = collapse_execution(rep_auto$execution),
    stringsAsFactors = FALSE
  )

  scenario_details[[sc_id]] <- list(
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

break_n <- sum(results_df$ground_truth_breakage, na.rm = TRUE)
non_break_n <- sum(!results_df$ground_truth_breakage, na.rm = TRUE)
expected_restore_n <- sum(results_df$expected_auto_restore %in% TRUE, na.rm = TRUE)
known_limit_n <- sum(results_df$known_limit, na.rm = TRUE)

coverage_summary <- data.frame(
  metric = c(
    "scenario_count",
    "ground_truth_breakage_count",
    "ground_truth_non_breakage_count",
    "detected_count",
    "true_positive_count",
    "true_negative_count",
    "false_positive_count",
    "false_negative_count",
    "false_positive_rate",
    "false_negative_rate",
    "auto_restore_expected_count",
    "auto_restore_success_count",
    "auto_restore_success_rate_expected",
    "known_limit_scenario_count",
    "known_limit_exposed_count",
    "known_limit_exposed_rate",
    "median_diag_runtime_sec",
    "median_auto_runtime_sec"
  ),
  value = c(
    nrow(results_df),
    break_n,
    non_break_n,
    sum(results_df$omicslake_detected, na.rm = TRUE),
    sum(results_df$ground_truth_breakage & results_df$omicslake_detected, na.rm = TRUE),
    sum(!results_df$ground_truth_breakage & !results_df$omicslake_detected, na.rm = TRUE),
    sum(results_df$false_positive, na.rm = TRUE),
    sum(results_df$false_negative, na.rm = TRUE),
    if (non_break_n > 0) sum(results_df$false_positive, na.rm = TRUE) / non_break_n else NA_real_,
    if (break_n > 0) sum(results_df$false_negative, na.rm = TRUE) / break_n else NA_real_,
    expected_restore_n,
    sum(results_df$auto_restore_success %in% TRUE, na.rm = TRUE),
    if (expected_restore_n > 0) sum(results_df$auto_restore_success %in% TRUE, na.rm = TRUE) / expected_restore_n else NA_real_,
    known_limit_n,
    sum(results_df$known_limit & results_df$limitation_exposed %in% TRUE, na.rm = TRUE),
    if (known_limit_n > 0) sum(results_df$known_limit & results_df$limitation_exposed %in% TRUE, na.rm = TRUE) / known_limit_n else NA_real_,
    median_or_na(results_df$diag_runtime_sec),
    median_or_na(results_df$auto_runtime_sec)
  ),
  stringsAsFactors = FALSE
)

confusion <- data.frame(
  truth = c("breakage", "breakage", "no_breakage", "no_breakage"),
  predicted = c("detected", "not_detected", "detected", "not_detected"),
  count = c(
    sum(results_df$ground_truth_breakage & results_df$omicslake_detected, na.rm = TRUE),
    sum(results_df$ground_truth_breakage & !results_df$omicslake_detected, na.rm = TRUE),
    sum(!results_df$ground_truth_breakage & results_df$omicslake_detected, na.rm = TRUE),
    sum(!results_df$ground_truth_breakage & !results_df$omicslake_detected, na.rm = TRUE)
  ),
  stringsAsFactors = FALSE
)

write.csv(results_df, "results_limit_evaluation.csv", row.names = FALSE)
write.csv(coverage_summary, "results_limit_coverage_summary.csv", row.names = FALSE)
write.csv(confusion, "results_limit_confusion_matrix.csv", row.names = FALSE)

md_lines <- c(
  "# Reproducibility Limit/Boundary Stress Test",
  "",
  sprintf("Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  "",
  "This report evaluates boundary conditions for fair assessment, including expected failures and known limitations.",
  "",
  "## Coverage Summary",
  "",
  "```",
  capture.output(print(coverage_summary, row.names = FALSE)),
  "```",
  "",
  "## Detection Confusion Matrix",
  "",
  "```",
  capture.output(print(confusion, row.names = FALSE)),
  "```",
  ""
)

for (id in names(scenario_details)) {
  d <- scenario_details[[id]]
  rr <- d$result

  md_lines <- c(
    md_lines,
    paste0("## ", rr$scenario_id, ": ", rr$title),
    "",
    paste0("- Category: ", rr$category),
    paste0("- Limit focus: ", rr$limit_focus),
    paste0("- Ground truth breakage: ", rr$ground_truth_breakage),
    paste0("- Semantic equivalent: ", rr$semantic_equivalent),
    paste0("- OmicsLake detected: ", rr$omicslake_detected),
    paste0("- False positive: ", rr$false_positive),
    paste0("- False negative: ", rr$false_negative),
    paste0("- Limitation exposed: ", rr$limitation_exposed),
    paste0("- Restore status: ", rr$restore_status),
    paste0("- Restored to baseline: ", rr$restored_to_baseline),
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

writeLines(md_lines, con = "results_limit_report.md")

cat("\nOutputs:\n")
cat("  - results_limit_evaluation.csv\n")
cat("  - results_limit_coverage_summary.csv\n")
cat("  - results_limit_confusion_matrix.csv\n")
cat("  - results_limit_report.md\n\n")
cat("Done.\n")
