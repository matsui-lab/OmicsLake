#' Repair workflow for the default lake
#'
#' Runs a five-step workflow:
#' 1) situation summary, 2) cause identification, 3) fix proposals,
#' 4) optional auto-execution, and 5) before/after comparison.
#'
#' @param target Optional data name to focus lineage diagnostics
#' @param restore_label Optional snapshot label used for rollback proposals
#' @param auto If TRUE, execute auto-supported proposals
#' @param enable_strict If TRUE, enable strict reproducibility mode during auto execution
#' @param strict_path Path used when enabling strict reproducibility mode
#' @param renv_restore If TRUE, run renv::restore() during auto execution
#' @param numeric_tolerance Numeric tolerance used for semantic value comparison
#' @param ignore_row_order If TRUE, compare tabular targets ignoring row order
#' @param verbose If TRUE, print the five-step report
#' @return A \code{lake_repair_report} object
#' @export
lake_repair <- function(target = NULL,
                        restore_label = NULL,
                        auto = FALSE,
                        enable_strict = FALSE,
                        strict_path = getOption("ol.repro.path", getwd()),
                        renv_restore = FALSE,
                        numeric_tolerance = 1e-8,
                        ignore_row_order = TRUE,
                        verbose = TRUE) {
  lake()$repair(
    target = target,
    restore_label = restore_label,
    auto = auto,
    enable_strict = enable_strict,
    strict_path = strict_path,
    renv_restore = renv_restore,
    numeric_tolerance = numeric_tolerance,
    ignore_row_order = ignore_row_order,
    verbose = verbose
  )
}

.ol_run_lake_repair <- function(lake,
                                project,
                                target = NULL,
                                restore_label = NULL,
                                auto = FALSE,
                                enable_strict = FALSE,
                                strict_path = getOption("ol.repro.path", getwd()),
                                renv_restore = FALSE,
                                numeric_tolerance = 1e-8,
                                ignore_row_order = TRUE,
                                verbose = TRUE) {
  if (!is.null(target)) {
    .ol_validate_name(target, "target")
  }
  if (!is.null(restore_label)) {
    .ol_validate_name(restore_label, "restore_label")
  }
  .ol_repair_assert_flag(auto, "auto")
  .ol_repair_assert_flag(enable_strict, "enable_strict")
  .ol_repair_assert_flag(renv_restore, "renv_restore")
  .ol_repair_assert_flag(ignore_row_order, "ignore_row_order")
  .ol_repair_assert_flag(verbose, "verbose")
  if (!is.character(strict_path) || length(strict_path) != 1 || !nzchar(strict_path)) {
    stop("strict_path must be a non-empty character string", call. = FALSE)
  }
  if (!is.numeric(numeric_tolerance) || length(numeric_tolerance) != 1 || is.na(numeric_tolerance) || numeric_tolerance < 0) {
    stop("numeric_tolerance must be a single non-negative number", call. = FALSE)
  }

  status_before <- tryCatch(
    lake$status(),
    error = function(e) {
      data.frame(message = conditionMessage(e), stringsAsFactors = FALSE)
    }
  )
  doctor_before <- tryCatch(
    lake$doctor(verbose = FALSE),
    error = function(e) {
      data.frame(
        check = "doctor execution",
        ok = FALSE,
        detail = conditionMessage(e),
        fix = "Fix project setup and rerun lake$doctor().",
        stringsAsFactors = FALSE
      )
    }
  )
  doctor_fail_before <- .ol_repair_doctor_failures(doctor_before)
  fail_before_n <- nrow(doctor_fail_before)

  commits <- tryCatch(
    ol_log_commits(project = project, n = 20),
    error = function(e) data.frame()
  )
  validation <- .ol_repair_latest_validation(commits)
  restore_candidate <- .ol_repair_restore_candidate(
    restore_label = restore_label,
    validation = validation
  )

  target_before <- .ol_repair_target_edges(lake = lake, target = target)
  target_diff_before <- .ol_repair_target_diff(
    lake = lake,
    target = target,
    reference_label = restore_candidate,
    numeric_tolerance = as.numeric(numeric_tolerance),
    ignore_row_order = isTRUE(ignore_row_order)
  )

  situation <- data.frame(
    project = as.character(project),
    generated_at_utc = .ol_repair_now_utc(),
    target = if (is.null(target)) NA_character_ else as.character(target),
    latest_commit_id = .ol_repair_col_scalar(commits, "commit_id"),
    latest_commit_time = .ol_repair_col_scalar(commits, "created_at"),
    validation_previous_label = .ol_repair_list_chr(validation, "previous_label"),
    validation_structural_changes = .ol_repair_list_int(validation, "structural_changes"),
    validation_row_count_changes = .ol_repair_list_int(validation, "row_count_changes"),
    target_reference_label = if (is.null(restore_candidate)) NA_character_ else as.character(restore_candidate),
    target_value_drift = if (is.null(target) || is.na(target_diff_before$drift_detected)) NA else isTRUE(target_diff_before$drift_detected),
    target_compare_mode = .ol_repair_list_chr(target_diff_before, "compare_mode"),
    target_semantic_equivalent = .ol_repair_list_lgl(target_diff_before, "semantic_equivalent"),
    target_numeric_tolerance = as.numeric(numeric_tolerance),
    target_external_dependencies_n = .ol_repair_list_int(target_diff_before, "external_dependencies_n", default = 0L),
    target_external_dependency_drift = .ol_repair_list_lgl(target_diff_before, "external_dependency_drift"),
    doctor_failures = as.integer(fail_before_n),
    stringsAsFactors = FALSE
  )

  causes <- .ol_repair_build_causes(
    doctor_failures = doctor_fail_before,
    validation = validation,
    target = target,
    target_edges = target_before,
    target_diff = target_diff_before
  )

  proposals <- .ol_repair_build_proposals(
    doctor_failures = doctor_fail_before,
    validation = validation,
    project = project,
    restore_candidate = restore_candidate,
    auto = auto,
    enable_strict = enable_strict,
    renv_restore = renv_restore,
    strict_path = strict_path
  )

  execution <- .ol_repair_execute(
    lake = lake,
    project = project,
    proposals = proposals,
    auto = auto,
    restore_candidate = restore_candidate,
    strict_path = strict_path
  )

  status_after <- tryCatch(
    lake$status(),
    error = function(e) {
      data.frame(message = conditionMessage(e), stringsAsFactors = FALSE)
    }
  )
  doctor_after <- tryCatch(
    lake$doctor(verbose = FALSE),
    error = function(e) {
      data.frame(
        check = "doctor execution",
        ok = FALSE,
        detail = conditionMessage(e),
        fix = "Fix project setup and rerun lake$doctor().",
        stringsAsFactors = FALSE
      )
    }
  )
  fail_after_n <- nrow(.ol_repair_doctor_failures(doctor_after))
  target_after <- .ol_repair_target_edges(lake = lake, target = target)

  comparison <- .ol_repair_build_comparison(
    status_before = status_before,
    status_after = status_after,
    fail_before_n = fail_before_n,
    fail_after_n = fail_after_n,
    target = target,
    target_before = target_before,
    target_after = target_after,
    target_diff_before = target_diff_before
  )

  report <- list(
    project = as.character(project),
    generated_at_utc = .ol_repair_now_utc(),
    target = if (is.null(target)) NA_character_ else as.character(target),
    situation = situation,
    causes = causes,
    proposals = proposals,
    execution = execution,
    comparison = comparison,
    before = list(status = status_before, doctor = doctor_before),
    after = list(status = status_after, doctor = doctor_after)
  )
  class(report) <- c("lake_repair_report", "list")

  if (isTRUE(verbose)) {
    print(report)
    return(invisible(report))
  }
  report
}

.ol_repair_assert_flag <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1 || is.na(x)) {
    stop(arg, " must be TRUE or FALSE", call. = FALSE)
  }
}

.ol_repair_now_utc <- function() {
  format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%d %H:%M:%S UTC")
}

.ol_repair_col_scalar <- function(df, col, default = NA_character_) {
  if (!is.data.frame(df) || nrow(df) == 0 || !col %in% names(df)) {
    return(default)
  }
  val <- df[[col]][[1]]
  if (is.null(val) || (length(val) == 1 && is.na(val))) {
    return(default)
  }
  as.character(val)
}

.ol_repair_list_chr <- function(x, key, default = NA_character_) {
  if (is.null(x) || is.null(x[[key]])) {
    return(default)
  }
  val <- x[[key]]
  if (length(val) < 1 || is.na(val[[1]])) {
    return(default)
  }
  as.character(val[[1]])
}

.ol_repair_list_int <- function(x, key, default = NA_integer_) {
  if (is.null(x) || is.null(x[[key]])) {
    return(default)
  }
  val <- as.integer(x[[key]][[1]])
  if (length(val) < 1 || is.na(val)) {
    return(default)
  }
  val
}

.ol_repair_list_lgl <- function(x, key, default = NA) {
  if (is.null(x) || is.null(x[[key]])) {
    return(default)
  }
  val <- as.logical(x[[key]][[1]])
  if (length(val) < 1 || is.na(val)) {
    return(default)
  }
  isTRUE(val)
}

.ol_repair_doctor_failures <- function(doctor_df) {
  if (!is.data.frame(doctor_df) || !"ok" %in% names(doctor_df)) {
    return(data.frame(
      check = character(0),
      ok = logical(0),
      detail = character(0),
      fix = character(0),
      stringsAsFactors = FALSE
    ))
  }
  idx <- which(!as.logical(doctor_df$ok))
  if (!length(idx)) {
    return(doctor_df[0, , drop = FALSE])
  }
  doctor_df[idx, , drop = FALSE]
}

.ol_repair_latest_validation <- function(commits) {
  req <- c(
    "snapshot_validation_prev",
    "snapshot_validation_structural_changes",
    "snapshot_validation_row_count_changes"
  )
  if (!is.data.frame(commits) || nrow(commits) == 0 || !all(req %in% names(commits))) {
    return(NULL)
  }
  idx <- which(
    !is.na(commits$snapshot_validation_structural_changes) |
      !is.na(commits$snapshot_validation_row_count_changes)
  )
  if (!length(idx)) {
    return(NULL)
  }
  i <- idx[[1]]
  list(
    previous_label = .ol_repair_col_scalar(commits[i, , drop = FALSE], "snapshot_validation_prev"),
    structural_changes = as.integer(commits$snapshot_validation_structural_changes[[i]]),
    row_count_changes = as.integer(commits$snapshot_validation_row_count_changes[[i]]),
    commit_id = .ol_repair_col_scalar(commits[i, , drop = FALSE], "commit_id"),
    created_at = .ol_repair_col_scalar(commits[i, , drop = FALSE], "created_at")
  )
}

.ol_repair_restore_candidate <- function(restore_label, validation) {
  if (!is.null(restore_label)) {
    return(as.character(restore_label))
  }
  prev <- .ol_repair_list_chr(validation, "previous_label")
  if (is.na(prev) || !nzchar(prev)) {
    return(NULL)
  }
  prev
}

.ol_repair_target_edges <- function(lake, target) {
  if (is.null(target)) {
    return(list(exists = NA, upstream = NA_integer_, downstream = NA_integer_))
  }
  exists_target <- tryCatch(lake$exists(target), error = function(e) FALSE)
  if (!isTRUE(exists_target)) {
    return(list(exists = FALSE, upstream = NA_integer_, downstream = NA_integer_))
  }
  upstream <- tryCatch(nrow(lake$deps(target, direction = "up")), error = function(e) NA_integer_)
  downstream <- tryCatch(nrow(lake$deps(target, direction = "down")), error = function(e) NA_integer_)
  list(exists = TRUE, upstream = as.integer(upstream), downstream = as.integer(downstream))
}

.ol_repair_object_hash <- function(x) {
  tf <- tempfile(pattern = "omicslake_repair_", fileext = ".rds")
  on.exit(unlink(tf), add = TRUE)
  saveRDS(x, tf)
  as.character(unname(tools::md5sum(tf))[1])
}

.ol_repair_scalar_order_key <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }
  if (inherits(x, "POSIXt")) {
    x <- format(as.POSIXct(x, tz = "UTC"), "%Y-%m-%d %H:%M:%OS6 UTC")
  }
  if (inherits(x, "Date")) {
    x <- format(as.Date(x), "%Y-%m-%d")
  }
  if (is.numeric(x)) {
    y <- ifelse(is.na(x), "NA", sprintf("%.16g", as.numeric(x)))
    return(y)
  }
  if (is.logical(x)) {
    y <- ifelse(is.na(x), "NA", ifelse(x, "TRUE", "FALSE"))
    return(y)
  }
  y <- as.character(x)
  y[is.na(y)] <- "NA"
  y
}

.ol_repair_normalize_df <- function(df, ignore_row_order = TRUE, ignore_col_order = FALSE) {
  if (!is.data.frame(df)) {
    return(df)
  }
  out <- df
  if (isTRUE(ignore_col_order) && ncol(out) > 1) {
    out <- out[, sort(names(out)), drop = FALSE]
  }
  if (!isTRUE(ignore_row_order) || nrow(out) <= 1 || ncol(out) < 1) {
    rownames(out) <- NULL
    return(out)
  }
  sort_cols <- vapply(out, .ol_repair_scalar_order_key, character(nrow(out)))
  if (!is.matrix(sort_cols)) {
    sort_cols <- matrix(sort_cols, ncol = 1)
  }
  ord <- do.call(order, as.data.frame(sort_cols, stringsAsFactors = FALSE))
  out <- out[ord, , drop = FALSE]
  rownames(out) <- NULL
  out
}

.ol_repair_table_semantic_compare <- function(latest,
                                              reference,
                                              numeric_tolerance = 1e-8,
                                              ignore_row_order = TRUE) {
  out <- list(
    comparable = FALSE,
    equal = NA,
    max_abs_numeric_diff = NA_real_,
    mode = "hash",
    reason = ""
  )
  if (!is.data.frame(latest) || !is.data.frame(reference)) {
    out$reason <- "not_both_data_frames"
    return(out)
  }
  if (!setequal(names(latest), names(reference))) {
    out$reason <- "column_names_differ"
    return(out)
  }
  if (!identical(nrow(latest), nrow(reference))) {
    out$comparable <- TRUE
    out$equal <- FALSE
    out$mode <- "semantic_table"
    out$reason <- "row_count_differs"
    return(out)
  }

  a <- .ol_repair_normalize_df(latest, ignore_row_order = ignore_row_order, ignore_col_order = TRUE)
  b <- .ol_repair_normalize_df(reference, ignore_row_order = ignore_row_order, ignore_col_order = TRUE)
  cols <- names(a)
  numeric_diffs <- numeric(0)
  eq <- TRUE
  reason <- ""

  for (col in cols) {
    av <- a[[col]]
    bv <- b[[col]]

    if (inherits(av, "POSIXt")) {
      av <- as.POSIXct(av, tz = "UTC")
    }
    if (inherits(bv, "POSIXt")) {
      bv <- as.POSIXct(bv, tz = "UTC")
    }
    if (inherits(av, "Date")) {
      av <- as.Date(av)
    }
    if (inherits(bv, "Date")) {
      bv <- as.Date(bv)
    }

    if (is.factor(av)) {
      av <- as.character(av)
    }
    if (is.factor(bv)) {
      bv <- as.character(bv)
    }

    if (is.numeric(av) && is.numeric(bv)) {
      if (length(av) != length(bv)) {
        eq <- FALSE
        reason <- paste0("numeric_length_mismatch:", col)
        break
      }
      na_mismatch <- xor(is.na(av), is.na(bv))
      if (any(na_mismatch)) {
        eq <- FALSE
        reason <- paste0("na_pattern_mismatch:", col)
        break
      }
      idx <- !(is.na(av) | is.na(bv))
      if (any(idx)) {
        diff <- abs(as.numeric(av[idx]) - as.numeric(bv[idx]))
        numeric_diffs <- c(numeric_diffs, diff)
        if (any(diff > numeric_tolerance)) {
          eq <- FALSE
          reason <- paste0("numeric_tolerance_exceeded:", col)
          break
        }
      }
      next
    }

    if (inherits(av, "POSIXt") || inherits(bv, "POSIXt")) {
      av_chr <- format(as.POSIXct(av, tz = "UTC"), "%Y-%m-%d %H:%M:%OS6 UTC")
      bv_chr <- format(as.POSIXct(bv, tz = "UTC"), "%Y-%m-%d %H:%M:%OS6 UTC")
      if (!isTRUE(all.equal(av_chr, bv_chr, check.attributes = FALSE))) {
        eq <- FALSE
        reason <- paste0("timestamp_mismatch:", col)
        break
      }
      next
    }

    av_chr <- as.character(av)
    bv_chr <- as.character(bv)
    av_chr[is.na(av_chr)] <- "<NA>"
    bv_chr[is.na(bv_chr)] <- "<NA>"
    if (!identical(av_chr, bv_chr)) {
      eq <- FALSE
      reason <- paste0("value_mismatch:", col)
      break
    }
  }

  out$comparable <- TRUE
  out$equal <- isTRUE(eq)
  out$mode <- "semantic_table"
  out$reason <- reason
  if (length(numeric_diffs) > 0) {
    out$max_abs_numeric_diff <- max(numeric_diffs)
  }
  out
}

.ol_repair_collect_external_nodes <- function(lake, target) {
  if (is.null(target) || !nzchar(target)) {
    return(character(0))
  }
  nodes <- character(0)
  deps_up <- tryCatch(lake$deps(target, direction = "up"), error = function(e) data.frame())
  if (is.data.frame(deps_up) && "parent_name" %in% names(deps_up) && nrow(deps_up) > 0) {
    nodes <- c(nodes, as.character(deps_up$parent_name))
  }
  tree_up <- tryCatch(lake$tree(target, direction = "up", depth = 20), error = function(e) data.frame())
  if (is.data.frame(tree_up) && nrow(tree_up) > 0) {
    if ("parent" %in% names(tree_up)) {
      nodes <- c(nodes, as.character(tree_up$parent))
    }
    if ("parent_name" %in% names(tree_up)) {
      nodes <- c(nodes, as.character(tree_up$parent_name))
    }
  }
  nodes <- unique(nodes[nzchar(nodes)])
  nodes[grepl("^(file:|url:|api:|external:)", nodes, ignore.case = TRUE)]
}

.ol_repair_marker_object_name <- function(node_name) {
  paste0(".__marker__.", node_name)
}

.ol_repair_external_fingerprint <- function(meta) {
  if (!is.list(meta)) {
    return(list(value = meta))
  }
  keep <- c(
    "source_kind",
    "source_id",
    "source_exists",
    "size_bytes",
    "mtime_utc",
    "content_md5",
    "hash_skipped",
    "etag",
    "last_modified",
    "status_code",
    "response_md5"
  )
  out <- meta[intersect(keep, names(meta))]
  if (length(out) > 0) {
    return(out)
  }
  fallback <- list(
    class = .ol_repair_or(meta$class, NA_character_),
    type = .ol_repair_or(meta$type, NA_character_),
    dims = .ol_repair_or(meta$dims, NULL),
    length = .ol_repair_or(meta$length, NA_integer_)
  )
  fallback
}

.ol_repair_or <- function(x, y) {
  if (is.null(x) || length(x) < 1) {
    return(y)
  }
  x
}

.ol_repair_external_dependency_diff <- function(lake, target, reference_label = NULL) {
  details <- data.frame(
    node = character(0),
    marker_object = character(0),
    comparable = logical(0),
    drift_detected = logical(0),
    latest_fingerprint_hash = character(0),
    reference_fingerprint_hash = character(0),
    reason = character(0),
    stringsAsFactors = FALSE
  )
  out <- list(
    nodes = character(0),
    external_dependencies_n = 0L,
    external_dependency_drift = NA,
    details = details,
    summary = ""
  )
  if (is.null(target) || is.null(reference_label) || !nzchar(reference_label)) {
    return(out)
  }
  nodes <- .ol_repair_collect_external_nodes(lake, target)
  out$nodes <- nodes
  out$external_dependencies_n <- as.integer(length(nodes))
  if (!length(nodes)) {
    out$external_dependency_drift <- FALSE
    return(out)
  }

  add_row <- function(details, node, marker_object, comparable, drift_detected, latest_hash, ref_hash, reason) {
    details[nrow(details) + 1L, ] <- list(
      as.character(node),
      as.character(marker_object),
      isTRUE(comparable),
      isTRUE(drift_detected),
      as.character(latest_hash),
      as.character(ref_hash),
      as.character(reason)
    )
    details
  }

  for (node in nodes) {
    marker_object <- .ol_repair_marker_object_name(node)
    marker_exists <- tryCatch(lake$exists(marker_object, type = "object"), error = function(e) FALSE)
    if (!isTRUE(marker_exists)) {
      details <- add_row(details, node, marker_object, FALSE, FALSE, NA_character_, NA_character_, "marker_object_not_found")
      next
    }
    latest_meta <- tryCatch(lake$get(marker_object), error = function(e) e)
    if (inherits(latest_meta, "error")) {
      details <- add_row(details, node, marker_object, FALSE, FALSE, NA_character_, NA_character_, paste0("latest_marker_read_failed: ", conditionMessage(latest_meta)))
      next
    }
    ref_meta <- tryCatch(lake$get(marker_object, ref = paste0("@", reference_label)), error = function(e) e)
    if (inherits(ref_meta, "error")) {
      details <- add_row(details, node, marker_object, FALSE, FALSE, NA_character_, NA_character_, paste0("reference_marker_read_failed: ", conditionMessage(ref_meta)))
      next
    }

    latest_fp <- .ol_repair_external_fingerprint(latest_meta)
    ref_fp <- .ol_repair_external_fingerprint(ref_meta)
    latest_hash <- tryCatch(.ol_repair_object_hash(latest_fp), error = function(e) NA_character_)
    ref_hash <- tryCatch(.ol_repair_object_hash(ref_fp), error = function(e) NA_character_)

    if (!is.na(latest_hash) && !is.na(ref_hash)) {
      drift <- !identical(latest_hash, ref_hash)
      details <- add_row(
        details,
        node,
        marker_object,
        TRUE,
        drift,
        latest_hash,
        ref_hash,
        if (isTRUE(drift)) "fingerprint_changed" else "fingerprint_unchanged"
      )
    } else {
      details <- add_row(details, node, marker_object, FALSE, FALSE, latest_hash, ref_hash, "fingerprint_hash_failed")
    }
  }

  out$details <- details
  if (nrow(details) > 0) {
    if (any(details$drift_detected)) {
      out$external_dependency_drift <- TRUE
    } else if (any(!details$comparable)) {
      out$external_dependency_drift <- NA
    } else {
      out$external_dependency_drift <- FALSE
    }
    changed <- details$node[details$drift_detected]
    if (length(changed) > 0) {
      out$summary <- paste("changed external nodes:", paste(changed, collapse = ", "))
    } else if (any(!details$comparable)) {
      out$summary <- "external dependencies include non-comparable markers"
    } else {
      out$summary <- "no external dependency fingerprint changes"
    }
  } else {
    out$external_dependency_drift <- FALSE
  }
  out
}

.ol_repair_external_diff_summary <- function(details, max_items = 3L) {
  if (!is.data.frame(details) || nrow(details) == 0) {
    return("")
  }
  bad <- details[details$drift_detected | !details$comparable, , drop = FALSE]
  if (nrow(bad) == 0) {
    return("external dependencies unchanged")
  }
  bad <- utils::head(bad, as.integer(max_items))
  parts <- vapply(seq_len(nrow(bad)), function(i) {
    row <- bad[i, , drop = FALSE]
    paste0(
      row$node[[1]],
      " (",
      row$reason[[1]],
      if (isTRUE(row$comparable[[1]])) {
        paste0(", ", substr(as.character(row$latest_fingerprint_hash[[1]]), 1, 8), " -> ", substr(as.character(row$reference_fingerprint_hash[[1]]), 1, 8))
      } else {
        ""
      },
      ")"
    )
  }, character(1))
  paste(parts, collapse = "; ")
}

.ol_repair_target_diff <- function(lake,
                                   target,
                                   reference_label = NULL,
                                   numeric_tolerance = 1e-8,
                                   ignore_row_order = TRUE) {
  out <- list(
    comparable = FALSE,
    drift_detected = NA,
    reference_label = if (is.null(reference_label)) NA_character_ else as.character(reference_label),
    latest_hash = NA_character_,
    reference_hash = NA_character_,
    compare_mode = NA_character_,
    semantic_equivalent = NA,
    max_abs_numeric_diff = NA_real_,
    external_dependencies_n = 0L,
    external_dependency_drift = NA,
    external_dependency_details = data.frame(),
    message = ""
  )
  if (is.null(target) || is.null(reference_label) || !nzchar(reference_label)) {
    return(out)
  }

  exists_target <- tryCatch(lake$exists(target), error = function(e) FALSE)
  if (!isTRUE(exists_target)) {
    out$message <- "target not found"
    return(out)
  }

  latest_obj <- tryCatch(lake$get(target), error = function(e) e)
  if (inherits(latest_obj, "error")) {
    out$message <- paste0("latest read failed: ", conditionMessage(latest_obj))
    return(out)
  }
  ref_obj <- tryCatch(lake$get(target, ref = paste0("@", reference_label)), error = function(e) e)
  if (inherits(ref_obj, "error")) {
    out$message <- paste0("reference read failed: ", conditionMessage(ref_obj))
    return(out)
  }

  latest_hash <- tryCatch(.ol_repair_object_hash(latest_obj), error = function(e) NA_character_)
  reference_hash <- tryCatch(.ol_repair_object_hash(ref_obj), error = function(e) NA_character_)
  out$latest_hash <- latest_hash
  out$reference_hash <- reference_hash

  semantic <- .ol_repair_table_semantic_compare(
    latest = latest_obj,
    reference = ref_obj,
    numeric_tolerance = numeric_tolerance,
    ignore_row_order = ignore_row_order
  )
  if (isTRUE(semantic$comparable)) {
    out$comparable <- TRUE
    out$compare_mode <- as.character(semantic$mode)
    out$semantic_equivalent <- isTRUE(semantic$equal)
    out$max_abs_numeric_diff <- semantic$max_abs_numeric_diff
    out$drift_detected <- !isTRUE(semantic$equal)
    if (isTRUE(semantic$equal) && !identical(latest_hash, reference_hash)) {
      out$message <- "hash differs but semantic comparison found equivalence within tolerance"
    } else if (!isTRUE(semantic$equal) && nzchar(semantic$reason)) {
      out$message <- paste0("semantic comparison mismatch: ", semantic$reason)
    }
  } else if (!is.na(latest_hash) && !is.na(reference_hash)) {
    out$comparable <- TRUE
    out$compare_mode <- "hash"
    out$drift_detected <- !identical(latest_hash, reference_hash)
    if (isTRUE(out$drift_detected)) {
      out$message <- "hash mismatch"
    }
  } else {
    out$message <- "hash computation failed"
  }

  ext <- .ol_repair_external_dependency_diff(
    lake = lake,
    target = target,
    reference_label = reference_label
  )
  out$external_dependencies_n <- as.integer(ext$external_dependencies_n)
  out$external_dependency_drift <- ext$external_dependency_drift
  out$external_dependency_details <- ext$details
  if (nzchar(ext$summary)) {
    if (nzchar(out$message)) {
      out$message <- paste(out$message, ext$summary, sep = "; ")
    } else {
      out$message <- ext$summary
    }
  }
  out
}

.ol_repair_empty_causes <- function() {
  data.frame(
    source = character(0),
    item = character(0),
    diagnosis = character(0),
    evidence = character(0),
    stringsAsFactors = FALSE
  )
}

.ol_repair_empty_proposals <- function() {
  data.frame(
    action_id = character(0),
    summary = character(0),
    command = character(0),
    auto_supported = logical(0),
    selected = logical(0),
    rationale = character(0),
    stringsAsFactors = FALSE
  )
}

.ol_repair_empty_execution <- function() {
  data.frame(
    action_id = character(0),
    status = character(0),
    message = character(0),
    stringsAsFactors = FALSE
  )
}

.ol_repair_build_causes <- function(doctor_failures, validation, target, target_edges, target_diff = NULL) {
  out <- .ol_repair_empty_causes()
  add_cause <- function(out, source, item, diagnosis, evidence = "") {
    out[nrow(out) + 1L, ] <- list(
      as.character(source),
      as.character(item),
      as.character(diagnosis),
      as.character(evidence)
    )
    out
  }

  if (is.data.frame(doctor_failures) && nrow(doctor_failures) > 0) {
    for (i in seq_len(nrow(doctor_failures))) {
      out <- add_cause(
        out,
        source = "doctor",
        item = .ol_repair_col_scalar(doctor_failures[i, , drop = FALSE], "check", default = "unknown_check"),
        diagnosis = .ol_repair_col_scalar(doctor_failures[i, , drop = FALSE], "detail", default = "check failed"),
        evidence = .ol_repair_col_scalar(doctor_failures[i, , drop = FALSE], "fix", default = "")
      )
    }
  }

  structural <- .ol_repair_list_int(validation, "structural_changes", default = 0L)
  row_n <- .ol_repair_list_int(validation, "row_count_changes", default = 0L)
  prev_label <- .ol_repair_list_chr(validation, "previous_label", default = NA_character_)
  if (isTRUE(structural > 0L) || isTRUE(row_n > 0L)) {
    out <- add_cause(
      out,
      source = "snapshot_validation",
      item = if (is.na(prev_label)) "previous_label_unknown" else prev_label,
      diagnosis = "Snapshot drift detected against previous labeled state.",
      evidence = paste0("structural_changes=", structural, ", row_count_changes=", row_n)
    )
  }

  if (!is.null(target)) {
    if (!isTRUE(target_edges$exists)) {
      out <- add_cause(
        out,
        source = "target",
        item = as.character(target),
        diagnosis = "Target node was not found in the lake.",
        evidence = "Use lake_find() to locate the correct name."
      )
    } else {
      out <- add_cause(
        out,
        source = "lineage",
        item = as.character(target),
        diagnosis = "Dependency footprint was collected for the target node.",
        evidence = paste0(
          "upstream_edges=", .ol_repair_as_char(target_edges$upstream),
          ", downstream_edges=", .ol_repair_as_char(target_edges$downstream)
        )
      )
    }
    if (is.list(target_diff) && isTRUE(target_diff$comparable) && isTRUE(target_diff$drift_detected)) {
      out <- add_cause(
        out,
        source = "target_diff",
        item = as.character(target),
        diagnosis = "Target differs from the selected reference label.",
        evidence = paste0(
          "reference_label=", .ol_repair_as_char(target_diff$reference_label),
          ", compare_mode=", .ol_repair_as_char(target_diff$compare_mode),
          ", latest_hash=", .ol_repair_as_char(target_diff$latest_hash),
          ", reference_hash=", .ol_repair_as_char(target_diff$reference_hash),
          ", max_abs_numeric_diff=", .ol_repair_as_char(target_diff$max_abs_numeric_diff)
        )
      )
    } else if (is.list(target_diff) && !isTRUE(target_diff$comparable) && nzchar(.ol_repair_as_char(target_diff$message))) {
      out <- add_cause(
        out,
        source = "target_diff",
        item = as.character(target),
        diagnosis = "Could not compare target with the selected reference label.",
        evidence = .ol_repair_as_char(target_diff$message)
      )
    }
    if (is.list(target_diff) && isTRUE(target_diff$external_dependency_drift)) {
      out <- add_cause(
        out,
        source = "external_dependency",
        item = as.character(target),
        diagnosis = "External dependency fingerprints changed from the selected reference label.",
        evidence = .ol_repair_external_diff_summary(target_diff$external_dependency_details)
      )
    } else if (is.list(target_diff) &&
               is.data.frame(target_diff$external_dependency_details) &&
               nrow(target_diff$external_dependency_details) > 0 &&
               any(!target_diff$external_dependency_details$comparable)) {
      out <- add_cause(
        out,
        source = "external_dependency",
        item = as.character(target),
        diagnosis = "Some external dependencies could not be compared against the selected reference label.",
        evidence = .ol_repair_external_diff_summary(target_diff$external_dependency_details)
      )
    }
  }

  if (nrow(out) == 0) {
    out <- add_cause(
      out,
      source = "summary",
      item = "no_obvious_issue",
      diagnosis = "No failing checks were found.",
      evidence = "Current status appears healthy."
    )
  }
  out
}

.ol_repair_build_proposals <- function(doctor_failures,
                                       validation,
                                       project,
                                       restore_candidate,
                                       auto,
                                       enable_strict,
                                       renv_restore,
                                       strict_path) {
  out <- .ol_repair_empty_proposals()
  add_proposal <- function(out, action_id, summary, command, auto_supported, selected, rationale) {
    if (action_id %in% out$action_id) {
      return(out)
    }
    out[nrow(out) + 1L, ] <- list(
      as.character(action_id),
      as.character(summary),
      as.character(command),
      isTRUE(auto_supported),
      isTRUE(selected),
      as.character(rationale)
    )
    out
  }

  if (is.data.frame(doctor_failures) && nrow(doctor_failures) > 0) {
    for (i in seq_len(nrow(doctor_failures))) {
      check_name <- .ol_repair_col_scalar(doctor_failures[i, , drop = FALSE], "check", default = "")
      detail <- .ol_repair_col_scalar(doctor_failures[i, , drop = FALSE], "detail", default = "")
      fix <- .ol_repair_col_scalar(doctor_failures[i, , drop = FALSE], "fix", default = "")

      if (identical(check_name, "default shortcuts point to this project")) {
        out <- add_proposal(
          out,
          action_id = "bind_shortcuts",
          summary = "Bind default shortcuts to the current lake project.",
          command = paste0("use_lake('", project, "')"),
          auto_supported = TRUE,
          selected = isTRUE(auto),
          rationale = detail
        )
      } else if (identical(check_name, "automatic reproducibility metadata capture")) {
        out <- add_proposal(
          out,
          action_id = "enable_repro_capture",
          summary = "Enable automatic reproducibility metadata capture.",
          command = "options(ol.repro.capture = TRUE)",
          auto_supported = TRUE,
          selected = isTRUE(auto),
          rationale = detail
        )
      } else if (identical(check_name, "reproducibility context path exists")) {
        out <- add_proposal(
          out,
          action_id = "set_repro_path",
          summary = "Set a valid reproducibility path.",
          command = "options(ol.repro.path = '<analysis_repo_path>')",
          auto_supported = FALSE,
          selected = FALSE,
          rationale = fix
        )
      } else if (identical(check_name, "git working tree is clean")) {
        out <- add_proposal(
          out,
          action_id = "clean_git_state",
          summary = "Commit or stash local Git changes before final snapshots.",
          command = "git status && git add -A && git commit -m 'snapshot prep'  # or git stash",
          auto_supported = FALSE,
          selected = FALSE,
          rationale = detail
        )
      } else if (identical(check_name, "renv lockfile detected") || identical(check_name, "renv lockfile parseable")) {
        out <- add_proposal(
          out,
          action_id = "refresh_renv_lockfile",
          summary = "Regenerate renv.lock to stabilize package environments.",
          command = "renv::snapshot()",
          auto_supported = FALSE,
          selected = FALSE,
          rationale = fix
        )
      } else {
        out <- add_proposal(
          out,
          action_id = paste0("manual_", i),
          summary = paste0("Resolve failing check: ", check_name),
          command = if (nzchar(fix)) fix else "Inspect lake_doctor() details and fix manually.",
          auto_supported = FALSE,
          selected = FALSE,
          rationale = detail
        )
      }
    }
  }

  structural <- .ol_repair_list_int(validation, "structural_changes", default = 0L)
  row_n <- .ol_repair_list_int(validation, "row_count_changes", default = 0L)
  if (!is.null(restore_candidate)) {
    out <- add_proposal(
      out,
      action_id = "restore_snapshot",
      summary = paste0("Rollback to snapshot label '", restore_candidate, "'."),
      command = paste0("restore('", restore_candidate, "')"),
      auto_supported = TRUE,
      selected = isTRUE(auto),
      rationale = "Fastest recovery path when drift is detected."
    )
  }

  out <- add_proposal(
    out,
    action_id = "enable_strict_mode",
    summary = "Enable strict reproducibility guardrails for future runs.",
    command = paste0("ol_enable_strict_repro_mode(path = '", strict_path, "')"),
    auto_supported = TRUE,
    selected = isTRUE(auto) && isTRUE(enable_strict),
    rationale = "Prevents drift by enforcing capture + validation defaults."
  )

  if (isTRUE(renv_restore)) {
    out <- add_proposal(
      out,
      action_id = "renv_restore",
      summary = "Restore R package environment from renv.lock.",
      command = paste0("renv::restore(project = '", strict_path, "', prompt = FALSE)"),
      auto_supported = TRUE,
      selected = isTRUE(auto),
      rationale = "Align package versions with lockfile."
    )
  }

  out
}

.ol_repair_label_exists <- function(lake, label) {
  if (is.null(label) || !nzchar(label)) {
    return(FALSE)
  }
  labels <- tryCatch(lake$snaps(), error = function(e) data.frame())
  is.data.frame(labels) &&
    "tag" %in% names(labels) &&
    any(as.character(labels$tag) == as.character(label), na.rm = TRUE)
}

.ol_repair_execute <- function(lake, project, proposals, auto, restore_candidate, strict_path) {
  out <- .ol_repair_empty_execution()
  add_exec <- function(out, action_id, status, message) {
    out[nrow(out) + 1L, ] <- list(
      as.character(action_id),
      as.character(status),
      as.character(message)
    )
    out
  }

  if (!isTRUE(auto)) {
    out <- add_exec(out, "auto_execution", "skipped", "auto = FALSE; generated diagnosis and proposals only.")
    return(out)
  }

  if (!is.data.frame(proposals) || nrow(proposals) == 0) {
    out <- add_exec(out, "auto_execution", "skipped", "No proposals available.")
    return(out)
  }

  idx <- which(as.logical(proposals$auto_supported) & as.logical(proposals$selected))
  selected <- proposals$action_id[idx]
  selected <- as.character(selected)
  selected <- selected[nzchar(selected)]
  if (!length(selected)) {
    out <- add_exec(out, "auto_execution", "skipped", "No auto-supported proposals were selected.")
    return(out)
  }

  for (action_id in selected) {
    res <- tryCatch(
      {
        if (identical(action_id, "bind_shortcuts")) {
          use_lake(project)
          "default shortcuts are now bound to this project"
        } else if (identical(action_id, "enable_repro_capture")) {
          options(ol.repro.capture = TRUE)
          "set options(ol.repro.capture = TRUE)"
        } else if (identical(action_id, "restore_snapshot")) {
          if (is.null(restore_candidate)) {
            stop("No restore label available for rollback.", call. = FALSE)
          }
          if (!isTRUE(.ol_repair_label_exists(lake, restore_candidate))) {
            stop("Restore label not found: ", restore_candidate, call. = FALSE)
          }
          lake$restore(restore_candidate)
          paste0("restored to label '", restore_candidate, "'")
        } else if (identical(action_id, "enable_strict_mode")) {
          ol_enable_strict_repro_mode(path = strict_path)
          "strict reproducibility mode enabled"
        } else if (identical(action_id, "renv_restore")) {
          if (!requireNamespace("renv", quietly = TRUE)) {
            stop("renv package is not installed.", call. = FALSE)
          }
          lockfile <- file.path(strict_path, "renv.lock")
          if (!file.exists(lockfile)) {
            stop("renv.lock was not found under strict_path: ", strict_path, call. = FALSE)
          }
          renv::restore(project = strict_path, prompt = FALSE)
          paste0("restored renv environment from ", lockfile)
        } else {
          stop("Unsupported auto action: ", action_id, call. = FALSE)
        }
      },
      error = function(e) e
    )

    if (inherits(res, "error")) {
      out <- add_exec(out, action_id, "failed", conditionMessage(res))
    } else {
      out <- add_exec(out, action_id, "ok", as.character(res))
    }
  }

  out
}

.ol_repair_build_comparison <- function(status_before,
                                        status_after,
                                        fail_before_n,
                                        fail_after_n,
                                        target,
                                        target_before,
                                        target_after,
                                        target_diff_before = NULL) {
  rows <- list(
    list(metric = "doctor_failures", before = fail_before_n, after = fail_after_n),
    list(metric = "tables", before = .ol_repair_status_value(status_before, "tables"), after = .ol_repair_status_value(status_after, "tables")),
    list(metric = "objects", before = .ol_repair_status_value(status_before, "objects"), after = .ol_repair_status_value(status_after, "objects")),
    list(metric = "default_shortcuts", before = .ol_repair_status_value(status_before, "default_shortcuts"), after = .ol_repair_status_value(status_after, "default_shortcuts")),
    list(metric = "git_dirty", before = .ol_repair_status_value(status_before, "git_dirty"), after = .ol_repair_status_value(status_after, "git_dirty"))
  )

  if (!is.null(target)) {
    rows[[length(rows) + 1L]] <- list(
      metric = paste0("target_exists:", target),
      before = target_before$exists,
      after = target_after$exists
    )
    rows[[length(rows) + 1L]] <- list(
      metric = paste0("target_upstream_edges:", target),
      before = target_before$upstream,
      after = target_after$upstream
    )
    rows[[length(rows) + 1L]] <- list(
      metric = paste0("target_downstream_edges:", target),
      before = target_before$downstream,
      after = target_after$downstream
    )
    rows[[length(rows) + 1L]] <- list(
      metric = paste0("target_value_drift:", target),
      before = if (is.list(target_diff_before)) target_diff_before$drift_detected else NA,
      after = NA
    )
    rows[[length(rows) + 1L]] <- list(
      metric = paste0("target_external_dependency_drift:", target),
      before = if (is.list(target_diff_before)) target_diff_before$external_dependency_drift else NA,
      after = NA
    )
  }

  out <- do.call(rbind, lapply(rows, function(x) {
    before_chr <- .ol_repair_as_char(x$before)
    after_chr <- .ol_repair_as_char(x$after)
    changed <- !((is.na(before_chr) && is.na(after_chr)) || identical(before_chr, after_chr))
    data.frame(
      metric = as.character(x$metric),
      before = before_chr,
      after = after_chr,
      changed = isTRUE(changed),
      stringsAsFactors = FALSE
    )
  }))
  rownames(out) <- NULL
  out
}

.ol_repair_status_value <- function(status_df, col, default = NA) {
  if (!is.data.frame(status_df) || nrow(status_df) == 0 || !col %in% names(status_df)) {
    return(default)
  }
  status_df[[col]][[1]]
}

.ol_repair_as_char <- function(x) {
  if (length(x) < 1 || is.null(x) || is.na(x[[1]])) {
    return(NA_character_)
  }
  as.character(x[[1]])
}

#' Print method for lake_repair_report
#'
#' @param x A lake_repair_report object
#' @param ... Additional arguments (ignored)
#' @return The input object, invisibly
#' @export
print.lake_repair_report <- function(x, ...) {
  writeLines(c(
    "Lake Repair Report",
    paste0("Project: ", .ol_repair_as_char(x$project)),
    paste0("Generated: ", .ol_repair_as_char(x$generated_at_utc)),
    ""
  ))
  writeLines("1) Situation")
  if (is.data.frame(x$situation) && nrow(x$situation) > 0) {
    print(x$situation, row.names = FALSE)
  } else {
    writeLines("  (no situation data)")
  }

  writeLines(c("", "2) Cause Identification"))
  if (is.data.frame(x$causes) && nrow(x$causes) > 0) {
    print(x$causes, row.names = FALSE)
  } else {
    writeLines("  (no causes)")
  }

  writeLines(c("", "3) Fix Proposals"))
  if (is.data.frame(x$proposals) && nrow(x$proposals) > 0) {
    print(x$proposals, row.names = FALSE)
  } else {
    writeLines("  (no proposals)")
  }

  writeLines(c("", "4) Auto Execution"))
  if (is.data.frame(x$execution) && nrow(x$execution) > 0) {
    print(x$execution, row.names = FALSE)
  } else {
    writeLines("  (no execution records)")
  }

  writeLines(c("", "5) Before/After Comparison"))
  if (is.data.frame(x$comparison) && nrow(x$comparison) > 0) {
    print(x$comparison, row.names = FALSE)
  } else {
    writeLines("  (no comparison data)")
  }

  invisible(x)
}
