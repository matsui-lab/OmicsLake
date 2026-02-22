#' @title Observation Mode
#' @description Track file I/O operations without modifying existing code.
#' Provides a non-invasive way to build lineage from existing workflows.
#'
#' @details
#' Observation mode provides a framework for tracking file I/O operations.
#' By default, \code{observe()} now auto-tracks common unqualified I/O calls
#' (e.g. \code{read.csv()}, \code{write.csv()}, \code{readRDS()}, \code{saveRDS()}).
#'
#' ## Limitations
#'
#' Automatic interception is best-effort:
#' - Namespaced calls (e.g. \code{utils::read.csv}) are not intercepted
#' - I/O executed outside the observed R expression cannot be intercepted
#' - For unsupported functions, use \code{record_read()} / \code{record_write()}
#'
#' ## Recommended Approach
#'
#' For reliable lineage tracking, use OmicsLake's native functions:
#' - `ol_write()` / `ol_read()` for tables
#' - `ol_save()` / `ol_read_object()` for R objects
#'
#' These automatically track dependencies when `depends_on` is specified.
#'
#' ## Manual Recording
#'
#' For legacy code, use `record_read()` and `record_write()` within observe()
#' to manually record file operations.
#'
#' @examples
#' if (FALSE) {
#' # Manual recording within observe()
#' lineage <- observe({
#'   record_read("input.csv")
#'   data <- read.csv("input.csv")
#'   result <- transform(data)
#'   record_write("output.csv")
#'   write.csv(result, "output.csv")
#' })
#'
#' # View what was tracked
#' print(lineage)
#' }
#'
#' @name observe
NULL

# Global tracking environment
.observe_env <- new.env(parent = emptyenv())
.observe_env$active <- FALSE
.observe_env$reads <- character(0)
.observe_env$writes <- character(0)
.observe_env$events <- list()
.observe_env$depth <- 0

.auto_track_env <- new.env(parent = emptyenv())
.auto_track_env$active <- FALSE
.auto_track_env$wrappers <- list()
.auto_track_env$lake <- NULL
.auto_track_env$prefix <- "file:"
.auto_track_env$snapshot <- NULL
.auto_track_env$store_observation <- TRUE
.auto_track_env$observation_name <- NULL
.auto_track_env$observation_depends_on <- "both"
.auto_track_env$installed_last_hook <- FALSE

.ol_default_track_functions <- function() {
  c(
    "read.csv", "read.csv2", "read.delim", "read.table", "readRDS", "load",
    "write.csv", "write.csv2", "write.table", "saveRDS", "save",
    "fread", "fwrite",
    "read_csv", "read_csv2", "read_tsv", "read_delim", "read_rds",
    "write_csv", "write_tsv", "write_delim", "write_rds",
    "read_parquet", "write_parquet", "read_feather", "write_feather",
    "read_excel", "write_xlsx"
  )
}

.ol_global_env <- function() {
  base::globalenv()
}

.ol_track_mode <- function(fun_name) {
  if (fun_name %in% c(
    "read.csv", "read.csv2", "read.delim", "read.table", "readRDS", "load",
    "fread", "read_csv", "read_csv2", "read_tsv", "read_delim", "read_rds",
    "read_parquet", "read_feather", "read_excel"
  )) {
    return("read")
  }
  if (fun_name %in% c(
    "write.csv", "write.csv2", "write.table", "saveRDS", "save",
    "fwrite", "write_csv", "write_tsv", "write_delim", "write_rds",
    "write_parquet", "write_feather", "write_xlsx"
  )) {
    return("write")
  }
  if (grepl("^(read|load|fread)", fun_name)) return("read")
  if (grepl("^(write|save|fwrite)", fun_name)) return("write")
  NA_character_
}

.ol_extract_io_path <- function(fun_name, args, mode) {
  path_arg_names <- c("file", "path", "input", "output", "filename")
  for (arg_name in path_arg_names) {
    if (arg_name %in% names(args) &&
      is.character(args[[arg_name]]) &&
      length(args[[arg_name]]) >= 1 &&
      nzchar(args[[arg_name]][[1]])) {
      return(args[[arg_name]][[1]])
    }
  }

  if (fun_name %in% c(
    "write.csv", "write.csv2", "write.table", "saveRDS",
    "fwrite", "write_csv", "write_tsv", "write_delim", "write_rds",
    "write_parquet", "write_feather", "write_xlsx"
  )) {
    if (length(args) >= 2 &&
      is.character(args[[2]]) &&
      length(args[[2]]) >= 1 &&
      nzchar(args[[2]][[1]])) {
      return(args[[2]][[1]])
    }
  }

  if (identical(mode, "write")) {
    if (length(args) >= 2 &&
      is.character(args[[2]]) &&
      length(args[[2]]) >= 1 &&
      nzchar(args[[2]][[1]])) {
      return(args[[2]][[1]])
    }
  }

  if (mode %in% c("read", "write")) {
    if (length(args) >= 1 &&
      is.character(args[[1]]) &&
      length(args[[1]]) >= 1 &&
      nzchar(args[[1]][[1]])) {
      return(args[[1]][[1]])
    }
  }
  NULL
}

.ol_make_io_wrapper <- function(fun_name, fun_mode, original) {
  force(fun_name)
  force(fun_mode)
  force(original)
  function(...) {
    args <- list(...)
    path <- .ol_extract_io_path(fun_name, args, fun_mode)
    if (!is.null(path)) {
      if (identical(fun_mode, "read")) {
        .record_read(path)
      } else if (identical(fun_mode, "write")) {
        .record_write(path)
      }
    }
    do.call(original, args, envir = parent.frame())
  }
}

#' Start observation mode
#'
#' @keywords internal
.start_observe <- function() {
  .observe_env$active <- TRUE
  .observe_env$depth <- .observe_env$depth + 1
  if (.observe_env$depth == 1) {
    .observe_env$reads <- character(0)
    .observe_env$writes <- character(0)
    .observe_env$events <- list()
  }
}

#' Stop observation mode
#'
#' @keywords internal
.stop_observe <- function() {
  .observe_env$depth <- max(0, .observe_env$depth - 1)
  if (.observe_env$depth == 0) {
    .observe_env$active <- FALSE
  }
}

#' Check if observation is active
#'
#' @keywords internal
.is_observing <- function() {
  .observe_env$active && .observe_env$depth > 0
}

#' Record a file read
#'
#' @keywords internal
.record_read <- function(path) {
  if (.is_observing()) {
    norm_path <- normalizePath(path, winslash = "/", mustWork = FALSE)
    .observe_env$reads <- unique(c(.observe_env$reads, norm_path))
    .observe_env$events <- append(.observe_env$events, list(list(type = "read", path = norm_path)))
  }
}

#' Record a file write
#'
#' @keywords internal
.record_write <- function(path) {
  if (.is_observing()) {
    norm_path <- normalizePath(path, winslash = "/", mustWork = FALSE)
    .observe_env$writes <- unique(c(.observe_env$writes, norm_path))
    .observe_env$events <- append(.observe_env$events, list(list(type = "write", path = norm_path)))
  }
}

.ol_build_observation_lineage <- function(events) {
  if (!length(events)) {
    return(data.frame(
      parent = character(0),
      child = character(0),
      parent_path = character(0),
      child_path = character(0),
      stringsAsFactors = FALSE
    ))
  }

  reads_so_far <- character(0)
  edge_parent <- character(0)
  edge_child <- character(0)
  for (event in events) {
    if (identical(event$type, "read")) {
      reads_so_far <- unique(c(reads_so_far, event$path))
      next
    }
    if (identical(event$type, "write") && length(reads_so_far) > 0) {
      edge_parent <- c(edge_parent, reads_so_far)
      edge_child <- c(edge_child, rep(event$path, length(reads_so_far)))
    }
  }

  if (!length(edge_parent)) {
    return(data.frame(
      parent = character(0),
      child = character(0),
      parent_path = character(0),
      child_path = character(0),
      stringsAsFactors = FALSE
    ))
  }

  lineage <- unique(data.frame(
    parent_path = edge_parent,
    child_path = edge_child,
    stringsAsFactors = FALSE
  ))
  lineage$parent <- basename(lineage$parent_path)
  lineage$child <- basename(lineage$child_path)
  lineage[, c("parent", "child", "parent_path", "child_path")]
}

.ol_simple_hash <- function(x) {
  ints <- as.integer(charToRaw(enc2utf8(x)))
  if (!length(ints)) {
    return("00000000")
  }
  val <- sum(ints * seq_along(ints)) %% 2147483647
  sprintf("%08x", as.integer(val))
}

.ol_observed_node_names <- function(paths, prefix = "file:") {
  if (!length(paths)) {
    return(character(0))
  }
  paths <- unique(paths)
  bases <- basename(paths)
  dup_bases <- unique(bases[duplicated(bases) | duplicated(bases, fromLast = TRUE)])

  node_names <- vapply(seq_along(paths), function(i) {
    base <- bases[[i]]
    if (base %in% dup_bases) {
      paste0(prefix, base, "#", .ol_simple_hash(paths[[i]]))
    } else {
      paste0(prefix, base)
    }
  }, character(1))
  names(node_names) <- paths
  node_names
}

#' Manually record a file read for observation
#'
#' Use this to explicitly record file reads when automatic tracking
#' is not available. Call this within an observe() block.
#'
#' @param path File path that was read
#' @return Invisibly returns the path
#' @export
#' @examples
#' if (FALSE) {
#' observe({
#'   record_read("input.csv")
#'   data <- read.csv("input.csv")
#'   record_write("output.csv")
#'   write.csv(data, "output.csv")
#' })
#' }
record_read <- function(path) {
  .record_read(path)
  invisible(path)
}

#' Manually record a file write for observation
#'
#' Use this to explicitly record file writes when automatic tracking
#' is not available. Call this within an observe() block.
#'
#' @param path File path that was written
#' @return Invisibly returns the path
#' @export
record_write <- function(path) {
  .record_write(path)
  invisible(path)
}

#' Observe I/O operations in a code block
#'
#' Executes code while tracking file reads and writes to build a lineage graph.
#' This is useful for understanding data flow in existing scripts without
#' modifying them.
#'
#' @param expr Expression or code block to observe
#' @param track_functions Character vector of function names to intercept.
#'   `NULL` uses common I/O defaults; `character(0)` disables auto interception.
#' @return A list containing:
#'   \item{result}{The result of evaluating expr}
#'   \item{reads}{Character vector of files read}
#'   \item{writes}{Character vector of files written}
#'   \item{lineage}{Data frame of inferred dependencies}
#' @export
#' @examples
#' if (FALSE) {
#' result <- observe({
#'   data <- read.csv("data.csv")
#'   processed <- data[data$value > 0, ]
#'   write.csv(processed, "processed.csv")
#' })
#'
#' print(result$reads)   # "data.csv"
#' print(result$writes)  # "processed.csv"
#' print(result$lineage) # data.csv -> processed.csv
#' }
observe <- function(expr, track_functions = NULL) {
  # Start tracking
.start_observe()

  # Ensure cleanup on exit
  on.exit(.stop_observe(), add = TRUE)

  if (is.null(track_functions)) {
    track_functions <- .ol_default_track_functions()
  }
  if (!is.character(track_functions)) {
    stop("track_functions must be a character vector", call. = FALSE)
  }

  eval_env <- new.env(parent = parent.frame())
  if (length(track_functions) > 0) {
    for (fun_name in unique(track_functions)) {
      fun_mode <- .ol_track_mode(fun_name)
      if (is.na(fun_mode)) {
        next
      }
      original <- tryCatch(get(fun_name, mode = "function", inherits = TRUE), error = function(e) NULL)
      if (is.null(original)) {
        next
      }
      assign(fun_name, .ol_make_io_wrapper(fun_name, fun_mode, original), envir = eval_env)
    }
  }

  # Execute the expression
  result <- tryCatch({
    eval(substitute(expr), envir = eval_env)
  }, error = function(e) {
    msg <- sprintf("Error during observation: %s", conditionMessage(e))
    message(msg)
    NULL
  })

  # Build lineage from tracked I/O
  reads <- .observe_env$reads
  writes <- .observe_env$writes

  # Create lineage data frame from read/write event order
  lineage <- .ol_build_observation_lineage(.observe_env$events)

  structure(
    list(
      result = result,
      reads = reads,
      writes = writes,
      lineage = lineage
    ),
    class = "lake_observation"
  )
}

#' Print method for lake_observation
#'
#' @param x A lake_observation object
#' @param ... Additional arguments (ignored)
#' @export
print.lake_observation <- function(x, ...) {
  lines <- c(
    "Lake Observation",
    "================",
    paste0("Files read: ", length(x$reads))
  )
  if (length(x$reads) > 0) {
    for (f in x$reads) {
      lines <- c(lines, paste0("  <- ", basename(f)))
    }
  }
  lines <- c(lines, paste0("Files written: ", length(x$writes)))
  if (length(x$writes) > 0) {
    for (f in x$writes) {
      lines <- c(lines, paste0("  -> ", basename(f)))
    }
  }
  if (nrow(x$lineage) > 0) {
    lines <- c(lines, "Inferred lineage:")
    for (i in seq_len(nrow(x$lineage))) {
      lines <- c(lines, paste0("  ", x$lineage$parent[i], " -> ", x$lineage$child[i]))
    }
  }
  writeLines(lines)
  invisible(x)
}

.ol_record_observation_to_lake <- function(obs, lake, prefix = "file:") {
  all_paths <- unique(c(
    obs$reads,
    obs$writes,
    obs$lineage$parent_path,
    obs$lineage$child_path
  ))
  path_nodes <- .ol_observed_node_names(all_paths, prefix = prefix)

  # Record reads as marker nodes
  for (path in obs$reads) {
    name <- unname(path_nodes[[path]])
    tryCatch({
      mark(name, path, lake)
    }, error = function(e) {
      # Ignore errors for marker nodes
    })
  }

  # Record writes with dependencies
  for (path in obs$writes) {
    name <- unname(path_nodes[[path]])
    deps <- obs$lineage$parent_path[obs$lineage$child_path == path]
    dep_nodes <- unique(unname(path_nodes[deps]))
    tryCatch({
      mark(name, path, lake)
      for (dep in dep_nodes) {
        link(dep, name, lake)
      }
    }, error = function(e) {
      # Ignore errors
    })
  }

  list(
    path_nodes = path_nodes,
    read_nodes = unique(unname(path_nodes[obs$reads])),
    write_nodes = unique(unname(path_nodes[obs$writes]))
  )
}

.ol_pick_result_dependencies <- function(mode, read_nodes, write_nodes) {
  mode <- match.arg(mode, c("writes", "reads", "both", "none"))
  switch(mode,
    writes = unique(write_nodes),
    reads = unique(read_nodes),
    both = unique(c(write_nodes, read_nodes)),
    none = character(0)
  )
}

.ol_default_result_name <- function(prefix = "pipeline_result") {
  paste0(prefix, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
}

.ol_build_observation_record <- function(obs, recorded) {
  list(
    recorded_at = as.character(Sys.time()),
    reads = obs$reads,
    writes = obs$writes,
    lineage = obs$lineage,
    read_nodes = recorded$read_nodes,
    write_nodes = recorded$write_nodes
  )
}

.ol_reset_auto_track_env <- function() {
  .auto_track_env$active <- FALSE
  .auto_track_env$wrappers <- list()
  .auto_track_env$lake <- NULL
  .auto_track_env$prefix <- "file:"
  .auto_track_env$snapshot <- NULL
  .auto_track_env$store_observation <- TRUE
  .auto_track_env$observation_name <- NULL
  .auto_track_env$observation_depends_on <- "both"
  .auto_track_env$installed_last_hook <- FALSE
}

#' Observe and record to a Lake
#'
#' Executes code while tracking I/O, then records the lineage to a Lake.
#'
#' @param expr Expression to observe
#' @param lake Lake instance to record lineage to
#' @param prefix Prefix for file-based node names in the lake
#' @param track_functions Character vector forwarded to `observe()`
#' @return The result of evaluating expr
#' @export
#' @examples
#' if (FALSE) {
#' lake <- Lake$new("my_project")
#'
#' observe_to_lake({
#'   source("analysis_pipeline.R")
#' }, lake = lake)
#'
#' # View the recorded lineage
#' lake$tree()
#' }
observe_to_lake <- function(expr, lake, prefix = "file:", track_functions = NULL) {
  expr_sub <- substitute(expr)
  obs <- eval(
    substitute(observe(EXPR, track_functions = TRACK), list(EXPR = expr_sub, TRACK = track_functions)),
    envir = parent.frame()
  )
  .ol_record_observation_to_lake(obs, lake, prefix = prefix)

  obs$result
}

#' Enable transparent background tracking for a session
#'
#' Installs temporary wrappers for common unqualified I/O functions in
#' `.GlobalEnv`, so existing analysis code can run unchanged while reads/writes
#' are tracked and recorded to Lake.
#'
#' @param project Optional project name. If set, calls `use_lake(project, ...)`.
#' @param prefix Prefix for file-based node names in the lake
#' @param snapshot Optional snapshot label created when tracking is disabled with commit
#' @param track_functions Character vector forwarded to tracking wrappers
#' @param store_observation If TRUE, store observed reads/writes/lineage as an object
#' @param observation_name Optional target name when `store_observation = TRUE`
#' @param observation_depends_on Dependencies used for observation record:
#'   `"writes"`, `"reads"`, `"both"`, or `"none"`
#' @param auto_disable If TRUE, installs a `.Last()` hook to auto-commit on session end
#' @param ... Additional arguments passed to `use_lake()` when `project` is provided
#' @return Invisible Lake object
#' @export
#' @examples
#' if (FALSE) {
#' # In .Rprofile (one-time setup for transparent mode)
#' OmicsLake::ol_enable_transparent_tracking(project = "rna_project")
#'
#' # Existing script code runs unchanged
#' data <- read.csv("counts.csv")
#' write.csv(data, "counts_qc.csv", row.names = FALSE)
#'
#' # Explicit end if not using auto_disable
#' OmicsLake::ol_disable_transparent_tracking(commit = TRUE)
#' }
ol_enable_transparent_tracking <- function(project = NULL,
                                           prefix = "file:",
                                           snapshot = NULL,
                                           track_functions = NULL,
                                           store_observation = TRUE,
                                           observation_name = NULL,
                                           observation_depends_on = c("writes", "reads", "both", "none"),
                                           auto_disable = TRUE,
                                           ...) {
  if (.auto_track_env$active) {
    stop("Transparent tracking is already active. Call ol_disable_transparent_tracking() first.", call. = FALSE)
  }
  if (!is.null(snapshot) && (!is.character(snapshot) || length(snapshot) != 1 || !nzchar(snapshot))) {
    stop("snapshot must be NULL or a non-empty character string", call. = FALSE)
  }
  if (!is.logical(store_observation) || length(store_observation) != 1 || is.na(store_observation)) {
    stop("store_observation must be TRUE or FALSE", call. = FALSE)
  }
  if (!is.null(observation_name) && (!is.character(observation_name) || length(observation_name) != 1 || !nzchar(observation_name))) {
    stop("observation_name must be NULL or a non-empty character string", call. = FALSE)
  }
  if (!is.logical(auto_disable) || length(auto_disable) != 1 || is.na(auto_disable)) {
    stop("auto_disable must be TRUE or FALSE", call. = FALSE)
  }

  extra <- list(...)
  target_lake <- if (is.null(project)) {
    if (length(extra) > 0) {
      warning("Ignoring ... because project is NULL", call. = FALSE)
    }
    lake()
  } else {
    do.call(use_lake, c(list(project = project), extra))
  }

  if (is.null(track_functions)) {
    track_functions <- .ol_default_track_functions()
  }
  if (!is.character(track_functions)) {
    stop("track_functions must be a character vector", call. = FALSE)
  }

  wrappers <- list()
  g_env <- .ol_global_env()
  for (fun_name in unique(track_functions)) {
    fun_mode <- .ol_track_mode(fun_name)
    if (is.na(fun_mode)) {
      next
    }
    original <- tryCatch(get(fun_name, mode = "function", inherits = TRUE), error = function(e) NULL)
    if (is.null(original)) {
      next
    }
    had_global <- exists(fun_name, envir = g_env, inherits = FALSE)
    old_global <- if (had_global) get(fun_name, envir = g_env, inherits = FALSE) else NULL
    assign(fun_name, .ol_make_io_wrapper(fun_name, fun_mode, original), envir = g_env)
    wrappers[[length(wrappers) + 1L]] <- list(
      name = fun_name,
      had_global = had_global,
      old_global = old_global
    )
  }

  .start_observe()
  .auto_track_env$active <- TRUE
  .auto_track_env$wrappers <- wrappers
  .auto_track_env$lake <- target_lake
  .auto_track_env$prefix <- prefix
  .auto_track_env$snapshot <- snapshot
  .auto_track_env$store_observation <- store_observation
  .auto_track_env$observation_name <- observation_name
  .auto_track_env$observation_depends_on <- match.arg(observation_depends_on, c("writes", "reads", "both", "none"))
  .auto_track_env$installed_last_hook <- FALSE

  if (isTRUE(auto_disable)) {
    if (exists(".ol_prev_last_fn", envir = g_env, inherits = FALSE)) {
      rm(list = ".ol_prev_last_fn", envir = g_env)
    }
    assign(".ol_prev_last_fn", get0(".Last", envir = g_env, inherits = FALSE), envir = g_env)
    assign(
      ".Last",
      function() {
        try(OmicsLake::ol_disable_transparent_tracking(commit = TRUE), silent = TRUE)
        old_last <- get0(".ol_prev_last_fn", envir = .ol_global_env(), inherits = FALSE)
        if (exists(".ol_prev_last_fn", envir = .ol_global_env(), inherits = FALSE)) {
          rm(list = ".ol_prev_last_fn", envir = .ol_global_env())
        }
        if (is.function(old_last)) {
          old_last()
        }
      },
      envir = g_env
    )
    .auto_track_env$installed_last_hook <- TRUE
  }

  invisible(target_lake)
}

#' Disable transparent background tracking and optionally commit results
#'
#' Restores wrapped I/O functions in `.GlobalEnv` and optionally records tracked
#' reads/writes to Lake before stopping observation mode.
#'
#' @param commit If TRUE, write tracked lineage and optional observation object to Lake
#' @return A `lake_observation` object (invisible) with tracked reads/writes/lineage
#' @export
#' @examples
#' if (FALSE) {
#' ol_enable_transparent_tracking(project = "rna_project", auto_disable = FALSE)
#' # ... run normal analysis code ...
#' ol_disable_transparent_tracking(commit = TRUE)
#' }
ol_disable_transparent_tracking <- function(commit = TRUE) {
  if (!is.logical(commit) || length(commit) != 1 || is.na(commit)) {
    stop("commit must be TRUE or FALSE", call. = FALSE)
  }
  if (!.auto_track_env$active) {
    return(invisible(structure(
      list(
        result = NULL,
        reads = character(0),
        writes = character(0),
        lineage = data.frame(
          parent = character(0),
          child = character(0),
          parent_path = character(0),
          child_path = character(0),
          stringsAsFactors = FALSE
        )
      ),
      class = "lake_observation"
    )))
  }

  obs <- structure(
    list(
      result = NULL,
      reads = .observe_env$reads,
      writes = .observe_env$writes,
      lineage = .ol_build_observation_lineage(.observe_env$events)
    ),
    class = "lake_observation"
  )

  if (isTRUE(commit)) {
    recorded <- .ol_record_observation_to_lake(obs, .auto_track_env$lake, prefix = .auto_track_env$prefix)
    if (isTRUE(.auto_track_env$store_observation)) {
      obs_name <- .auto_track_env$observation_name
      if (is.null(obs_name)) {
        obs_name <- .ol_default_result_name(prefix = "session_observation")
      }
      .ol_validate_name(obs_name, "observation_name")
      deps <- .ol_pick_result_dependencies(
        .auto_track_env$observation_depends_on,
        read_nodes = recorded$read_nodes,
        write_nodes = recorded$write_nodes
      )
      if (!length(deps)) {
        deps <- NULL
      }
      .auto_track_env$lake$put(obs_name, .ol_build_observation_record(obs, recorded), depends_on = deps)
    }
    if (!is.null(.auto_track_env$snapshot)) {
      .auto_track_env$lake$snap(.auto_track_env$snapshot)
    }
  }

  for (wrapper in rev(.auto_track_env$wrappers)) {
    g_env <- .ol_global_env()
    if (isTRUE(wrapper$had_global)) {
      assign(wrapper$name, wrapper$old_global, envir = g_env)
    } else if (exists(wrapper$name, envir = g_env, inherits = FALSE)) {
      rm(list = wrapper$name, envir = g_env)
    }
  }

  while (.observe_env$depth > 0) {
    .stop_observe()
  }

  g_env <- .ol_global_env()
  if (isTRUE(.auto_track_env$installed_last_hook) && exists(".ol_prev_last_fn", envir = g_env, inherits = FALSE)) {
    old_last <- get(".ol_prev_last_fn", envir = g_env, inherits = FALSE)
    rm(list = ".ol_prev_last_fn", envir = g_env)
    if (is.function(old_last)) {
      assign(".Last", old_last, envir = g_env)
    } else if (exists(".Last", envir = g_env, inherits = FALSE)) {
      rm(list = ".Last", envir = g_env)
    }
  }

  .ol_reset_auto_track_env()
  invisible(obs)
}

#' Track a pipeline block into a Lake with minimal boilerplate
#'
#' Uses `observe_to_lake()` under the hood and optionally creates a snapshot
#' after successful execution.
#'
#' @param expr Expression to execute and track
#' @param project Optional project name. If set, calls `use_lake(project, ...)`.
#' @param prefix Prefix for file-based node names in the lake
#' @param snapshot Optional snapshot label created after successful execution
#' @param track_functions Character vector forwarded to `observe()`
#' @param save_result If TRUE, store the expression result in the Lake
#' @param result_name Optional target name when `save_result = TRUE`
#' @param result_depends_on Dependencies used for saved result:
#'   `"writes"`, `"reads"`, `"both"`, or `"none"`
#' @param store_observation If TRUE, store observed reads/writes/lineage as an object
#' @param observation_name Optional target name when `store_observation = TRUE`
#' @param observation_depends_on Dependencies used for observation record:
#'   `"writes"`, `"reads"`, `"both"`, or `"none"`
#' @param ... Additional arguments passed to `use_lake()` when `project` is provided
#' @return The result of evaluating `expr`
#' @export
#' @examples
#' if (FALSE) {
#' # Track an existing block with the current default lake
#' use_lake("rna_seq_project")
#' track_pipeline({
#'   x <- read.csv("counts.csv")
#'   write.csv(x, "counts_copy.csv", row.names = FALSE)
#' }, snapshot = "ingest_v1", save_result = TRUE, result_name = "counts_copy_summary",
#'   store_observation = TRUE, observation_name = "obs_ingest_v1")
#' }
track_pipeline <- function(expr,
                           project = NULL,
                           prefix = "file:",
                           snapshot = NULL,
                           track_functions = NULL,
                           save_result = FALSE,
                           result_name = NULL,
                           result_depends_on = c("writes", "reads", "both", "none"),
                           store_observation = FALSE,
                           observation_name = NULL,
                           observation_depends_on = c("writes", "reads", "both", "none"),
                           ...) {
  if (!is.null(snapshot) && (!is.character(snapshot) || length(snapshot) != 1 || !nzchar(snapshot))) {
    stop("snapshot must be NULL or a non-empty character string", call. = FALSE)
  }
  if (!is.logical(save_result) || length(save_result) != 1 || is.na(save_result)) {
    stop("save_result must be TRUE or FALSE", call. = FALSE)
  }
  if (!is.null(result_name) && (!is.character(result_name) || length(result_name) != 1 || !nzchar(result_name))) {
    stop("result_name must be NULL or a non-empty character string", call. = FALSE)
  }
  if (!is.logical(store_observation) || length(store_observation) != 1 || is.na(store_observation)) {
    stop("store_observation must be TRUE or FALSE", call. = FALSE)
  }
  if (!is.null(observation_name) && (!is.character(observation_name) || length(observation_name) != 1 || !nzchar(observation_name))) {
    stop("observation_name must be NULL or a non-empty character string", call. = FALSE)
  }

  extra <- list(...)
  target_lake <- if (is.null(project)) {
    if (length(extra) > 0) {
      warning("Ignoring ... because project is NULL", call. = FALSE)
    }
    lake()
  } else {
    do.call(use_lake, c(list(project = project), extra))
  }

  expr_sub <- substitute(expr)
  obs <- eval(
    substitute(
      observe(EXPR, track_functions = TRACK),
      list(EXPR = expr_sub, TRACK = track_functions)
    ),
    envir = parent.frame()
  )
  recorded <- .ol_record_observation_to_lake(obs, target_lake, prefix = prefix)

  if (isTRUE(save_result)) {
    if (is.null(result_name)) {
      result_name <- .ol_default_result_name()
    }
    .ol_validate_name(result_name, "result_name")
    deps <- .ol_pick_result_dependencies(
      result_depends_on,
      read_nodes = recorded$read_nodes,
      write_nodes = recorded$write_nodes
    )
    if (!length(deps)) {
      deps <- NULL
    }
    target_lake$put(result_name, obs$result, depends_on = deps)
  }

  if (isTRUE(store_observation)) {
    if (is.null(observation_name)) {
      observation_name <- .ol_default_result_name(prefix = "pipeline_observation")
    }
    .ol_validate_name(observation_name, "observation_name")
    observation_deps <- .ol_pick_result_dependencies(
      observation_depends_on,
      read_nodes = recorded$read_nodes,
      write_nodes = recorded$write_nodes
    )
    if (!length(observation_deps)) {
      observation_deps <- NULL
    }
    target_lake$put(
      observation_name,
      .ol_build_observation_record(obs, recorded),
      depends_on = observation_deps
    )
  }

  if (!is.null(snapshot)) {
    target_lake$snap(snapshot)
  }

  obs$result
}

#' Track an existing analysis script with one function call
#'
#' Sources a script with `local = TRUE` inside observation mode so common
#' unqualified I/O calls in the script can be intercepted.
#'
#' @param path Script path passed to `source()`
#' @param project Optional project name. If set, calls `use_lake(project, ...)`.
#' @param prefix Prefix for file-based node names in the lake
#' @param snapshot Optional snapshot label created after successful execution
#' @param track_functions Character vector forwarded to `observe()`
#' @param save_result If TRUE, store the script return value in the Lake
#' @param result_name Optional target name when `save_result = TRUE`
#' @param result_depends_on Dependencies used for saved result:
#'   `"writes"`, `"reads"`, `"both"`, or `"none"`
#' @param store_observation If TRUE, store observed reads/writes/lineage as an object
#' @param observation_name Optional target name when `store_observation = TRUE`
#' @param observation_depends_on Dependencies used for observation record:
#'   `"writes"`, `"reads"`, `"both"`, or `"none"`
#' @param chdir Passed to `source()`
#' @param echo Passed to `source()`
#' @param source_args Named list of additional arguments passed to `source()`.
#'   Reserved names (`file`, `local`, `chdir`, `echo`) are not allowed.
#' @param ... Additional arguments passed to `use_lake()` when `project` is provided
#' @return The return value of the sourced script (`source(...)$value`)
#' @export
#' @examples
#' if (FALSE) {
#' track_script("analysis_pipeline.R", project = "rna_seq_project", snapshot = "run_2026_02_21")
#' }
track_script <- function(path,
                         project = NULL,
                         prefix = "file:",
                         snapshot = NULL,
                         track_functions = NULL,
                         save_result = FALSE,
                         result_name = NULL,
                         result_depends_on = c("writes", "reads", "both", "none"),
                         store_observation = FALSE,
                         observation_name = NULL,
                         observation_depends_on = c("writes", "reads", "both", "none"),
                         chdir = FALSE,
                         echo = FALSE,
                         source_args = list(),
                         ...) {
  if (!is.character(path) || length(path) != 1 || !nzchar(path)) {
    stop("path must be a non-empty character string", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop("File does not exist: ", path, call. = FALSE)
  }
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (!is.list(source_args)) {
    stop("source_args must be a list", call. = FALSE)
  }
  reserved_source_args <- c("file", "local", "chdir", "echo")
  duplicate_source_args <- intersect(names(source_args), reserved_source_args)
  if (length(duplicate_source_args) > 0) {
    stop(
      "source_args cannot include reserved names: ",
      paste(duplicate_source_args, collapse = ", "),
      call. = FALSE
    )
  }
  if (isTRUE(save_result) && is.null(result_name)) {
    base <- sub("\\.[^.]+$", "", basename(path))
    base <- gsub("[^A-Za-z0-9_]+", "_", base)
    result_name <- paste0("script_result_", base)
  }
  if (isTRUE(store_observation) && is.null(observation_name)) {
    base <- sub("\\.[^.]+$", "", basename(path))
    base <- gsub("[^A-Za-z0-9_]+", "_", base)
    observation_name <- paste0("script_observation_", base)
  }

  old_agent_script <- getOption("ol.agent.script_path")
  options(ol.agent.script_path = path)
  on.exit(options(ol.agent.script_path = old_agent_script), add = TRUE)

  track_pipeline(
    do.call(
      source,
      c(
        list(file = path, local = TRUE, chdir = chdir, echo = echo),
        source_args
      )
    )$value,
    project = project,
    prefix = prefix,
    snapshot = snapshot,
    track_functions = track_functions,
    save_result = save_result,
    result_name = result_name,
    result_depends_on = result_depends_on,
    store_observation = store_observation,
    observation_name = observation_name,
    observation_depends_on = observation_depends_on,
    ...
  )
}

#' Track a code block with explicit lake integration
#'
#' A simpler alternative to observe() that directly integrates with Lake.
#' Wraps code execution and records any lake operations.
#'
#' @param lake Lake instance
#' @param name Name for the output in the lineage
#' @param expr Expression to track
#' @return The result of the expression
#' @export
#' @examples
#' if (FALSE) {
#' lake <- Lake$new("project")
#'
#' result <- with_tracking(lake, "analysis_result", {
#'   data <- lake$get("raw_data")
#'   processed <- transform(data)
#'   processed
#' })
#'
#' # Result is automatically saved with tracked dependencies
#' lake$tree("analysis_result")
#' }
with_tracking <- function(lake, name, expr) {
  # Start tracking in the lake's tracker
  tracker <- lake$.__enclos_env__$private$.tracker
  tracker$start_write(name)

  result <- tryCatch({
    eval(substitute(expr), envir = parent.frame())
  }, finally = {
    # End tracking and get dependencies
    deps <- tracker$end_write()
  })

  # Save result with dependencies
  lake$put(name, result, depends_on = deps)

  result
}

#' Create an observed session
#'
#' Starts an observation session that tracks all Lake operations until stopped.
#'
#' @param lake Lake instance to observe
#' @return An ObserveSession object
#' @export
#' @examples
#' if (FALSE) {
#' lake <- Lake$new("project")
#' session <- observe_session(lake)
#'
#' # Do work...
#' lake$put("data1", df1)
#' lake$put("data2", df2, depends_on = "data1")
#'
#' # Stop and get summary
#' summary <- session$stop()
#' print(summary)
#' }
observe_session <- function(lake) {
  ObserveSession$new(lake)
}

#' @title Observation Session
#' @description Tracks Lake operations over time
#' @keywords internal
ObserveSession <- R6::R6Class("ObserveSession",
  public = list(
    initialize = function(lake) {
      private$.lake <- lake
      private$.start_time <- Sys.time()
      private$.operations <- list()
      private$.active <- TRUE
      invisible(self)
    },

    #' Record an operation
    record = function(op_type, name, details = list()) {
      if (private$.active) {
        private$.operations <- append(private$.operations, list(list(
          type = op_type,
          name = name,
          time = Sys.time(),
          details = details
        )))
      }
      invisible(self)
    },

    #' Stop the session
    stop = function() {
      private$.active <- FALSE
      private$.end_time <- Sys.time()

      list(
        start_time = private$.start_time,
        end_time = private$.end_time,
        duration = difftime(private$.end_time, private$.start_time, units = "secs"),
        operations = private$.operations,
        n_operations = length(private$.operations)
      )
    },

    #' Check if session is active
    is_active = function() {
      private$.active
    },

    print = function() {
      lines <- c(
        "ObserveSession",
        paste0("  Active: ", private$.active),
        paste0("  Operations: ", length(private$.operations)),
        paste0("  Started: ", format(private$.start_time))
      )
      writeLines(lines)
      invisible(self)
    }
  ),

  private = list(
    .lake = NULL,
    .start_time = NULL,
    .end_time = NULL,
    .operations = list(),
    .active = FALSE
  )
)
