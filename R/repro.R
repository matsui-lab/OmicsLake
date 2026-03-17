# Reproducibility metadata helpers (internal)

.ol_repro_components <- function() {
    c("git", "renv", "session", "system")
}

.ol_repro_capture_enabled <- function() {
    isTRUE(getOption("ol.repro.capture", TRUE))
}

.ol_repro_strict <- function() {
    isTRUE(getOption("ol.repro.strict", FALSE))
}

.ol_repro_require_clean_git <- function() {
    isTRUE(getOption("ol.repro.require_clean_git", FALSE))
}

.ol_repro_path <- function() {
    opt <- getOption("ol.repro.path", NULL)
    path <- if (!is.null(opt) && is.character(opt) && length(opt) == 1 && 
        nzchar(opt)) {
        opt
    } else {
        getwd()
    }
    if (file.exists(path) && !dir.exists(path)) {
        path <- dirname(path)
    }
    .ol_norm(path)
}

.ol_repro_include <- function() {
    include <- getOption("ol.repro.include", .ol_repro_components())
    if (is.null(include)) {
        return(character(0))
    }
    include <- unique(as.character(include))
    include <- include[nzchar(include)]
    allowed <- .ol_repro_components()
    unknown <- setdiff(include, allowed)
    if (length(unknown) > 0) {
        warning(
            "Ignoring unknown ol.repro.include values: ",
            paste(unknown, collapse = ", "),
            call. = FALSE
        )
    }
    intersect(include, allowed)
}

.ol_format_time_utc <- function(x = Sys.time()) {
    format(as.POSIXct(x, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

.ol_run_command <- function(command, args) {
    out <- tryCatch(
        withCallingHandlers(
            system2(command, args = args, stdout = TRUE, stderr = TRUE),
            warning = function(w) invokeRestart("muffleWarning")
        ),
        error = function(e) structure(conditionMessage(e), status = 1L)
    )
    status <- attr(out, "status")
    text <- trimws(paste(out, collapse = "\n"))
    list(
        ok = is.null(status) || identical(as.integer(status), 0L),
        status = if (is.null(status)) 0L else as.integer(status),
        output = text
    )
}

.ol_git_exec <- function(path, args) {
    .ol_run_command("git", c("-C", path, args))
}

.ol_find_upwards <- function(start, target) {
    if (!is.character(start) || length(start) != 1 || !nzchar(start)) {
        return(NULL)
    }
    current <- .ol_norm(start)
    if (file.exists(current) && !dir.exists(current)) {
        current <- dirname(current)
    }

    repeat {
        candidate <- file.path(current, target)
        if (file.exists(candidate)) {
            return(.ol_norm(candidate))
        }
        parent <- dirname(current)
        if (identical(parent, current)) {
            break
        }
        current <- parent
    }

    NULL
}

.ol_collect_git_context <- function(path) {
    git_bin <- Sys.which("git")
    if (!nzchar(git_bin)) {
        return(list(found = FALSE, reason = "git_not_installed"))
    }

    root_res <- .ol_git_exec(path, c("rev-parse", "--show-toplevel"))
    if (!isTRUE(root_res$ok) || !nzchar(root_res$output)) {
        return(list(found = FALSE, reason = "not_git_repository"))
    }

    root <- .ol_norm(root_res$output)
    commit_res <- .ol_git_exec(root, c("rev-parse", "HEAD"))
    branch_res <- .ol_git_exec(root, c("rev-parse", "--abbrev-ref", "HEAD"))
    status_res <- .ol_git_exec(root, c("status", "--porcelain",
        "--untracked-files=normal"))
    remote_res <- .ol_git_exec(root, c("remote", "get-url", "origin"))

    status_lines <- if (isTRUE(status_res$ok) && nzchar(status_res$output)) {
        strsplit(status_res$output, "\n", fixed = TRUE)[[1]]
    } else {
        character(0)
    }

    list(
        found = TRUE,
        root = root,
        branch = if (isTRUE(branch_res$ok) && 
            nzchar(branch_res$output)) branch_res$output else NA_character_,
        commit = if (isTRUE(commit_res$ok) && 
            nzchar(commit_res$output)) commit_res$output else NA_character_,
        commit_short = if (isTRUE(commit_res$ok) && 
            nzchar(commit_res$output)) substr(commit_res$output,
            1L, 12L) else NA_character_,
        dirty = length(status_lines) > 0,
        changed_files = as.integer(length(status_lines)),
        remote_origin = if (isTRUE(remote_res$ok) && 
            nzchar(remote_res$output)) remote_res$output else NA_character_
    )
}

.ol_collect_renv_file_context <- function(lockfile) {
    info <- tryCatch(file.info(lockfile), error = function(e) NULL)
    md5 <- tryCatch(as.character(tools::md5sum(lockfile)[[1]]),
        error = function(e) NA_character_)
    list(info = info, md5 = md5)
}

.ol_collect_renv_parsed_fields <- function(parsed) {
    list(
        format_version = if (!is.null(parsed$Version)) {
            as.character(parsed$Version)
        } else {
            NA_character_
        },
        r_version = if (!is.null(parsed$R$Version)) {
            as.character(parsed$R$Version)
        } else {
            NA_character_
        },
        packages_n = if (is.list(parsed$Packages)) {
            as.integer(length(parsed$Packages))
        } else {
            NA_integer_
        },
        renv_version = if (!is.null(parsed$Packages$renv$Version)) {
            as.character(parsed$Packages$renv$Version)
        } else {
            NA_character_
        }
    )
}

.ol_collect_renv_context <- function(path) {
    lockfile <- .ol_find_upwards(path, "renv.lock")
    if (is.null(lockfile)) {
        return(list(found = FALSE, reason = "renv_lock_not_found"))
    }
    parsed <- tryCatch(
        jsonlite::fromJSON(lockfile, simplifyVector = FALSE),
        error = function(e) NULL
    )
    file_ctx <- .ol_collect_renv_file_context(lockfile)
    out <- list(
        found = TRUE,
        lockfile = lockfile,
        lockfile_md5 = file_ctx$md5,
        parse_ok = !is.null(parsed)
    )
    if (!is.null(file_ctx$info) && nrow(file_ctx$info) > 0) {
        out$lockfile_size <- as.integer(file_ctx$info$size[[1]])
        out$lockfile_mtime_utc <- .ol_format_time_utc(file_ctx$info$mtime[[1]])
    }
    if (is.null(parsed)) {
        return(out)
    }
    out <- utils::modifyList(out, .ol_collect_renv_parsed_fields(parsed))
    out
}

.ol_collect_session_context <- function() {
    list(
        found = TRUE,
        r_version = R.version$version.string,
        platform = R.version$platform,
        major = R.version$major,
        minor = R.version$minor,
        locale = Sys.getlocale(),
        timezone = as.character(Sys.timezone())
    )
}

.ol_collect_system_context <- function() {
    info <- Sys.info()
    list(
        found = TRUE,
        sysname = unname(info[["sysname"]]),
        release = unname(info[["release"]]),
        machine = unname(info[["machine"]])
    )
}

.ol_collect_repro_context <- function(path = .ol_repro_path(),
    include = .ol_repro_include()) {
    include <- intersect(unique(as.character(include)), .ol_repro_components())
    ctx <- list(
        schema_version = 1L,
        captured_at_utc = .ol_format_time_utc(),
        path = .ol_norm(path),
        include = include
    )
    if ("git" %in% include) {
        ctx$git <- .ol_collect_git_context(path)
    }
    if ("renv" %in% include) {
        ctx$renv <- .ol_collect_renv_context(path)
    }
    if ("session" %in% include) {
        ctx$session <- .ol_collect_session_context()
    }
    if ("system" %in% include) {
        ctx$system <- .ol_collect_system_context()
    }
    ctx
}

.ol_apply_repro_guards <- function(ctx) {
    if (!.ol_repro_require_clean_git()) {
        return(invisible(TRUE))
    }
    git <- ctx$git
    if (is.null(git) || !isTRUE(git$found)) {
        stop(
            "Cannot verify Git cleanliness because no repository was detected.",
            "Set options(ol.repro.path = '<git repo>') or disable with",
            "options(ol.repro.require_clean_git = FALSE).",
            call. = FALSE
        )
    }
    if (isTRUE(git$dirty)) {
        stop(
            "Git working tree is dirty (",
            as.integer(git$changed_files),
            " changed files). Commit or stash changes before snapshot/commit.",
            call. = FALSE
        )
    }
    invisible(TRUE)
}

.ol_option_or_env <- function(option_name, env_names = character(0),
    default = NA_character_) {
    val <- getOption(option_name, NULL)
    if (!is.null(val) && length(val) > 0 && nzchar(as.character(val[[1]]))) {
        return(as.character(val[[1]]))
    }
    for (env_name in env_names) {
        env_val <- Sys.getenv(env_name, unset = "")
        if (nzchar(env_val)) {
            return(env_val)
        }
    }
    default
}

.ol_collect_agent_context <- function() {
    prompt_id <- .ol_option_or_env(
        "ol.agent.prompt_id",
        c("OL_PROMPT_ID", "CODEX_PROMPT_ID", "OPENAI_PROMPT_ID")
    )
    run_id <- .ol_option_or_env(
        "ol.agent.run_id",
        c("OL_AGENT_RUN_ID", "CODEX_SESSION_ID")
    )
    agent_name <- .ol_option_or_env(
        "ol.agent.name",
        c("OL_AGENT_NAME", "CODEX_AGENT_NAME")
    )
    script_path <- .ol_option_or_env(
        "ol.agent.script_path",
        c("OL_AGENT_SCRIPT_PATH")
    )

    out <- list()
    if (!is.na(prompt_id)) out$prompt_id <- prompt_id
    if (!is.na(run_id)) out$run_id <- run_id
    if (!is.na(agent_name)) out$agent_name <- agent_name
    if (!is.na(script_path)) {
        script_path <- .ol_norm(script_path)
        out$script_path <- script_path
        if (file.exists(script_path) && !dir.exists(script_path)) {
            out$script_md5 <- tryCatch(
                as.character(tools::md5sum(script_path)[[1]]),
                error = function(e) NA_character_
            )
        }
    }
    if (!length(out)) {
        return(NULL)
    }
    out$captured_at_utc <- .ol_format_time_utc()
    out
}

.ol_snapshot_validation_enabled <- function() {
    isTRUE(getOption("ol.snapshot.auto_validate", TRUE))
}

.ol_snapshot_validation_mode <- function() {
    mode <- getOption("ol.snapshot.validate.mode", "off")
    mode <- tolower(as.character(mode)[1])
    if (!mode %in% c("off", "warn", "error")) {
        mode <- "off"
    }
    mode
}

.ol_snapshot_validation_max_tables <- function() {
    raw <- getOption("ol.snapshot.validate.max_tables", 200L)
    if (!is.numeric(raw) || length(raw) != 1 || is.na(raw) || raw < 1) {
        return(200L)
    }
    as.integer(raw)
}

.ol_internal_table_names <- function() {
    c("__ol_refs", "__ol_objects", "__ol_commits", "__ol_dependencies")
}

.ol_list_live_user_tables <- function(state) {
    conn <- state$conn
    tabs <- DBI::dbListTables(conn, DBI::Id(schema = state$namespace))
    tabs <- setdiff(tabs, .ol_internal_table_names())
    tabs <- grep("^__ol_backup_", tabs, value = TRUE, invert = TRUE)
    sort(unique(tabs))
}

.ol_latest_project_label <- function(state) {
    .ol_ensure_refs_table(state)
    conn <- state$conn
    ident <- .ol_sql_ident(conn, state, "__ol_refs")
    query <- sprintf(
        paste(
            "SELECT tag, snapshot FROM %s WHERE table_name = %s",
            "ORDER BY snapshot DESC LIMIT 1"
        ),
        ident,
        DBI::dbQuoteString(conn, "__project__")
    )
    res <- tryCatch(DBI::dbGetQuery(conn, query),
        error = function(e) data.frame())
    if (nrow(res) == 0) {
        return(NULL)
    }
    list(
        label = as.character(res$tag[[1]]),
        snapshot = as.character(res$snapshot[[1]])
    )
}

.ol_snapshot_table_map <- function(state, label) {
    .ol_ensure_refs_table(state)
    conn <- state$conn
    ident <- .ol_sql_ident(conn, state, "__ol_refs")
    query <- sprintf(
        paste(
            "SELECT table_name, snapshot, as_of FROM %s WHERE tag = %s",
            "AND table_name != %s ORDER BY as_of DESC"
        ),
        ident,
        DBI::dbQuoteString(conn, label),
        DBI::dbQuoteString(conn, "__project__")
    )
    res <- tryCatch(DBI::dbGetQuery(conn, query),
        error = function(e) data.frame())
    if (nrow(res) == 0) {
        return(stats::setNames(character(0), character(0)))
    }
    out <- stats::setNames(character(0), character(0))
    for (i in seq_len(nrow(res))) {
        nm <- as.character(res$table_name[[i]])
        if (!nm %in% names(out)) {
            out[[nm]] <- as.character(res$snapshot[[i]])
        }
    }
    out
}

.ol_table_row_count <- function(state, table_name) {
    conn <- state$conn
    ident <- .ol_sql_ident(conn, state, table_name)
    query <- sprintf("SELECT COUNT(*) AS n FROM %s", ident)
    res <- tryCatch(DBI::dbGetQuery(conn, query),
        error = function(e) data.frame())
    if (nrow(res) == 0) {
        return(NA_integer_)
    }
    as.integer(res$n[[1]])
}

.ol_table_columns <- function(state, table_name) {
    conn <- state$conn
    cols <- tryCatch(
        DBI::dbListFields(conn, DBI::Id(schema = state$namespace,
            table = table_name)),
        error = function(e) character(0)
    )
    sort(unique(as.character(cols)))
}

.ol_empty_snapshot_validation <- function(label) {
    list(
        target_label = as.character(label),
        previous_label = NA_character_,
        checked_at_utc = .ol_format_time_utc(),
        summary = list(
            total_tables = 0L,
            unchanged = 0L,
            row_count_changes = 0L,
            structural_changes = 0L
        ),
        table_changes = data.frame(
            table_name = character(0),
            change_type = character(0),
            in_previous = logical(0),
            in_current = logical(0),
            previous_rows = integer(0),
            current_rows = integer(0),
            previous_columns = character(0),
            current_columns = character(0),
            stringsAsFactors = FALSE
        ),
        truncated = FALSE
    )
}

.ol_snapshot_change_row <- function(state, table_name, prev_map,
    current_tables) {
    in_prev <- table_name %in% names(prev_map)
    in_curr <- table_name %in% current_tables
    prev_rows <- NA_integer_
    curr_rows <- NA_integer_
    prev_cols <- character(0)
    curr_cols <- character(0)
    if (isTRUE(in_prev)) {
        prev_rows <- .ol_table_row_count(state, prev_map[[table_name]])
        prev_cols <- .ol_table_columns(state, prev_map[[table_name]])
    }
    if (isTRUE(in_curr)) {
        curr_rows <- .ol_table_row_count(state, table_name)
        curr_cols <- .ol_table_columns(state, table_name)
    }
    change_type <- "unchanged"
    if (!in_prev && in_curr) {
        change_type <- "added_table"
    } else if (in_prev && !in_curr) {
        change_type <- "removed_table"
    } else if (!identical(prev_cols, curr_cols)) {
        change_type <- "schema_changed"
    } else if (!is.na(prev_rows) && !is.na(curr_rows) && 
        prev_rows != curr_rows) {
        change_type <- "row_count_changed"
    }
    data.frame(
        table_name = table_name,
        change_type = change_type,
        in_previous = isTRUE(in_prev),
        in_current = isTRUE(in_curr),
        previous_rows = prev_rows,
        current_rows = curr_rows,
        previous_columns = paste(prev_cols, collapse = ","),
        current_columns = paste(curr_cols, collapse = ","),
        stringsAsFactors = FALSE
    )
}

.ol_snapshot_table_changes <- function(state, prev_map, current_tables) {
    all_tables <- sort(unique(c(names(prev_map), current_tables)))
    rows <- lapply(all_tables, function(table_name) {
        .ol_snapshot_change_row(state, table_name, prev_map, current_tables)
    })
    table_changes <- if (length(rows) > 0) {
        do.call(rbind, rows)
    } else {
        .ol_empty_snapshot_validation(label = "")$table_changes
    }
    list(all_tables = all_tables, table_changes = table_changes)
}

.ol_snapshot_finalize <- function(label, previous, all_tables, table_changes) {
    structural_n <- sum(table_changes$change_type %in% c("added_table",
        "removed_table", "schema_changed"))
    row_n <- sum(table_changes$change_type %in% c("row_count_changed"))
    unchanged_n <- sum(table_changes$change_type %in% c("unchanged"))
    max_tables <- .ol_snapshot_validation_max_tables()
    truncated <- nrow(table_changes) > max_tables
    if (truncated) {
        table_changes <- utils::head(table_changes, max_tables)
    }
    list(
        target_label = as.character(label),
        previous_label = previous$label,
        checked_at_utc = .ol_format_time_utc(),
        summary = list(
            total_tables = as.integer(length(all_tables)),
            unchanged = as.integer(unchanged_n),
            row_count_changes = as.integer(row_n),
            structural_changes = as.integer(structural_n)
        ),
        table_changes = table_changes,
        truncated = isTRUE(truncated)
    )
}

.ol_build_snapshot_validation <- function(project, label) {
    state <- .ol_get_backend_state(project)
    previous <- .ol_latest_project_label(state)
    if (is.null(previous) || is.null(previous$label) || 
        !nzchar(previous$label)) {
        return(.ol_empty_snapshot_validation(label))
    }
    prev_map <- .ol_snapshot_table_map(state, previous$label)
    current_tables <- .ol_list_live_user_tables(state)
    changes <- .ol_snapshot_table_changes(state, prev_map, current_tables)
    .ol_snapshot_finalize(
        label,
        previous,
        changes$all_tables,
        changes$table_changes
    )
}

.ol_snapshot_validation_issue_text <- function(validation) {
    previous_label <- .ol_as_scalar_character(validation$previous_label,
        "unknown")
    structural_changes <- .ol_as_scalar_integer(.ol_get_nested(validation,
        c("summary", "structural_changes")), 0L)
    row_changes <- .ol_as_scalar_integer(.ol_get_nested(validation, c("summary",
        "row_count_changes")), 0L)
    paste0(
        "Snapshot validation vs previous label '", previous_label,
            "' detected ",
        structural_changes, " structural change(s) and ",
        row_changes, " row-count change(s)."
    )
}

.ol_add_snapshot_validation_to_params <- function(params, validation) {
    if (is.null(validation)) {
        return(params)
    }
    if (is.null(params)) {
        params <- list()
    }
    if (!is.list(params)) {
        params <- list(user_params = params)
    }
    if (is.null(params$omicslake_snapshot_validation)) {
        params$omicslake_snapshot_validation <- validation
    } else {
        params$omicslake_snapshot_validation_auto <- validation
    }
    params
}

.ol_add_repro_to_params <- function(params) {
    if (is.null(params)) {
        params <- list()
    }
    if (!.ol_repro_capture_enabled()) {
        return(params)
    }
    if (!is.list(params)) {
        params <- list(user_params = params)
    }

    ctx <- tryCatch(
        .ol_collect_repro_context(path = .ol_repro_path(),
            include = .ol_repro_include()),
        error = function(e) e
    )
    if (inherits(ctx, "error")) {
        msg <- paste0("Failed to capture reproducibility context: ",
            conditionMessage(ctx))
        if (.ol_repro_strict()) {
            stop(msg, call. = FALSE)
        }
        warning(msg, call. = FALSE)
        return(params)
    }
    .ol_apply_repro_guards(ctx)

    if (is.null(params$omicslake_repro)) {
        params$omicslake_repro <- ctx
    } else {
        params$omicslake_repro_auto <- ctx
    }

    agent_ctx <- .ol_collect_agent_context()
    if (!is.null(agent_ctx)) {
        if (is.null(params$omicslake_agent)) {
            params$omicslake_agent <- agent_ctx
        } else {
            params$omicslake_agent_auto <- agent_ctx
        }
    }
    params
}

.ol_parse_params_json <- function(json_text) {
    if (!is.character(json_text) || length(json_text) != 1 || 
        !nzchar(json_text)) {
        return(NULL)
    }
    tryCatch(
        jsonlite::fromJSON(json_text, simplifyVector = FALSE),
        error = function(e) NULL
    )
}

.ol_get_nested <- function(x, keys) {
    if (is.null(x)) {
        return(NULL)
    }
    cur <- x
    for (key in keys) {
        if (!is.list(cur) || is.null(cur[[key]])) {
            return(NULL)
        }
        cur <- cur[[key]]
    }
    cur
}

.ol_as_scalar_character <- function(x, default = NA_character_) {
    if (is.null(x) || length(x) < 1) {
        return(default)
    }
    val <- as.character(x[[1]])
    if (!nzchar(val)) default else val
}

.ol_as_scalar_logical <- function(x, default = NA) {
    if (is.null(x) || length(x) < 1) {
        return(default)
    }
    as.logical(x[[1]])
}

.ol_as_scalar_integer <- function(x, default = NA_integer_) {
    if (is.null(x) || length(x) < 1) {
        return(default)
    }
    as.integer(x[[1]])
}

#' Enable Strict Reproducibility Mode
#'
#' Applies an opinionated, audit-first option preset for AI-assisted analysis:
#' reproducibility metadata capture ON, clean-git requirement ON, and
#' snapshot validation ON.
#'
#' @param path Base path used for Git/renv detection (usually repository root)
#' @param prompt_id Optional AI prompt/work-item identifier
#' @param run_id Optional AI run/session identifier
#' @param agent_name Optional AI agent name
#' @param include Metadata components to capture
#' @param snapshot_validate_mode Snapshot validation behavior: "error", "warn",
#' or "off"
#' @param max_tables Maximum tables stored in snapshot validation details
#' @return Named list of previous option values (invisible)
#' @export
.ol_validate_strict_repro_inputs <- function(path, include,
    snapshot_validate_mode, max_tables) {
    if (!is.character(path) || length(path) != 1 || !nzchar(path)) {
        stop("path must be a non-empty character string", call. = FALSE)
    }
    if (file.exists(path) && !dir.exists(path)) {
        path <- dirname(path)
    }
    include <- unique(as.character(include))
    include <- include[nzchar(include)]
    allowed <- .ol_repro_components()
    unknown <- setdiff(include, allowed)
    if (length(unknown) > 0) {
        stop(
            "Unknown include values: ",
            paste(unknown, collapse = ", "),
            ". Allowed values: ",
            paste(allowed, collapse = ", "),
            call. = FALSE
        )
    }
    snapshot_validate_mode <- match.arg(
        snapshot_validate_mode,
        choices = c("error", "warn", "off")
    )
    if (!is.numeric(max_tables) ||
        length(max_tables) != 1 ||
        is.na(max_tables) ||
        max_tables < 1) {
        stop("max_tables must be a single positive number", call. = FALSE)
    }
    list(
        path = path,
        include = include,
        snapshot_validate_mode = snapshot_validate_mode,
        max_tables = as.integer(max_tables)
    )
}

.ol_strict_repro_option_names <- function() {
    c(
        "ol.repro.capture",
        "ol.repro.strict",
        "ol.repro.require_clean_git",
        "ol.repro.path",
        "ol.repro.include",
        "ol.snapshot.auto_validate",
        "ol.snapshot.validate.mode",
        "ol.snapshot.validate.max_tables",
        "ol.agent.prompt_id",
        "ol.agent.run_id",
        "ol.agent.name"
    )
}

ol_enable_strict_repro_mode <- function(path = getwd(),
                                        prompt_id = NULL,
                                        run_id = NULL,
                                        agent_name = NULL,
                                        include = c("git", "renv", "session",
                                            "system"),
                                        snapshot_validate_mode = c("error",
                                            "warn", "off"),
                                        max_tables = 200L) {
    validated <- .ol_validate_strict_repro_inputs(
        path,
        include,
        snapshot_validate_mode,
        max_tables
    )
    opt_names <- c(
        .ol_strict_repro_option_names()
    )
    previous <- stats::setNames(lapply(opt_names, getOption), opt_names)
    new_opts <- list(
        ol.repro.capture = TRUE,
        ol.repro.strict = TRUE,
        ol.repro.require_clean_git = TRUE,
        ol.repro.path = .ol_norm(validated$path),
        ol.repro.include = validated$include,
        ol.snapshot.auto_validate = TRUE,
        ol.snapshot.validate.mode = validated$snapshot_validate_mode,
        ol.snapshot.validate.max_tables = validated$max_tables
    )
    if (!is.null(prompt_id)) {
        new_opts$ol.agent.prompt_id <- as.character(prompt_id)[1]
    }
    if (!is.null(run_id)) {
        new_opts$ol.agent.run_id <- as.character(run_id)[1]
    }
    if (!is.null(agent_name)) {
        new_opts$ol.agent.name <- as.character(agent_name)[1]
    }
    do.call(options, new_opts)

    invisible(previous)
}
