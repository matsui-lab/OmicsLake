#' Write a table using the DuckDB backend
#' @param name Table name
#' @param data Data frame to store
#' @param project Project name
#' @param mode Write mode: "create", "overwrite", or "append"
#' @param depends_on Optional character vector of table/object names that this table depends on
#' @return Invisible qualified table name
#' @export
ol_write <- function(name, data, project = getOption("ol.project"), mode = c("create", "overwrite", "append"), depends_on = NULL) {
  .ol_require(c("arrow", "duckdb"))
  .ol_validate_name(name, "table name")
  .ol_validate_data_frame(data)
  project <- .ol_assert_project(project, "Call ol_init() first or set options(ol.project=...).")
  state <- .ol_get_backend_state(project)
  mode <- match.arg(mode)
  conn <- state$conn
  schema_sql <- .ol_schema_sql(conn, state)
  DBI::dbExecute(conn, sprintf("CREATE SCHEMA IF NOT EXISTS %s", schema_sql))
  tbl <- arrow::arrow_table(data)
  tmp_name <- paste0("ol_tmp_", gsub("[^0-9]", "", format(Sys.time(), "%Y%m%d%H%M%OS6")))
  duckdb::duckdb_register_arrow(conn, tmp_name, tbl)
  on.exit(duckdb::duckdb_unregister_arrow(conn, tmp_name), add = TRUE)
  ident <- .ol_sql_ident(conn, state, name)
  sql <- switch(mode,
    create = sprintf("CREATE TABLE %s AS SELECT * FROM %s", ident, DBI::dbQuoteIdentifier(conn, tmp_name)),
    overwrite = sprintf("CREATE OR REPLACE TABLE %s AS SELECT * FROM %s", ident, DBI::dbQuoteIdentifier(conn, tmp_name)),
    append = sprintf("INSERT INTO %s SELECT * FROM %s", ident, DBI::dbQuoteIdentifier(conn, tmp_name))
  )
  DBI::dbExecute(conn, sql)
  
  .ol_record_dependencies(state, name, "table", depends_on)
  
  invisible(.ol_qualified_name(state, name))
}

#' Save an R object via the backend metadata table
#' @param name Object name
#' @param object R object to save
#' @param project Project name
#' @param mime MIME type for object payload
#' @param depends_on Optional character vector of table/object names that this object depends on
#' @return Invisible TRUE on success
#' @export
ol_save <- function(name, object, project = getOption("ol.project"), mime = NULL, depends_on = NULL) {
  .ol_validate_name(name, "object name")
  project <- .ol_assert_project(project, "Call ol_init() first or set options(ol.project=...).")
  state <- .ol_get_backend_state(project)
  conn <- state$conn
  .ol_ensure_objects_table(state)
  version_ts <- Sys.time()
  mime <- if (is.null(mime)) "application/octet-stream" else as.character(mime)
  if (identical(state$object_mode, "external")) {
    root <- state$object_root
    if (is.null(root)) stop("External object storage root is not configured", call. = FALSE)
    fname <- sprintf("%s-%s.rds", name, format(version_ts, "%Y%m%d-%H%M%S"))
    fpath <- file.path(root, fname)
    saveRDS(object, fpath)
    bytes <- charToRaw(.ol_norm(fpath))
  } else {
    bytes <- serialize(object, NULL)
  }
  payload <- data.frame(
    name = name,
    version_ts = as.POSIXct(version_ts, tz = "UTC"),
    mime = mime,
    bytes = I(list(bytes)),
    stringsAsFactors = FALSE
  )
  DBI::dbAppendTable(conn, DBI::Id(schema = state$namespace, table = "__ol_objects"), payload)
  
  .ol_record_dependencies(state, name, "object", depends_on)
  
  invisible(TRUE)
}

#' Commit with metadata (note and parameters)
#' @param note Commit message describing the changes
#' @param params Named list of parameters to store with the commit
#' @param project Project name
#' @details
#' By default, commits automatically embed reproducibility context under
#' `params$omicslake_repro` (Git state, `renv.lock`, and session/system metadata
#' when available). Control this with:
#' `options(ol.repro.capture = TRUE/FALSE)`,
#' `options(ol.repro.path = "/path/to/analysis")`,
#' `options(ol.repro.include = c("git","renv","session","system"))`,
#' `options(ol.repro.strict = TRUE/FALSE)`,
#' `options(ol.repro.require_clean_git = TRUE/FALSE)`,
#' and AI metadata options such as `options(ol.agent.prompt_id = "...")`.
#' @export
ol_commit <- function(note = "", params = list(), project = getOption("ol.project")) {
  project <- .ol_assert_project(project, "Call ol_init() first or set options(ol.project=...).")
  state <- .ol_get_backend_state(project)
  commit_id <- format(Sys.time(), "%Y%m%d-%H%M%S")
  
  .ol_ensure_commits_table(state)
  conn <- state$conn
  
  params <- .ol_add_repro_to_params(params)
  params_json <- if (length(params) > 0) jsonlite::toJSON(params, auto_unbox = TRUE, null = "null") else ""
  
  commit_data <- data.frame(
    commit_id = commit_id,
    note = as.character(note),
    params_json = as.character(params_json),
    created_at = as.POSIXct(Sys.time(), tz = "UTC"),
    stringsAsFactors = FALSE
  )
  
  DBI::dbAppendTable(conn, DBI::Id(schema = state$namespace, table = "__ol_commits"), commit_data)
  
  invisible(commit_id)
}

#' Read a table by name and reference
#' @param name Table or object name
#' @param ref Reference string (e.g. "@latest", "@tag(v1)")
#' @param project Project name
#' @param collect If TRUE, return data.frame; if FALSE, return lazy table
#' @return Data frame, lazy table, or stored object
#' @export
ol_read <- function(name, ref = "@latest", project = getOption("ol.project"), collect = TRUE) {
  project <- .ol_assert_project(project, "Call ol_init() first or set options(ol.project=...).")
  state <- .ol_get_backend_state(project)

  if (!isTRUE(collect)) {
    if (.ol_is_object(state, name) && !.ol_table_exists(state, name)) {
      stop(
        "collect = FALSE is only supported for tables. ",
        "The name '", name, "' resolves to an object; use collect = TRUE.",
        call. = FALSE
      )
    }
    resolved <- .ol_resolve_reference(state, name, ref)
    sql <- .ol_get_table_sql(state, name, resolved)
    .ol_require(c("dplyr", "dbplyr"))
    return(dplyr::tbl(state$conn, dbplyr::sql(sql)))
  }

  table_attempt <- tryCatch(
    {
      resolved <- .ol_resolve_reference(state, name, ref)
      sql <- .ol_get_table_sql(state, name, resolved)
      list(result = DBI::dbGetQuery(state$conn, sql), error = NULL)
    },
    error = function(e) list(result = NULL, error = conditionMessage(e))
  )
  table_err <- table_attempt$error
  res <- table_attempt$result

  if (!is.null(res) && is.data.frame(res)) {
    rownames(res) <- NULL
    return(res)
  }

  object_attempt <- tryCatch(
    {
      list(result = ol_read_object(name, ref = ref, project = project), error = NULL)
    },
    error = function(e) list(result = NULL, error = conditionMessage(e))
  )
  obj_err <- object_attempt$error
  obj <- object_attempt$result

  if (!is.null(obj)) {
    if (!is.atomic(obj)) {
      attr(obj, "ol.ref") <- ref
    }
    return(obj)
  }

  msg <- paste0(
    "Failed to read '", name, "' at ref '", ref, "'. ",
    if (!is.null(table_err)) paste0("Table read error: ", table_err, ". ") else "",
    if (!is.null(obj_err)) paste0("Object read error: ", obj_err, ".") else ""
  )
  stop(msg, call. = FALSE)
}

#' Alias of \code{ol_read()}
#' @param name Table or object name
#' @param ref Reference string (e.g. "@latest", "@tag(v1)")
#' @param project Project name
#' @return Data frame, lazy table, or stored object
#' @export
ol_load <- function(name, ref = "@latest", project = getOption("ol.project")) ol_read(name, ref, project)

#' Return version log for a table
#' @param name Optional table name. If NULL, returns commit log.
#' @param project Project name
#' @return Data frame of version history
#' @export
ol_log <- function(name = NULL, project = getOption("ol.project")) {
  project <- .ol_assert_project(project, "Call ol_init() first or set options(ol.project=...).")
  state <- try(.ol_get_backend_state(project), silent = TRUE)
  if (inherits(state, "try-error")) return(utils::head(data.frame()))
  
  if (is.null(name)) {
    return(ol_log_commits(project = project))
  }
  
  .ol_ensure_refs_table(state)
  conn <- state$conn
  ident <- .ol_sql_ident(conn, state, "__ol_refs")
  
  query <- sprintf(
    "SELECT tag, as_of FROM %s WHERE table_name = %s ORDER BY as_of DESC",
    ident,
    DBI::dbQuoteString(conn, name)
  )
  res <- tryCatch(DBI::dbGetQuery(conn, query), error = function(e) data.frame())
  
  res
}

#' List all tables in the project
#' @param project Project name
#' @export
ol_list_tables <- function(project = getOption("ol.project")) {
  project <- .ol_assert_project(project, "Call ol_init() first or set options(ol.project=...).")
  state <- .ol_get_backend_state(project)
  conn <- state$conn
  
  tables <- DBI::dbListTables(conn, DBI::Id(schema = state$namespace))
  tables <- setdiff(tables, c("__ol_refs", "__ol_objects", "__ol_commits", "__ol_dependencies", "__ol_adapters"))
  
  data.frame(
    table_name = tables,
    stringsAsFactors = FALSE
  )
}

#' List all saved objects in the project
#' @param project Project name
#' @export
ol_list_objects <- function(project = getOption("ol.project")) {
  project <- .ol_assert_project(project, "Call ol_init() first or set options(ol.project=...).")
  state <- .ol_get_backend_state(project)
  .ol_ensure_objects_table(state)
  conn <- state$conn
  
  ident <- .ol_sql_ident(conn, state, "__ol_objects")
  query <- sprintf("SELECT DISTINCT name FROM %s ORDER BY name", ident)
  DBI::dbGetQuery(conn, query)
}

#' List all tags for a table or all project labels
#' @param name Optional table name to list tags for. If NULL, lists all tags.
#' @param project Project name
#' @export
ol_list_tags <- function(name = NULL, project = getOption("ol.project")) {
  project <- .ol_assert_project(project, "Call ol_init() first or set options(ol.project=...).")
  state <- .ol_get_backend_state(project)
  .ol_ensure_refs_table(state)
  conn <- state$conn
  
  ident <- .ol_sql_ident(conn, state, "__ol_refs")
  
  if (is.null(name)) {
    query <- sprintf(
      "SELECT tag, table_name, snapshot FROM %s ORDER BY tag, table_name",
      ident
    )
  } else {
    query <- sprintf(
      "SELECT tag, snapshot, as_of FROM %s WHERE table_name = %s ORDER BY tag",
      ident,
      DBI::dbQuoteString(conn, name)
    )
  }
  
  DBI::dbGetQuery(conn, query)
}

#' List all project-level labels
#' @param project Project name
#' @export
ol_list_labels <- function(project = getOption("ol.project")) {
  project <- .ol_assert_project(project, "Call ol_init() first or set options(ol.project=...).")
  state <- .ol_get_backend_state(project)
  .ol_ensure_refs_table(state)
  conn <- state$conn
  
  ident <- .ol_sql_ident(conn, state, "__ol_refs")
  query <- sprintf(
    "SELECT tag, snapshot FROM %s WHERE table_name = %s ORDER BY tag",
    ident,
    DBI::dbQuoteString(conn, "__project__")
  )
  
  DBI::dbGetQuery(conn, query)
}

#' Drop (delete) a table from the project
#' @param name Name of the table to drop
#' @param project Project name
#' @export
ol_drop <- function(name, project = getOption("ol.project")) {
  project <- .ol_assert_project(project, "Call ol_init() first or set options(ol.project=...).")
  state <- .ol_get_backend_state(project)
  conn <- state$conn

  ident <- .ol_sql_ident(conn, state, name)

  tryCatch({
    DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS %s", ident))
    invisible(TRUE)
  }, error = function(e) {
    stop("Failed to drop table '", name, "': ", conditionMessage(e), call. = FALSE)
  })
}

#' Drop (delete) an object from the project
#' @param name Name of the object to drop
#' @param project Project name
#' @export
ol_drop_object <- function(name, project = getOption("ol.project")) {
  project <- .ol_assert_project(project, "Call ol_init() first or set options(ol.project=...).")
  state <- .ol_get_backend_state(project)
  .ol_ensure_objects_table(state)
  conn <- state$conn

  ident <- .ol_sql_ident(conn, state, "__ol_objects")

  # Check if object exists
  check_query <- sprintf(
    "SELECT COUNT(*) as cnt FROM %s WHERE name = %s",
    ident,
    DBI::dbQuoteString(conn, name)
  )
  count <- DBI::dbGetQuery(conn, check_query)$cnt

  if (count == 0) {
    stop("Object '", name, "' not found", call. = FALSE)
  }

  # Delete all versions of the object
  delete_query <- sprintf(
    "DELETE FROM %s WHERE name = %s",
    ident,
    DBI::dbQuoteString(conn, name)
  )

  tryCatch({
    DBI::dbExecute(conn, delete_query)
    invisible(TRUE)
  }, error = function(e) {
    stop("Failed to drop object '", name, "': ", conditionMessage(e), call. = FALSE)
  })
}

#' View commit history
#' @param project Project name
#' @param n Maximum number of commits to return
#' @export
ol_log_commits <- function(project = getOption("ol.project"), n = 20) {
  project <- .ol_assert_project(project, "Call ol_init() first or set options(ol.project=...).")
  state <- try(.ol_get_backend_state(project), silent = TRUE)
  if (inherits(state, "try-error")) return(utils::head(data.frame()))
  
  .ol_ensure_commits_table(state)
  conn <- state$conn
  ident <- .ol_sql_ident(conn, state, "__ol_commits")
  
  query <- sprintf("SELECT commit_id, note, params_json, created_at FROM %s ORDER BY created_at DESC LIMIT %d", ident, as.integer(n))
  res <- tryCatch(DBI::dbGetQuery(conn, query), error = function(e) data.frame())
  if (nrow(res) > 0) {
    parsed <- lapply(res$params_json, .ol_parse_params_json)
    repro <- lapply(parsed, function(x) {
      r <- .ol_get_nested(x, c("omicslake_repro"))
      if (is.null(r)) {
        r <- .ol_get_nested(x, c("omicslake_repro_auto"))
      }
      r
    })
    agent <- lapply(parsed, function(x) {
      a <- .ol_get_nested(x, c("omicslake_agent"))
      if (is.null(a)) {
        a <- .ol_get_nested(x, c("omicslake_agent_auto"))
      }
      a
    })
    validation <- lapply(parsed, function(x) {
      v <- .ol_get_nested(x, c("omicslake_snapshot_validation"))
      if (is.null(v)) {
        v <- .ol_get_nested(x, c("omicslake_snapshot_validation_auto"))
      }
      v
    })
    res$git_commit <- vapply(
      repro,
      function(x) .ol_as_scalar_character(.ol_get_nested(x, c("git", "commit")), NA_character_),
      character(1)
    )
    res$git_branch <- vapply(
      repro,
      function(x) .ol_as_scalar_character(.ol_get_nested(x, c("git", "branch")), NA_character_),
      character(1)
    )
    res$git_dirty <- vapply(
      repro,
      function(x) {
        val <- .ol_as_scalar_logical(.ol_get_nested(x, c("git", "dirty")), NA)
        if (is.na(val)) NA else isTRUE(val)
      },
      logical(1)
    )
    res$renv_lockfile <- vapply(
      repro,
      function(x) .ol_as_scalar_character(.ol_get_nested(x, c("renv", "lockfile")), NA_character_),
      character(1)
    )
    res$renv_lockfile_md5 <- vapply(
      repro,
      function(x) .ol_as_scalar_character(.ol_get_nested(x, c("renv", "lockfile_md5")), NA_character_),
      character(1)
    )
    res$agent_prompt_id <- vapply(
      agent,
      function(x) .ol_as_scalar_character(.ol_get_nested(x, c("prompt_id")), NA_character_),
      character(1)
    )
    res$agent_run_id <- vapply(
      agent,
      function(x) .ol_as_scalar_character(.ol_get_nested(x, c("run_id")), NA_character_),
      character(1)
    )
    res$agent_name <- vapply(
      agent,
      function(x) .ol_as_scalar_character(.ol_get_nested(x, c("agent_name")), NA_character_),
      character(1)
    )
    res$agent_script_path <- vapply(
      agent,
      function(x) .ol_as_scalar_character(.ol_get_nested(x, c("script_path")), NA_character_),
      character(1)
    )
    res$agent_script_md5 <- vapply(
      agent,
      function(x) .ol_as_scalar_character(.ol_get_nested(x, c("script_md5")), NA_character_),
      character(1)
    )
    res$snapshot_validation_prev <- vapply(
      validation,
      function(x) .ol_as_scalar_character(.ol_get_nested(x, c("previous_label")), NA_character_),
      character(1)
    )
    res$snapshot_validation_structural_changes <- vapply(
      validation,
      function(x) .ol_as_scalar_integer(.ol_get_nested(x, c("summary", "structural_changes")), NA_integer_),
      integer(1)
    )
    res$snapshot_validation_row_count_changes <- vapply(
      validation,
      function(x) .ol_as_scalar_integer(.ol_get_nested(x, c("summary", "row_count_changes")), NA_integer_),
      integer(1)
    )
  }
  
  res
}

#' Get dependencies for a table or object
#' @param name Name of the table or object
#' @param direction Direction to query: "upstream" (parents), "downstream" (children), or "both"
#' @param project Project name
#' @return Data frame with dependency info including version references (parent_ref, parent_version_id)
#' @export
ol_get_dependencies <- function(name, direction = c("upstream", "downstream", "both"), project = getOption("ol.project")) {
  direction <- match.arg(direction)
  project <- .ol_assert_project(project, "Call ol_init() first or set options(ol.project=...).")
  state <- .ol_get_backend_state(project)

  .ol_ensure_dependencies_table(state)
  conn <- state$conn
  ident <- .ol_sql_ident(conn, state, "__ol_dependencies")

  if (direction == "upstream") {
    query <- sprintf(
      "SELECT parent_name, parent_type, relationship_type, created_at, parent_ref, parent_version_id FROM %s WHERE child_name = %s ORDER BY created_at DESC",
      ident,
      DBI::dbQuoteString(conn, name)
    )
  } else if (direction == "downstream") {
    query <- sprintf(
      "SELECT child_name, child_type, relationship_type, created_at, child_version_id FROM %s WHERE parent_name = %s ORDER BY created_at DESC",
      ident,
      DBI::dbQuoteString(conn, name)
    )
  } else {
    upstream_query <- sprintf(
      "SELECT parent_name AS name, parent_type AS type, 'upstream' AS direction, relationship_type, created_at, parent_ref, parent_version_id FROM %s WHERE child_name = %s",
      ident,
      DBI::dbQuoteString(conn, name)
    )
    downstream_query <- sprintf(
      "SELECT child_name AS name, child_type AS type, 'downstream' AS direction, relationship_type, created_at, NULL as parent_ref, child_version_id as parent_version_id FROM %s WHERE parent_name = %s",
      ident,
      DBI::dbQuoteString(conn, name)
    )
    query <- sprintf("(%s) UNION ALL (%s) ORDER BY created_at DESC", upstream_query, downstream_query)
  }

  DBI::dbGetQuery(conn, query)
}

#' Show lineage (full dependency tree) for a table or object
#' @param name Name of the table or object
#' @param direction Direction to traverse: "upstream" (all ancestors) or "downstream" (all descendants)
#' @param max_depth Maximum depth to traverse (default: 10)
#' @param project Project name
#' @export
ol_show_lineage <- function(name, direction = c("upstream", "downstream"), max_depth = 10, project = getOption("ol.project")) {
  direction <- match.arg(direction)
  project <- .ol_assert_project(project, "Call ol_init() first or set options(ol.project=...).")
  state <- .ol_get_backend_state(project)
  
  .ol_ensure_dependencies_table(state)
  conn <- state$conn
  
  visited <- character(0)
  to_visit <- data.frame(
    name = name,
    depth = 0,
    stringsAsFactors = FALSE
  )
  result <- list()
  
  while (nrow(to_visit) > 0 && min(to_visit$depth) < max_depth) {
    current_row <- to_visit[1, ]
    to_visit <- to_visit[-1, , drop = FALSE]
    current_name <- current_row$name
    current_depth <- current_row$depth
    
    if (current_name %in% visited) next
    visited <- c(visited, current_name)
    
    deps <- ol_get_dependencies(current_name, direction = direction, project = project)
    
    if (nrow(deps) > 0) {
      if (direction == "upstream") {
        result[[length(result) + 1]] <- data.frame(
          child = current_name,
          parent = deps$parent_name,
          parent_type = deps$parent_type,
          relationship = deps$relationship_type,
          depth = current_depth,
          stringsAsFactors = FALSE
        )
        new_names <- deps$parent_name[!deps$parent_name %in% visited]
        if (length(new_names) > 0) {
          new_to_visit <- data.frame(
            name = new_names,
            depth = current_depth + 1,
            stringsAsFactors = FALSE
          )
          to_visit <- rbind(to_visit, new_to_visit)
        }
      } else {
        result[[length(result) + 1]] <- data.frame(
          parent = current_name,
          child = deps$child_name,
          child_type = deps$child_type,
          relationship = deps$relationship_type,
          depth = current_depth,
          stringsAsFactors = FALSE
        )
        new_names <- deps$child_name[!deps$child_name %in% visited]
        if (length(new_names) > 0) {
          new_to_visit <- data.frame(
            name = new_names,
            depth = current_depth + 1,
            stringsAsFactors = FALSE
          )
          to_visit <- rbind(to_visit, new_to_visit)
        }
      }
    }
  }
  
  if (length(result) == 0) {
    return(data.frame())
  }
  
  do.call(rbind, result)
}
