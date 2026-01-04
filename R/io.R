#' Write a table using the DuckDB backend
#' @param depends_on Optional character vector of table/object names that this table depends on
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
#' @param depends_on Optional character vector of table/object names that this object depends on
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
#' @export
ol_commit <- function(note = "", params = list(), project = getOption("ol.project")) {
  project <- .ol_assert_project(project, "Call ol_init() first or set options(ol.project=...).")
  state <- .ol_get_backend_state(project)
  commit_id <- format(Sys.time(), "%Y%m%d-%H%M%S")
  
  .ol_ensure_commits_table(state)
  conn <- state$conn
  
  params_json <- if (length(params) > 0) jsonlite::toJSON(params, auto_unbox = TRUE) else ""
  
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
#' @export
ol_read <- function(name, ref = "@latest", project = getOption("ol.project"), collect = TRUE) {
  project <- .ol_assert_project(project, "Call ol_init() first or set options(ol.project=...).")
  state <- .ol_get_backend_state(project)
  resolved <- .ol_resolve_reference(state, name, ref)
  sql <- .ol_get_table_sql(state, name, resolved)
  if (!isTRUE(collect)) {
    .ol_require(c("dplyr", "dbplyr"))
    return(dplyr::tbl(state$conn, dbplyr::sql(sql)))
  }
  res <- tryCatch(DBI::dbGetQuery(state$conn, sql), error = function(e) {
    tryCatch({
      obj <- ol_read_object(name, project = project)
      attr(obj, "ol.ref") <- ref
      obj
    }, error = function(e2) stop(e))
  })
  if (is.data.frame(res)) {
    rownames(res) <- NULL
  }
  res
}

#' @export
ol_load <- function(name, ref = "@latest", project = getOption("ol.project")) ol_read(name, ref, project)

#' Return version log for a table
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
  tables <- setdiff(tables, c("__ol_refs", "__ol_objects", "__ol_commits"))
  
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
