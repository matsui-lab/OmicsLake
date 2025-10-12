#' Write a table using the Iceberg backend
#' @export
ol_write <- function(name, data, project = getOption("ol.project"), mode = c("create", "overwrite", "append")) {
  .ol_require(c("arrow", "duckdb"))
  .ol_validate_name(name, "table name")
  .ol_validate_data_frame(data)
  project <- .ol_assert_project(project, "Call ol_init_iceberg() first or set options(ol.project=...).")
  state <- .ol_get_iceberg_state(project)
  mode <- match.arg(mode)
  conn <- state$conn
  schema_sql <- .ol_iceberg_schema_sql(conn, state)
  DBI::dbExecute(conn, sprintf("CREATE SCHEMA IF NOT EXISTS %s", schema_sql))
  tbl <- arrow::arrow_table(data)
  tmp_name <- paste0("ol_tmp_", as.integer(as.numeric(Sys.time()) * 1e6))
  duckdb::duckdb_register_arrow(conn, tmp_name, tbl)
  on.exit(duckdb::duckdb_unregister_arrow(conn, tmp_name), add = TRUE)
  ident <- .ol_iceberg_sql_ident(conn, state, name)
  sql <- switch(mode,
    create = sprintf("CREATE TABLE %s USING ICEBERG AS SELECT * FROM %s", ident, DBI::dbQuoteIdentifier(conn, tmp_name)),
    overwrite = sprintf("CREATE OR REPLACE TABLE %s USING ICEBERG AS SELECT * FROM %s", ident, DBI::dbQuoteIdentifier(conn, tmp_name)),
    append = sprintf("INSERT INTO %s SELECT * FROM %s", ident, DBI::dbQuoteIdentifier(conn, tmp_name))
  )
  DBI::dbExecute(conn, sql)
  invisible(.ol_iceberg_qualified_name(state, name))
}

#' Save an R object via the Iceberg metadata table
#' @export
ol_save <- function(name, object, project = getOption("ol.project"), mime = NULL) {
  .ol_validate_name(name, "object name")
  project <- .ol_assert_project(project, "Call ol_init_iceberg() first or set options(ol.project=...).")
  state <- .ol_get_iceberg_state(project)
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
  invisible(TRUE)
}

#' Commit with metadata (note and parameters)
#' @param note Commit message describing the changes
#' @param params Named list of parameters to store with the commit
#' @param project Project name
#' @export
ol_commit <- function(note = "", params = list(), project = getOption("ol.project")) {
  project <- .ol_assert_project(project, "Call ol_init_iceberg() first or set options(ol.project=...).")
  state <- .ol_get_iceberg_state(project)
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
  project <- .ol_assert_project(project, "Call ol_init_iceberg() first or set options(ol.project=...).")
  state <- .ol_get_iceberg_state(project)
  resolved <- .ol_iceberg_resolve_reference(state, name, ref)
  sql <- .ol_iceberg_scan_sql(state, name, resolved)
  if (!isTRUE(collect)) {
    .ol_require("dplyr")
    return(dplyr::tbl(state$conn, DBI::sql(sql)))
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

#' Return Iceberg snapshot log for a table
#' @export
ol_log <- function(name = NULL, project = getOption("ol.project")) {
  project <- .ol_assert_project(project, "Call ol_init_iceberg() first or set options(ol.project=...).")
  if (is.null(name)) return(utils::head(data.frame()))
  state <- .ol_get_iceberg_state(project)
  ident <- .ol_iceberg_qualified_name(state, name)
  queries <- c(
    sprintf("SELECT * FROM iceberg_snapshots('%s') ORDER BY committed_at DESC", ident),
    sprintf("SELECT snapshot_id, committed_at FROM iceberg_snapshots('%s') ORDER BY committed_at DESC", ident)
  )
  for (q in queries) {
    res <- tryCatch(DBI::dbGetQuery(state$conn, q), error = function(e) NULL)
    if (!is.null(res)) return(res)
  }
  utils::head(data.frame())
}

#' List all tables in the project
#' @param project Project name
#' @export
ol_list_tables <- function(project = getOption("ol.project")) {
  project <- .ol_assert_project(project, "Call ol_init_iceberg() first or set options(ol.project=...).")
  state <- .ol_get_iceberg_state(project)
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
  project <- .ol_assert_project(project, "Call ol_init_iceberg() first or set options(ol.project=...).")
  state <- .ol_get_iceberg_state(project)
  .ol_ensure_objects_table(state)
  conn <- state$conn
  
  ident <- .ol_iceberg_sql_ident(conn, state, "__ol_objects")
  query <- sprintf("SELECT DISTINCT name FROM %s ORDER BY name", ident)
  DBI::dbGetQuery(conn, query)
}

#' List all tags for a table or all project labels
#' @param name Optional table name to list tags for. If NULL, lists all tags.
#' @param project Project name
#' @export
ol_list_tags <- function(name = NULL, project = getOption("ol.project")) {
  project <- .ol_assert_project(project, "Call ol_init_iceberg() first or set options(ol.project=...).")
  state <- .ol_get_iceberg_state(project)
  .ol_ensure_refs_table(state)
  conn <- state$conn
  
  ident <- .ol_iceberg_sql_ident(conn, state, "__ol_refs")
  
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
  project <- .ol_assert_project(project, "Call ol_init_iceberg() first or set options(ol.project=...).")
  state <- .ol_get_iceberg_state(project)
  .ol_ensure_refs_table(state)
  conn <- state$conn
  
  ident <- .ol_iceberg_sql_ident(conn, state, "__ol_refs")
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
  project <- .ol_assert_project(project, "Call ol_init_iceberg() first or set options(ol.project=...).")
  state <- .ol_get_iceberg_state(project)
  conn <- state$conn
  
  ident <- .ol_iceberg_sql_ident(conn, state, name)
  
  tryCatch({
    DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS %s", ident))
    invisible(TRUE)
  }, error = function(e) {
    stop("Failed to drop table '", name, "': ", conditionMessage(e), call. = FALSE)
  })
}

#' View commit history
#' @param project Project name
#' @param n Maximum number of commits to return
#' @export
ol_log_commits <- function(project = getOption("ol.project"), n = 20) {
  project <- .ol_assert_project(project, "Call ol_init_iceberg() first or set options(ol.project=...).")
  state <- try(.ol_get_iceberg_state(project), silent = TRUE)
  if (inherits(state, "try-error")) return(utils::head(data.frame()))
  
  .ol_ensure_commits_table(state)
  conn <- state$conn
  ident <- .ol_iceberg_sql_ident(conn, state, "__ol_commits")
  
  query <- sprintf("SELECT commit_id, note, params_json, created_at FROM %s ORDER BY created_at DESC LIMIT %d", ident, as.integer(n))
  res <- tryCatch(DBI::dbGetQuery(conn, query), error = function(e) data.frame())
  
  res
}
