# Internal registry for Iceberg-backed projects
.ol_iceberg_registry <- new.env(parent = emptyenv())

.ol_iceberg_key <- function(project) {
  .ol_assert_project(project, "Call ol_init_iceberg() first or set options(ol.project=...).")
}

.ol_get_iceberg_state <- function(project) {
  key <- .ol_iceberg_key(project)
  state <- .ol_iceberg_registry[[key]]
  if (is.null(state)) stop("Iceberg backend not initialized for project: ", project, call. = FALSE)
  state
}

.ol_store_iceberg_state <- function(project, state) {
  assign(.ol_iceberg_key(project), state, envir = .ol_iceberg_registry)
  invisible(state)
}

.ol_disconnect_iceberg <- function(project) {
  key <- .ol_iceberg_key(project)
  state <- .ol_iceberg_registry[[key]]
  if (!is.null(state) && inherits(state$conn, "duckdb_connection")) {
    try(DBI::dbDisconnect(state$conn, shutdown = FALSE), silent = TRUE)
  }
  rm(list = key, envir = .ol_iceberg_registry)
  invisible(TRUE)
}

.ol_iceberg_schema_sql <- function(conn, state) {
  if (nzchar(state$catalog_name)) {
    paste(DBI::dbQuoteIdentifier(conn, c(state$catalog_name, state$namespace)), collapse = ".")
  } else {
    DBI::dbQuoteIdentifier(conn, state$namespace)
  }
}

.ol_iceberg_sql_ident <- function(conn, state, name) {
  paste0(.ol_iceberg_schema_sql(conn, state), ".", DBI::dbQuoteIdentifier(conn, name))
}

.ol_iceberg_qualified_name <- function(state, name) {
  if (nzchar(state$catalog_name)) {
    paste(state$catalog_name, state$namespace, name, sep = ".")
  } else {
    paste(state$namespace, name, sep = ".")
  }
}

.ol_ensure_refs_table <- function(state) {
  conn <- state$conn
  sql <- sprintf(
    "CREATE TABLE IF NOT EXISTS %s (table_name TEXT, tag TEXT, snapshot TEXT, as_of TIMESTAMP)",
    .ol_iceberg_sql_ident(conn, state, "__ol_refs")
  )
  tryCatch(DBI::dbExecute(conn, sql), error = function(e) {
    msg <- conditionMessage(e)
    if (!grepl("already", msg, ignore.case = TRUE)) stop(e)
  })
}

.ol_ensure_objects_table <- function(state) {
  conn <- state$conn
  sql <- sprintf(
    "CREATE TABLE IF NOT EXISTS %s (name TEXT, version_ts TIMESTAMP, mime TEXT, bytes BLOB)",
    .ol_iceberg_sql_ident(conn, state, "__ol_objects")
  )
  tryCatch(DBI::dbExecute(conn, sql), error = function(e) {
    msg <- conditionMessage(e)
    if (!grepl("already", msg, ignore.case = TRUE)) stop(e)
  })
}

.ol_ensure_commits_table <- function(state) {
  conn <- state$conn
  sql <- sprintf(
    "CREATE TABLE IF NOT EXISTS %s (commit_id TEXT, note TEXT, params_json TEXT, created_at TIMESTAMP)",
    .ol_iceberg_sql_ident(conn, state, "__ol_commits")
  )
  tryCatch(DBI::dbExecute(conn, sql), error = function(e) {
    msg <- conditionMessage(e)
    if (!grepl("already", msg, ignore.case = TRUE)) stop(e)
  })
}

.ol_ensure_dependencies_table <- function(state) {
  conn <- state$conn
  sql <- sprintf(
    "CREATE TABLE IF NOT EXISTS %s (child_name TEXT, child_type TEXT, parent_name TEXT, parent_type TEXT, relationship_type TEXT, created_at TIMESTAMP)",
    .ol_iceberg_sql_ident(conn, state, "__ol_dependencies")
  )
  tryCatch(DBI::dbExecute(conn, sql), error = function(e) {
    msg <- conditionMessage(e)
    if (!grepl("already", msg, ignore.case = TRUE)) stop(e)
  })
}

.ol_record_dependency <- function(state, child_name, child_type, parent_name, parent_type, relationship_type = "derived_from") {
  .ol_ensure_dependencies_table(state)
  conn <- state$conn
  
  dep_data <- data.frame(
    child_name = child_name,
    child_type = child_type,
    parent_name = parent_name,
    parent_type = parent_type,
    relationship_type = relationship_type,
    created_at = as.POSIXct(Sys.time(), tz = "UTC"),
    stringsAsFactors = FALSE
  )
  
  DBI::dbAppendTable(conn, DBI::Id(schema = state$namespace, table = "__ol_dependencies"), dep_data)
#' Record multiple dependencies for a child entity
#' @param state Iceberg state object
#' @param child_name Name of the child entity
#' @param child_type Type of child entity ("table", "object", "view")
#' @param depends_on Character vector of parent names
#' @keywords internal
.ol_record_dependencies <- function(state, child_name, child_type, depends_on) {
  if (!is.null(depends_on) && length(depends_on) > 0) {
    if (!is.character(depends_on)) {
      stop("depends_on must be a character vector", call. = FALSE)
    }
    
    for (parent in depends_on) {
      parent_type <- if (.ol_is_object(state, parent)) "object" else "table"
      .ol_record_dependency(state, child_name, child_type, parent, parent_type)
    }
  }
}

}
.ol_is_object <- function(state, name) {
  .ol_ensure_objects_table(state)
  conn <- state$conn
  ident <- .ol_iceberg_sql_ident(conn, state, "__ol_objects")
  query <- sprintf("SELECT COUNT(*) as cnt FROM %s WHERE name = %s", ident, DBI::dbQuoteString(conn, name))
  res <- DBI::dbGetQuery(conn, query)
  res$cnt[1] > 0
}



#' Initialize DuckDB + Iceberg backend
#' @export
ol_init_iceberg <- function(project, engine = "duckdb", catalog = NULL, namespace = "ol", object_mode = c("blobs", "external"), object_root = NULL) {
  object_mode <- match.arg(object_mode)
  if (!identical(engine, "duckdb")) stop("Unsupported Iceberg engine: ", engine, call. = FALSE)
  .ol_require(c("DBI", "duckdb", "arrow"))
  project <- .ol_assert_project(project, "project must be specified")
  if (!is.null(object_root)) object_root <- .ol_norm(object_root)
  pr <- .ol_proj_root(project)
  dir.create(pr, recursive = TRUE, showWarnings = FALSE)
  if (is.null(catalog)) catalog <- file.path(pr, "iceberg")
  catalog <- .ol_norm(catalog)
  dir.create(catalog, recursive = TRUE, showWarnings = FALSE)
  if (identical(object_mode, "external")) {
    if (is.null(object_root)) object_root <- file.path(pr, "objects")
    object_root <- .ol_norm(object_root)
    dir.create(object_root, recursive = TRUE, showWarnings = FALSE)
  }
  dbfile <- file.path(pr, "duckdb.db")
  conn <- DBI::dbConnect(duckdb::duckdb(), dbdir = dbfile, read_only = FALSE)
  catalog_name <- ""
  schema_sql <- DBI::dbQuoteIdentifier(conn, namespace)
  DBI::dbExecute(conn, sprintf("CREATE SCHEMA IF NOT EXISTS %s", schema_sql))
  state <- list(
    project = project,
    engine = engine,
    conn = conn,
    catalog_name = catalog_name,
    catalog_path = catalog,
    namespace = namespace,
    object_mode = object_mode,
    object_root = if (identical(object_mode, "external")) object_root else NULL
  )
  .ol_store_iceberg_state(project, state)
  options(ol.project = project)
  invisible(state)
}

.ol_ref_parse <- function(ref) {
  if (is.null(ref) || identical(ref, "@latest")) return(list(type = "latest"))
  if (!is.character(ref) || !length(ref)) return(list(type = "latest"))
  ref <- ref[[1]]
  if (!startsWith(ref, "@")) return(list(type = "tag", value = ref))
  inner <- substring(ref, 2L)
  if (startsWith(inner, "tag(") && endsWith(inner, ")")) {
    value <- substring(inner, 5L, nchar(inner) - 1L)
    return(list(type = "tag", value = value))
  }
  list(type = "tag", value = inner)
}

.ol_get_backup_table_name <- function(table_name, tag) {
  paste0("__ol_backup_", gsub("[^a-zA-Z0-9_]", "_", table_name), "_", gsub("[^a-zA-Z0-9_]", "_", tag))
}

.ol_iceberg_resolve_reference <- function(state, name, ref) {
  parsed <- .ol_ref_parse(ref)
  if (identical(parsed$type, "latest")) {
    return(list(backup_table = NULL))
  }
  if (identical(parsed$type, "tag")) {
    .ol_ensure_refs_table(state)
    conn <- state$conn
    ident <- .ol_iceberg_sql_ident(conn, state, "__ol_refs")
    sql <- sprintf(
      "SELECT snapshot FROM %s WHERE table_name = %s AND tag = %s ORDER BY as_of DESC LIMIT 1",
      ident,
      DBI::dbQuoteString(conn, name),
      DBI::dbQuoteString(conn, parsed$value)
    )
    res <- DBI::dbGetQuery(conn, sql)
    if (!nrow(res)) stop("Unknown tag: ", parsed$value, call. = FALSE)
    return(list(backup_table = res$snapshot[[1]]))
  }
  list(backup_table = NULL)
}

.ol_get_table_sql <- function(state, name, resolved) {
  conn <- state$conn
  if (is.null(resolved$backup_table)) {
    ident <- .ol_iceberg_sql_ident(conn, state, name)
    sprintf("SELECT * FROM %s", ident)
  } else {
    backup_ident <- .ol_iceberg_sql_ident(conn, state, resolved$backup_table)
    sprintf("SELECT * FROM %s", backup_ident)
  }
}

#' Tag a table by creating a backup
#' @export
ol_tag <- function(name, tag, ref = "@latest", project = getOption("ol.project")) {
  project <- .ol_assert_project(project, "Call ol_init_iceberg() first or set options(ol.project=...).")
  if (missing(name) || !nzchar(name)) stop("name must be provided", call. = FALSE)
  if (missing(tag) || !nzchar(tag)) stop("tag must be provided", call. = FALSE)
  state <- .ol_get_iceberg_state(project)
  conn <- state$conn
  
  backup_table <- .ol_get_backup_table_name(name, tag)
  source_ident <- .ol_iceberg_sql_ident(conn, state, name)
  backup_ident <- .ol_iceberg_sql_ident(conn, state, backup_table)
  
  create_backup_sql <- sprintf(
    "CREATE OR REPLACE TABLE %s AS SELECT * FROM %s",
    backup_ident,
    source_ident
  )
  tryCatch(DBI::dbExecute(conn, create_backup_sql), error = function(e) {
    stop("Failed to create backup for table '", name, "': ", conditionMessage(e), call. = FALSE)
  })
  
  .ol_ensure_refs_table(state)
  ident <- .ol_iceberg_sql_ident(conn, state, "__ol_refs")
  delete_sql <- sprintf(
    "DELETE FROM %s WHERE table_name = %s AND tag = %s",
    ident,
    DBI::dbQuoteString(conn, name),
    DBI::dbQuoteString(conn, tag)
  )
  DBI::dbExecute(conn, delete_sql)
  insert_sql <- sprintf(
    "INSERT INTO %s (table_name, tag, snapshot, as_of) VALUES (%s, %s, %s, CURRENT_TIMESTAMP)",
    ident,
    DBI::dbQuoteString(conn, name),
    DBI::dbQuoteString(conn, tag),
    DBI::dbQuoteString(conn, backup_table)
  )
  DBI::dbExecute(conn, insert_sql)
  invisible(backup_table)
}

#' Tag a stored object version
#' @param name Name of the object to tag
#' @param tag Tag name to assign
#' @param when Which version to tag: "latest" (default) or "first", or a specific version_ts timestamp
#' @param project Project name
#' @export
ol_tag_object <- function(name, tag, when = "latest", project = getOption("ol.project")) {
  .ol_validate_name(name, "object name")
  .ol_validate_name(tag, "tag")
  project <- .ol_assert_project(project, "Call ol_init_iceberg() first or set options(ol.project=...).")
  state <- .ol_get_iceberg_state(project)
  .ol_ensure_objects_table(state)
  conn <- state$conn
  
  ident_objects <- .ol_iceberg_sql_ident(conn, state, "__ol_objects")
  if (is.character(when) && when %in% c("latest", "first")) {
    order_dir <- if (identical(when, "latest")) "DESC" else "ASC"
    query <- sprintf(
      "SELECT version_ts FROM %s WHERE name = %s ORDER BY version_ts %s LIMIT 1",
      ident_objects,
      DBI::dbQuoteString(conn, name),
      order_dir
    )
    res <- DBI::dbGetQuery(conn, query)
    if (!nrow(res)) stop("Object not found: ", name, call. = FALSE)
    version_ts <- res$version_ts[[1]]
  } else {
    version_ts <- when
  }
  
  .ol_ensure_refs_table(state)
  ident_refs <- .ol_iceberg_sql_ident(conn, state, "__ol_refs")
  ref_name <- paste0("__object__", name)
  
  delete_sql <- sprintf(
    "DELETE FROM %s WHERE table_name = %s AND tag = %s",
    ident_refs,
    DBI::dbQuoteString(conn, ref_name),
    DBI::dbQuoteString(conn, tag)
  )
  DBI::dbExecute(conn, delete_sql)
  
  insert_sql <- sprintf(
    "INSERT INTO %s (table_name, tag, snapshot, as_of) VALUES (%s, %s, %s, CURRENT_TIMESTAMP)",
    ident_refs,
    DBI::dbQuoteString(conn, ref_name),
    DBI::dbQuoteString(conn, tag),
    DBI::dbQuoteString(conn, as.character(version_ts))
  )
  DBI::dbExecute(conn, insert_sql)
  
  invisible(version_ts)
}

#' Restore entire project to a labeled state
#'
#' @param label The label name to restore to
#' @param project The project name
#' @return Invisible TRUE on success, FALSE if no tables found
#' @export
ol_checkout <- function(label, project = getOption("ol.project")) {
  project <- .ol_assert_project(project, "Call ol_init_iceberg() first or set options(ol.project=...).")
  state <- .ol_get_iceberg_state(project)
  .ol_ensure_refs_table(state)
  conn <- state$conn
  
  ident <- .ol_iceberg_sql_ident(conn, state, "__ol_refs")
  query <- sprintf(
    "SELECT table_name, snapshot FROM %s WHERE tag = %s AND table_name != %s",
    ident,
    DBI::dbQuoteString(conn, label),
    DBI::dbQuoteString(conn, "__project__")
  )
  ref_entries <- DBI::dbGetQuery(conn, query)
  
  if (nrow(ref_entries) == 0) {
    warning("No tables found with label: ", label, call. = FALSE)
    return(invisible(FALSE))
  }
  
  for (i in seq_len(nrow(ref_entries))) {
    tbl <- ref_entries$table_name[[i]]
    backup_table <- ref_entries$snapshot[[i]]
    
    tryCatch({
      backup_ident <- .ol_iceberg_sql_ident(conn, state, backup_table)
      target_ident <- .ol_iceberg_sql_ident(conn, state, tbl)
      restore_sql <- sprintf(
        "CREATE OR REPLACE TABLE %s AS SELECT * FROM %s",
        target_ident,
        backup_ident
      )
      DBI::dbExecute(conn, restore_sql)
    }, error = function(e) {
      warning("Failed to restore table '", tbl, "': ", conditionMessage(e), call. = FALSE)
    })
  }
  
  invisible(TRUE)
}

#' Read a stored object
#' @param name Name of the object to read
#' @param ref Reference to read from (e.g., "@latest", "@tag", "v1.0"). Defaults to "@latest"
#' @param when Deprecated. Use ref parameter instead. If provided, "latest" or "first"
#' @param project Project name
#' @export
ol_read_object <- function(name, ref = "@latest", when = NULL, project = getOption("ol.project")) {
  .ol_validate_name(name, "object name")
  project <- .ol_assert_project(project, "Call ol_init_iceberg() first or set options(ol.project=...).")
  state <- .ol_get_iceberg_state(project)
  .ol_ensure_objects_table(state)
  conn <- state$conn
  ident <- .ol_iceberg_sql_ident(conn, state, "__ol_objects")
  
  if (!is.null(when)) {
    when <- match.arg(when, c("latest", "first"))
    ref <- if (identical(when, "latest")) "@latest" else "@first"
  }
  
  parsed <- .ol_ref_parse(ref)
  
  version_ts_filter <- NULL
  if (identical(parsed$type, "latest")) {
    order_dir <- "DESC"
  } else if (identical(ref, "@first")) {
    order_dir <- "ASC"
  } else if (identical(parsed$type, "tag")) {
    .ol_ensure_refs_table(state)
    ident_refs <- .ol_iceberg_sql_ident(conn, state, "__ol_refs")
    ref_name <- paste0("__object__", name)
    query <- sprintf(
      "SELECT snapshot FROM %s WHERE table_name = %s AND tag = %s ORDER BY as_of DESC LIMIT 1",
      ident_refs,
      DBI::dbQuoteString(conn, ref_name),
      DBI::dbQuoteString(conn, parsed$value)
    )
    res <- DBI::dbGetQuery(conn, query)
    if (!nrow(res)) stop("Tag not found for object '", name, "': ", parsed$value, call. = FALSE)
    version_ts_filter <- res$snapshot[[1]]
  } else {
    stop("Unsupported reference type for objects: ", parsed$type, call. = FALSE)
  }
  
  if (is.null(version_ts_filter)) {
    sql <- sprintf(
      "SELECT name, version_ts, mime, bytes FROM %s WHERE name = %s ORDER BY version_ts %s LIMIT 1",
      ident,
      DBI::dbQuoteString(conn, name),
      order_dir
    )
  } else {
    sql <- sprintf(
      "SELECT name, version_ts, mime, bytes FROM %s WHERE name = %s AND version_ts = %s LIMIT 1",
      ident,
      DBI::dbQuoteString(conn, name),
      DBI::dbQuoteString(conn, version_ts_filter)
    )
  }
  
  res <- DBI::dbGetQuery(conn, sql)
  if (!nrow(res)) stop("Object not found: ", name, call. = FALSE)
  payload <- res$bytes[[1]]
  if (is.list(payload)) payload <- payload[[1]]
  if (state$object_mode == "external") {
    path <- rawToChar(payload)
    if (!file.exists(path)) stop("External object missing: ", path, call. = FALSE)
    return(readRDS(path))
  }
  unserialize(payload)
}
