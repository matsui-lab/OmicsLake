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
  paste(DBI::dbQuoteIdentifier(conn, c(state$catalog_name, state$namespace)), collapse = ".")
}

.ol_iceberg_sql_ident <- function(conn, state, name) {
  paste0(.ol_iceberg_schema_sql(conn, state), ".", DBI::dbQuoteIdentifier(conn, name))
}

.ol_iceberg_qualified_name <- function(state, name) {
  paste(state$catalog_name, state$namespace, name, sep = ".")
}

.ol_ensure_refs_table <- function(state) {
  conn <- state$conn
  sql <- sprintf(
    "CREATE TABLE %s USING ICEBERG AS SELECT * FROM (SELECT CAST('' AS TEXT) AS table_name, CAST('' AS TEXT) AS tag, CAST('' AS TEXT) AS snapshot, CAST(NULL AS TIMESTAMP) AS as_of) WHERE 1=0",
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
    "CREATE TABLE %s USING ICEBERG AS SELECT * FROM (SELECT CAST('' AS TEXT) AS name, CURRENT_TIMESTAMP AS version_ts, CAST('' AS TEXT) AS mime, CAST('' AS BLOB) AS bytes) WHERE 1=0",
    .ol_iceberg_sql_ident(conn, state, "__ol_objects")
  )
  tryCatch(DBI::dbExecute(conn, sql), error = function(e) {
    msg <- conditionMessage(e)
    if (!grepl("already", msg, ignore.case = TRUE)) stop(e)
  })
}

.ol_register_catalog <- function(conn, catalog_name, catalog_path) {
  escaped <- gsub("'", "''", catalog_path)
  statements <- c(
    sprintf("ATTACH '%s' AS %s (TYPE ICEBERG)", escaped, DBI::dbQuoteIdentifier(conn, catalog_name)),
    sprintf("CALL create_iceberg_catalog('%s', 'duckdb', map_value(['type','path'], ['duckdb','%s']))", catalog_name, escaped),
    sprintf("CALL create_iceberg_catalog('%s', 'duckdb', {'type':'duckdb','path':'%s'})", catalog_name, escaped)
  )
  success <- FALSE
  for (stmt in statements) {
    success <- tryCatch({
      DBI::dbExecute(conn, stmt)
      TRUE
    }, error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("already", msg, ignore.case = TRUE)) return(TRUE)
      FALSE
    })
    if (isTRUE(success)) break
  }
  invisible(TRUE)
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
  for (stmt in c("INSTALL iceberg", "LOAD iceberg")) {
    tryCatch(DBI::dbExecute(conn, stmt), error = function(e) {
      msg <- conditionMessage(e)
      if (!grepl("already", msg, ignore.case = TRUE)) stop(e)
    })
  }
  catalog_name <- paste0("ol_", gsub("[^A-Za-z0-9_]", "_", basename(catalog)))
  .ol_register_catalog(conn, catalog_name, catalog)
  schema_sql <- paste(DBI::dbQuoteIdentifier(conn, c(catalog_name, namespace)), collapse = ".")
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
  if (!is.character(ref) || !length(ref)) return(list(type = "snapshot", value = as.character(ref)))
  ref <- ref[[1]]
  if (!startsWith(ref, "@")) return(list(type = "snapshot", value = ref))
  inner <- substring(ref, 2L)
  if (startsWith(inner, "version(") && endsWith(inner, ")")) {
    value <- substring(inner, 9L, nchar(inner) - 1L)
    if (!nzchar(value)) return(list(type = "latest"))
    if (!is.na(as.POSIXct(value, tz = "UTC", tryFormats = c("%Y%m%d-%H%M%S", "%Y-%m-%d %H:%M:%S")))) {
      return(list(type = "as_of", value = value))
    }
    return(list(type = "snapshot", value = value))
  }
  if (startsWith(inner, "tag(") && endsWith(inner, ")")) {
    value <- substring(inner, 5L, nchar(inner) - 1L)
    return(list(type = "tag", value = value))
  }
  list(type = "tag", value = inner)
}

.ol_iceberg_latest_snapshot <- function(state, name) {
  conn <- state$conn
  ident <- .ol_iceberg_qualified_name(state, name)
  sql <- sprintf("SELECT snapshot_id, committed_at FROM iceberg_snapshots('%s') ORDER BY committed_at DESC LIMIT 1", ident)
  res <- tryCatch(DBI::dbGetQuery(conn, sql), error = function(e) data.frame())
  if (nrow(res)) return(res$snapshot_id[[1]])
  NULL
}

.ol_iceberg_resolve_reference <- function(state, name, ref) {
  parsed <- .ol_ref_parse(ref)
  if (identical(parsed$type, "latest")) {
    return(list(snapshot = NULL, as_of = NULL))
  }
  if (identical(parsed$type, "snapshot")) {
    return(list(snapshot = parsed$value, as_of = NULL))
  }
  if (identical(parsed$type, "as_of")) {
    return(list(snapshot = NULL, as_of = parsed$value))
  }
  if (identical(parsed$type, "tag")) {
    .ol_ensure_refs_table(state)
    conn <- state$conn
    ident <- .ol_iceberg_sql_ident(conn, state, "__ol_refs")
    sql <- sprintf(
      "SELECT snapshot, as_of FROM %s WHERE table_name = %s AND tag = %s ORDER BY as_of DESC LIMIT 1",
      ident,
      DBI::dbQuoteString(conn, name),
      DBI::dbQuoteString(conn, parsed$value)
    )
    res <- DBI::dbGetQuery(conn, sql)
    if (!nrow(res)) stop("Unknown tag: ", parsed$value, call. = FALSE)
    as_of <- res$as_of[[1]]
    if (length(as_of) && is.na(as_of)) as_of <- NULL
    return(list(snapshot = res$snapshot[[1]], as_of = as_of))
  }
  list(snapshot = NULL, as_of = NULL)
}

.ol_iceberg_scan_sql <- function(state, name, resolved) {
  ident <- .ol_iceberg_qualified_name(state, name)
  args <- sprintf("table => '%s'", ident)
  if (!is.null(resolved$snapshot)) {
    args <- paste(args, sprintf(", snapshot_id => '%s'", resolved$snapshot), sep = "")
  } else if (!is.null(resolved$as_of)) {
    args <- paste(args, sprintf(", as_of => '%s'", resolved$as_of), sep = "")
  }
  sprintf("SELECT * FROM iceberg_scan(%s)", args)
}

#' Tag an Iceberg snapshot
#' @export
ol_tag <- function(name, tag, ref = "@latest", project = getOption("ol.project")) {
  project <- .ol_assert_project(project, "Call ol_init_iceberg() first or set options(ol.project=...).")
  if (missing(name) || !nzchar(name)) stop("name must be provided", call. = FALSE)
  if (missing(tag) || !nzchar(tag)) stop("tag must be provided", call. = FALSE)
  state <- .ol_get_iceberg_state(project)
  resolved <- .ol_iceberg_resolve_reference(state, name, ref)
  snapshot <- resolved$snapshot
  if (is.null(snapshot)) {
    snapshot <- .ol_iceberg_latest_snapshot(state, name)
  }
  if (is.null(snapshot)) stop("No snapshot found for table: ", name, call. = FALSE)
  .ol_ensure_refs_table(state)
  conn <- state$conn
  ident <- .ol_iceberg_sql_ident(conn, state, "__ol_refs")
  delete_sql <- sprintf(
    "DELETE FROM %s WHERE table_name = %s AND tag = %s",
    ident,
    DBI::dbQuoteString(conn, name),
    DBI::dbQuoteString(conn, tag)
  )
  DBI::dbExecute(conn, delete_sql)
  insert_sql <- sprintf(
    "INSERT INTO %s (table_name, tag, snapshot, as_of) VALUES (%s, %s, %s, %s)",
    ident,
    DBI::dbQuoteString(conn, name),
    DBI::dbQuoteString(conn, tag),
    DBI::dbQuoteString(conn, snapshot),
    if (is.null(resolved$as_of)) "NULL" else DBI::dbQuoteString(conn, as.character(resolved$as_of))
  )
  DBI::dbExecute(conn, insert_sql)
  invisible(snapshot)
}

#' Read a stored object
#' @export
ol_read_object <- function(name, when = c("latest", "first"), project = getOption("ol.project")) {
  when <- match.arg(when)
  project <- .ol_assert_project(project, "Call ol_init_iceberg() first or set options(ol.project=...).")
  state <- .ol_get_iceberg_state(project)
  .ol_ensure_objects_table(state)
  conn <- state$conn
  ident <- .ol_iceberg_sql_ident(conn, state, "__ol_objects")
  order_dir <- if (identical(when, "latest")) "DESC" else "ASC"
  sql <- sprintf(
    "SELECT name, version_ts, mime, bytes FROM %s WHERE name = %s ORDER BY version_ts %s LIMIT 1",
    ident,
    DBI::dbQuoteString(conn, name),
    order_dir
  )
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
