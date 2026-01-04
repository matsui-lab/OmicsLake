# Internal registry for backend projects
.ol_backend_registry <- new.env(parent = emptyenv())

.ol_backend_key <- function(project) {
  .ol_assert_project(project, "Call ol_init() first or set options(ol.project=...).")
}

.ol_get_backend_state <- function(project) {
  key <- .ol_backend_key(project)
  state <- .ol_backend_registry[[key]]
  if (is.null(state)) stop("Backend not initialized for project: ", project, call. = FALSE)
  state
}

.ol_store_backend_state <- function(project, state) {
  assign(.ol_backend_key(project), state, envir = .ol_backend_registry)
  invisible(state)
}

.ol_disconnect_backend <- function(project) {
  key <- .ol_backend_key(project)
  state <- .ol_backend_registry[[key]]
  if (!is.null(state) && inherits(state$conn, "duckdb_connection")) {
    try(DBI::dbDisconnect(state$conn, shutdown = FALSE), silent = TRUE)
  }
  rm(list = key, envir = .ol_backend_registry)
  invisible(TRUE)
}

.ol_schema_sql <- function(conn, state) {
  if (nzchar(state$catalog_name)) {
    paste(DBI::dbQuoteIdentifier(conn, c(state$catalog_name, state$namespace)), collapse = ".")
  } else {
    DBI::dbQuoteIdentifier(conn, state$namespace)
  }
}

.ol_sql_ident <- function(conn, state, name) {
  paste0(.ol_schema_sql(conn, state), ".", DBI::dbQuoteIdentifier(conn, name))
}

.ol_qualified_name <- function(state, name) {
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
    .ol_sql_ident(conn, state, "__ol_refs")
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
    .ol_sql_ident(conn, state, "__ol_objects")
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
    .ol_sql_ident(conn, state, "__ol_commits")
  )
  tryCatch(DBI::dbExecute(conn, sql), error = function(e) {
    msg <- conditionMessage(e)
    if (!grepl("already", msg, ignore.case = TRUE)) stop(e)
  })
}

#' Ensure the adapter registry table exists
#' Stores metadata about adapter-managed objects for deterministic detection
#' @keywords internal
.ol_ensure_adapters_table <- function(state) {
  conn <- state$conn
  sql <- sprintf(
    "CREATE TABLE IF NOT EXISTS %s (name TEXT, adapter_name TEXT, format_version INTEGER, components_json TEXT, created_at TIMESTAMP, PRIMARY KEY (name, created_at))",
    .ol_sql_ident(conn, state, "__ol_adapters")
  )
  tryCatch(DBI::dbExecute(conn, sql), error = function(e) {
    msg <- conditionMessage(e)
    if (!grepl("already", msg, ignore.case = TRUE)) stop(e)
  })
}

#' Register an adapter-managed object
#' @keywords internal
.ol_register_adapter_object <- function(state, name, adapter_name, components, format_version = 1L) {
  .ol_ensure_adapters_table(state)
  conn <- state$conn
  ident <- .ol_sql_ident(conn, state, "__ol_adapters")

  components_json <- jsonlite::toJSON(components, auto_unbox = TRUE)

  insert_sql <- sprintf(
    "INSERT INTO %s (name, adapter_name, format_version, components_json, created_at) VALUES (%s, %s, %d, %s, %s)",
    ident,
    DBI::dbQuoteString(conn, name),
    DBI::dbQuoteString(conn, adapter_name),
    as.integer(format_version),
    DBI::dbQuoteString(conn, as.character(components_json)),
    DBI::dbQuoteString(conn, format(Sys.time(), "%Y-%m-%d %H:%M:%OS6", tz = "UTC"))
  )
  DBI::dbExecute(conn, insert_sql)
  invisible(TRUE)
}

#' Get adapter info for an object (deterministic lookup)
#' @keywords internal
.ol_get_adapter_info <- function(state, name) {
  .ol_ensure_adapters_table(state)
  conn <- state$conn
  ident <- .ol_sql_ident(conn, state, "__ol_adapters")

  query <- sprintf(
    "SELECT adapter_name, format_version, components_json, created_at FROM %s WHERE name = %s ORDER BY created_at DESC LIMIT 1",
    ident,
    DBI::dbQuoteString(conn, name)
  )
  result <- DBI::dbGetQuery(conn, query)

  if (nrow(result) == 0) {
    return(NULL)
  }

  list(
    adapter_name = result$adapter_name[1],
    format_version = result$format_version[1],
    components = jsonlite::fromJSON(result$components_json[1]),
    created_at = result$created_at[1]
  )
}

#' Check if object is adapter-managed
#' @keywords internal
.ol_is_adapter_object <- function(state, name) {
  info <- .ol_get_adapter_info(state, name)
  !is.null(info)
}

.ol_ensure_dependencies_table <- function(state) {
  conn <- state$conn
  ident <- .ol_sql_ident(conn, state, "__ol_dependencies")

  # Create table with extended schema for version-aware lineage
  sql <- sprintf(
    "CREATE TABLE IF NOT EXISTS %s (child_name TEXT, child_type TEXT, parent_name TEXT, parent_type TEXT, relationship_type TEXT, created_at TIMESTAMP, parent_ref TEXT, parent_version_id TEXT, child_version_id TEXT)",
    ident
  )
  tryCatch(DBI::dbExecute(conn, sql), error = function(e) {
    msg <- conditionMessage(e)
    if (!grepl("already", msg, ignore.case = TRUE)) stop(e)
  })

  # Migration: add new columns if they don't exist (for existing databases)
  .ol_migrate_dependencies_table(state)
}

#' Migrate dependencies table to add version columns
#' @keywords internal
.ol_migrate_dependencies_table <- function(state) {
  conn <- state$conn
  ident <- .ol_sql_ident(conn, state, "__ol_dependencies")

  # Check if columns exist and add them if missing
  # DuckDB allows ADD COLUMN IF NOT EXISTS in recent versions
  tryCatch({
    DBI::dbExecute(conn, sprintf("ALTER TABLE %s ADD COLUMN IF NOT EXISTS parent_ref TEXT DEFAULT '@latest'", ident))
  }, error = function(e) {
    # Column may already exist - that's OK
    if (!grepl("already exists|duplicate column", conditionMessage(e), ignore.case = TRUE)) {
      warning("Could not add parent_ref column: ", conditionMessage(e), call. = FALSE)
    }
  })

  tryCatch({
    DBI::dbExecute(conn, sprintf("ALTER TABLE %s ADD COLUMN IF NOT EXISTS parent_version_id TEXT", ident))
  }, error = function(e) {
    if (!grepl("already exists|duplicate column", conditionMessage(e), ignore.case = TRUE)) {
      warning("Could not add parent_version_id column: ", conditionMessage(e), call. = FALSE)
    }
  })

  tryCatch({
    DBI::dbExecute(conn, sprintf("ALTER TABLE %s ADD COLUMN IF NOT EXISTS child_version_id TEXT", ident))
  }, error = function(e) {
    if (!grepl("already exists|duplicate column", conditionMessage(e), ignore.case = TRUE)) {
      warning("Could not add child_version_id column: ", conditionMessage(e), call. = FALSE)
    }
  })
}

.ol_record_dependency <- function(state, child_name, child_type, parent_name, parent_type,
                                   relationship_type = "derived_from",
                                   parent_ref = "@latest", parent_version_id = NULL,
                                   child_version_id = NULL) {
  .ol_ensure_dependencies_table(state)
  conn <- state$conn

  # Resolve parent ref to version_id if not provided
  if (is.null(parent_version_id)) {
    resolved <- .ol_resolve_ref_to_version(state, parent_name, parent_ref, parent_type)
    parent_version_id <- resolved$version_id
  }

  dep_data <- data.frame(
    child_name = child_name,
    child_type = child_type,
    parent_name = parent_name,
    parent_type = parent_type,
    relationship_type = relationship_type,
    created_at = as.POSIXct(Sys.time(), tz = "UTC"),
    parent_ref = as.character(parent_ref),
    parent_version_id = if (is.null(parent_version_id)) NA_character_ else as.character(parent_version_id),
    child_version_id = if (is.null(child_version_id)) NA_character_ else as.character(child_version_id),
    stringsAsFactors = FALSE
  )

  DBI::dbAppendTable(conn, DBI::Id(schema = state$namespace, table = "__ol_dependencies"), dep_data)
}

#' Resolve a reference to a version identifier
#'
#' @param state Backend state object
#' @param name Name of the table or object
#' @param ref Reference string (e.g., "@latest", "@tag(v1)", "@first")
#' @param type Type of entity ("table" or "object")
#' @return List with version_id, snapshot_table (for tables), and type
#' @keywords internal
.ol_resolve_ref_to_version <- function(state, name, ref, type = "table") {
  conn <- state$conn

  if (identical(type, "object")) {
    # For objects, resolve to version_ts
    .ol_ensure_objects_table(state)
    ident <- .ol_sql_ident(conn, state, "__ol_objects")
    parsed <- .ol_ref_parse(ref)

    if (identical(parsed$type, "latest") || identical(ref, "@latest")) {
      query <- sprintf(
        "SELECT version_ts FROM %s WHERE name = %s ORDER BY version_ts DESC LIMIT 1",
        ident, DBI::dbQuoteString(conn, name)
      )
    } else if (identical(ref, "@first")) {
      query <- sprintf(
        "SELECT version_ts FROM %s WHERE name = %s ORDER BY version_ts ASC LIMIT 1",
        ident, DBI::dbQuoteString(conn, name)
      )
    } else if (identical(parsed$type, "tag")) {
      # Look up tag in refs table
      .ol_ensure_refs_table(state)
      ident_refs <- .ol_sql_ident(conn, state, "__ol_refs")
      ref_name <- paste0("__object__", name)
      query <- sprintf(
        "SELECT snapshot as version_ts FROM %s WHERE table_name = %s AND tag = %s ORDER BY as_of DESC LIMIT 1",
        ident_refs, DBI::dbQuoteString(conn, ref_name), DBI::dbQuoteString(conn, parsed$value)
      )
    } else {
      return(list(version_id = NULL, snapshot_table = NULL, type = "object"))
    }

    res <- tryCatch(DBI::dbGetQuery(conn, query), error = function(e) data.frame())
    version_id <- if (nrow(res) > 0) as.character(res$version_ts[1]) else NULL
    return(list(version_id = version_id, snapshot_table = NULL, type = "object"))

  } else {
    # For tables, resolve to snapshot table name or current table
    resolved <- .ol_resolve_reference(state, name, ref)
    if (!is.null(resolved$backup_table)) {
      return(list(version_id = resolved$backup_table, snapshot_table = resolved$backup_table, type = "table"))
    } else {
      # Current version - use table name as version_id
      return(list(version_id = paste0(name, "@current"), snapshot_table = NULL, type = "table"))
    }
  }
}

#' Record multiple dependencies for a child entity
#' @param state Backend state object
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

.ol_is_object <- function(state, name) {
  .ol_ensure_objects_table(state)
  conn <- state$conn
  ident <- .ol_sql_ident(conn, state, "__ol_objects")
  query <- sprintf("SELECT COUNT(*) as cnt FROM %s WHERE name = %s", ident, DBI::dbQuoteString(conn, name))
  res <- DBI::dbGetQuery(conn, query)
  res$cnt[1] > 0
}



#' Initialize DuckDB backend (internal)
.ol_init_backend <- function(project, engine = "duckdb", catalog = NULL, namespace = "ol", object_mode = c("blobs", "external"), object_root = NULL) {
  object_mode <- match.arg(object_mode)
  if (!identical(engine, "duckdb")) stop("Unsupported engine: ", engine, call. = FALSE)
  .ol_require(c("DBI", "duckdb", "arrow"))
  project <- .ol_assert_project(project, "project must be specified")
  if (!is.null(object_root)) object_root <- .ol_norm(object_root)
  pr <- .ol_proj_root(project)
  dir.create(pr, recursive = TRUE, showWarnings = FALSE)
  if (is.null(catalog)) catalog <- file.path(pr, "catalog")
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
  .ol_store_backend_state(project, state)
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

.ol_resolve_reference <- function(state, name, ref) {
  parsed <- .ol_ref_parse(ref)
  if (identical(parsed$type, "latest")) {
    return(list(backup_table = NULL))
  }
  if (identical(parsed$type, "tag")) {
    .ol_ensure_refs_table(state)
    conn <- state$conn
    ident <- .ol_sql_ident(conn, state, "__ol_refs")
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
    ident <- .ol_sql_ident(conn, state, name)
    sprintf("SELECT * FROM %s", ident)
  } else {
    backup_ident <- .ol_sql_ident(conn, state, resolved$backup_table)
    sprintf("SELECT * FROM %s", backup_ident)
  }
}

#' Tag a table by creating a backup
#' @param .in_transaction Internal parameter - if TRUE, skip transaction management (caller handles it)
#' @export
ol_tag <- function(name, tag, ref = "@latest", project = getOption("ol.project"), .in_transaction = FALSE) {
  project <- .ol_assert_project(project, "Call ol_init() first or set options(ol.project=...).")
  if (missing(name) || !nzchar(name)) stop("name must be provided", call. = FALSE)
  if (missing(tag) || !nzchar(tag)) stop("tag must be provided", call. = FALSE)
  state <- .ol_get_backend_state(project)
  conn <- state$conn

  backup_table <- .ol_get_backup_table_name(name, tag)
  source_ident <- .ol_sql_ident(conn, state, name)
  backup_ident <- .ol_sql_ident(conn, state, backup_table)

  # Internal function to do the actual work
  .do_tag <- function() {
    create_backup_sql <- sprintf(
      "CREATE OR REPLACE TABLE %s AS SELECT * FROM %s",
      backup_ident,
      source_ident
    )
    DBI::dbExecute(conn, create_backup_sql)

    .ol_ensure_refs_table(state)
    ident <- .ol_sql_ident(conn, state, "__ol_refs")
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
  }

  # If caller is managing transaction, just do the work
  if (isTRUE(.in_transaction)) {
    .do_tag()
  } else {
    # Use transaction for atomicity: backup creation + refs update must both succeed
    DBI::dbBegin(conn)
    tryCatch({
      .do_tag()
      DBI::dbCommit(conn)
    }, error = function(e) {
      DBI::dbRollback(conn)
      stop("Failed to tag table '", name, "' (rolled back): ", conditionMessage(e), call. = FALSE)
    })
  }

  invisible(backup_table)
}

#' Tag a stored object version
#' @param name Name of the object to tag
#' @param tag Tag name to assign
#' @param when Which version to tag: "latest" (default) or "first", or a specific version_ts timestamp
#' @param project Project name
#' @param .in_transaction Internal parameter - if TRUE, skip transaction management (caller handles it)
#' @export
ol_tag_object <- function(name, tag, when = "latest", project = getOption("ol.project"), .in_transaction = FALSE) {
  .ol_validate_name(name, "object name")
  .ol_validate_name(tag, "tag")
  project <- .ol_assert_project(project, "Call ol_init() first or set options(ol.project=...).")
  state <- .ol_get_backend_state(project)
  .ol_ensure_objects_table(state)
  conn <- state$conn

  ident_objects <- .ol_sql_ident(conn, state, "__ol_objects")
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

  # Internal function to do the actual work
  .do_tag_object <- function() {
    .ol_ensure_refs_table(state)
    ident_refs <- .ol_sql_ident(conn, state, "__ol_refs")
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
  }

  # If caller is managing transaction, just do the work
  if (isTRUE(.in_transaction)) {
    .do_tag_object()
  } else {
    # Use transaction for atomicity
    DBI::dbBegin(conn)
    tryCatch({
      .do_tag_object()
      DBI::dbCommit(conn)
    }, error = function(e) {
      DBI::dbRollback(conn)
      stop("Failed to tag object '", name, "' (rolled back): ", conditionMessage(e), call. = FALSE)
    })
  }

  invisible(version_ts)
}

#' Restore entire project to a labeled state
#'
#' @param label The label name to restore to
#' @param project The project name
#' @return Invisible TRUE on success, FALSE if no tables found
#' @export
ol_checkout <- function(label, project = getOption("ol.project")) {
  project <- .ol_assert_project(project, "Call ol_init() first or set options(ol.project=...).")
  state <- .ol_get_backend_state(project)
  .ol_ensure_refs_table(state)
  conn <- state$conn
  
  ident <- .ol_sql_ident(conn, state, "__ol_refs")
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
      backup_ident <- .ol_sql_ident(conn, state, backup_table)
      target_ident <- .ol_sql_ident(conn, state, tbl)
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
  project <- .ol_assert_project(project, "Call ol_init() first or set options(ol.project=...).")
  state <- .ol_get_backend_state(project)
  .ol_ensure_objects_table(state)
  conn <- state$conn
  ident <- .ol_sql_ident(conn, state, "__ol_objects")
  
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
    ident_refs <- .ol_sql_ident(conn, state, "__ol_refs")
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
