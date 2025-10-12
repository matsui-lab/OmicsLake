#' Write a table using the Iceberg backend
#' @export
ol_write <- function(name, data, project = getOption("ol.project"), mode = c("create", "overwrite", "append")) {
  .ol_require(c("arrow", "duckdb"))
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
  DBI::dbAppendTable(conn, DBI::Id(catalog = state$catalog_name, schema = state$namespace, table = "__ol_objects"), payload)
  invisible(TRUE)
}

#' Commit is a no-op under Iceberg (snapshots are implicit)
#' @export
ol_commit <- function(note = "", params = list(), project = getOption("ol.project")) {
  project <- .ol_assert_project(project, "Call ol_init_iceberg() first or set options(ol.project=...).")
  .ol_get_iceberg_state(project)
  invisible(format(Sys.time(), "%Y%m%d-%H%M%S"))
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
