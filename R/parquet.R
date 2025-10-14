#' Export a table or object to a Parquet file
#'
#' @param name Name of the table or object to export
#' @param path Output file path for the Parquet file
#' @param ref Version reference (default: "@latest")
#' @param project Project name
#' @param compression Compression algorithm: "snappy" (default), "zstd", "lz4", "brotli", or "uncompressed"
#' @param compression_level Compression level (codec-specific, NULL for default)
#' @param row_group_size Number of rows per row group (default: 100000)
#' @param overwrite Whether to overwrite existing file (default: FALSE)
#' @export
ol_export_parquet <- function(name, path, ref = "@latest", 
                              project = getOption("ol.project"),
                              compression = c("snappy", "zstd", "lz4", "brotli", "uncompressed"),
                              compression_level = NULL,
                              row_group_size = 100000,
                              overwrite = FALSE) {
  .ol_validate_name(name, "table/object name")
  
  if (!is.character(path) || length(path) != 1 || nchar(path) == 0) {
    stop("path must be a non-empty character string", call. = FALSE)
  }
  
  if (file.exists(path) && !isTRUE(overwrite)) {
    stop("File '", path, "' already exists. Set overwrite=TRUE to replace it.", call. = FALSE)
  }
  
  compression <- match.arg(compression)
  project <- .ol_assert_project(project, "Call ol_init() first or set options(ol.project=...).")
  state <- .ol_get_iceberg_state(project)
  conn <- state$conn
  
  is_table <- tryCatch({
    ident <- .ol_iceberg_sql_ident(conn, state, name)
    DBI::dbExistsTable(conn, DBI::Id(schema = state$namespace, table = name))
  }, error = function(e) FALSE)
  
  if (is_table) {
    ident <- .ol_iceberg_sql_ident(conn, state, name)
    
    copy_sql <- sprintf("COPY %s TO %s (FORMAT parquet, COMPRESSION %s",
                       ident,
                       DBI::dbQuoteString(conn, normalizePath(path, mustWork = FALSE)),
                       compression)
    
    if (!is.null(compression_level)) {
      copy_sql <- paste0(copy_sql, sprintf(", COMPRESSION_LEVEL %d", as.integer(compression_level)))
    }
    
    if (!is.null(row_group_size)) {
      copy_sql <- paste0(copy_sql, sprintf(", ROW_GROUP_SIZE %d", as.integer(row_group_size)))
    }
    
    copy_sql <- paste0(copy_sql, ")")
    
    DBI::dbExecute(conn, copy_sql)
  } else {
    obj <- ol_read_object(name, ref = ref, project = project)
    
    obj_data <- data.frame(
      name = name,
      object_data = I(list(serialize(obj, NULL))),
      stringsAsFactors = FALSE
    )
    
    tmp_name <- paste0("ol_tmp_export_", gsub("[^0-9]", "", format(Sys.time(), "%Y%m%d%H%M%OS6")))
    duckdb::duckdb_register_arrow(conn, tmp_name, arrow::arrow_table(obj_data))
    on.exit(duckdb::duckdb_unregister_arrow(conn, tmp_name), add = TRUE)
    
    copy_sql <- sprintf("COPY %s TO %s (FORMAT parquet, COMPRESSION %s",
                       DBI::dbQuoteIdentifier(conn, tmp_name),
                       DBI::dbQuoteString(conn, normalizePath(path, mustWork = FALSE)),
                       compression)
    
    if (!is.null(compression_level)) {
      copy_sql <- paste0(copy_sql, sprintf(", COMPRESSION_LEVEL %d", as.integer(compression_level)))
    }
    
    copy_sql <- paste0(copy_sql, ")")
    
    DBI::dbExecute(conn, copy_sql)
  }
  
  invisible(normalizePath(path))
}

#' Import a Parquet file into the project
#'
#' @param path Input file path(s) for Parquet file(s). Can be a single file, list of files, or glob pattern.
#' @param name Destination table name in the project
#' @param project Project name
#' @param mode Import mode: "create", "overwrite", or "append"
#' @param depends_on Optional character vector of table/object names that this import depends on
#' @param hive_partitioning Whether to interpret path as Hive partitioned (NULL for auto-detect, TRUE/FALSE to force)
#' @param union_by_name Whether to unify columns by name rather than position when reading multiple files
#' @export
ol_import_parquet <- function(path, name, 
                              project = getOption("ol.project"),
                              mode = c("create", "overwrite", "append"),
                              depends_on = NULL,
                              hive_partitioning = NULL,
                              union_by_name = FALSE) {
  .ol_validate_name(name, "table name")
  
  if (!is.character(path) || length(path) == 0) {
    stop("path must be a non-empty character vector", call. = FALSE)
  }
  
  if (any(nchar(path) == 0)) {
    stop("path must be a non-empty character vector", call. = FALSE)
  }
  
  mode <- match.arg(mode)
  project <- .ol_assert_project(project, "Call ol_init() first or set options(ol.project=...).")
  state <- .ol_get_iceberg_state(project)
  conn <- state$conn
  
  read_opts <- list()
  if (!is.null(hive_partitioning)) {
    read_opts$hive_partitioning <- isTRUE(hive_partitioning)
  }
  if (isTRUE(union_by_name)) {
    read_opts$union_by_name <- TRUE
  }
  
  if (length(path) == 1) {
    read_expr <- sprintf("read_parquet(%s", DBI::dbQuoteString(conn, path))
  } else {
    path_list <- paste(sapply(path, function(p) DBI::dbQuoteString(conn, p)), collapse = ", ")
    read_expr <- sprintf("read_parquet([%s]", path_list)
  }
  
  if (length(read_opts) > 0) {
    opts_str <- paste(names(read_opts), "=", 
                     sapply(read_opts, function(x) if (is.logical(x)) tolower(as.character(x)) else x),
                     collapse = ", ")
    read_expr <- paste0(read_expr, ", ", opts_str)
  }
  read_expr <- paste0(read_expr, ")")
  
  schema_sql <- .ol_iceberg_schema_sql(conn, state)
  DBI::dbExecute(conn, sprintf("CREATE SCHEMA IF NOT EXISTS %s", schema_sql))
  
  ident <- .ol_iceberg_sql_ident(conn, state, name)
  
  sql <- switch(mode,
    create = sprintf("CREATE TABLE %s AS SELECT * FROM %s", ident, read_expr),
    overwrite = sprintf("CREATE OR REPLACE TABLE %s AS SELECT * FROM %s", ident, read_expr),
    append = sprintf("INSERT INTO %s SELECT * FROM %s", ident, read_expr)
  )
  
  DBI::dbExecute(conn, sql)
  
  if (!is.null(depends_on) && length(depends_on) > 0) {
    for (parent in depends_on) {
      parent_type <- if (.ol_is_object(state, parent)) "object" else "table"
      .ol_record_dependency(state, name, "table", parent, parent_type)
    }
  }
  
  invisible(.ol_iceberg_qualified_name(state, name))
}
