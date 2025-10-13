#' Execute a custom SQL query on the OmicsLake database
#'
#' This function allows you to run arbitrary SQL queries against the tables
#' in your OmicsLake project. It provides full access to DuckDB's SQL capabilities
#' including JOINs, aggregations, window functions, and more.
#'
#' @param sql Character string containing the SQL query to execute
#' @param project Project name (default: current project from options)
#' @param collect Logical; if TRUE (default), returns a data.frame. If FALSE,
#'   returns a lazy dplyr table for further manipulation
#' @param params Optional named list of parameters for parameterized queries
#'
#' @return If collect=TRUE, returns a data.frame with query results.
#'   If collect=FALSE, returns a lazy dplyr tbl for further manipulation.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ol_init("myproject")
#' ol_write("genes", data.frame(gene_id = 1:100, expression = rnorm(100)))
#' 
#' results <- ol_query("SELECT * FROM genes WHERE expression > 0")
#' 
#' lazy_result <- ol_query("SELECT * FROM genes", collect = FALSE)
#' filtered <- lazy_result %>%
#'   dplyr::filter(expression > 0) %>%
#'   dplyr::collect()
#' 
#' joined <- ol_query("
#'   SELECT a.gene_id, a.expression, b.annotation
#'   FROM genes a
#'   JOIN annotations b ON a.gene_id = b.gene_id
#' ")
#' }
ol_query <- function(sql, project = getOption("ol.project"), collect = TRUE, params = NULL) {
  if (missing(sql) || !is.character(sql) || length(sql) != 1 || !nzchar(sql)) {
    stop("sql must be a non-empty character string", call. = FALSE)
  }
  
  project <- .ol_assert_project(project, "Call ol_init() first or set options(ol.project=...).")
  state <- .ol_get_iceberg_state(project)
  conn <- state$conn
  
  if (!is.null(params)) {
    if (!is.list(params) || is.null(names(params)) || any(!nzchar(names(params)))) {
      stop("params must be a named list with non-empty names", call. = FALSE)
    }
    
    for (param_name in names(params)) {
      param_value <- params[[param_name]]
      placeholder <- paste0(":", param_name)
      
      if (is.character(param_value)) {
        replacement <- DBI::dbQuoteString(conn, param_value)
      } else if (is.numeric(param_value)) {
        replacement <- as.character(param_value)
      } else {
        stop("Parameter '", param_name, "' must be character or numeric", call. = FALSE)
      }
      
      sql <- gsub(placeholder, replacement, sql, fixed = TRUE)
    }
  }
  
  if (!isTRUE(collect)) {
    .ol_require(c("dplyr", "dbplyr"))
    return(dplyr::tbl(conn, dbplyr::sql(sql)))
  }
  
  tryCatch({
    result <- DBI::dbGetQuery(conn, sql)
    if (is.data.frame(result)) {
      rownames(result) <- NULL
    }
    result
  }, error = function(e) {
    msg <- conditionMessage(e)
    stop("SQL query failed: ", msg, call. = FALSE)
  })
}
