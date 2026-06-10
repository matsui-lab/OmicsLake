.ol_query_apply_params <- function(sql, params, conn) {
    if (is.null(params)) {
        return(sql)
    }
    if (!is.list(params) || is.null(names(params)) || 
        any(!nzchar(names(params)))) {
        stop("params must be a named list with non-empty names", call. = FALSE)
    }
    for (param_name in names(params)) {
        param_value <- params[[param_name]]
        replacement <- if (is.character(param_value)) {
            DBI::dbQuoteString(conn, param_value)
        } else if (is.numeric(param_value)) {
            as.character(param_value)
        } else {
            stop(
                "Parameter '", param_name, "' must be character or numeric",
                call. = FALSE
            )
        }
        sql <- gsub(paste0(":", param_name), replacement, sql, fixed = TRUE)
    }
    sql
}

#' Execute a custom SQL query on the OmicsLake database
#'
#' This function allows you to run arbitrary SQL queries against the tables
#' in your OmicsLake project. It provides full access to DuckDB's SQL
#' capabilities
#' including JOINs, aggregations, window functions, and more. Table names can be
#' referenced without the schema prefix (e.g., 'genes' instead of 'ol.genes').
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
#' ol_init("ex_query", root = tempfile())
#' ol_write("t", data.frame(x = 1:3))
#' ol_query("SELECT COUNT(*) AS n FROM t")
ol_query <- function(sql, project = getOption("ol.project"), collect = TRUE,
    params = NULL) {
    if (missing(sql) || !is.character(sql) || length(sql) != 1 || 
        !nzchar(sql)) {
        stop("sql must be a non-empty character string", call. = FALSE)
    }
    project <- .ol_assert_project(project,
        "Call ol_init() first or set options(ol.project=...).")
    state <- .ol_get_backend_state(project)
    conn <- state$conn
    DBI::dbExecute(conn, sprintf("SET search_path TO %s", state$namespace))
    sql <- .ol_query_apply_params(sql, params, conn)
    if (!isTRUE(collect)) {
        .ol_require(c("dplyr", "dbplyr"))
        return(dplyr::tbl(conn, dbplyr::sql(sql)))
    }
    tryCatch(
        {
            result <- DBI::dbGetQuery(conn, sql)
            if (is.data.frame(result)) {
                rownames(result) <- NULL
            }
            result
        },
        error = function(e) {
            msg <- conditionMessage(e)
            stop("SQL query failed: ", msg, call. = FALSE)
        }
    )
}
