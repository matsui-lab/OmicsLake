#' Aggregate data with multiple statistics
#'
#' Calculate multiple aggregate statistics, optionally grouped by columns.
#' Supports common aggregates: count, sum, avg, min, max, stddev, median, etc.
#'
#' @param table Character string, name of the table to aggregate
#' @param group_by Character vector of column names to group by (default: NULL for overall aggregates)
#' @param ... Named arguments specifying aggregates. Each argument should be a list with
#'   'func' (aggregate function name) and 'col' (column name). The argument name becomes
#'   the output column name.
#' @param project Project name (default: current project)
#' @param collect Logical; if TRUE returns data.frame, if FALSE returns lazy dplyr table
#'
#' @return Aggregated results as data.frame (if collect=TRUE) or lazy table (if collect=FALSE)
#' @export
#' @examples
#' \dontrun{
#'   ol_aggregate("genes", 
#'                mean_expr = list(func = "avg", col = "expression"),
#'                sd_expr = list(func = "stddev", col = "expression"))
#'   
#'   ol_aggregate("genes", 
#'                group_by = "sample",
#'                mean_expr = list(func = "avg", col = "expression"),
#'                count = list(func = "count", col = "*"))
#' }
ol_aggregate <- function(table, group_by = NULL, ..., project = getOption("ol.project"), collect = TRUE) {
  if (missing(table) || !is.character(table) || length(table) != 1 || !nzchar(table)) {
    stop("table must be a non-empty character string", call. = FALSE)
  }
  
  aggregates <- list(...)
  if (length(aggregates) == 0) {
    stop("At least one aggregate must be specified", call. = FALSE)
  }
  
  for (name in names(aggregates)) {
    if (!nzchar(name)) {
      stop("All aggregates must have names", call. = FALSE)
    }
    agg <- aggregates[[name]]
    if (!is.list(agg) || is.null(agg$func) || is.null(agg$col)) {
      stop("Each aggregate must be a list with 'func' and 'col' elements", call. = FALSE)
    }
  }
  
  if (!is.null(group_by)) {
    if (!is.character(group_by) || any(!nzchar(group_by))) {
      stop("group_by must be a character vector with non-empty values", call. = FALSE)
    }
  }
  
  select_parts <- character(0)
  if (!is.null(group_by)) {
    select_parts <- paste(sprintf("`%s`", group_by), collapse = ", ")
  }
  
  agg_parts <- character(0)
  for (name in names(aggregates)) {
    agg <- aggregates[[name]]
    func <- toupper(agg$func)
    col <- agg$col
    
    if (col == "*") {
      agg_sql <- sprintf("%s(*) AS `%s`", func, name)
    } else {
      agg_sql <- sprintf("%s(`%s`) AS `%s`", func, col, name)
    }
    agg_parts <- c(agg_parts, agg_sql)
  }
  
  all_selects <- c(select_parts, agg_parts)
  select_clause <- paste(all_selects, collapse = ", ")
  
  sql <- sprintf("SELECT %s FROM `%s`", select_clause, table)
  
  if (!is.null(group_by)) {
    group_clause <- paste(sprintf("`%s`", group_by), collapse = ", ")
    sql <- paste(sql, "GROUP BY", group_clause)
  }
  
  ol_query(sql, project = project, collect = collect)
}

#' Add ranking column to table
#'
#' Add a ranking column using window functions (ROW_NUMBER, RANK, DENSE_RANK).
#'
#' @param table Character string, name of the table
#' @param rank_by Character string, column name to rank by
#' @param partition_by Character vector of columns to partition by (default: NULL)
#' @param method Character, ranking method: "row_number", "rank", or "dense_rank" (default: "row_number")
#' @param descending Logical; if TRUE, rank in descending order (default: TRUE)
#' @param as_column Character, name of the ranking column to add (default: "rank")
#' @param project Project name (default: current project)
#' @param collect Logical; if TRUE returns data.frame, if FALSE returns lazy dplyr table
#'
#' @return Original table with added ranking column
#' @export
#' @examples
#' \dontrun{
#'   ol_add_rank("genes", rank_by = "expression", method = "dense_rank")
#'   
#'   ol_add_rank("genes", 
#'               rank_by = "expression", 
#'               partition_by = "sample",
#'               as_column = "expression_rank")
#' }
ol_add_rank <- function(table, rank_by, partition_by = NULL, method = "row_number", 
                        descending = TRUE, as_column = "rank", 
                        project = getOption("ol.project"), collect = TRUE) {
  if (missing(table) || !is.character(table) || length(table) != 1 || !nzchar(table)) {
    stop("table must be a non-empty character string", call. = FALSE)
  }
  
  if (missing(rank_by) || !is.character(rank_by) || length(rank_by) != 1 || !nzchar(rank_by)) {
    stop("rank_by must be a non-empty character string", call. = FALSE)
  }
  
  method <- tolower(method)
  if (!method %in% c("row_number", "rank", "dense_rank")) {
    stop("method must be one of: 'row_number', 'rank', 'dense_rank'", call. = FALSE)
  }
  
  if (!is.character(as_column) || length(as_column) != 1 || !nzchar(as_column)) {
    stop("as_column must be a non-empty character string", call. = FALSE)
  }
  
  if (!is.null(partition_by)) {
    if (!is.character(partition_by) || any(!nzchar(partition_by))) {
      stop("partition_by must be a character vector with non-empty values", call. = FALSE)
    }
  }
  
  rank_func <- toupper(method)
  order_dir <- if (descending) "DESC" else "ASC"
  
  over_clause <- sprintf("ORDER BY `%s` %s", rank_by, order_dir)
  if (!is.null(partition_by)) {
    partition_clause <- paste(sprintf("`%s`", partition_by), collapse = ", ")
    over_clause <- sprintf("PARTITION BY %s %s", partition_clause, over_clause)
  }
  
  sql <- sprintf("SELECT *, %s() OVER (%s) AS `%s` FROM `%s`", 
                 rank_func, over_clause, as_column, table)
  
  ol_query(sql, project = project, collect = collect)
}

#' Calculate moving average
#'
#' Add a moving average column using window functions.
#'
#' @param table Character string, name of the table
#' @param column Character string, column name to calculate moving average on
#' @param window_size Integer, size of the moving window (default: 3)
#' @param partition_by Character vector of columns to partition by (default: NULL)
#' @param order_by Character string, column to order by (required)
#' @param as_column Character, name of the output column (default: paste0(column, "_ma", window_size))
#' @param project Project name (default: current project)
#' @param collect Logical; if TRUE returns data.frame, if FALSE returns lazy dplyr table
#'
#' @return Original table with added moving average column
#' @export
#' @examples
#' \dontrun{
#'   ol_moving_avg("counts", "expression", window_size = 3, order_by = "gene_id")
#'   
#'   ol_moving_avg("counts", "expression", 
#'                 window_size = 5, 
#'                 partition_by = "sample",
#'                 order_by = "time")
#' }
ol_moving_avg <- function(table, column, window_size = 3, partition_by = NULL, order_by,
                          as_column = NULL, project = getOption("ol.project"), collect = TRUE) {
  if (missing(table) || !is.character(table) || length(table) != 1 || !nzchar(table)) {
    stop("table must be a non-empty character string", call. = FALSE)
  }
  
  if (missing(column) || !is.character(column) || length(column) != 1 || !nzchar(column)) {
    stop("column must be a non-empty character string", call. = FALSE)
  }
  
  if (missing(order_by) || !is.character(order_by) || length(order_by) != 1 || !nzchar(order_by)) {
    stop("order_by must be a non-empty character string", call. = FALSE)
  }
  
  if (!is.numeric(window_size) || length(window_size) != 1 || window_size < 1) {
    stop("window_size must be a positive integer", call. = FALSE)
  }
  window_size <- as.integer(window_size)
  
  if (!is.null(partition_by)) {
    if (!is.character(partition_by) || any(!nzchar(partition_by))) {
      stop("partition_by must be a character vector with non-empty values", call. = FALSE)
    }
  }
  
  if (is.null(as_column)) {
    as_column <- paste0(column, "_ma", window_size)
  }
  
  if (!is.character(as_column) || length(as_column) != 1 || !nzchar(as_column)) {
    stop("as_column must be a non-empty character string", call. = FALSE)
  }
  
  half_window <- as.integer((window_size - 1) / 2)
  frame_clause <- sprintf("ROWS BETWEEN %d PRECEDING AND %d FOLLOWING", 
                          half_window, window_size - 1 - half_window)
  
  over_clause <- sprintf("ORDER BY `%s` %s", order_by, frame_clause)
  if (!is.null(partition_by)) {
    partition_clause <- paste(sprintf("`%s`", partition_by), collapse = ", ")
    over_clause <- sprintf("PARTITION BY %s %s", partition_clause, over_clause)
  }
  
  sql <- sprintf("SELECT *, AVG(`%s`) OVER (%s) AS `%s` FROM `%s`", 
                 column, over_clause, as_column, table)
  
  ol_query(sql, project = project, collect = collect)
}

#' Calculate cumulative sum
#'
#' Add a cumulative sum column using window functions.
#'
#' @param table Character string, name of the table
#' @param column Character string, column name to calculate cumulative sum on
#' @param partition_by Character vector of columns to partition by (default: NULL)
#' @param order_by Character string, column to order by (required)
#' @param as_column Character, name of the output column (default: paste0(column, "_cumsum"))
#' @param project Project name (default: current project)
#' @param collect Logical; if TRUE returns data.frame, if FALSE returns lazy dplyr table
#'
#' @return Original table with added cumulative sum column
#' @export
#' @examples
#' \dontrun{
#'   ol_cumulative_sum("counts", "value", order_by = "time")
#'   
#'   ol_cumulative_sum("counts", "value", 
#'                     partition_by = "sample",
#'                     order_by = "gene_id")
#' }
ol_cumulative_sum <- function(table, column, partition_by = NULL, order_by,
                              as_column = NULL, project = getOption("ol.project"), collect = TRUE) {
  if (missing(table) || !is.character(table) || length(table) != 1 || !nzchar(table)) {
    stop("table must be a non-empty character string", call. = FALSE)
  }
  
  if (missing(column) || !is.character(column) || length(column) != 1 || !nzchar(column)) {
    stop("column must be a non-empty character string", call. = FALSE)
  }
  
  if (missing(order_by) || !is.character(order_by) || length(order_by) != 1 || !nzchar(order_by)) {
    stop("order_by must be a non-empty character string", call. = FALSE)
  }
  
  if (!is.null(partition_by)) {
    if (!is.character(partition_by) || any(!nzchar(partition_by))) {
      stop("partition_by must be a character vector with non-empty values", call. = FALSE)
    }
  }
  
  if (is.null(as_column)) {
    as_column <- paste0(column, "_cumsum")
  }
  
  if (!is.character(as_column) || length(as_column) != 1 || !nzchar(as_column)) {
    stop("as_column must be a non-empty character string", call. = FALSE)
  }
  
  frame_clause <- "ROWS UNBOUNDED PRECEDING"
  over_clause <- sprintf("ORDER BY `%s` %s", order_by, frame_clause)
  
  if (!is.null(partition_by)) {
    partition_clause <- paste(sprintf("`%s`", partition_by), collapse = ", ")
    over_clause <- sprintf("PARTITION BY %s %s", partition_clause, over_clause)
  }
  
  sql <- sprintf("SELECT *, SUM(`%s`) OVER (%s) AS `%s` FROM `%s`", 
                 column, over_clause, as_column, table)
  
  ol_query(sql, project = project, collect = collect)
}

#' Get top N rows
#'
#' Get top N rows from a table, optionally partitioned.
#'
#' @param table Character string, name of the table
#' @param n Integer, number of top rows to return per partition (default: 10)
#' @param order_by Character string, column to order by (required)
#' @param partition_by Character vector of columns to partition by (default: NULL for overall top N)
#' @param descending Logical; if TRUE, order in descending order (default: TRUE)
#' @param project Project name (default: current project)
#' @param collect Logical; if TRUE returns data.frame, if FALSE returns lazy dplyr table
#'
#' @return Top N rows
#' @export
#' @examples
#' \dontrun{
#'   ol_top_n("genes", n = 10, order_by = "expression")
#'   
#'   ol_top_n("genes", n = 5, 
#'            order_by = "expression", 
#'            partition_by = "sample")
#' }
ol_top_n <- function(table, n = 10, order_by, partition_by = NULL, descending = TRUE,
                     project = getOption("ol.project"), collect = TRUE) {
  if (missing(table) || !is.character(table) || length(table) != 1 || !nzchar(table)) {
    stop("table must be a non-empty character string", call. = FALSE)
  }
  
  if (missing(order_by) || !is.character(order_by) || length(order_by) != 1 || !nzchar(order_by)) {
    stop("order_by must be a non-empty character string", call. = FALSE)
  }
  
  if (!is.numeric(n) || length(n) != 1 || n < 1) {
    stop("n must be a positive integer", call. = FALSE)
  }
  n <- as.integer(n)
  
  if (!is.null(partition_by)) {
    if (!is.character(partition_by) || any(!nzchar(partition_by))) {
      stop("partition_by must be a character vector with non-empty values", call. = FALSE)
    }
  }
  
  order_dir <- if (descending) "DESC" else "ASC"
  over_clause <- sprintf("ORDER BY `%s` %s", order_by, order_dir)
  
  if (!is.null(partition_by)) {
    partition_clause <- paste(sprintf("`%s`", partition_by), collapse = ", ")
    over_clause <- sprintf("PARTITION BY %s %s", partition_clause, over_clause)
  }
  
  sql <- sprintf(
    "SELECT * FROM (SELECT *, ROW_NUMBER() OVER (%s) AS __rank FROM `%s`) WHERE __rank <= %d",
    over_clause, table, n
  )
  
  result_sql <- sprintf("SELECT * EXCLUDE __rank FROM (%s)", sql)
  
  ol_query(result_sql, project = project, collect = collect)
}
