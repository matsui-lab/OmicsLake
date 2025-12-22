#' @title QueryBuilder - Fluent Query Interface
#' @description Build complex queries using a fluent, chainable API.
#' Provides an intuitive way to construct queries without writing SQL.
#'
#' @examples
#' \dontrun{
#' lake <- Lake$new("my_project")
#'
#' # Basic query
#' lake$from("users")$
#'   where(age > 30)$
#'   select(name, age)$
#'   run()
#'
#' # Join and aggregate
#' lake$from("orders")$
#'   join("customers", on = "customer_id")$
#'   where(status == "completed")$
#'   group_by(region)$
#'   summarize(total = sum(amount))$
#'   order_by(desc(total))$
#'   run()
#'
#' # Save results to lake
#' lake$from("data")$
#'   filter(quality > 0.8)$
#'   as("filtered_data")
#' }
#'
#' @importFrom R6 R6Class
#' @export
QueryBuilder <- R6::R6Class("QueryBuilder",
  cloneable = TRUE,

  public = list(

    #' @description Initialize a QueryBuilder
    #' @param lake Lake instance to query from
    initialize = function(lake) {
      private$.lake <- lake
      private$.reset()
      invisible(self)
    },

    #' @description Specify the source table
    #' @param table Table name
    #' @param alias Optional alias for the table
    #' @return Self for chaining
    from = function(table, alias = NULL) {
      private$.from <- list(
        table = table,
        alias = alias %||% table
      )
      # Track this read for lineage
      private$.sources <- c(private$.sources, table)
      self
    },

    # ========== Joins ==========

    #' @description Add a join clause
    #' @param table Table to join
    #' @param on Join condition (character vector of column names, or named vector for different column names)
    #' @param type Join type ("left", "inner", "right", "full")
    #' @param alias Optional alias for the joined table
    #' @return Self for chaining
    join = function(table, on = NULL, type = "left", alias = NULL) {
      private$.joins <- append(private$.joins, list(list(
        table = table,
        on = on,
        type = type,
        alias = alias %||% table
      )))
      private$.sources <- c(private$.sources, table)
      self
    },

    #' @description Add a LEFT JOIN
    #' @param table Table to join
    #' @param on Join condition
    #' @param alias Optional alias
    #' @return Self for chaining
    left_join = function(table, on = NULL, alias = NULL) {
      self$join(table, on, "left", alias)
    },

    #' @description Add an INNER JOIN
    #' @param table Table to join
    #' @param on Join condition
    #' @param alias Optional alias
    #' @return Self for chaining
    inner_join = function(table, on = NULL, alias = NULL) {
      self$join(table, on, "inner", alias)
    },

    #' @description Add a RIGHT JOIN
    #' @param table Table to join
    #' @param on Join condition
    #' @param alias Optional alias
    #' @return Self for chaining
    right_join = function(table, on = NULL, alias = NULL) {
      self$join(table, on, "right", alias)
    },

    #' @description Add a FULL JOIN
    #' @param table Table to join
    #' @param on Join condition
    #' @param alias Optional alias
    #' @return Self for chaining
    full_join = function(table, on = NULL, alias = NULL) {
      self$join(table, on, "full", alias)
    },

    # ========== Filtering ==========

    #' @description Add a filter condition (WHERE clause)
    #' @param ... Filter expressions (combined with AND)
    #' @return Self for chaining
    where = function(...) {
      exprs <- rlang::enquos(...)
      private$.where <- append(private$.where, exprs)
      self
    },

    #' @description Alias for where
    #' @param ... Filter expressions
    #' @return Self for chaining
    filter = function(...) {
      self$where(...)
    },

    # ========== Selection ==========

    #' @description Select columns
    #' @param ... Column names or expressions
    #' @return Self for chaining
    select = function(...) {
      private$.select <- rlang::enquos(...)
      self
    },

    #' @description Alias for select
    #' @param ... Column names or expressions
    #' @return Self for chaining
    pick = function(...) {
      self$select(...)
    },

    #' @description Add computed columns
    #' @param ... Name-value pairs for new columns
    #' @return Self for chaining
    mutate = function(...) {
      exprs <- rlang::enquos(...)
      private$.mutate <- append(private$.mutate, exprs)
      self
    },

    # ========== Grouping & Aggregation ==========

    #' @description Group by columns
    #' @param ... Grouping columns
    #' @return Self for chaining
    group_by = function(...) {
      private$.group_by <- rlang::enquos(...)
      self
    },

    #' @description Summarize after grouping
    #' @param ... Aggregation expressions
    #' @return Self for chaining
    summarize = function(...) {
      private$.summarize <- rlang::enquos(...)
      self
    },

    #' @description Alias for summarize
    #' @param ... Aggregation expressions
    #' @return Self for chaining
    summarise = function(...) {
      self$summarize(...)
    },

    #' @description Add HAVING clause (filter after grouping)
    #' @param ... Filter conditions
    #' @return Self for chaining
    having = function(...) {
      exprs <- rlang::enquos(...)
      private$.having <- append(private$.having, exprs)
      self
    },

    # ========== Ordering ==========

    #' @description Order results
    #' @param ... Columns to order by (use desc() for descending)
    #' @return Self for chaining
    order_by = function(...) {
      private$.order_by <- rlang::enquos(...)
      self
    },

    #' @description Alias for order_by
    #' @param ... Columns to order by
    #' @return Self for chaining
    arrange = function(...) {
      self$order_by(...)
    },

    # ========== Limiting ==========

    #' @description Limit the number of results
    #' @param n Maximum number of rows
    #' @return Self for chaining
    limit = function(n) {
      private$.limit <- as.integer(n)
      self
    },

    #' @description Alias for limit
    #' @param n Maximum number of rows
    #' @return Self for chaining
    take = function(n) {
      self$limit(n)
    },

    #' @description Get top N rows by a column
    #' @param n Number of rows
    #' @param by Column to order by
    #' @param desc Use descending order (default: TRUE)
    #' @return Self for chaining
    top = function(n, by, desc = TRUE) {
      by_expr <- rlang::enquo(by)
      if (desc) {
        private$.order_by <- list(rlang::expr(dplyr::desc(!!by_expr)))
      } else {
        private$.order_by <- list(by_expr)
      }
      private$.limit <- as.integer(n)
      self
    },

    #' @description Skip rows
    #' @param n Number of rows to skip
    #' @return Self for chaining
    offset = function(n) {
      private$.offset <- as.integer(n)
      self
    },

    # ========== Distinct ==========

    #' @description Return distinct rows only
    #' @param ... Optional columns to check for distinctness
    #' @return Self for chaining
    distinct = function(...) {
      exprs <- rlang::enquos(...)
      if (length(exprs) == 0) {
        private$.distinct <- TRUE
      } else {
        private$.distinct <- exprs
      }
      self
    },

    # ========== Execution ==========

    #' @description Execute the query and return results
    #' @return Data frame of results
    run = function() {
      private$.execute()
    },

    #' @description Alias for run
    #' @return Data frame of results
    collect = function() {
      self$run()
    },

    #' @description Execute and save to lake
    #' @param name Name to save as
    #' @return Invisible Lake object
    as = function(name) {
      result <- self$run()
      private$.lake$put(name, result, depends_on = private$.sources)
      invisible(private$.lake)
    },

    #' @description Save alias for as
    #' @param name Name to save as
    #' @return Invisible Lake object
    save_as = function(name) {
      self$as(name)
    },

    #' @description Get the generated SQL (for debugging)
    #' @return SQL string
    show_sql = function() {
      tbl <- private$.build_query()
      dbplyr::sql_render(tbl)
    },

    #' @description Explain the query plan
    #' @return Explanation data frame
    explain = function() {
      tbl <- private$.build_query()
      dplyr::explain(tbl)
    },

    #' @description Print query builder state
    print = function() {
      cat("QueryBuilder:\n")
      if (!is.null(private$.from)) {
        cat("  FROM:", private$.from$table)
        if (private$.from$alias != private$.from$table) {
          cat(" AS", private$.from$alias)
        }
        cat("\n")
      }
      if (length(private$.joins) > 0) {
        cat("  JOINS:", length(private$.joins), "\n")
        for (j in private$.joins) {
          cat("    ", toupper(j$type), "JOIN", j$table, "\n")
        }
      }
      if (length(private$.where) > 0) {
        cat("  WHERE:", length(private$.where), "condition(s)\n")
      }
      if (!is.null(private$.select) && length(private$.select) > 0) {
        cat("  SELECT:", length(private$.select), "column(s)\n")
      }
      if (!is.null(private$.group_by) && length(private$.group_by) > 0) {
        cat("  GROUP BY:", length(private$.group_by), "column(s)\n")
      }
      if (!is.null(private$.order_by) && length(private$.order_by) > 0) {
        cat("  ORDER BY:", length(private$.order_by), "column(s)\n")
      }
      if (!is.null(private$.limit)) {
        cat("  LIMIT:", private$.limit, "\n")
      }
      cat("  Sources tracked:", paste(private$.sources, collapse = ", "), "\n")
      invisible(self)
    }
  ),

  private = list(
    .lake = NULL,
    .from = NULL,
    .joins = list(),
    .where = list(),
    .select = NULL,
    .mutate = list(),
    .group_by = NULL,
    .summarize = NULL,
    .having = list(),
    .order_by = NULL,
    .limit = NULL,
    .offset = NULL,
    .distinct = FALSE,
    .sources = character(0),

    # Reset all query parts
    .reset = function() {
      private$.from <- NULL
      private$.joins <- list()
      private$.where <- list()
      private$.select <- NULL
      private$.mutate <- list()
      private$.group_by <- NULL
      private$.summarize <- NULL
      private$.having <- list()
      private$.order_by <- NULL
      private$.limit <- NULL
      private$.offset <- NULL
      private$.distinct <- FALSE
      private$.sources <- character(0)
    },

    # Build the dplyr query chain
    .build_query = function() {
      if (is.null(private$.from)) {
        stop("No table specified. Use $from() first.", call. = FALSE)
      }

      # Start with the source table
      result <- private$.lake$ref(private$.from$table)

      # Apply JOINs
      for (join_spec in private$.joins) {
        right_tbl <- private$.lake$ref(join_spec$table)

        join_fn <- switch(join_spec$type,
          "left" = dplyr::left_join,
          "inner" = dplyr::inner_join,
          "right" = dplyr::right_join,
          "full" = dplyr::full_join,
          stop("Unknown join type: ", join_spec$type)
        )

        if (!is.null(join_spec$on)) {
          result <- join_fn(result, right_tbl, by = join_spec$on)
        } else {
          result <- join_fn(result, right_tbl)
        }
      }

      # Apply WHERE filters
      for (expr in private$.where) {
        result <- dplyr::filter(result, !!expr)
      }

      # Apply MUTATE
      if (length(private$.mutate) > 0) {
        result <- dplyr::mutate(result, !!!private$.mutate)
      }

      # Apply GROUP BY
      if (!is.null(private$.group_by) && length(private$.group_by) > 0) {
        result <- dplyr::group_by(result, !!!private$.group_by)
      }

      # Apply SUMMARIZE
      if (!is.null(private$.summarize) && length(private$.summarize) > 0) {
        result <- dplyr::summarize(result, !!!private$.summarize, .groups = "drop")
      }

      # Apply HAVING (filter after group)
      for (expr in private$.having) {
        result <- dplyr::filter(result, !!expr)
      }

      # Apply SELECT
      if (!is.null(private$.select) && length(private$.select) > 0) {
        result <- dplyr::select(result, !!!private$.select)
      }

      # Apply DISTINCT
      if (isTRUE(private$.distinct)) {
        result <- dplyr::distinct(result)
      } else if (is.list(private$.distinct) && length(private$.distinct) > 0) {
        result <- dplyr::distinct(result, !!!private$.distinct, .keep_all = TRUE)
      }

      # Apply ORDER BY
      if (!is.null(private$.order_by) && length(private$.order_by) > 0) {
        result <- dplyr::arrange(result, !!!private$.order_by)
      }

      result
    },

    # Execute the query
    .execute = function() {
      result <- private$.build_query()

      # Apply OFFSET and LIMIT using slice
      if (!is.null(private$.offset) || !is.null(private$.limit)) {
        # Collect first to apply offset/limit correctly
        result <- dplyr::collect(result)

        start_row <- (private$.offset %||% 0) + 1
        end_row <- if (!is.null(private$.limit)) {
          start_row + private$.limit - 1
        } else {
          nrow(result)
        }

        result <- result[start_row:min(end_row, nrow(result)), , drop = FALSE]
      } else {
        result <- dplyr::collect(result)
      }

      # Attach lineage info
      attr(result, "lake_deps") <- private$.sources

      result
    }
  )
)

# Helper for NULL coalescing (in case rlang's isn't available in scope)
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
