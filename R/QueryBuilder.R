#' @title QueryBuilder - Fluent Query Interface
#' @description Build complex queries using a fluent, chainable API.
#' Provides an intuitive way to construct queries without writing SQL.
#'
#' @examples
#' if (FALSE) {
#'     lake <- Lake$new("my_project")
#'
#'     # Basic query
#'     lake$from("users")$
#'         where(age > 30)$
#'         select(name, age)$
#'         run()
#'
#'     # Join and aggregate
#'     lake$from("orders")$
#'         join("customers", on = "customer_id")$
#'         where(status == "completed")$
#'         group_by(region)$
#'         summarize(total = sum(amount))$
#'         order_by(desc(total))$
#'         run()
#'
#'     # Save results to lake
#'     lake$from("data")$
#'         filter(quality > 0.8)$
#'         as("filtered_data")
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
            private$.validate_name(table, "table")
            if (!private$.lake$.__enclos_env__$private$.is_table(table)) {
                available <- tryCatch(private$.lake$tables()$table_name,
                    error = function(e) character(0))
                hint <- private$.unknown_table_hint(table, available)
                stop("Unknown table '", table, "'.", hint, call. = FALSE)
            }
            if (!is.null(alias)) {
                private$.validate_name(alias, "alias")
            }
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
        #' @param on Join condition (character vector of column names, or named
        #' vector for different column names)
        #' @param type Join type ("left", "inner", "right", "full")
        #' @param alias Optional alias for the joined table
        #' @return Self for chaining
        join = function(table, on = NULL, type = "left", alias = NULL) {
            private$.validate_name(table, "table")
            type <- match.arg(type, choices = c("left", "inner", "right",
                "full"))
            if (!private$.lake$.__enclos_env__$private$.is_table(table)) {
                available <- tryCatch(private$.lake$tables()$table_name,
                    error = function(e) character(0))
                hint <- private$.unknown_table_hint(table, available)
                stop("Unknown join table '", table, "'.", hint, call. = FALSE)
            }
            if (!is.null(alias)) {
                private$.validate_name(alias, "alias")
            }
            private$.validate_join_by(on)

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
            private$.limit <- private$.validate_nonnegative_integer(n, "n",
                allow_zero = FALSE)
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
            n <- private$.validate_nonnegative_integer(n, "n",
                allow_zero = FALSE)
            by_expr <- rlang::enquo(by)
            if (desc) {
                private$.order_by <- rlang::quos(dplyr::desc(!!by_expr))
            } else {
                private$.order_by <- rlang::quos(!!by_expr)
            }
            private$.limit <- n
            self
        },

        #' @description Skip rows
        #' @param n Number of rows to skip
        #' @return Self for chaining
        offset = function(n) {
            private$.offset <- private$.validate_nonnegative_integer(n, "n",
                allow_zero = TRUE)
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
            tryCatch(
                private$.execute(),
                error = function(e) {
                    sql <- tryCatch(self$show_sql(), error = function(e2) NULL)
                    if (is.null(sql)) {
                        stop("Query execution failed: ", conditionMessage(e),
                            call. = FALSE)
                    }
                    stop(
                        "Query execution failed: ", conditionMessage(e),
                        "\nRendered SQL: ", as.character(sql),
                        call. = FALSE
                    )
                }
            )
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
            private$.validate_name(name, "name")
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
            lines <- c("QueryBuilder:")
            if (!is.null(private$.from)) {
                from_line <- paste0("  FROM: ", private$.from$table)
                if (private$.from$alias != private$.from$table) {
                    from_line <- paste0(from_line, " AS ", private$.from$alias)
                }
                lines <- c(lines, from_line)
            }
            if (length(private$.joins) > 0) {
                lines <- c(lines, paste0("  JOINS: ", length(private$.joins)))
                for (j in private$.joins) {
                    lines <- c(lines, paste0("    ", toupper(j$type), " JOIN ",
                        j$table))
                }
            }
            if (length(private$.where) > 0) {
                lines <- c(lines, paste0("  WHERE: ", length(private$.where),
                    " condition(s)"))
            }
            if (!is.null(private$.select) && length(private$.select) > 0) {
                lines <- c(lines, paste0("  SELECT: ", length(private$.select),
                    " column(s)"))
            }
            if (!is.null(private$.group_by) && length(private$.group_by) > 0) {
                lines <- c(lines, paste0("  GROUP BY: ",
                    length(private$.group_by), " column(s)"))
            }
            if (!is.null(private$.order_by) && length(private$.order_by) > 0) {
                lines <- c(lines, paste0("  ORDER BY: ",
                    length(private$.order_by), " column(s)"))
            }
            if (!is.null(private$.limit)) {
                lines <- c(lines, paste0("  LIMIT: ", private$.limit))
            }
            lines <- c(lines, paste0("  Sources tracked: ",
                paste(private$.sources, collapse = ", ")))
            writeLines(lines)
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
        .validate_name = function(x, arg = "name") {
            if (!is.character(x) || length(x) != 1 || !nzchar(x)) {
                stop(arg, " must be a non-empty character string",
                    call. = FALSE)
            }
            invisible(TRUE)
        },
        .validate_nonnegative_integer = function(x, arg = "value",
            allow_zero = TRUE) {
            if (!is.numeric(x) || length(x) != 1 || is.na(x) || !is.finite(x)) {
                stop(arg, " must be a single finite number", call. = FALSE)
            }
            if (x %% 1 != 0) {
                stop(arg, " must be an integer value", call. = FALSE)
            }
            if (allow_zero && x < 0) {
                stop(arg, " must be >= 0", call. = FALSE)
            }
            if (!allow_zero && x <= 0) {
                stop(arg, " must be > 0", call. = FALSE)
            }
            as.integer(x)
        },
        .validate_join_by = function(on) {
            if (is.null(on)) {
                return(invisible(TRUE))
            }
            if (!is.character(on) || !length(on) || any(!nzchar(on))) {
                stop("on must be a non-empty character vector when provided",
                    call. = FALSE)
            }
            if (!is.null(names(on)) && any(names(on) == "")) {
                stop("named join mappings in 'on' must not contain empty names",
                    call. = FALSE)
            }
            invisible(TRUE)
        },
        .suggest_names = function(target, candidates, max_dist = 3L,
            limit = 3L) {
            if (
                !length(candidates) ||
                !is.character(target) ||
                !length(target) ||
                !nzchar(target)
            ) {
                return(character(0))
            }
            candidates <- as.character(candidates)
            d <- utils::adist(tolower(target), tolower(candidates))[1, ]
            keep <- which(d <= max_dist)
            if (!length(keep)) {
                return(character(0))
            }
            ord <- keep[order(d[keep], candidates[keep])]
            unique(candidates[ord])[seq_len(min(length(ord), limit))]
        },
        .unknown_table_hint = function(target, available) {
            if (!length(available)) {
                return(" No tables are currently stored in the lake.")
            }
            available <- sort(unique(as.character(available)))
            hint <- paste0(" Available tables: ", paste(available,
                collapse = ", "), ".")
            similar <- private$.suggest_names(target, available)
            if (length(similar)) {
                hint <- paste0(hint, " Closest matches: ", paste(similar,
                    collapse = ", "), ".")
            }
            hint
        },

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
        .join_fn = function(type) {
            switch(type,
                left = dplyr::left_join,
                inner = dplyr::inner_join,
                right = dplyr::right_join,
                full = dplyr::full_join,
                stop("Unknown join type: ", type)
            )
        },
        .apply_joins = function(result) {
            for (join_spec in private$.joins) {
                join_fn <- private$.join_fn(join_spec$type)
                right_tbl <- private$.lake$ref(join_spec$table)
                if (is.null(join_spec$on)) {
                    result <- join_fn(result, right_tbl)
                } else {
                    result <- join_fn(result, right_tbl, by = join_spec$on)
                }
            }
            result
        },
        .apply_filters = function(result, filters) {
            for (expr in filters) {
                result <- dplyr::filter(result, !!expr)
            }
            result
        },
        .apply_group_summarize = function(result) {
            if (!is.null(private$.group_by) && length(private$.group_by) > 0) {
                result <- dplyr::group_by(result, !!!private$.group_by)
            }
            if (!is.null(private$.summarize) && 
                length(private$.summarize) > 0) {
                result <- dplyr::summarize(
                    result,
                    !!!private$.summarize,
                    .groups = "drop"
                )
            }
            result
        },
        .apply_select_distinct_order = function(result) {
            if (!is.null(private$.select) && length(private$.select) > 0) {
                result <- dplyr::select(result, !!!private$.select)
            }
            if (isTRUE(private$.distinct)) {
                result <- dplyr::distinct(result)
            } else if (is.list(private$.distinct) && 
                length(private$.distinct) > 0) {
                result <- dplyr::distinct(
                    result,
                    !!!private$.distinct,
                    .keep_all = TRUE
                )
            }
            if (!is.null(private$.order_by) && length(private$.order_by) > 0) {
                result <- dplyr::arrange(result, !!!private$.order_by)
            }
            result
        },
        .build_query = function() {
            if (is.null(private$.from)) {
                stop("No table specified. Use $from() first.", call. = FALSE)
            }
            result <- private$.lake$ref(private$.from$table)
            result <- private$.apply_joins(result)
            result <- private$.apply_filters(result, private$.where)
            if (length(private$.mutate) > 0) {
                result <- dplyr::mutate(result, !!!private$.mutate)
            }
            result <- private$.apply_group_summarize(result)
            result <- private$.apply_filters(result, private$.having)
            private$.apply_select_distinct_order(result)
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

                result <- result[start_row:min(end_row, nrow(result)), ,
                    drop = FALSE]
            } else {
                result <- dplyr::collect(result)
            }

            # Attach lineage info
            attr(result, "lake_deps") <- private$.sources

            result
        }
    )
)

# NULL coalescing operator (local definition to avoid importFrom dependency)
`%||%` <- function(x, y) if (is.null(x)) y else x
