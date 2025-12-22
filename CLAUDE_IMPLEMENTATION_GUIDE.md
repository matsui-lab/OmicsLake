# OmicsLake v2.0 実装指示書

## 概要

OmicsLakeを「Data Lineage中核思想を保持しつつ、既存オミクス解析ワークフローに最小限の変更で導入できる透過的アドオンパッケージ」へと進化させる。

### 設計原則

1. **Lineage First**: すべての操作でデータ系譜を自動追跡
2. **Zero Friction**: 既存パイプラインへの導入障壁を最小化
3. **R Native**: SQL不要、R構文でクエリ記述
4. **Progressive Adoption**: 段階的に機能を導入可能
5. **Extensible**: アダプター/プラグインによる拡張性

---

## Phase 1: コアAPI再設計

### 1.1 R6クラスベースのメインAPI

**ファイル**: `R/Lake.R`

```r
#' @title Lake - OmicsLake Core Class
#' @description R6 class for versioned, lineage-tracked data management
#' @export
Lake <- R6::R6Class("Lake",

  public = list(

    #' @description Initialize a Lake project
    #' @param project Project name or path
    #' @param backend Storage backend ("duckdb", "sqlite", "memory")
    #' @param auto_track Enable automatic dependency tracking (default: TRUE)
    initialize = function(project = NULL,
                          backend = "duckdb",
                          auto_track = TRUE) {
      # 自動プロジェクト名生成（指定なしの場合）
      if (is.null(project)) {
        project <- private$.generate_project_name()
      }
      private$.project <- project
      private$.backend <- backend
      private$.auto_track <- auto_track
      private$.init_backend()
      invisible(self)
    },

    # ========== Core I/O ==========

    #' @description Write data to the lake
    #' @param name Name for the data
    #' @param data Data to store (data.frame, matrix, list, or any R object)
    #' @param depends_on Optional explicit dependencies (auto-detected if NULL)
    #' @param tags Optional tags for this version
    put = function(name, data, depends_on = NULL, tags = NULL) {
      private$.validate_name(name)

      # 型に応じた保存戦略
      storage_type <- private$.detect_storage_type(data)

      # 依存関係の自動検出
      if (is.null(depends_on) && private$.auto_track) {
        depends_on <- private$.detect_dependencies()
      }

      # 保存実行
      result <- switch(storage_type,
        "table" = private$.put_table(name, data),
        "object" = private$.put_object(name, data),
        "se" = private$.put_summarized_experiment(name, data),
        "seurat" = private$.put_seurat(name, data),
        stop("Unsupported data type")
      )

      # 依存関係記録
      private$.record_dependencies(name, storage_type, depends_on)

      # タグ付け
      if (!is.null(tags)) {
        for (tag in tags) {
          self$tag(name, tag)
        }
      }

      invisible(self)
    },

    #' @description Read data from the lake
    #' @param name Name of the data to read
    #' @param ref Version reference ("@latest", "@first", "@tag(name)", timestamp)
    #' @param where Filter condition (formula or expression)
    #' @param select Columns to select (character vector or tidyselect)
    #' @param collect Whether to collect results (FALSE returns lazy reference)
    get = function(name,
                   ref = "@latest",
                   where = NULL,
                   select = NULL,
                   collect = TRUE) {
      private$.validate_name(name)

      # 依存関係追跡に記録
      private$.track_read(name, ref)

      # データ取得
      data <- private$.get_data(name, ref)

      # フィルタ適用
      if (!is.null(where)) {
        data <- private$.apply_filter(data, where)
      }

      # カラム選択
      if (!is.null(select)) {
        data <- private$.apply_select(data, select)
      }

      # 遅延評価 or 即時評価
      if (!collect && private$.is_lazy(data)) {
        return(data)
      }

      if (!collect) {
        return(private$.as_lazy(data))
      }

      private$.collect(data)
    },

    #' @description Get a lazy reference for dplyr operations
    #' @param name Table name
    ref = function(name) {
      private$.track_read(name, "@latest")
      private$.get_lazy_ref(name)
    },

    # ========== Versioning ==========

    #' @description Create a project-wide snapshot
    #' @param label Label for this snapshot
    #' @param note Optional description
    #' @param params Optional parameters to store
    snap = function(label, note = "", params = list()) {
      private$.validate_name(label)
      private$.create_snapshot(label, note, params)
      invisible(self)
    },

    #' @description Tag a specific data version
    #' @param name Data name
    #' @param tag Tag to apply
    tag = function(name, tag) {
      private$.validate_name(name)
      private$.validate_name(tag)
      private$.apply_tag(name, tag)
      invisible(self)
    },

    #' @description Restore project to a snapshot
    #' @param label Snapshot label to restore
    restore = function(label) {
      private$.restore_snapshot(label)
      invisible(self)
    },

    #' @description Compare versions
    #' @param name Data name
    #' @param ref1 First version reference

    #' @param ref2 Second version reference
    diff = function(name, ref1 = "@latest", ref2 = "@first") {
      private$.compare_versions(name, ref1, ref2)
    },

    # ========== Lineage ==========

    #' @description Show lineage tree
    #' @param name Starting node
    #' @param direction "up" (ancestors), "down" (descendants), "both"
    #' @param depth Maximum depth to traverse
    tree = function(name = NULL, direction = "up", depth = 10) {
      private$.get_lineage_tree(name, direction, depth)
    },

    #' @description Plot lineage graph
    #' @param name Starting node (NULL for full graph)
    #' @param direction "up", "down", or "both"
    plot = function(name = NULL, direction = "both") {
      private$.plot_lineage(name, direction)
    },

    #' @description Get direct dependencies
    #' @param name Data name
    #' @param direction "up" (parents) or "down" (children)
    deps = function(name, direction = "up") {
      private$.get_dependencies(name, direction)
    },

    #' @description Analyze impact of changing a data source
    #' @param name Data name to analyze
    impact = function(name) {
      private$.analyze_impact(name)
    },

    # ========== Query Builder ==========

    #' @description Start a query builder chain
    query = function() {
      QueryBuilder$new(self)
    },

    #' @description Shortcut to start query from a table
    #' @param table Table name
    from = function(table) {
      self$query()$from(table)
    },

    #' @description Join two tables
    #' @param left Left table name
    #' @param right Right table name
    #' @param by Join columns
    #' @param type Join type ("left", "inner", "right", "full")
    join = function(left, right, by = NULL, type = "left") {
      private$.join_tables(left, right, by, type)
    },

    # ========== Aggregation Shortcuts ==========

    #' @description Count rows
    #' @param table Table name
    #' @param ... Grouping variables
    count = function(table, ...) {
      self$ref(table) |>
        dplyr::group_by(...) |>
        dplyr::summarize(n = dplyr::n(), .groups = "drop") |>
        dplyr::collect()
    },

    #' @description Calculate mean
    #' @param table Table name
    #' @param col Column to average
    #' @param ... Grouping variables
    mean = function(table, col, ...) {
      col_sym <- rlang::ensym(col)
      self$ref(table) |>
        dplyr::group_by(...) |>
        dplyr::summarize(mean = mean(!!col_sym, na.rm = TRUE), .groups = "drop") |>
        dplyr::collect()
    },

    # ========== Listing ==========

    #' @description List all tables
    tables = function() {
      private$.list_tables()
    },

    #' @description List all objects
    objects = function() {
      private$.list_objects()
    },

    #' @description List all data (tables + objects)
    ls = function() {
      list(
        tables = self$tables(),
        objects = self$objects()
      )
    },

    #' @description List snapshots/labels
    snaps = function() {
      private$.list_snapshots()
    },

    #' @description Show history/log
    #' @param name Optional data name (NULL for project history)
    #' @param n Number of entries
    log = function(name = NULL, n = 20) {
      private$.get_history(name, n)
    },

    history = function(name = NULL, n = 20) {
      self$log(name, n)
    },

    # ========== Data Management ==========

    #' @description Remove data
    #' @param name Data name
    #' @param force Force removal even if has dependents
    drop = function(name, force = FALSE) {
      # 依存関係チェック
      dependents <- self$deps(name, direction = "down")
      if (nrow(dependents) > 0 && !force) {
        stop("Cannot drop '", name, "': has ", nrow(dependents),
             " dependent(s). Use force = TRUE to override.")
      }
      private$.drop_data(name)
      invisible(self)
    },

    rm = function(name, force = FALSE) {
      self$drop(name, force)
    },

    # ========== Import/Export ==========

    #' @description Export data
    #' @param name Data name
    #' @param path Output file path
    #' @param format Output format ("parquet", "csv", "rds", "h5ad")
    export = function(name, path, format = NULL) {
      if (is.null(format)) {
        format <- private$.detect_format(path)
      }
      private$.export_data(name, path, format)
      invisible(path)
    },

    #' @description Import external data
    #' @param path Input file path
    #' @param name Name to store as
    #' @param format Input format (auto-detected if NULL)
    import = function(path, name, format = NULL) {
      if (is.null(format)) {
        format <- private$.detect_format(path)
      }
      data <- private$.import_data(path, format)
      self$put(name, data)
      invisible(self)
    },

    # ========== SQL (Escape Hatch) ==========

    #' @description Execute raw SQL query
    #' @param sql SQL string
    #' @param collect Whether to collect results
    sql = function(sql, collect = TRUE) {
      private$.execute_sql(sql, collect)
    },

    q = function(sql, collect = TRUE) {
      self$sql(sql, collect)
    },

    # ========== Bracket Notation ==========

    #' @description Bracket access for get/put
    `[` = function(name, i, j) {
      if (missing(i) && missing(j)) {
        return(self$get(name))
      }

      data <- self$get(name)

      if (!missing(i)) {
        # Row filter
        i_expr <- substitute(i)
        data <- dplyr::filter(data, !!i_expr)
      }

      if (!missing(j)) {
        # Column select
        j_expr <- substitute(j)
        if (is.call(j_expr) && identical(j_expr[[1]], as.name("."))) {
          # data.table style: .(col1, col2)
          cols <- as.character(j_expr[-1])
          data <- dplyr::select(data, dplyr::all_of(cols))
        } else {
          data <- dplyr::select(data, !!j_expr)
        }
      }

      data
    },

    `[<-` = function(name, value) {
      self$put(name, value)
      invisible(self)
    },

    # ========== Printing ==========

    print = function() {
      cat("Lake:", private$.project, "\n")
      cat("Backend:", private$.backend, "\n")
      cat("Tables:", nrow(self$tables()), "\n")
      cat("Objects:", nrow(self$objects()), "\n")
      cat("Auto-track:", private$.auto_track, "\n")
      invisible(self)
    }
  ),

  private = list(
    .project = NULL,
    .backend = NULL,
    .conn = NULL,
    .state = NULL,
    .auto_track = TRUE,
    .read_stack = list(),  # 依存関係追跡用スタック

    # Implementation methods (see Phase 1.2)
    .init_backend = function() {},
    .validate_name = function(name) {},
    .detect_storage_type = function(data) {},
    .detect_dependencies = function() {},
    .put_table = function(name, data) {},
    .put_object = function(name, data) {},
    .get_data = function(name, ref) {},
    .apply_filter = function(data, where) {},
    .apply_select = function(data, select) {},
    .record_dependencies = function(name, type, depends_on) {},
    .track_read = function(name, ref) {},
    .get_lazy_ref = function(name) {},
    .create_snapshot = function(label, note, params) {},
    .apply_tag = function(name, tag) {},
    .restore_snapshot = function(label) {},
    .compare_versions = function(name, ref1, ref2) {},
    .get_lineage_tree = function(name, direction, depth) {},
    .plot_lineage = function(name, direction) {},
    .get_dependencies = function(name, direction) {},
    .analyze_impact = function(name) {},
    .join_tables = function(left, right, by, type) {},
    .list_tables = function() {},
    .list_objects = function() {},
    .list_snapshots = function() {},
    .get_history = function(name, n) {},
    .drop_data = function(name) {},
    .export_data = function(name, path, format) {},
    .import_data = function(path, format) {},
    .execute_sql = function(sql, collect) {},
    .generate_project_name = function() {},
    .is_lazy = function(data) {},
    .as_lazy = function(data) {},
    .collect = function(data) {}
  )
)
```

### 1.2 グローバルショートカット関数

**ファイル**: `R/shortcuts.R`

```r
#' @title Global Lake Shortcuts
#' @description Convenience functions for working with a default lake

# グローバルデフォルトLake
.lake_env <- new.env(parent = emptyenv())

#' Set or get the default lake
#' @param project Project name (if setting)
#' @param ... Additional arguments passed to Lake$new()
#' @export
use_lake <- function(project = NULL, ...) {
  if (!is.null(project)) {
    .lake_env$default <- Lake$new(project, ...)
  }
  invisible(.lake_env$default)
}

#' Get the current default lake
#' @export
lake <- function() {
  if (is.null(.lake_env$default)) {
    stop("No default lake set. Use use_lake('project') first.")
  }
  .lake_env$default
}

#' @describeIn use_lake Write data to default lake
#' @export
put <- function(name, data, ...) {
  lake()$put(name, data, ...)
}

#' @describeIn use_lake Read data from default lake
#' @export
get <- function(name, ...) {
  lake()$get(name, ...)
}
# Note: This shadows base::get(). Consider alternative name if problematic.

#' @describeIn use_lake Get lazy reference
#' @export
ref <- function(name) {
  lake()$ref(name)
}

#' @describeIn use_lake Create snapshot
#' @export
snap <- function(label, ...) {
  lake()$snap(label, ...)
}

#' @describeIn use_lake Tag data
#' @export
tag <- function(name, tag) {
  lake()$tag(name, tag)
}

#' @describeIn use_lake Show lineage
#' @export
tree <- function(name = NULL, ...) {
  lake()$tree(name, ...)
}

#' @describeIn use_lake Show history
#' @export
history <- function(name = NULL, ...) {
  lake()$history(name, ...)
}

#' @describeIn use_lake List tables
#' @export
tables <- function() {
  lake()$tables()
}

#' @describeIn use_lake Drop data
#' @export
drop <- function(name, ...) {
  lake()$drop(name, ...)
}

#' @describeIn use_lake Execute SQL
#' @export
sql <- function(query, ...) {
  lake()$sql(query, ...)
}
```

---

## Phase 2: Query Builder実装

### 2.1 QueryBuilder クラス

**ファイル**: `R/QueryBuilder.R`

```r
#' @title Query Builder for Lake
#' @description Fluent interface for building complex queries
#' @export
QueryBuilder <- R6::R6Class("QueryBuilder",

  public = list(

    initialize = function(lake) {
      private$.lake <- lake
      private$.parts <- list(
        from = NULL,
        joins = list(),
        where = list(),
        select = NULL,
        group_by = NULL,
        having = list(),
        order_by = NULL,
        limit = NULL,
        offset = NULL
      )
      invisible(self)
    },

    #' @description Specify source table
    #' @param table Table name
    #' @param alias Optional alias
    from = function(table, alias = NULL) {
      private$.parts$from <- list(table = table, alias = alias %||% table)
      private$.lake$.__enclos_env__$private$.track_read(table, "@latest")
      self
    },

    #' @description Add a join
    #' @param table Table to join
    #' @param on Join condition (formula or expression)
    #' @param type Join type
    #' @param alias Optional alias
    join = function(table, on = NULL, type = "left", alias = NULL) {
      private$.parts$joins <- append(private$.parts$joins, list(
        list(table = table, on = on, type = type, alias = alias %||% table)
      ))
      private$.lake$.__enclos_env__$private$.track_read(table, "@latest")
      self
    },

    left_join = function(table, on = NULL, alias = NULL) {
      self$join(table, on, "left", alias)
    },

    inner_join = function(table, on = NULL, alias = NULL) {
      self$join(table, on, "inner", alias)
    },

    right_join = function(table, on = NULL, alias = NULL) {
      self$join(table, on, "right", alias)
    },

    full_join = function(table, on = NULL, alias = NULL) {
      self$join(table, on, "full", alias)
    },

    #' @description Add filter condition
    #' @param ... Filter expressions (combined with AND)
    where = function(...) {
      exprs <- rlang::enquos(...)
      private$.parts$where <- append(private$.parts$where, exprs)
      self
    },

    #' @description Alias for where
    filter = function(...) {
      self$where(...)
    },

    #' @description Select columns
    #' @param ... Column names or expressions
    select = function(...) {
      private$.parts$select <- rlang::enquos(...)
      self
    },

    #' @description Alias for select
    pick = function(...) {
      self$select(...)
    },

    #' @description Group by columns
    #' @param ... Grouping columns
    group_by = function(...) {
      private$.parts$group_by <- rlang::enquos(...)
      self
    },

    #' @description Having clause (after group_by)
    #' @param ... Having conditions
    having = function(...) {
      exprs <- rlang::enquos(...)
      private$.parts$having <- append(private$.parts$having, exprs)
      self
    },

    #' @description Order results
    #' @param ... Columns to order by
    order_by = function(...) {
      private$.parts$order_by <- rlang::enquos(...)
      self
    },

    #' @description Alias for order_by
    arrange = function(...) {
      self$order_by(...)
    },

    #' @description Limit results
    #' @param n Number of rows
    limit = function(n) {
      private$.parts$limit <- n
      self
    },

    #' @description Alias for limit
    take = function(n) {
      self$limit(n)
    },

    #' @description Top N by column
    #' @param n Number of rows
    #' @param by Column to order by
    #' @param desc Descending order
    top = function(n, by, desc = TRUE) {
      by_expr <- rlang::enquo(by)
      if (desc) {
        self$order_by(dplyr::desc(!!by_expr))
      } else {
        self$order_by(!!by_expr)
      }
      self$limit(n)
    },

    #' @description Skip rows
    #' @param n Number of rows to skip
    offset = function(n) {
      private$.parts$offset <- n
      self
    },

    #' @description Execute query and return results
    run = function() {
      private$.execute()
    },

    #' @description Alias for run
    collect = function() {
      self$run()
    },

    #' @description Execute and save to lake
    #' @param name Name to save as
    as = function(name) {
      result <- self$run()
      private$.lake$put(name, result)
      invisible(private$.lake)
    },

    #' @description Show generated SQL (for debugging)
    show_sql = function() {
      private$.build_sql()
    },

    #' @description Explain query plan
    explain = function() {
      sql <- private$.build_sql()
      private$.lake$sql(paste("EXPLAIN", sql))
    },

    #' @description Print query builder state
    print = function() {
      cat("QueryBuilder:\n")
      cat("  FROM:", private$.parts$from$table, "\n")
      if (length(private$.parts$joins) > 0) {
        cat("  JOINS:", length(private$.parts$joins), "\n")
      }
      if (length(private$.parts$where) > 0) {
        cat("  WHERE conditions:", length(private$.parts$where), "\n")
      }
      invisible(self)
    }
  ),

  private = list(
    .lake = NULL,
    .parts = NULL,

    .execute = function() {
      # dplyrチェーンを構築して実行
      result <- private$.lake$ref(private$.parts$from$table)

      # JOINs
      for (join_spec in private$.parts$joins) {
        join_fn <- switch(join_spec$type,
          "left" = dplyr::left_join,
          "inner" = dplyr::inner_join,
          "right" = dplyr::right_join,
          "full" = dplyr::full_join
        )
        right_tbl <- private$.lake$ref(join_spec$table)

        if (!is.null(join_spec$on)) {
          result <- join_fn(result, right_tbl, by = join_spec$on)
        } else {
          result <- join_fn(result, right_tbl)
        }
      }

      # WHERE
      for (expr in private$.parts$where) {
        result <- dplyr::filter(result, !!expr)
      }

      # GROUP BY
      if (!is.null(private$.parts$group_by)) {
        result <- dplyr::group_by(result, !!!private$.parts$group_by)
      }

      # HAVING
      for (expr in private$.parts$having) {
        result <- dplyr::filter(result, !!expr)
      }

      # SELECT
      if (!is.null(private$.parts$select)) {
        result <- dplyr::select(result, !!!private$.parts$select)
      }

      # ORDER BY
      if (!is.null(private$.parts$order_by)) {
        result <- dplyr::arrange(result, !!!private$.parts$order_by)
      }

      # LIMIT + OFFSET
      if (!is.null(private$.parts$offset)) {
        # DuckDB supports OFFSET
        result <- dplyr::slice(result, (private$.parts$offset + 1):dplyr::n())
      }
      if (!is.null(private$.parts$limit)) {
        result <- dplyr::slice_head(result, n = private$.parts$limit)
      }

      dplyr::collect(result)
    },

    .build_sql = function() {
      # SQL文字列を生成（デバッグ用）
      # 実際の実装ではdbplyrのshow_query()を活用
      "-- SQL generation not implemented"
    }
  )
)
```

### 2.2 カスタム演算子

**ファイル**: `R/operators.R`

```r
#' @title Custom Query Operators
#' @description SQL-like operators for use in Lake queries

#' LIKE operator for pattern matching
#' @param x Column or value
#' @param pattern Pattern to match (SQL LIKE syntax)
#' @export
`%like%` <- function(x, pattern) {
  x_name <- rlang::as_name(rlang::enquo(x))
  # DuckDB/SQL LIKE互換のパターンマッチング
  grepl(glob2rx(pattern), x, ignore.case = FALSE)
}

#' Case-insensitive LIKE
#' @export
`%ilike%` <- function(x, pattern) {
  grepl(glob2rx(pattern), x, ignore.case = TRUE)
}

#' BETWEEN operator
#' @param x Column or value
#' @param range Numeric vector of length 2 (min, max)
#' @export
`%between%` <- function(x, range) {
  x >= range[1] & x <= range[2]
}

#' NOT BETWEEN operator
#' @export
`%!between%` <- function(x, range) {
  x < range[1] | x > range[2]
}

#' IN operator (alternative to %in%)
#' @export
`%in_set%` <- function(x, set) {
  x %in% set
}

#' NOT IN operator
#' @export
`%!in%` <- function(x, set) {
  !(x %in% set)
}

#' IS NULL check (NA in R)
#' @param x Value to check
#' @export
is_null <- function(x) {
  is.na(x)
}

#' IS NOT NULL check
#' @export
is_not_null <- function(x) {
  !is.na(x)
}

#' Regex match operator
#' @export
`%regex%` <- function(x, pattern) {
  grepl(pattern, x, perl = TRUE)
}

#' Starts with
#' @export
starts_with_str <- function(x, prefix) {
  startsWith(as.character(x), prefix)
}

#' Ends with
#' @export
ends_with_str <- function(x, suffix) {
  endsWith(as.character(x), suffix)
}

#' Contains
#' @export
contains_str <- function(x, substring) {
  grepl(substring, x, fixed = TRUE)
}
```

---

## Phase 3: 自動依存関係追跡

### 3.1 依存関係トラッカー

**ファイル**: `R/tracker.R`

```r
#' @title Automatic Dependency Tracker
#' @description Track data dependencies automatically during operations

DependencyTracker <- R6::R6Class("DependencyTracker",

  public = list(

    initialize = function(lake) {
      private$.lake <- lake
      private$.stack <- list()
      private$.current_writes <- character(0)
      invisible(self)
    },

    #' Record a read operation
    #' @param name Data name being read
    #' @param ref Version reference
    track_read = function(name, ref = "@latest") {
      if (length(private$.stack) > 0) {
        # 現在の書き込みコンテキストに追加
        ctx <- private$.stack[[length(private$.stack)]]
        ctx$reads <- unique(c(ctx$reads, name))
        private$.stack[[length(private$.stack)]] <- ctx
      }
      invisible(self)
    },

    #' Start a write context
    #' @param name Data name being written
    start_write = function(name) {
      private$.stack <- append(private$.stack, list(
        list(name = name, reads = character(0), start_time = Sys.time())
      ))
      invisible(self)
    },

    #' End write context and return dependencies
    #' @return Character vector of dependency names
    end_write = function() {
      if (length(private$.stack) == 0) {
        return(character(0))
      }

      ctx <- private$.stack[[length(private$.stack)]]
      private$.stack <- private$.stack[-length(private$.stack)]

      ctx$reads
    },

    #' Get current tracked reads
    current_reads = function() {
      if (length(private$.stack) == 0) {
        return(character(0))
      }
      private$.stack[[length(private$.stack)]]$reads
    },

    #' Execute expression while tracking dependencies
    #' @param name Output name
    #' @param expr Expression to evaluate
    #' @param env Environment for evaluation
    with_tracking = function(name, expr, env = parent.frame()) {
      self$start_write(name)
      tryCatch({
        result <- eval(expr, envir = env)
        deps <- self$end_write()
        list(result = result, dependencies = deps)
      }, error = function(e) {
        self$end_write()  # Clean up on error
        stop(e)
      })
    }
  ),

  private = list(
    .lake = NULL,
    .stack = list()
  )
)

#' Execute block with dependency tracking
#' @param lake Lake instance
#' @param name Output data name
#' @param expr Expression block
#' @export
with_lineage <- function(lake, name, expr) {
  tracker <- lake$.__enclos_env__$private$.tracker
  tracked <- tracker$with_tracking(name, substitute(expr), parent.frame())
  lake$put(name, tracked$result, depends_on = tracked$dependencies)
  invisible(tracked$result)
}
```

### 3.2 式からの依存関係抽出

**ファイル**: `R/expr_deps.R`

```r
#' @title Expression Dependency Extractor
#' @description Extract data dependencies from R expressions

#' Extract lake references from an expression
#' @param expr Expression to analyze
#' @param lake_var Name of lake variable in expression
#' @return Character vector of referenced data names
extract_lake_refs <- function(expr, lake_var = "lake") {
  refs <- character(0)

  # 再帰的に式を走査
  walk_expr <- function(e) {
    if (is.call(e)) {
      fn_name <- as.character(e[[1]])

      # lake$get(), lake$ref(), get(), ref() パターンを検出
      if (fn_name == "$" && length(e) >= 3) {
        obj <- as.character(e[[2]])
        method <- as.character(e[[3]])
        if (obj == lake_var && method %in% c("get", "ref")) {
          # 次の引数がデータ名
          # Note: これは簡略化された実装
        }
      }

      if (fn_name %in% c("get", "ref", "lake$get", "lake$ref")) {
        if (length(e) >= 2 && is.character(e[[2]])) {
          refs <<- c(refs, e[[2]])
        }
      }

      # 子要素を再帰処理
      for (i in seq_along(e)[-1]) {
        walk_expr(e[[i]])
      }
    }
  }

  walk_expr(expr)
  unique(refs)
}

#' Wrap a function to track its lake dependencies
#' @param fn Function to wrap
#' @param lake Lake instance
#' @return Wrapped function that tracks dependencies
track_function <- function(fn, lake) {
  function(...) {
    # 引数から依存関係を検出
    args <- list(...)
    deps <- character(0)

    for (arg in args) {
      if (inherits(arg, "tbl_lazy")) {
        # dbplyr lazy table - ソーステーブル名を抽出
        src_name <- attr(arg, "lake_source")
        if (!is.null(src_name)) {
          deps <- c(deps, src_name)
        }
      }
    }

    result <- fn(...)
    attr(result, "lake_deps") <- unique(deps)
    result
  }
}
```

---

## Phase 4: Formula構文サポート

### 4.1 Formula Parser

**ファイル**: `R/formula.R`

```r
#' @title Formula Query Parser
#' @description Parse formula syntax into filter expressions

#' Parse a filter formula
#' @param formula Formula like ~ col > 5 & col2 == "value"
#' @param data Optional data for validation
#' @return Quosure for use with dplyr
parse_filter_formula <- function(formula, data = NULL) {
  if (!inherits(formula, "formula")) {
    stop("Expected a formula (e.g., ~ col > 5)")
  }

  # RHS of formula is the filter expression
  expr <- rlang::f_rhs(formula)
  env <- rlang::f_env(formula)

  # カスタム演算子を環境に注入
  env <- rlang::env_clone(env)
  env$`%like%` <- `%like%`
  env$`%between%` <- `%between%`
  env$`%ilike%` <- `%ilike%`
  env$is_null <- is_null
  env$is_not_null <- is_not_null

  rlang::new_quosure(expr, env)
}

#' Apply formula filter to data
#' @param data Data frame or lazy table
#' @param formula Filter formula
#' @return Filtered data
apply_formula_filter <- function(data, formula) {
  if (is.null(formula)) {
    return(data)
  }

  filter_quo <- parse_filter_formula(formula)
  dplyr::filter(data, !!filter_quo)
}

#' Parse select specification
#' @param select Character vector, formula, or tidyselect expression
#' @return Column selection for dplyr
parse_select_spec <- function(select) {
  if (is.null(select)) {
    return(NULL)
  }

  if (is.character(select)) {
    return(dplyr::all_of(select))
  }

  if (inherits(select, "formula")) {
    # ~ c(col1, col2) or ~ starts_with("gene")
    expr <- rlang::f_rhs(select)
    return(expr)
  }

  select
}

#' Parse order_by specification
#' @param order_by Column name, formula, or expression
#' @param desc Descending order
#' @return Expression for dplyr::arrange
parse_order_spec <- function(order_by, desc = FALSE) {
  if (is.null(order_by)) {
    return(NULL)
  }

  if (is.character(order_by)) {
    order_by <- rlang::sym(order_by)
  } else if (inherits(order_by, "formula")) {
    order_by <- rlang::f_rhs(order_by)
  }

  if (desc) {
    rlang::expr(dplyr::desc(!!order_by))
  } else {
    order_by
  }
}
```

---

## Phase 5: dplyr統合

### 5.1 Pipe-friendlyメソッド

**ファイル**: `R/dplyr_compat.R`

```r
#' @title dplyr Compatibility Layer
#' @description Make Lake work seamlessly with dplyr pipelines

#' Pipe-friendly put
#' @param data Data to save
#' @param lake Lake instance
#' @param name Name to save as
#' @return Invisibly returns the data (for further piping)
#' @export
lake_put <- function(data, lake, name) {
  # 依存関係を属性から抽出
  deps <- attr(data, "lake_deps")

  # データフレームに変換（必要に応じて）
  if (inherits(data, "tbl_lazy")) {
    data <- dplyr::collect(data)
  }

  lake$put(name, data, depends_on = deps)
  invisible(data)
}

#' Create a pipe-compatible lake write function
#' @param lake Lake instance
#' @return Function that can be used at end of pipe
#' @export
into <- function(lake) {
  function(data, name) {
    lake_put(data, lake, name)
  }
}

#' Attach lineage tracking to a lazy table
#' @param tbl Lazy table from lake$ref()
#' @param source_name Source table name
#' @return Same table with lineage metadata
attach_lineage <- function(tbl, source_name) {
  attr(tbl, "lake_source") <- source_name
  class(tbl) <- c("lake_tbl", class(tbl))
  tbl
}

#' Custom collect that preserves lineage
#' @export
collect.lake_tbl <- function(x, ...) {
  result <- NextMethod()
  # 依存関係を結果に転送
  attr(result, "lake_deps") <- attr(x, "lake_source")
  result
}

# dplyr verb generics for lake_tbl
# これにより、パイプ中の全操作で依存関係が追跡される

#' @export
filter.lake_tbl <- function(.data, ...) {
  result <- NextMethod()
  attr(result, "lake_source") <- attr(.data, "lake_source")
  class(result) <- c("lake_tbl", class(result))
  result
}

#' @export
select.lake_tbl <- function(.data, ...) {
  result <- NextMethod()
  attr(result, "lake_source") <- attr(.data, "lake_source")
  class(result) <- c("lake_tbl", class(result))
  result
}

#' @export
mutate.lake_tbl <- function(.data, ...) {
  result <- NextMethod()
  attr(result, "lake_source") <- attr(.data, "lake_source")
  class(result) <- c("lake_tbl", class(result))
  result
}

#' @export
left_join.lake_tbl <- function(x, y, ...) {
  result <- NextMethod()
  # 両方のソースを記録
  x_source <- attr(x, "lake_source")
  y_source <- attr(y, "lake_source")
  attr(result, "lake_source") <- c(x_source, y_source)
  class(result) <- c("lake_tbl", class(result))
  result
}

# 同様に他のjoin, group_by, summarize等も実装
```

### 5.2 パイプ終端関数

**ファイル**: `R/pipe_end.R`

```r
#' @title Pipe Termination Functions
#' @description Functions to end a dplyr pipe and save to lake

#' Save pipe result to lake
#' @param .data Data from pipe
#' @param name Name to save as
#' @param lake Lake instance (uses default if NULL)
#' @export
save_as <- function(.data, name, lake = NULL) {
  if (is.null(lake)) {
    lake <- lake()
  }

  # 依存関係を収集
  deps <- attr(.data, "lake_source")

  # 遅延評価の場合は収集
  if (inherits(.data, "tbl_lazy")) {
    .data <- dplyr::collect(.data)
  }

  lake$put(name, .data, depends_on = deps)
  invisible(.data)
}

#' Alternative pipe-end operator
#' @param .data Data from pipe
#' @param target "lake_name" format string
#' @export
`%>_lake%` <- function(.data, target) {
  # target format: "lake_name" or "project/lake_name"
  parts <- strsplit(target, "/")[[1]]

  if (length(parts) == 2) {
    lake_inst <- Lake$new(parts[1])
    name <- parts[2]
  } else {
    lake_inst <- lake()
    name <- parts[1]
  }

  save_as(.data, name, lake_inst)
}
```

---

## Phase 6: 軽量モード（非侵入的統合）

### 6.1 観察モード

**ファイル**: `R/observe.R`

```r
#' @title Observation Mode
#' @description Track operations without modifying code

#' Observe I/O operations in a code block
#' @param expr Expression to observe
#' @param project Project to log to
#' @return Result of expression with lineage metadata
#' @export
observe <- function(expr, project = "observed") {
  lake <- Lake$new(project)

  # ファイルI/Oをフック
  original_read.csv <- utils::read.csv
  original_write.csv <- utils::write.csv
  original_readRDS <- base::readRDS
  original_saveRDS <- base::saveRDS

  reads <- character(0)
  writes <- character(0)

  # 読み込みをフック
  assignInNamespace("read.csv", function(file, ...) {
    reads <<- c(reads, normalizePath(file, mustWork = FALSE))
    original_read.csv(file, ...)
  }, "utils")

  assignInNamespace("readRDS", function(file, ...) {
    reads <<- c(reads, normalizePath(file, mustWork = FALSE))
    original_readRDS(file, ...)
  }, "base")

  # 書き込みをフック
  assignInNamespace("write.csv", function(x, file, ...) {
    writes <<- c(writes, normalizePath(file, mustWork = FALSE))
    original_write.csv(x, file, ...)
  }, "utils")

  assignInNamespace("saveRDS", function(object, file, ...) {
    writes <<- c(writes, normalizePath(file, mustWork = FALSE))
    original_saveRDS(object, file, ...)
  }, "base")

  # クリーンアップ関数
  cleanup <- function() {
    assignInNamespace("read.csv", original_read.csv, "utils")
    assignInNamespace("write.csv", original_write.csv, "utils")
    assignInNamespace("readRDS", original_readRDS, "base")
    assignInNamespace("saveRDS", original_saveRDS, "base")
  }

  # 実行
  result <- tryCatch({
    eval(substitute(expr), parent.frame())
  }, finally = {
    cleanup()
  })

  # リネージ記録
  for (write_path in writes) {
    name <- basename(write_path)
    lake$put(paste0("file:", name), data.frame(path = write_path))

    # 読み込みファイルを依存として記録
    for (read_path in reads) {
      lake$.__enclos_env__$private$.record_dependency(
        paste0("file:", name),
        "file",
        paste0("file:", basename(read_path)),
        "file"
      )
    }
  }

  result
}
```

### 6.2 ラップモード

**ファイル**: `R/wrap.R`

```r
#' @title Function Wrapping
#' @description Wrap existing functions to add lineage tracking

#' Wrap a function with lineage tracking
#' @param fn Function to wrap
#' @param lake Lake instance
#' @param input_arg Name of input argument(s) to track
#' @param output_name Name for output in lineage
#' @export
wrap_fn <- function(fn, lake, input_arg = NULL, output_name = NULL) {
  force(fn)
  force(lake)

  function(...) {
    args <- list(...)
    arg_names <- names(args)

    # 入力の依存関係を記録
    deps <- character(0)
    if (!is.null(input_arg)) {
      for (arg in input_arg) {
        if (arg %in% arg_names) {
          val <- args[[arg]]
          if (is.character(val) && length(val) == 1) {
            deps <- c(deps, val)
          }
        }
      }
    }

    # 関数実行
    result <- do.call(fn, args)

    # 出力を記録
    if (!is.null(output_name)) {
      lake$put(output_name, result, depends_on = deps)
    }

    attr(result, "lake_deps") <- deps
    result
  }
}

#' Mark data in lineage without storing
#' @param name Node name in lineage graph
#' @param data Data (for reference only)
#' @param lake Lake instance
#' @export
mark <- function(name, data = NULL, lake = NULL) {
  if (is.null(lake)) lake <- lake()

  # メタデータのみ記録（データ自体は保存しない）
  lake$.__enclos_env__$private$.record_node(name, "marker", list(
    class = class(data),
    dims = if (is.data.frame(data)) dim(data) else length(data),
    marked_at = Sys.time()
  ))

  invisible(data)
}

#' Create explicit dependency link
#' @param from Source name
#' @param to Target name
#' @param lake Lake instance
#' @export
link <- function(from, to, lake = NULL) {
  if (is.null(lake)) lake <- lake()

  lake$.__enclos_env__$private$.record_dependency(
    to, "unknown", from, "unknown", "linked"
  )

  invisible(TRUE)
}
```

---

## Phase 7: アダプター（Bioconductor/Seurat）

### 7.1 アダプターインターフェース

**ファイル**: `R/adapters/base.R`

```r
#' @title Adapter Base Class
#' @description Base class for type-specific adapters

LakeAdapter <- R6::R6Class("LakeAdapter",

  public = list(

    #' Check if adapter can handle this type
    #' @param data Data to check
    #' @return Logical
    can_handle = function(data) {
      FALSE
    },

    #' Store data
    #' @param lake Lake instance
    #' @param name Data name
    #' @param data Data to store
    put = function(lake, name, data) {
      stop("Not implemented")
    },

    #' Retrieve data
    #' @param lake Lake instance
    #' @param name Data name
    #' @param ref Version reference
    get = function(lake, name, ref = "@latest") {
      stop("Not implemented")
    },

    #' List stored components
    #' @param lake Lake instance
    #' @param name Data name
    components = function(lake, name) {
      stop("Not implemented")
    }
  )
)
```

### 7.2 SummarizedExperiment アダプター

**ファイル**: `R/adapters/se.R`

```r
#' @title SummarizedExperiment Adapter
#' @description Full-fidelity storage for Bioconductor SE objects

SEAdapter <- R6::R6Class("SEAdapter",
  inherit = LakeAdapter,

  public = list(

    can_handle = function(data) {
      inherits(data, "SummarizedExperiment")
    },

    put = function(lake, name, data) {
      requireNamespace("SummarizedExperiment", quietly = TRUE)

      # 各コンポーネントを分離保存
      prefix <- paste0(name, "__se__")

      # Assays
      assay_names <- SummarizedExperiment::assayNames(data)
      for (assay_name in assay_names) {
        mat <- SummarizedExperiment::assay(data, assay_name)
        # 疎行列の場合はlong formatに変換
        long_df <- private$.matrix_to_long(mat, assay_name)
        lake$put(paste0(prefix, "assay_", assay_name), long_df)
      }

      # colData
      col_data <- as.data.frame(SummarizedExperiment::colData(data))
      col_data$`.sample_id` <- rownames(col_data)
      lake$put(paste0(prefix, "colData"), col_data)

      # rowData
      row_data <- as.data.frame(SummarizedExperiment::rowData(data))
      row_data$`.feature_id` <- rownames(row_data)
      lake$put(paste0(prefix, "rowData"), row_data)

      # metadata
      meta <- S4Vectors::metadata(data)
      if (length(meta) > 0) {
        lake$put(paste0(prefix, "metadata"), meta)
      }

      # マニフェスト
      manifest <- list(
        type = "SummarizedExperiment",
        class = class(data),
        assays = assay_names,
        n_samples = ncol(data),
        n_features = nrow(data),
        created_at = Sys.time()
      )
      lake$put(paste0(prefix, "manifest"), manifest)

      invisible(TRUE)
    },

    get = function(lake, name, ref = "@latest") {
      requireNamespace("SummarizedExperiment", quietly = TRUE)

      prefix <- paste0(name, "__se__")

      # マニフェスト読み込み
      manifest <- lake$get(paste0(prefix, "manifest"), ref = ref)

      # Assays再構築
      assays_list <- list()
      for (assay_name in manifest$assays) {
        long_df <- lake$get(paste0(prefix, "assay_", assay_name), ref = ref)
        mat <- private$.long_to_matrix(long_df, assay_name)
        assays_list[[assay_name]] <- mat
      }

      # colData
      col_data <- lake$get(paste0(prefix, "colData"), ref = ref)
      rownames(col_data) <- col_data$`.sample_id`
      col_data$`.sample_id` <- NULL

      # rowData
      row_data <- lake$get(paste0(prefix, "rowData"), ref = ref)
      rownames(row_data) <- row_data$`.feature_id`
      row_data$`.feature_id` <- NULL

      # SE構築
      se <- SummarizedExperiment::SummarizedExperiment(
        assays = assays_list,
        colData = S4Vectors::DataFrame(col_data),
        rowData = S4Vectors::DataFrame(row_data)
      )

      # metadata復元
      meta_name <- paste0(prefix, "metadata")
      if (meta_name %in% lake$tables()$name) {
        S4Vectors::metadata(se) <- lake$get(meta_name, ref = ref)
      }

      se
    },

    components = function(lake, name) {
      prefix <- paste0(name, "__se__")
      tables <- lake$tables()
      tables[grepl(paste0("^", prefix), tables$name), ]
    }
  ),

  private = list(
    .matrix_to_long = function(mat, assay_name) {
      # 疎行列対応のlong format変換
      if (inherits(mat, "dgCMatrix") || inherits(mat, "dgTMatrix")) {
        # Matrix::summary で非ゼロ要素のみ
        summ <- Matrix::summary(mat)
        data.frame(
          feature = rownames(mat)[summ$i],
          sample = colnames(mat)[summ$j],
          value = summ$x,
          stringsAsFactors = FALSE
        )
      } else {
        # 密行列
        as.data.frame(as.table(mat))
      }
    },

    .long_to_matrix = function(long_df, assay_name) {
      # long format → 疎行列
      features <- unique(long_df$feature)
      samples <- unique(long_df$sample)

      mat <- Matrix::sparseMatrix(
        i = match(long_df$feature, features),
        j = match(long_df$sample, samples),
        x = long_df$value,
        dims = c(length(features), length(samples)),
        dimnames = list(features, samples)
      )

      mat
    }
  )
)
```

### 7.3 Seurat アダプター

**ファイル**: `R/adapters/seurat.R`

```r
#' @title Seurat Adapter
#' @description Full-fidelity storage for Seurat objects

SeuratAdapter <- R6::R6Class("SeuratAdapter",
  inherit = LakeAdapter,

  public = list(

    can_handle = function(data) {
      inherits(data, "Seurat")
    },

    put = function(lake, name, data) {
      requireNamespace("Seurat", quietly = TRUE)
      requireNamespace("SeuratObject", quietly = TRUE)

      prefix <- paste0(name, "__seurat__")

      # 各Assayを保存
      for (assay_name in Seurat::Assays(data)) {
        assay <- data[[assay_name]]

        # counts (疎行列)
        if (!is.null(SeuratObject::GetAssayData(assay, slot = "counts"))) {
          counts <- SeuratObject::GetAssayData(assay, slot = "counts")
          lake$put(paste0(prefix, assay_name, "_counts"),
                   private$.sparse_to_df(counts))
        }

        # data (正規化済み)
        if (!is.null(SeuratObject::GetAssayData(assay, slot = "data"))) {
          data_mat <- SeuratObject::GetAssayData(assay, slot = "data")
          lake$put(paste0(prefix, assay_name, "_data"),
                   private$.sparse_to_df(data_mat))
        }

        # scale.data (スケーリング済み、密行列の場合が多い)
        scale_data <- tryCatch(
          SeuratObject::GetAssayData(assay, slot = "scale.data"),
          error = function(e) NULL
        )
        if (!is.null(scale_data) && length(scale_data) > 0) {
          lake$put(paste0(prefix, assay_name, "_scale"),
                   as.data.frame(as.matrix(scale_data)))
        }
      }

      # meta.data
      meta <- data@meta.data
      meta$`.cell_id` <- rownames(meta)
      lake$put(paste0(prefix, "metadata"), meta)

      # Reductions
      for (reduc_name in Seurat::Reductions(data)) {
        reduc <- data[[reduc_name]]
        embeddings <- as.data.frame(Seurat::Embeddings(reduc))
        embeddings$`.cell_id` <- rownames(embeddings)
        lake$put(paste0(prefix, "reduc_", reduc_name), embeddings)
      }

      # マニフェスト
      manifest <- list(
        type = "Seurat",
        version = as.character(packageVersion("Seurat")),
        assays = Seurat::Assays(data),
        reductions = Seurat::Reductions(data),
        n_cells = ncol(data),
        n_features = nrow(data),
        default_assay = Seurat::DefaultAssay(data),
        created_at = Sys.time()
      )
      lake$put(paste0(prefix, "manifest"), manifest)

      invisible(TRUE)
    },

    get = function(lake, name, ref = "@latest") {
      # Seuratオブジェクトを再構築
      # 実装の詳細は省略
    }
  ),

  private = list(
    .sparse_to_df = function(mat) {
      # 疎行列をdata.frame形式で保存
      summ <- Matrix::summary(mat)
      data.frame(
        feature = rownames(mat)[summ$i],
        cell = colnames(mat)[summ$j],
        value = summ$x,
        stringsAsFactors = FALSE
      )
    }
  )
)
```

---

## Phase 8: 後方互換レイヤー

### 8.1 旧API互換関数

**ファイル**: `R/compat.R`

```r
#' @title Backward Compatibility Layer
#' @description Maintain compatibility with ol_* API

#' @export
#' @rdname compat
ol_init <- function(project, ...) {
  .Deprecated("Lake$new() or use_lake()")
  use_lake(project, ...)
  invisible(lake())
}

#' @export
#' @rdname compat
ol_write <- function(name, data, project = NULL, mode = "overwrite", depends_on = NULL) {
  .Deprecated("lake$put() or put()")
  if (!is.null(project)) use_lake(project)
  put(name, data, depends_on = depends_on)
}

#' @export
#' @rdname compat
ol_read <- function(name, ref = "@latest", project = NULL, collect = TRUE) {
  .Deprecated("lake$get() or get()")
  if (!is.null(project)) use_lake(project)
  get(name, ref = ref, collect = collect)
}

#' @export
#' @rdname compat
ol_save <- function(name, object, project = NULL, ...) {
  .Deprecated("lake$put()")
  if (!is.null(project)) use_lake(project)
  put(name, object, ...)
}

#' @export
#' @rdname compat
ol_read_object <- function(name, ref = "@latest", ...) {
  .Deprecated("lake$get()")
  get(name, ref = ref, ...)
}

#' @export
#' @rdname compat
ol_label <- function(label, ...) {
  .Deprecated("lake$snap()")
  snap(label, ...)
}

#' @export
#' @rdname compat
ol_tag <- function(name, tag, ...) {
  .Deprecated("lake$tag()")
  tag(name, tag)
}

#' @export
#' @rdname compat
ol_checkout <- function(label, ...) {
  .Deprecated("lake$restore()")
  lake()$restore(label)
}

#' @export
#' @rdname compat
ol_query <- function(sql, ...) {
  .Deprecated("lake$sql()")
  sql(sql, ...)
}

#' @export
#' @rdname compat
ol_show_lineage <- function(name, ...) {
  .Deprecated("lake$tree()")
  tree(name, ...)
}

#' @export
#' @rdname compat
ol_plot_lineage <- function(name, ...) {
  .Deprecated("lake$plot()")
  lake()$plot(name, ...)
}

#' @export
#' @rdname compat
ol_list_tables <- function(...) {
  .Deprecated("lake$tables()")
  tables()
}

#' @export
#' @rdname compat
ol_list_objects <- function(...) {
  .Deprecated("lake$objects()")
  lake()$objects()
}

#' @export
#' @rdname compat
ol_get_dependencies <- function(name, ...) {
  .Deprecated("lake$deps()")
  lake()$deps(name, ...)
}

#' @export
#' @rdname compat
ol_commit <- function(note = "", params = list(), ...) {
  .Deprecated("lake$snap()")
  snap(format(Sys.time(), "%Y%m%d-%H%M%S"), note = note, params = params)
}

#' @export
#' @rdname compat
ol_log <- function(name = NULL, ...) {
  .Deprecated("lake$log()")
  lake()$log(name, ...)
}

#' @export
#' @rdname compat
ol_drop <- function(name, ...) {
  .Deprecated("lake$drop()")
  drop(name, ...)
}

#' @export
#' @rdname compat
ol_export_parquet <- function(name, path, ...) {
  .Deprecated("lake$export()")
  lake()$export(name, path, format = "parquet")
}

#' @export
#' @rdname compat
ol_import_parquet <- function(path, name, ...) {
  .Deprecated("lake$import()")
  lake()$import(path, name)
}
```

---

## Phase 9: テスト

### 9.1 テスト構造

**ファイル**: `tests/testthat/test-lake.R`

```r
library(testthat)
library(lake)  # or OmicsLake

describe("Lake Core", {

  it("initializes with project name", {
    lake <- Lake$new("test_project")
    expect_s3_class(lake, "Lake")
    expect_equal(lake$.__enclos_env__$private$.project, "test_project")
  })

  it("auto-generates project name if not provided", {
    lake <- Lake$new()
    expect_true(nchar(lake$.__enclos_env__$private$.project) > 0)
  })

})

describe("Data I/O", {

  setup({
    lake <<- Lake$new("test_io")
  })

  it("puts and gets data frames", {
    df <- data.frame(a = 1:3, b = c("x", "y", "z"))
    lake$put("test_df", df)
    result <- lake$get("test_df")
    expect_equal(result, df)
  })

  it("puts and gets R objects", {
    obj <- list(x = 1, y = "hello")
    lake$put("test_obj", obj)
    result <- lake$get("test_obj")
    expect_equal(result, obj)
  })

  it("supports formula filter", {
    df <- data.frame(a = 1:10, b = letters[1:10])
    lake$put("filter_test", df)
    result <- lake$get("filter_test", where = ~ a > 5)
    expect_equal(nrow(result), 5)
  })

  it("supports column selection", {
    df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
    lake$put("select_test", df)
    result <- lake$get("select_test", select = c("a", "c"))
    expect_equal(names(result), c("a", "c"))
  })

})

describe("Versioning", {

  setup({
    lake <<- Lake$new("test_versioning")
  })

  it("creates and restores snapshots", {
    lake$put("data", data.frame(x = 1:3))
    lake$snap("v1")

    lake$put("data", data.frame(x = 4:6))
    lake$snap("v2")

    lake$restore("v1")
    result <- lake$get("data")
    expect_equal(result$x, 1:3)
  })

  it("supports tagging", {
    lake$put("tagged", data.frame(y = 1:5))
    lake$tag("tagged", "initial")

    lake$put("tagged", data.frame(y = 6:10))

    result <- lake$get("tagged", ref = "@tag(initial)")
    expect_equal(result$y, 1:5)
  })

})

describe("Lineage Tracking", {

  setup({
    lake <<- Lake$new("test_lineage")
  })

  it("tracks explicit dependencies", {
    lake$put("source", data.frame(a = 1:3))
    lake$put("derived", data.frame(b = 4:6), depends_on = "source")

    deps <- lake$deps("derived", direction = "up")
    expect_true("source" %in% deps$name)
  })

  it("tracks dependencies through dplyr pipes", {
    lake$put("raw", data.frame(x = 1:10, y = 11:20))

    lake$ref("raw") |>
      dplyr::filter(x > 5) |>
      dplyr::mutate(z = x + y) |>
      save_as("processed", lake)

    deps <- lake$deps("processed", direction = "up")
    expect_true("raw" %in% deps$name)
  })

})

describe("Query Builder", {

  setup({
    lake <<- Lake$new("test_query")
    lake$put("users", data.frame(
      id = 1:5,
      name = c("Alice", "Bob", "Carol", "Dave", "Eve"),
      age = c(25, 30, 35, 40, 45)
    ))
    lake$put("orders", data.frame(
      order_id = 1:10,
      user_id = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5),
      amount = c(100, 200, 150, 250, 300, 350, 400, 450, 500, 550)
    ))
  })

  it("supports fluent query building", {
    result <- lake$from("users")$
      where(age > 30)$
      select(name, age)$
      run()

    expect_equal(nrow(result), 3)
    expect_equal(names(result), c("name", "age"))
  })

  it("supports joins", {
    result <- lake$from("users")$
      join("orders", on = c("id" = "user_id"))$
      where(amount > 300)$
      select(name, amount)$
      run()

    expect_true(nrow(result) > 0)
  })

  it("saves query results to lake", {
    lake$from("users")$
      where(age >= 35)$
      as("senior_users")

    expect_true("senior_users" %in% lake$tables()$name)
  })

})

describe("Operators", {

  it("%like% works for pattern matching", {
    df <- data.frame(gene = c("ENSG001", "ENSG002", "MT-CO1", "MT-CO2"))
    result <- df[df$gene %like% "MT-%", ]
    expect_equal(nrow(result), 2)
  })

  it("%between% works for range filtering", {
    values <- 1:10
    result <- values[values %between% c(3, 7)]
    expect_equal(result, 3:7)
  })

})

describe("Bracket Notation", {

  setup({
    lake <<- Lake$new("test_bracket")
    lake$put("data", data.frame(a = 1:10, b = letters[1:10]))
  })

  it("reads with lake[name]", {
    result <- lake["data"]
    expect_equal(nrow(result), 10)
  })

  it("filters with lake[name, condition]", {
    result <- lake["data", a > 5]
    expect_equal(nrow(result), 5)
  })

  it("filters and selects with lake[name, condition, cols]", {
    result <- lake["data", a > 5, .(a)]
    expect_equal(nrow(result), 5)
    expect_equal(names(result), "a")
  })

})
```

---

## Phase 10: ドキュメント

### 10.1 README更新

**ファイル**: `README.md` (置き換え)

```markdown
# Lake (OmicsLake v2)

**Effortless data lineage for R.**

Lake tracks your data's journey automatically—no workflow changes required.

## Quick Start

```r
library(lake)

# Initialize
lake <- Lake("my_analysis")

# Store data
lake$put("counts", counts_df)
lake$put("metadata", sample_info)

# Process with dplyr (dependencies auto-tracked)
lake$ref("counts") |>
  left_join(lake$ref("metadata"), by = "sample_id") |>
  filter(quality > 0.8) |>
  group_by(condition) |>
  summarize(mean_expr = mean(expression)) |>
  save_as("summary", lake)

# View lineage
lake$tree("summary")

# Create snapshot
lake$snap("v1.0")
```

## Key Features

### 🔗 Automatic Lineage
Dependencies tracked through dplyr pipes—no manual annotation.

### 📦 Simple API
Just `put()`, `get()`, `snap()`, `tree()`. That's it.

### 🔍 R-Native Queries
No SQL required. Use formulas or dplyr.

```r
# Formula syntax
lake$get("counts", where = ~ gene %like% "MT-%" & expression > 100)

# Query builder
lake$from("counts")$
  join("annotations", on = "gene_id")$
  where(biotype == "protein_coding")$
  top(100, by = expression)$
  run()
```

### 🧬 Bioconductor Ready
Full SummarizedExperiment and Seurat support.

```r
lake$put("rna", summarized_experiment)
se <- lake$get("rna")  # Fully restored
```

### ⏱️ Time Travel
Tag versions, restore snapshots.

```r
lake$tag("results", "before_normalization")
lake$snap("checkpoint_1")
lake$restore("checkpoint_1")
```

## Installation

```r
remotes::install_github("matsui-lab/OmicsLake")
```

## Documentation

- [User Guide](vignettes/user_guide.Rmd)
- [API Reference](reference/index.html)
- [Migration from v1](vignettes/migration.Rmd)
```

---

## 実装順序

1. **Week 1-2**: Phase 1 (Lake R6クラス基本構造)
2. **Week 3**: Phase 2 (QueryBuilder)
3. **Week 4**: Phase 3-4 (自動追跡 + Formula)
4. **Week 5**: Phase 5 (dplyr統合)
5. **Week 6**: Phase 6 (軽量モード)
6. **Week 7**: Phase 7 (アダプター)
7. **Week 8**: Phase 8-10 (互換 + テスト + ドキュメント)

---

## ファイル構造（最終形）

```
OmicsLake/
├── R/
│   ├── Lake.R              # メインR6クラス
│   ├── QueryBuilder.R      # クエリビルダー
│   ├── shortcuts.R         # グローバルショートカット
│   ├── operators.R         # カスタム演算子
│   ├── formula.R           # Formula解析
│   ├── tracker.R           # 依存追跡
│   ├── expr_deps.R         # 式解析
│   ├── dplyr_compat.R      # dplyr統合
│   ├── pipe_end.R          # パイプ終端
│   ├── observe.R           # 観察モード
│   ├── wrap.R              # ラップモード
│   ├── compat.R            # 後方互換
│   ├── adapters/
│   │   ├── base.R          # アダプター基底
│   │   ├── se.R            # SE アダプター
│   │   └── seurat.R        # Seurat アダプター
│   └── backend/
│       ├── duckdb.R        # DuckDBバックエンド
│       └── interface.R     # バックエンドIF
├── tests/
│   └── testthat/
│       ├── test-lake.R
│       ├── test-query.R
│       ├── test-lineage.R
│       └── test-adapters.R
├── vignettes/
│   ├── user_guide.Rmd
│   └── migration.Rmd
├── DESCRIPTION
├── NAMESPACE
└── README.md
```

---

## 注意事項

1. **base::get() との衝突**: ショートカット `get()` は base R と衝突する。`fetch()` または明示的 `lake$get()` を検討
2. **パフォーマンス**: 大規模データでは遅延評価 (`collect = FALSE`) を推奨
3. **スレッドセーフティ**: DuckDB接続は単一スレッド想定
4. **後方互換**: `ol_*` 関数は非推奨警告付きで維持

---

この指示書に従って実装を進めてください。質問があれば随時確認してください。
