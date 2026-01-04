#' @title Lake - OmicsLake Core Class
#' @description R6 class for versioned, lineage-tracked data management.
#' Provides a modern, fluent API for data operations with automatic dependency tracking.
#'
#' @examples
#' \dontrun{
#' # Initialize a lake
#' lake <- Lake$new("my_project")
#'
#' # Store and retrieve data
#' lake$put("counts", counts_df)
#' data <- lake$get("counts")
#'
#' # Filter with formula syntax
#' filtered <- lake$get("counts", where = ~ expression > 100)
#'
#' # Use with dplyr
#' lake$ref("counts") |>
#'   dplyr::filter(quality > 0.8) |>
#'   dplyr::collect()
#'
#' # Version control
#' lake$snap("v1.0")
#' lake$tag("counts", "raw")
#' lake$restore("v1.0")
#'
#' # View lineage
#' lake$tree("results")
#' }
#'
#' @importFrom R6 R6Class
#' @export
Lake <- R6::R6Class("Lake",
  cloneable = FALSE,

  public = list(

    #' @description Initialize a Lake project
    #' @param project Project name or path. If NULL, auto-generates a name.
    #' @param backend Storage backend (currently only "duckdb" supported)
    #' @param auto_track Enable automatic dependency tracking (default: TRUE)
    #' @param root Root directory for lake storage (default: ~/.omicslake)
    #' @return A new Lake object
    initialize = function(project = NULL,
                          backend = "duckdb",
                          auto_track = TRUE,
                          root = NULL) {
      # Auto-generate project name if not provided
      if (is.null(project)) {
        project <- private$.generate_project_name()
      }

      private$.project <- project
      private$.backend_type <- backend
      private$.auto_track <- auto_track

      if (!is.null(root)) {
        options(ol.root = .ol_norm(root))
      }

      # Initialize the backend
      private$.init_backend()

      # Initialize dependency tracker
      private$.tracker <- DependencyTracker$new(self)

      invisible(self)
    },

    # ========== Core I/O ==========

    #' @description Write data to the lake
    #' @param name Name for the data
    #' @param data Data to store (data.frame, matrix, list, or any R object)
    #' @param depends_on Optional explicit dependencies. Can be:
    #'   - Character vector of names (uses @latest for all)
    #'   - List of lists with 'name' and 'ref' elements for version-aware deps
    #'
    #' **Auto-tracking note**: When `auto_track=TRUE` (default), dependencies are

    #' automatically detected from:
    #' 1. `lake_deps` attribute on data (set by `save_as()` from dplyr pipes)
    #' 2. Tracked reads within a `with_tracking()` or `wrap_fn()` context
    #'
    #' A simple `lake$get()` followed by `lake$put()` does NOT auto-capture
    #' dependencies. Use `save_as()`, `with_tracking()`, or specify `depends_on`.
    #' @param tags Optional tags for this version
    #' @return Invisible self for chaining
    put = function(name, data, depends_on = NULL, tags = NULL) {
      private$.validate_name(name)

      # Detect storage type
      storage_type <- private$.detect_storage_type(data)

      # Get auto-detected dependencies if tracking enabled
      # Dependencies are now stored as list of {name, ref} objects
      deps_with_refs <- list()

      if (is.null(depends_on) && private$.auto_track) {
        # Check for dependencies from tracked reads (already in {name, ref} format)
        tracked_deps <- private$.tracker$current_reads()
        deps_with_refs <- tracked_deps

        # Check for new paired format first (lake_sources)
        attr_sources <- attr(data, "lake_sources")
        if (!is.null(attr_sources) && length(attr_sources) > 0) {
          # New format: list of list(name=..., ref=...)
          for (src in attr_sources) {
            if (!is.null(src$name)) {
              # Add if not already present
              existing <- vapply(deps_with_refs, function(d) identical(d$name, src$name), logical(1))
              if (!any(existing)) {
                ref_val <- if (is.null(src$ref)) "@latest" else src$ref
                deps_with_refs <- append(deps_with_refs, list(list(name = src$name, ref = ref_val)))
              }
            }
          }
        } else {
          # Fallback to legacy format: lake_deps/lake_source with lake_source_ref
          # Note: lake_source_ref may be a vector (from joins), so we need to pair by index
          attr_deps <- attr(data, "lake_deps")
          if (is.null(attr_deps)) {
            attr_deps <- attr(data, "lake_source")
          }
          if (!is.null(attr_deps)) {
            # Check for lake_source_ref attribute
            attr_ref <- attr(data, "lake_source_ref")
            if (is.null(attr_ref)) attr_ref <- "@latest"

            for (i in seq_along(attr_deps)) {
              dep_name <- attr_deps[i]
              # Pair ref by index if attr_ref is a vector of same length
              # Otherwise use the single ref or first element
              if (length(attr_ref) == length(attr_deps)) {
                ref_val <- attr_ref[i]
              } else if (length(attr_ref) == 1) {
                ref_val <- attr_ref
              } else {
                # Mismatch: use @latest as safe fallback
                ref_val <- "@latest"
              }

              # Add as {name, ref} object if not already present
              existing <- vapply(deps_with_refs, function(d) identical(d$name, dep_name), logical(1))
              if (!any(existing)) {
                deps_with_refs <- append(deps_with_refs, list(list(name = dep_name, ref = ref_val)))
              }
            }
          }
        }
      } else if (!is.null(depends_on)) {
        # Convert explicit depends_on to {name, ref} format
        deps_with_refs <- private$.normalize_dependencies(depends_on)
      }

      # Store data based on type
      result <- switch(storage_type,
        "table" = private$.put_table(name, data),
        "object" = private$.put_object(name, data),
        "se" = private$.put_se(name, data),
        "seurat" = private$.put_seurat(name, data),
        stop("Unsupported data type: ", class(data)[1])
      )

      # Record dependencies with version info
      if (length(deps_with_refs) > 0) {
        private$.record_dependencies(name, storage_type, deps_with_refs)
      }

      # Apply tags if provided
      if (!is.null(tags)) {
        for (tag in tags) {
          self$tag(name, tag)
        }
      }

      invisible(self)
    },

    #' @description Read data from the lake
    #' @param name Name of the data to read
    #' @param ref Version reference ("@latest", "@first", "@tag(name)", or timestamp)
    #' @param where Filter condition (formula, e.g., ~ col > 5)
    #' @param select Columns to select (character vector)
    #' @param collect Whether to collect results immediately (FALSE returns lazy reference)
    #' @return The requested data
    get = function(name,
                   ref = "@latest",
                   where = NULL,
                   select = NULL,
                   collect = TRUE) {
      private$.validate_name(name)

      # Track this read for dependency detection
      private$.tracker$track_read(name, ref)

      # Determine if this is a table or object
      # For tagged refs, try table first even if current table is missing
      # (the backup table from the tag may still exist)
      is_table <- private$.is_table(name)
      is_tag_ref <- grepl("^@tag\\(", ref)

      if (is_table || is_tag_ref) {
        # Try table path first
        # For tagged refs, ol_read() can resolve to backup table even if current is dropped
        table_result <- tryCatch({
          data <- private$.get_lazy_data(name, ref)

          # Apply filter if provided (SQL pushdown)
          if (!is.null(where)) {
            data <- private$.apply_filter(data, where)
          }

          # Apply column selection if provided (SQL pushdown)
          if (!is.null(select)) {
            data <- private$.apply_select(data, select)
          }

          # Collect only if requested
          if (collect) {
            private$.collect(data)
          } else {
            data
          }
        }, error = function(e) {
          # If table read failed and this was a tag ref fallback attempt,
          # return NULL to try object path
          if (is_tag_ref && !is_table) {
            NULL
          } else {
            stop(e)
          }
        })

        if (!is.null(table_result)) {
          return(table_result)
        }
        # Fall through to object path if tag ref table read failed
      }

      # For objects: cannot be lazy, where/select not supported
      if (!is.null(where) || !is.null(select)) {
        warning("where/select parameters are ignored for non-table objects", call. = FALSE)
      }
      if (!collect) {
        warning("collect=FALSE is ignored for non-table objects (objects are always eager)", call. = FALSE)
      }
      return(private$.get_object_data(name, ref))
    },

    #' @description Get a lazy reference for dplyr operations
    #' @param name Table name
    #' @param ref Version reference (default: "@latest")
    #' @return A lazy table reference (tbl_lazy)
    ref = function(name, ref = "@latest") {
      private$.validate_name(name)
      private$.tracker$track_read(name, ref)
      tbl <- private$.get_lazy_data(name, ref)

      # Attach lineage metadata with version info (new paired format)
      attr(tbl, "lake_sources") <- list(list(name = name, ref = ref))
      # Legacy format for backward compatibility
      attr(tbl, "lake_source") <- name
      attr(tbl, "lake_source_ref") <- ref
      class(tbl) <- c("lake_tbl", class(tbl))
      tbl
    },

    # ========== Versioning ==========

    #' @description Create a project-wide snapshot
    #' @param label Label for this snapshot
    #' @param note Optional description
    #' @param params Optional parameters to store with the snapshot
    #' @return Invisible self for chaining
    snap = function(label, note = "", params = list()) {
      private$.validate_name(label)

      state <- private$.state
      conn <- state$conn

      # Use transaction for atomicity: commit + label must both succeed or both fail
      DBI::dbBegin(conn)
      tryCatch({
        # Create commit record
        ol_commit(note = note, params = params, project = private$.project)

        # Label all current tables and objects (pass .in_transaction=TRUE to avoid nested transactions)
        ol_label(label, project = private$.project, .in_transaction = TRUE)

        DBI::dbCommit(conn)
      }, error = function(e) {
        DBI::dbRollback(conn)
        stop("Snapshot failed (rolled back): ", conditionMessage(e), call. = FALSE)
      })

      invisible(self)
    },

    #' @description Tag a specific data version
    #' @param name Data name
    #' @param tag Tag to apply
    #' @return Invisible self for chaining
    tag = function(name, tag) {
      private$.validate_name(name)
      private$.validate_name(tag)

      state <- private$.state

      # Check if it's a table or object
      if (private$.is_table(name)) {
        ol_tag(name, tag, project = private$.project)
      } else if (private$.is_object(name)) {
        ol_tag_object(name, tag, project = private$.project)
      } else {
        stop("Data not found: ", name, call. = FALSE)
      }

      invisible(self)
    },

    #' @description Restore project to a labeled snapshot
    #' @param label Snapshot label to restore
    #' @return Invisible self for chaining
    restore = function(label) {
      private$.validate_name(label)
      ol_checkout(label, project = private$.project)
      invisible(self)
    },

    #' @description Compare two versions of data
    #' @param name Data name
    #' @param ref1 First version reference (default: "@latest")
    #' @param ref2 Second version reference (default: "@first")
    #' @return Comparison summary with row counts, column differences, and sample data
    diff = function(name, ref1 = "@latest", ref2 = "@first") {
      private$.validate_name(name)

      # Get data from both versions
      data1 <- tryCatch(
        self$get(name, ref = ref1, collect = TRUE),
        error = function(e) NULL
      )
      data2 <- tryCatch(
        self$get(name, ref = ref2, collect = TRUE),
        error = function(e) NULL
      )

      if (is.null(data1) && is.null(data2)) {
        stop("Neither version found for: ", name, call. = FALSE)
      }

      # Build comparison summary
      result <- list(
        name = name,
        ref1 = ref1,
        ref2 = ref2,
        ref1_rows = if (!is.null(data1) && is.data.frame(data1)) nrow(data1) else NA,
        ref2_rows = if (!is.null(data2) && is.data.frame(data2)) nrow(data2) else NA,
        ref1_cols = if (!is.null(data1) && is.data.frame(data1)) ncol(data1) else NA,
        ref2_cols = if (!is.null(data2) && is.data.frame(data2)) ncol(data2) else NA
      )

      # Column differences
      if (!is.null(data1) && !is.null(data2) && is.data.frame(data1) && is.data.frame(data2)) {
        cols1 <- names(data1)
        cols2 <- names(data2)
        result$cols_added <- setdiff(cols1, cols2)
        result$cols_removed <- setdiff(cols2, cols1)
        result$cols_common <- intersect(cols1, cols2)
        result$row_diff <- result$ref1_rows - result$ref2_rows
      }

      class(result) <- c("lake_diff", "list")
      result
    },

    # ========== Lineage ==========

    #' @description Show lineage tree
    #' @param name Starting node (NULL for all)
    #' @param direction "up" (ancestors), "down" (descendants), or "both"
    #' @param depth Maximum depth to traverse
    #' @return Data frame of lineage relationships
    tree = function(name = NULL, direction = "up", depth = 10) {
      dir_map <- c("up" = "upstream", "down" = "downstream", "both" = "both")
      direction <- dir_map[direction]

      if (is.null(name)) {
        # Show all dependencies
        state <- private$.state
        .ol_ensure_dependencies_table(state)
        conn <- state$conn
        ident <- .ol_sql_ident(conn, state, "__ol_dependencies")
        query <- sprintf("SELECT * FROM %s ORDER BY created_at DESC", ident)
        return(DBI::dbGetQuery(conn, query))
      }

      ol_show_lineage(name, direction = direction, max_depth = depth, project = private$.project)
    },

    #' @description Plot lineage graph
    #' @param name Starting node (NULL for full graph)
    #' @param direction "up", "down", or "both"
    #' @return Lineage plot (requires igraph)
    plot = function(name = NULL, direction = "both") {
      dir_map <- c("up" = "upstream", "down" = "downstream", "both" = "both")
      direction <- dir_map[direction]

      if (is.null(name)) {
        # Get first available node
        tables <- self$tables()
        if (nrow(tables) > 0) {
          name <- tables$table_name[1]
        } else {
          stop("No data in lake to plot", call. = FALSE)
        }
      }

      ol_plot_lineage(name, direction = direction, project = private$.project)
    },

    #' @description Get direct dependencies
    #' @param name Data name
    #' @param direction "up" (parents) or "down" (children)
    #' @return Data frame of dependencies
    deps = function(name, direction = "up") {
      dir_map <- c("up" = "upstream", "down" = "downstream")
      direction <- dir_map[direction]
      ol_get_dependencies(name, direction = direction, project = private$.project)
    },

    #' @description Analyze impact of changing a data source
    #' @param name Data name to analyze
    #' @return Data frame of affected downstream data
    impact = function(name) {
      self$tree(name, direction = "down", depth = 100)
    },

    # ========== Query Builder ==========

    #' @description Start a query builder chain
    #' @return A new QueryBuilder instance
    query = function() {
      QueryBuilder$new(self)
    },

    #' @description Shortcut to start query from a table
    #' @param table Table name
    #' @return A QueryBuilder instance with FROM clause set
    from = function(table) {
      self$query()$from(table)
    },

    #' @description Join two tables
    #' @param left Left table name
    #' @param right Right table name
    #' @param by Join columns (character vector or named vector for different column names)
    #' @param type Join type ("left", "inner", "right", "full")
    #' @return Joined data
    join = function(left, right, by = NULL, type = "left") {
      left_tbl <- self$ref(left)
      right_tbl <- self$ref(right)

      join_fn <- switch(type,
        "left" = dplyr::left_join,
        "inner" = dplyr::inner_join,
        "right" = dplyr::right_join,
        "full" = dplyr::full_join,
        stop("Unknown join type: ", type)
      )

      result <- join_fn(left_tbl, right_tbl, by = by)
      attr(result, "lake_deps") <- c(left, right)
      result
    },

    # ========== Aggregation Shortcuts ==========

    #' @description Count rows, optionally grouped
    #' @param table Table name
    #' @param ... Grouping variables (unquoted)
    #' @return Data frame with counts
    count = function(table, ...) {
      grp <- rlang::enquos(...)
      result <- self$ref(table)

      if (length(grp) > 0) {
        result <- dplyr::group_by(result, !!!grp)
      }

      result |>
        dplyr::summarize(n = dplyr::n(), .groups = "drop") |>
        dplyr::collect()
    },

    #' @description Calculate mean of a column
    #' @param table Table name
    #' @param col Column to average (unquoted)
    #' @param ... Grouping variables (unquoted)
    #' @return Data frame with means
    mean = function(table, col, ...) {
      col_sym <- rlang::ensym(col)
      grp <- rlang::enquos(...)
      result <- self$ref(table)

      if (length(grp) > 0) {
        result <- dplyr::group_by(result, !!!grp)
      }

      result |>
        dplyr::summarize(mean = mean(!!col_sym, na.rm = TRUE), .groups = "drop") |>
        dplyr::collect()
    },

    # ========== Listing ==========

    #' @description List all tables in the lake
    #' @return Data frame of table names
    tables = function() {
      ol_list_tables(project = private$.project)
    },

    #' @description List all objects in the lake
    #' @return Data frame of object names
    objects = function() {
      ol_list_objects(project = private$.project)
    },

    #' @description List all data (tables and objects)
    #' @return List with tables and objects data frames
    ls = function() {
      list(
        tables = self$tables(),
        objects = self$objects()
      )
    },

    #' @description List all snapshots/labels
    #' @return Data frame of snapshots
    snaps = function() {
      ol_list_labels(project = private$.project)
    },

    #' @description Show history/log
    #' @param name Optional data name (NULL for project history)
    #' @param n Maximum number of entries to return
    #' @return Data frame of history entries
    log = function(name = NULL, n = 20) {
      ol_log(name, project = private$.project)
    },

    #' @description Alias for log
    #' @param name Optional data name
    #' @param n Maximum number of entries
    #' @return Data frame of history entries
    history = function(name = NULL, n = 20) {
      self$log(name, n)
    },

    # ========== Data Management ==========

    #' @description Remove data from the lake
    #' @param name Data name
    #' @param force Force removal even if has dependents
    #' @return Invisible self for chaining
    drop = function(name, force = FALSE) {
      private$.validate_name(name)

      # Check for dependents
      dependents <- tryCatch(
        self$deps(name, direction = "down"),
        error = function(e) data.frame()
      )

      if (nrow(dependents) > 0 && !force) {
        stop("Cannot drop '", name, "': has ", nrow(dependents),
             " dependent(s). Use force = TRUE to override.", call. = FALSE)
      }

      # Try to drop as table first, then as object
      table_dropped <- tryCatch({
        ol_drop(name, project = private$.project)
        TRUE
      }, error = function(e) {
        FALSE
      })

      if (!table_dropped) {
        # Try to drop as object
        tryCatch({
          ol_drop_object(name, project = private$.project)
        }, error = function(e) {
          stop("'", name, "' not found as table or object", call. = FALSE)
        })
      }

      invisible(self)
    },

    #' @description Alias for drop
    #' @param name Data name
    #' @param force Force removal
    #' @return Invisible self for chaining
    rm = function(name, force = FALSE) {
      self$drop(name, force)
    },

    # ========== Import/Export ==========

    #' @description Export data to a file
    #' @param name Data name
    #' @param path Output file path
    #' @param format Output format ("parquet", "csv", "rds"). Auto-detected from extension if NULL.
    #' @return Invisible path
    export = function(name, path, format = NULL) {
      if (is.null(format)) {
        format <- private$.detect_format(path)
      }

      data <- self$get(name)

      switch(format,
        "parquet" = ol_export_parquet(name, path, project = private$.project),
        "csv" = utils::write.csv(data, path, row.names = FALSE),
        "rds" = saveRDS(data, path),
        stop("Unsupported format: ", format)
      )

      invisible(path)
    },

    #' @description Import external data into the lake
    #' @param path Input file path
    #' @param name Name to store as
    #' @param format Input format (auto-detected from extension if NULL)
    #' @return Invisible self for chaining
    import = function(path, name, format = NULL) {
      if (is.null(format)) {
        format <- private$.detect_format(path)
      }

      data <- switch(format,
        "parquet" = arrow::read_parquet(path),
        "csv" = utils::read.csv(path, stringsAsFactors = FALSE),
        "rds" = readRDS(path),
        "tsv" = utils::read.delim(path, stringsAsFactors = FALSE),
        stop("Unsupported format: ", format)
      )

      self$put(name, data)
      invisible(self)
    },

    # ========== SQL (Escape Hatch) ==========

    #' @description Execute raw SQL query
    #' @param query SQL query string
    #' @param collect Whether to collect results immediately
    #' @return Query results
    sql = function(query, collect = TRUE) {
      ol_query(query, collect = collect, project = private$.project)
    },

    #' @description Alias for sql
    #' @param query SQL query string
    #' @param collect Whether to collect results
    #' @return Query results
    q = function(query, collect = TRUE) {
      self$sql(query, collect)
    },

    # ========== Bracket Notation ==========

    #' @description Bracket access for reading data
    #' @param name Data name
    #' @param i Row filter expression (optional)
    #' @param j Column selection (optional)
    #' @return Filtered/selected data
    `[` = function(name, i, j) {
      if (missing(i) && missing(j)) {
        return(self$get(name))
      }

      data <- self$get(name)

      if (!missing(i)) {
        # Row filter using NSE
        i_expr <- substitute(i)
        data <- dplyr::filter(data, !!i_expr)
      }

      if (!missing(j)) {
        # Column selection
        j_val <- substitute(j)
        if (is.call(j_val) && identical(j_val[[1]], as.name("."))) {
          # data.table style: .(col1, col2)
          cols <- as.character(j_val[-1])
          data <- dplyr::select(data, dplyr::all_of(cols))
        } else if (is.character(j)) {
          data <- dplyr::select(data, dplyr::all_of(j))
        } else {
          data <- dplyr::select(data, !!j_val)
        }
      }

      data
    },

    #' @description Bracket assignment for writing data
    #' @param name Data name
    #' @param value Data to store
    `[<-` = function(name, value) {
      self$put(name, value)
      invisible(self)
    },

    # ========== Printing ==========

    #' @description Print lake summary
    print = function() {
      tables <- tryCatch(self$tables(), error = function(e) data.frame())
      objects <- tryCatch(self$objects(), error = function(e) data.frame())

      cat("Lake:", private$.project, "\n")
      cat("Backend:", private$.backend_type, "\n")
      cat("Tables:", nrow(tables), "\n")
      cat("Objects:", nrow(objects), "\n")
      cat("Auto-track:", private$.auto_track, "\n")
      invisible(self)
    }
  ),

  private = list(
    .project = NULL,
    .backend_type = NULL,
    .state = NULL,
    .auto_track = TRUE,
    .tracker = NULL,

    # Initialize the backend connection
    .init_backend = function() {
      .ol_init_backend(project = private$.project, engine = private$.backend_type)
      private$.state <- .ol_get_backend_state(private$.project)
    },

    # Validate a name string
    .validate_name = function(name) {
      .ol_validate_name(name, "name")
    },

    # Detect the storage type for data
    .detect_storage_type = function(data) {
      if (inherits(data, "SummarizedExperiment")) {
        return("se")
      }
      if (inherits(data, "Seurat")) {
        return("seurat")
      }
      if (is.data.frame(data) || inherits(data, "data.frame")) {
        return("table")
      }
      if (is.matrix(data)) {
        return("table")  # Convert matrix to data.frame
      }
      "object"  # Default: serialize as R object
    },

    # Store a table
    .put_table = function(name, data) {
      if (is.matrix(data)) {
        data <- as.data.frame(data)
      }
      ol_write(name, data, project = private$.project, mode = "overwrite")
    },

    # Store an R object
    .put_object = function(name, data) {
      ol_save(name, data, project = private$.project)
    },

    # Store SummarizedExperiment (placeholder for adapter)
    .put_se = function(name, data) {
      # For now, save as object; adapter will provide full support
      ol_save(name, data, project = private$.project)
    },

    # Store Seurat object (placeholder for adapter)
    .put_seurat = function(name, data) {
      # For now, save as object; adapter will provide full support
      ol_save(name, data, project = private$.project)
    },

    # Get lazy reference to table with version support
    .get_lazy_data = function(name, ref) {
      ol_read(name, ref = ref, project = private$.project, collect = FALSE)
    },

    # Get object data (always eager)
    .get_object_data = function(name, ref) {
      ol_read_object(name, ref = ref, project = private$.project)
    },

    # Get lazy reference to table (always @latest, for ref() method)
    .get_lazy_ref = function(name) {
      ol_read(name, ref = "@latest", project = private$.project, collect = FALSE)
    },

    # Apply formula filter
    .apply_filter = function(data, where) {
      if (!inherits(where, "formula")) {
        stop("where must be a formula (e.g., ~ col > 5)", call. = FALSE)
      }

      expr <- rlang::f_rhs(where)
      dplyr::filter(data, !!expr)
    },

    # Apply column selection
    .apply_select = function(data, select) {
      if (is.character(select)) {
        dplyr::select(data, dplyr::all_of(select))
      } else {
        dplyr::select(data, !!select)
      }
    },

    # Record dependencies with version info
    # depends_on is now a list of {name, ref} objects
    .record_dependencies = function(name, type, depends_on) {
      if (length(depends_on) == 0) return()

      state <- private$.state
      .ol_ensure_dependencies_table(state)

      for (dep in depends_on) {
        parent_name <- dep$name
        parent_ref <- if (!is.null(dep$ref)) dep$ref else "@latest"
        parent_type <- if (private$.is_object(parent_name)) "object" else "table"

        .ol_record_dependency(
          state = state,
          child_name = name,
          child_type = type,
          parent_name = parent_name,
          parent_type = parent_type,
          parent_ref = parent_ref
        )
      }
    },

    # Normalize depends_on to list of {name, ref} objects
    .normalize_dependencies = function(depends_on) {
      if (is.character(depends_on)) {
        # Character vector: convert each to {name, ref="@latest"}
        lapply(depends_on, function(n) list(name = n, ref = "@latest"))
      } else if (is.list(depends_on)) {
        # Check if it's already in the right format
        lapply(depends_on, function(d) {
          if (is.character(d) && length(d) == 1) {
            list(name = d, ref = "@latest")
          } else if (is.list(d) && !is.null(d$name)) {
            list(name = d$name, ref = if (!is.null(d$ref)) d$ref else "@latest")
          } else {
            stop("Invalid dependency format", call. = FALSE)
          }
        })
      } else {
        stop("depends_on must be a character vector or list", call. = FALSE)
      }
    },

    # Check if name is a table
    .is_table = function(name) {
      tables <- self$tables()
      name %in% tables$table_name
    },

    # Check if name is an object
    .is_object = function(name) {
      objects <- tryCatch(self$objects(), error = function(e) data.frame(name = character(0)))
      name %in% objects$name
    },

    # Check if data is lazy (tbl_lazy, tbl_sql, tbl_dbi, or other dbplyr types)
    .is_lazy = function(data) {
      inherits(data, c("tbl_lazy", "tbl_sql", "tbl_dbi", "tbl_duckdb_connection"))
    },

    # Convert to lazy reference
    .as_lazy = function(data) {
      # For in-memory data, we can't really make it lazy
      # Just return as-is with a warning
      warning("Cannot create lazy reference for in-memory data", call. = FALSE)
      data
    },

    # Collect lazy data
    .collect = function(data) {
      if (private$.is_lazy(data)) {
        dplyr::collect(data)
      } else {
        data
      }
    },

    # Detect file format from extension
    .detect_format = function(path) {
      ext <- tolower(tools::file_ext(path))
      switch(ext,
        "parquet" = "parquet",
        "csv" = "csv",
        "tsv" = "tsv",
        "rds" = "rds",
        "rda" = "rds",
        stop("Cannot detect format from extension: ", ext)
      )
    },

    # Generate a project name
    .generate_project_name = function() {
      paste0("lake_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    }
  )
)


#' @title Dependency Tracker
#' @description Tracks data dependencies during operations with version-aware lineage
#' @keywords internal
DependencyTracker <- R6::R6Class("DependencyTracker",
  public = list(
    initialize = function(lake) {
      private$.lake <- lake
      private$.read_stack <- list()
    },

    #' Record a read operation with version reference
    #' @param name Name of the data being read

    #' @param ref Version reference (e.g., "@latest", "@tag(v1)")
    track_read = function(name, ref = "@latest") {
      if (length(private$.read_stack) > 0) {
        # Add to current context as name+ref pair
        ctx <- private$.read_stack[[length(private$.read_stack)]]
        # Store as list of lists for version-aware tracking
        new_read <- list(name = name, ref = as.character(ref))
        # Check if this exact name+ref pair already exists
        existing <- vapply(ctx$reads, function(r) {
          identical(r$name, name) && identical(r$ref, as.character(ref))
        }, logical(1))
        if (!any(existing)) {
          ctx$reads <- append(ctx$reads, list(new_read))
        }
        private$.read_stack[[length(private$.read_stack)]] <- ctx
      }
      invisible(self)
    },

    #' Start tracking a write operation
    start_write = function(name) {
      private$.read_stack <- append(private$.read_stack, list(
        list(name = name, reads = list(), start_time = Sys.time())
      ))
      invisible(self)
    },

    #' End tracking and return dependencies with version info
    #' @return List of dependency objects with name and ref
    end_write = function() {
      if (length(private$.read_stack) == 0) {
        return(list())
      }

      ctx <- private$.read_stack[[length(private$.read_stack)]]
      private$.read_stack <- private$.read_stack[-length(private$.read_stack)]
      ctx$reads
    },

    #' Get current tracked reads with version info
    #' @return List of dependency objects with name and ref
    current_reads = function() {
      if (length(private$.read_stack) == 0) {
        return(list())
      }
      private$.read_stack[[length(private$.read_stack)]]$reads
    },

    #' Get current tracked reads as character vector (for backward compatibility)
    #' @return Character vector of dependency names
    current_read_names = function() {
      reads <- self$current_reads()
      if (length(reads) == 0) return(character(0))
      vapply(reads, function(r) r$name, character(1))
    },

    #' Clear all tracking
    clear = function() {
      private$.read_stack <- list()
      invisible(self)
    }
  ),

  private = list(
    .lake = NULL,
    .read_stack = list()
  )
)
