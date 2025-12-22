#' @title Backward Compatibility Layer
#' @description Maintains compatibility with the legacy ol_* API.
#' All functions are deprecated and users should migrate to the new Lake API.
#'
#' @section Migration Guide:
#' \itemize{
#'   \item \code{ol_init()} -> \code{Lake$new()} or \code{use_lake()}
#'   \item \code{ol_write()} -> \code{lake$put()} or \code{put()}
#'   \item \code{ol_read()} -> \code{lake$get()} or \code{fetch()}
#'   \item \code{ol_save()} -> \code{lake$put()}
#'   \item \code{ol_label()} -> \code{lake$snap()}
#'   \item \code{ol_tag()} -> \code{lake$tag()}
#'   \item \code{ol_checkout()} -> \code{lake$restore()}
#'   \item \code{ol_show_lineage()} -> \code{lake$tree()}
#'   \item \code{ol_plot_lineage()} -> \code{lake$plot()}
#'   \item \code{ol_query()} -> \code{lake$sql()} or QueryBuilder
#' }
#'
#' @name compat
#' @keywords internal
NULL

# Note: The original ol_* functions are defined in other files (init.R, io.R, etc.)
# This file provides documentation for the deprecation status.
# The original functions continue to work but will show deprecation warnings
# in a future release.

#' @rdname compat
#' @section Deprecation:
#' The ol_* API is deprecated and will be removed in a future version.
#' Please migrate to the new Lake API.
#'
#' @examples
#' \dontrun{
#' # Old API (deprecated)
#' ol_init("project")
#' ol_write("data", df)
#' result <- ol_read("data")
#'
#' # New API (recommended)
#' lake <- Lake$new("project")
#' lake$put("data", df)
#' result <- lake$get("data")
#'
#' # Or with shortcuts
#' use_lake("project")
#' put("data", df)
#' result <- fetch("data")
#' }
NULL

# API Migration helpers

#' Show migration guide for ol_* to Lake API
#'
#' @export
show_migration_guide <- function() {
  cat("
================================================================================
                    OmicsLake API Migration Guide
================================================================================

The ol_* API is being deprecated in favor of a cleaner, more intuitive API.
Below is a mapping from old functions to new equivalents:

INITIALIZATION
--------------
  ol_init('project')           ->  Lake$new('project')
                               or  use_lake('project')

DATA I/O
--------
  ol_write('name', data)       ->  lake$put('name', data)
                               or  put('name', data)

  ol_read('name')              ->  lake$get('name')
                               or  fetch('name')

  ol_save('name', object)      ->  lake$put('name', object)

  ol_read_object('name')       ->  lake$get('name')

VERSION CONTROL
---------------
  ol_label('v1.0')             ->  lake$snap('v1.0')
                               or  snap('v1.0')

  ol_tag('name', 'tag')        ->  lake$tag('name', 'tag')
                               or  tag('name', 'tag')

  ol_checkout('v1.0')          ->  lake$restore('v1.0')
                               or  restore('v1.0')

  ol_commit('note')            ->  lake$snap('label', note='note')

LINEAGE
-------
  ol_show_lineage('name')      ->  lake$tree('name')
                               or  tree('name')

  ol_plot_lineage('name')      ->  lake$plot('name')

  ol_get_dependencies('name')  ->  lake$deps('name')
                               or  deps('name')

QUERYING
--------
  ol_query('SELECT ...')       ->  lake$sql('SELECT ...')
                               or  sql('SELECT ...')

  # Better: Use QueryBuilder or dplyr
  ol_query('SELECT * FROM t    ->  lake$from('t')$
            WHERE x > 5')             where(x > 5)$
                                      run()

  # Or with formula syntax
                               ->  lake$get('t', where = ~ x > 5)

LISTING
-------
  ol_list_tables()             ->  lake$tables()
                               or  tables()

  ol_list_objects()            ->  lake$objects()
                               or  objects()

  ol_list_labels()             ->  lake$snaps()

  ol_log()                     ->  lake$log()
                               or  history()

DATA MANAGEMENT
---------------
  ol_drop('name')              ->  lake$drop('name')
                               or  drop('name')

IMPORT/EXPORT
-------------
  ol_export_parquet('n', 'f')  ->  lake$export('n', 'f')

  ol_import_parquet('f', 'n')  ->  lake$import('f', 'n')

================================================================================
                    New Features in Lake API
================================================================================

DPLYR INTEGRATION (dependencies auto-tracked):

  lake$ref('counts') |>
    dplyr::filter(quality > 0.8) |>
    dplyr::left_join(lake$ref('metadata'), by = 'sample_id') |>
    save_as('filtered', lake)

QUERY BUILDER:

  lake$from('counts')$
    join('metadata', on = 'sample_id')$
    where(quality > 0.8)$
    select(gene_id, expression)$
    top(100, by = expression)$
    run()

FORMULA SYNTAX:

  lake$get('counts', where = ~ gene_id %like% 'MT-%' & expression > 100)

BRACKET NOTATION:

  lake['counts']                    # read
  lake['counts', x > 5]             # filter
  lake['counts', x > 5, .(a, b)]    # filter + select
  lake['new_data'] <- df            # write

================================================================================
")
  invisible(NULL)
}

#' Check if using deprecated API
#'
#' @keywords internal
.warn_deprecated <- function(old_fn, new_fn, when = "0.2.0") {
  .Deprecated(
    new_fn,
    package = "OmicsLake",
    msg = sprintf(
      "'%s' is deprecated.\nUse '%s' instead.\nSee show_migration_guide() for details.",
      old_fn, new_fn
    )
  )
}

# Future: Add these wrappers when ready to show deprecation warnings
# For now, original functions in other files continue to work without warning

# #' @export
# #' @rdname compat
# ol_init_deprecated <- function(project, ...) {
#   .warn_deprecated("ol_init()", "Lake$new() or use_lake()")
#   ol_init(project, ...)
# }
