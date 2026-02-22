#' @title Global Lake Shortcuts
#' @description Convenience functions for working with a default lake.
#' These functions provide a simpler API when working with a single project.
#'
#' @examples
#' if (FALSE) {
#' # Set up default lake
#' use_lake("my_project")
#'
#' # Use shortcuts
#' put("counts", counts_df)
#' data <- fetch("counts")
#' snap("v1.0")
#' tree("counts")
#' }
#'
#' @name shortcuts
NULL

# Global environment for default lake
.lake_env <- new.env(parent = emptyenv())

.ol_available_projects <- function(root = .ol_root(), max_show = 8L) {
  if (!dir.exists(root)) {
    return(character(0))
  }
  dirs <- list.dirs(root, recursive = FALSE, full.names = FALSE)
  if (!length(dirs)) {
    return(character(0))
  }
  has_db <- vapply(
    dirs,
    function(p) file.exists(file.path(root, p, "duckdb.db")),
    logical(1)
  )
  projects <- sort(unique(dirs[has_db]))
  if (length(projects) > max_show) {
    c(projects[seq_len(max_show)], "...")
  } else {
    projects
  }
}

.ol_no_default_lake_error <- function() {
  root <- .ol_root()
  projects <- .ol_available_projects(root)
  suggestion <- if (length(projects)) {
    paste0("Available projects under ", root, ": ", paste(projects, collapse = ", "), ". ")
  } else {
    paste0("No project directories found under ", root, ". ")
  }
  stop(
    suggestion,
    "Use use_lake('project') to create/open one.",
    call. = FALSE
  )
}

#' Set or get the default lake
#'
#' @param project Project name. If provided, creates/opens a lake and sets it as default.
#' @param ... Additional arguments passed to Lake$new()
#' @return The default Lake object (invisibly when setting)
#' @export
#' @examples
#' if (FALSE) {
#' use_lake("my_analysis")
#' lake <- use_lake()  # Get current lake
#' }
use_lake <- function(project = NULL, ...) {
  if (!is.null(project)) {
    if (!is.character(project) || length(project) != 1 || !nzchar(project)) {
      stop("project must be a non-empty character string", call. = FALSE)
    }
    .lake_env$default <- Lake$new(project, ...)
    options(ol.project = project)
    return(invisible(.lake_env$default))
  }

  if (!is.null(.lake_env$default)) {
    return(.lake_env$default)
  }

  opt_project <- getOption("ol.project")
  if (!is.null(opt_project) && is.character(opt_project) && length(opt_project) == 1 && nzchar(opt_project)) {
    .lake_env$default <- Lake$new(opt_project)
    return(.lake_env$default)
  }

  .ol_no_default_lake_error()
}

#' Get the current default lake
#'
#' @rdname default_lake
#' @return The default Lake object
#' @export
#' @examples
#' if (FALSE) {
#' use_lake("my_project")
#' l <- lake()
#' l$put("data", df)
#' }
lake <- function() {
  if (is.null(.lake_env$default)) {
    use_lake()
  }
  .lake_env$default
}

#' Write data to the default lake
#'
#' @param name Name for the data
#' @param data Data to store
#' @param ... Additional arguments passed to Lake$put()
#' @return Invisible Lake object
#' @export
#' @seealso \code{\link{Lake}}
put <- function(name, data, ...) {
  lake()$put(name, data, ...)
}

#' Read data from the default lake
#'
#' Note: This function is named 'fetch' to avoid conflict with base::get()
#'
#' @param name Name of the data to read
#' @param ... Additional arguments passed to Lake$get()
#' @return The requested data
#' @export
#' @seealso \code{\link{Lake}}
fetch <- function(name, ...) {
  lake()$get(name, ...)
}

#' Get a lazy reference from the default lake
#'
#' @param name Table name
#' @return A lazy table reference for use with dplyr
#' @export
ref <- function(name) {
  lake()$ref(name)
}

#' Create a snapshot of the default lake
#'
#' @param label Label for the snapshot
#' @param ... Additional arguments passed to Lake$snap()
#' @return Invisible Lake object
#' @export
snap <- function(label, ...) {
  lake()$snap(label, ...)
}

#' Tag data in the default lake
#'
#' @param name Data name
#' @param tag Tag to apply
#' @return Invisible Lake object
#' @export
tag <- function(name, tag) {
  lake()$tag(name, tag)
}

#' Show lineage tree from the default lake
#'
#' @param name Starting node (optional)
#' @param ... Additional arguments passed to Lake$tree()
#' @return Lineage data frame
#' @export
tree <- function(name = NULL, ...) {
  lake()$tree(name, ...)
}

#' Show history from the default lake
#'
#' @param name Optional data name
#' @param ... Additional arguments passed to Lake$history()
#' @return History data frame
#' @export
history <- function(name = NULL, ...) {
  lake()$history(name, ...)
}

#' List tables in the default lake
#'
#' @return Data frame of table names
#' @export
tables <- function() {
  lake()$tables()
}

#' List objects in the default lake
#'
#' @return Data frame of object names
#' @export
objects <- function() {
  lake()$objects()
}

#' Check whether a data name exists in the default lake
#'
#' @param name Data name
#' @param type One of "any", "table", or "object"
#' @return TRUE/FALSE
#' @export
lake_exists <- function(name, type = c("any", "table", "object")) {
  lake()$exists(name, type = type)
}

#' Find data names in the default lake
#'
#' @param pattern Optional regex/fixed pattern; NULL returns all names
#' @param type One of "any", "table", or "object"
#' @param ignore.case Whether matching should ignore case
#' @param fixed Whether to treat pattern as fixed string
#' @param fuzzy Whether to include fuzzy matches when exact/regex matching misses
#' @param max_distance Maximum edit distance for fuzzy matches
#' @param min_score Minimum score threshold for returned candidates
#' @param prefer_type Optional type priority in tie-breaks: "none", "table", or "object"
#' @param limit Maximum number of rows to return (Inf for all)
#' @return Data frame with columns name, type, score, distance, and match_type
#' @export
lake_find <- function(pattern = NULL,
                      type = c("any", "table", "object"),
                      ignore.case = TRUE,
                      fixed = FALSE,
                      fuzzy = TRUE,
                      max_distance = 3L,
                      min_score = -Inf,
                      prefer_type = c("none", "table", "object"),
                      limit = Inf) {
  lake()$find(
    pattern = pattern,
    type = type,
    ignore.case = ignore.case,
    fixed = fixed,
    fuzzy = fuzzy,
    max_distance = max_distance,
    min_score = min_score,
    prefer_type = prefer_type,
    limit = limit
  )
}

#' Show one-line status for the default lake (or a specified project)
#'
#' @param project Optional project name. If provided, checks that project directly.
#' @param ... Additional arguments passed to Lake$new() when project is provided.
#' @return One-row data frame with status fields
#' @export
lake_status <- function(project = NULL, ...) {
  if (!is.null(project)) {
    return(Lake$new(project, ...)$status())
  }
  lake()$status()
}

#' Run diagnostics for the default lake (or a specified project)
#'
#' @param project Optional project name. If provided, checks that project directly.
#' @param ... Additional arguments passed to Lake$new() when project is provided.
#' @param verbose If TRUE, print a readable report
#' @return Data frame with diagnostic checks
#' @export
lake_doctor <- function(project = NULL, ..., verbose = TRUE) {
  if (!is.null(project)) {
    return(Lake$new(project, ...)$doctor(verbose = verbose))
  }
  lake()$doctor(verbose = verbose)
}

#' Drop data from the default lake
#'
#' @param name Data name
#' @param ... Additional arguments passed to Lake$drop()
#' @return Invisible Lake object
#' @export
drop <- function(name, ...) {
  lake()$drop(name, ...)
}

#' Execute SQL on the default lake
#'
#' @param query SQL query string
#' @param ... Additional arguments passed to Lake$sql()
#' @return Query results
#' @export
sql <- function(query, ...) {
  lake()$sql(query, ...)
}

#' Restore the default lake to a snapshot
#'
#' @param label Snapshot label
#' @return Invisible Lake object
#' @export
restore <- function(label) {
  lake()$restore(label)
}

#' Get dependencies from the default lake
#'
#' @param name Data name
#' @param direction "up" or "down"
#' @return Dependencies data frame
#' @export
deps <- function(name, direction = "up") {
  lake()$deps(name, direction)
}

#' Import data into the default lake
#'
#' @param path File path to import
#' @param name Name to store as
#' @param ... Additional arguments
#' @return Invisible Lake object
#' @export
import_data <- function(path, name, ...) {
  lake()$import(path, name, ...)
}

#' Export data from the default lake
#'
#' @param name Data name to export
#' @param path Output file path
#' @param ... Additional arguments
#' @return Invisible path
#' @export
export_data <- function(name, path, ...) {
  lake()$export(name, path, ...)
}

#' Start a query builder on the default lake
#'
#' @return A QueryBuilder instance
#' @export
query <- function() {
  lake()$query()
}

#' Start a query from a table on the default lake
#'
#' @param table Table name
#' @return A QueryBuilder instance
#' @export
from <- function(table) {
  lake()$from(table)
}

#' @title Simple `lake_*` Aliases
#' @description Consistent, memorable aliases that follow one naming rule:
#' use `lake_<verb>` for common operations.
#'
#' @name lake_aliases
NULL

#' @rdname lake_aliases
#' @param project Project name for the default lake
#' @param ... Additional arguments forwarded to the corresponding function
#' @export
lake_use <- function(project = NULL, ...) {
  use_lake(project = project, ...)
}

#' @rdname lake_aliases
#' @param name Data name
#' @param data Data to store
#' @export
lake_put <- function(name, data, ...) {
  put(name, data, ...)
}

#' @rdname lake_aliases
#' @param name Data name
#' @export
lake_get <- function(name, ...) {
  fetch(name, ...)
}

#' @rdname lake_aliases
#' @param name Table name
#' @export
lake_ref <- function(name) {
  ref(name)
}

#' @rdname lake_aliases
#' @param label Snapshot label
#' @export
lake_snap <- function(label, ...) {
  snap(label, ...)
}

#' @rdname lake_aliases
#' @param name Data name
#' @param tag Tag name
#' @export
lake_tag <- function(name, tag) {
  tag(name, tag)
}

#' @rdname lake_aliases
#' @param name Data/table/node name (meaning depends on each function)
#' @export
lake_tree <- function(name = NULL, ...) {
  tree(name, ...)
}

#' @rdname lake_aliases
#' @param type One of "any", "table", or "object"
#' @export
lake_has <- function(name, type = c("any", "table", "object")) {
  lake_exists(name, type = type)
}

#' @rdname lake_aliases
#' @param expr Expression to track
#' @export
lake_track <- function(expr, ...) {
  expr_sub <- substitute(expr)
  args <- list(...)
  call <- as.call(c(list(as.name("track_pipeline"), expr_sub), args))
  eval(call, envir = parent.frame())
}

#' @rdname lake_aliases
#' @param path Script file path
#' @export
lake_track_script <- function(path, ...) {
  track_script(path = path, ...)
}

#' @rdname lake_aliases
#' @export
lake_auto_on <- function(...) {
  ol_enable_transparent_tracking(...)
}

#' @rdname lake_aliases
#' @param commit If TRUE, write tracked lineage before disabling transparent mode
#' @export
lake_auto_off <- function(commit = TRUE) {
  ol_disable_transparent_tracking(commit = commit)
}

#' @rdname lake_aliases
#' @export
lake_strict_on <- function(...) {
  ol_enable_strict_repro_mode(...)
}
