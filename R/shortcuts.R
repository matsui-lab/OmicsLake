#' @title Global Lake Shortcuts
#' @description Convenience functions for working with a default lake.
#' These functions provide a simpler API when working with a single project.
#'
#' @return Each shortcut returns the value of the underlying \code{Lake}
#' method it wraps (see the individual help pages).
#' @examples
#' use_lake("ex_shortcuts", root = tempfile())
#' put("counts", data.frame(gene = c("A", "B"), value = c(1, 2)))
#' fetch("counts")
#' snap("v1.0")
#' tree()
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
        paste0("Available projects under ", root, ": ", paste(projects,
            collapse = ", "), ". ")
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
#' @param project Project name. If provided, creates/opens a lake and sets it
#' as default.
#' @param ... Additional arguments passed to Lake$new()
#' @return The default Lake object (invisibly when setting)
#' @export
#' @examples
#' use_lake("ex_use_lake", root = tempfile())
#' current <- use_lake() # Get current lake
use_lake <- function(project = NULL, ...) {
    if (!is.null(project)) {
        if (!is.character(project) || length(project) != 1 || 
            !nzchar(project)) {
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
    if (!is.null(opt_project) && is.character(opt_project) && 
        length(opt_project) == 1 && nzchar(opt_project)) {
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
#' use_lake("ex_lake", root = tempfile())
#' l <- lake()
#' l$put("data", data.frame(x = 1:3))
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
#' @examples
#' use_lake("ex_put", root = tempfile())
#' put("counts", data.frame(gene = c("A", "B"), value = c(1, 2)))
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
#' @examples
#' use_lake("ex_fetch", root = tempfile())
#' put("counts", data.frame(gene = c("A", "B"), value = c(1, 2)))
#' fetch("counts")
#' @seealso \code{\link{Lake}}
fetch <- function(name, ...) {
    lake()$get(name, ...)
}

#' Get a lazy reference from the default lake
#'
#' @param name Table name
#' @return A lazy table reference for use with dplyr
#' @export
#' @examples
#' use_lake("ex_ref", root = tempfile())
#' put("t", data.frame(x = 1:5))
#' dplyr::collect(dplyr::filter(ref("t"), x > 2))
ref <- function(name) {
    lake()$ref(name)
}

#' Create a snapshot of the default lake
#'
#' @param label Label for the snapshot
#' @param ... Additional arguments passed to Lake$snap()
#' @return Invisible Lake object
#' @export
#' @examples
#' use_lake("ex_snap", root = tempfile())
#' put("t", data.frame(x = 1:3))
#' snap("v1.0")
snap <- function(label, ...) {
    lake()$snap(label, ...)
}

#' Tag data in the default lake
#'
#' @param name Data name
#' @param tag Tag to apply
#' @return Invisible Lake object
#' @export
#' @examples
#' use_lake("ex_tag", root = tempfile())
#' put("counts", data.frame(x = 1:3))
#' tag("counts", "raw")
tag <- function(name, tag) {
    lake()$tag(name, tag)
}

#' Show lineage tree from the default lake
#'
#' @param name Starting node (optional)
#' @param ... Additional arguments passed to Lake$tree()
#' @return Lineage data frame
#' @export
#' @examples
#' use_lake("ex_tree", root = tempfile())
#' put("raw", data.frame(x = 1:3))
#' tree()
tree <- function(name = NULL, ...) {
    lake()$tree(name, ...)
}

#' Show history from the default lake
#'
#' @param name Optional data name
#' @param ... Additional arguments passed to Lake$history()
#' @return History data frame
#' @export
#' @examples
#' use_lake("ex_history", root = tempfile())
#' put("t", data.frame(x = 1:3))
#' history()
history <- function(name = NULL, ...) {
    lake()$history(name, ...)
}

#' List tables in the default lake
#'
#' @return Data frame of table names
#' @export
#' @examples
#' use_lake("ex_tables", root = tempfile())
#' put("t", data.frame(x = 1:3))
#' tables()
tables <- function() {
    lake()$tables()
}

#' List objects in the default lake
#'
#' @return Data frame of object names
#' @export
#' @examples
#' use_lake("ex_objects", root = tempfile())
#' put("model", list(a = 1, b = 2))
#' objects()
objects <- function() {
    lake()$objects()
}

#' Check whether a data name exists in the default lake
#'
#' @param name Data name
#' @param type One of "any", "table", or "object"
#' @return TRUE/FALSE
#' @export
#' @examples
#' use_lake("ex_exists", root = tempfile())
#' put("t", data.frame(x = 1:3))
#' lake_exists("t")
lake_exists <- function(name, type = c("any", "table", "object")) {
    lake()$exists(name, type = type)
}

#' Find data names in the default lake
#'
#' @param pattern Optional regex/fixed pattern; NULL returns all names
#' @param type One of "any", "table", or "object"
#' @param ignore.case Whether matching should ignore case
#' @param fixed Whether to treat pattern as fixed string
#' @param fuzzy Whether to include fuzzy matches when exact/regex matching
#' misses
#' @param max_distance Maximum edit distance for fuzzy matches
#' @param min_score Minimum score threshold for returned candidates
#' @param prefer_type Optional type priority in tie-breaks: "none", "table", or
#' "object"
#' @param limit Maximum number of rows to return (Inf for all)
#' @return Data frame with columns name, type, score, distance, and match_type
#' @export
#' @examples
#' use_lake("ex_find", root = tempfile())
#' put("counts", data.frame(x = 1:3))
#' lake_find("count")
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
#' @param project Optional project name. If provided, checks that project
#' directly.
#' @param ... Additional arguments passed to Lake$new() when project is
#' provided.
#' @return One-row data frame with status fields
#' @export
#' @examples
#' use_lake("ex_status", root = tempfile())
#' put("t", data.frame(x = 1:3))
#' lake_status()
lake_status <- function(project = NULL, ...) {
    if (!is.null(project)) {
        return(Lake$new(project, ...)$status())
    }
    lake()$status()
}

#' Run diagnostics for the default lake (or a specified project)
#'
#' @param project Optional project name. If provided, checks that project
#' directly.
#' @param ... Additional arguments passed to Lake$new() when project is
#' provided.
#' @param verbose If TRUE, print a readable report
#' @return Data frame with diagnostic checks
#' @export
#' @examples
#' use_lake("ex_doctor", root = tempfile())
#' put("t", data.frame(x = 1:3))
#' lake_doctor(verbose = FALSE)
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
#' @examples
#' use_lake("ex_drop", root = tempfile())
#' put("t", data.frame(x = 1:3))
#' drop("t")
drop <- function(name, ...) {
    lake()$drop(name, ...)
}

#' Execute SQL on the default lake
#'
#' @param query SQL query string
#' @param ... Additional arguments passed to Lake$sql()
#' @return Query results
#' @export
#' @examples
#' use_lake("ex_sql", root = tempfile())
#' put("t", data.frame(x = 1:3))
#' sql("SELECT COUNT(*) AS n FROM t")
sql <- function(query, ...) {
    lake()$sql(query, ...)
}

#' Restore the default lake to a snapshot
#'
#' @param label Snapshot label
#' @return Invisible Lake object
#' @export
#' @examples
#' use_lake("ex_restore", root = tempfile())
#' put("t", data.frame(x = 1:3))
#' snap("v1")
#' restore("v1")
restore <- function(label) {
    lake()$restore(label)
}

#' Get dependencies from the default lake
#'
#' @param name Data name
#' @param direction "up" or "down"
#' @return Dependencies data frame
#' @export
#' @examples
#' use_lake("ex_deps", root = tempfile())
#' put("raw", data.frame(x = 1:3))
#' deps("raw")
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
#' @examples
#' use_lake("ex_import", root = tempfile())
#' f <- file.path(tempdir(), "imp.csv")
#' write.csv(data.frame(x = 1:3), f, row.names = FALSE)
#' import_data(f, "imported")
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
#' @examples
#' use_lake("ex_export", root = tempfile())
#' put("t", data.frame(x = 1:3))
#' export_data("t", file.path(tempdir(), "t.parquet"))
export_data <- function(name, path, ...) {
    lake()$export(name, path, ...)
}

#' Start a query builder on the default lake
#'
#' @return A QueryBuilder instance
#' @export
#' @examples
#' use_lake("ex_query", root = tempfile())
#' put("t", data.frame(x = 1:3))
#' qb <- query()
query <- function() {
    lake()$query()
}

#' Start a query from a table on the default lake
#'
#' @param table Table name
#' @return A QueryBuilder instance
#' @export
#' @examples
#' use_lake("ex_from", root = tempfile())
#' put("t", data.frame(x = 1:3))
#' qb <- from("t")
from <- function(table) {
    lake()$from(table)
}

#' @title Simple `lake_*` Aliases
#' @description Consistent, memorable aliases that follow one naming rule:
#' use `lake_<verb>` for common operations.
#'
#' @return Each alias returns the value of the function it forwards to.
#' @examples
#' lake_use("ex_aliases", root = tempfile())
#' lake_put("counts", data.frame(x = 1:3))
#' lake_get("counts")
#' lake_snap("v1")
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
#' @param commit If TRUE, write tracked lineage before disabling transparent
#' mode
#' @export
lake_auto_off <- function(commit = TRUE) {
    ol_disable_transparent_tracking(commit = commit)
}

#' @rdname lake_aliases
#' @export
lake_strict_on <- function(...) {
    ol_enable_strict_repro_mode(...)
}
