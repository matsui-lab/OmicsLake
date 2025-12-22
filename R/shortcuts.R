#' @title Global Lake Shortcuts
#' @description Convenience functions for working with a default lake.
#' These functions provide a simpler API when working with a single project.
#'
#' @examples
#' \dontrun{
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

#' Set or get the default lake
#'
#' @param project Project name. If provided, creates/opens a lake and sets it as default.
#' @param ... Additional arguments passed to Lake$new()
#' @return The default Lake object (invisibly when setting)
#' @export
#' @examples
#' \dontrun{
#' use_lake("my_analysis")
#' lake <- use_lake()  # Get current lake
#' }
use_lake <- function(project = NULL, ...) {
  if (!is.null(project)) {
    .lake_env$default <- Lake$new(project, ...)
  }
  invisible(.lake_env$default)
}

#' Get the current default lake
#'
#' @return The default Lake object
#' @export
#' @examples
#' \dontrun{
#' use_lake("my_project")
#' l <- lake()
#' l$put("data", df)
#' }
lake <- function() {
  if (is.null(.lake_env$default)) {
    stop("No default lake set. Use use_lake('project') first.", call. = FALSE)
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
