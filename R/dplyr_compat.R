#' @title dplyr Compatibility Layer
#' @description Make Lake work seamlessly with dplyr pipelines.
#' Automatically tracks dependencies through dplyr operations.
#'
#' @examples
#' \dontrun
#' lake <- Lake$new("my_project")
#'
#' # Dependencies are automatically tracked through the pipe
#' lake$ref("counts") |>
#'   dplyr::filter(quality > 0.8) |>
#'   dplyr::left_join(lake$ref("metadata"), by = "sample_id") |>
#'   dplyr::group_by(condition) |>
#'   dplyr::summarize(mean_expr = mean(expression)) |>
#'   save_as("summary", lake)
#' }
#'
#' @name dplyr_compat
NULL

#' Save pipe result to lake
#'
#' This function is designed to be used at the end of a dplyr pipe
#' to save results to a Lake with automatic dependency tracking.
#'
#' @param .data Data from pipe (data.frame or tbl_lazy)
#' @param name Name to save as in the lake
#' @param lake Lake instance. If NULL, uses the default lake from use_lake()
#' @return Invisibly returns the data (for potential further piping)
#' @export
#' @examples
#' \dontrun{
#' lake$ref("raw_data") |>
#'   dplyr::filter(quality > 0.5) |>
#'   dplyr::mutate(normalized = value / mean(value)) |>
#'   save_as("processed_data", lake)
#' }
save_as <- function(.data, name, lake = NULL) {
  if (is.null(lake)) {
    lake <- lake()
  }

  # Extract tracked dependencies from attributes
  deps <- attr(.data, "lake_source")
  if (is.null(deps)) {
    deps <- attr(.data, "lake_deps")
  }

  # Collect if lazy

  if (inherits(.data, "tbl_lazy")) {
    .data <- dplyr::collect(.data)
  }

  # Save to lake with dependencies
  lake$put(name, .data, depends_on = deps)

  invisible(.data)
}

#' Create a pipe-compatible lake write function
#'
#' Creates a function that can be used at the end of a pipe to save data.
#'
#' @param lake Lake instance
#' @return A function that accepts data and name, saves to lake
#' @export
#' @examples
#' \dontrun{
#' write_to <- into(lake)
#'
#' df |>
#'   dplyr::filter(x > 5) |>
#'   write_to("filtered_data")
#' }
into <- function(lake) {
  force(lake)
  function(.data, name) {
    save_as(.data, name, lake)
  }
}

#' Pipe operator that saves to lake
#'
#' Use this operator at the end of a pipe to save results to a lake.
#' Format: data %>>% "lake_name" or data %>>% "project/name"
#'
#' @param .data Data from pipe
#' @param target Target specification ("name" or "project/name")
#' @return Invisibly returns the data
#' @export
#' @examples
#' \dontrun{
#' use_lake("my_project")
#'
#' df |>
#'   dplyr::filter(x > 5) %>>%
#'   "filtered_data"
#' }
`%>>%` <- function(.data, target) {
  parts <- strsplit(target, "/", fixed = TRUE)[[1]]

  if (length(parts) == 2) {
    lake_inst <- Lake$new(parts[1])
    name <- parts[2]
  } else {
    lake_inst <- lake()
    name <- parts[1]
  }

  save_as(.data, name, lake_inst)
}

# ============================================================
# S3 methods for lake_tbl class to preserve lineage through
# dplyr operations
# ============================================================

#' @export
filter.lake_tbl <- function(.data, ..., .preserve = FALSE) {
  result <- NextMethod()
  # Preserve lineage metadata
  attr(result, "lake_source") <- attr(.data, "lake_source")
  class(result) <- unique(c("lake_tbl", class(result)))
  result
}

#' @export
select.lake_tbl <- function(.data, ...) {
  result <- NextMethod()
  attr(result, "lake_source") <- attr(.data, "lake_source")
  class(result) <- unique(c("lake_tbl", class(result)))
  result
}

#' @export
mutate.lake_tbl <- function(.data, ...) {
  result <- NextMethod()
  attr(result, "lake_source") <- attr(.data, "lake_source")
  class(result) <- unique(c("lake_tbl", class(result)))
  result
}

#' @export
summarise.lake_tbl <- function(.data, ..., .groups = NULL) {
  result <- NextMethod()
  attr(result, "lake_source") <- attr(.data, "lake_source")
  class(result) <- unique(c("lake_tbl", class(result)))
  result
}

#' @export
summarize.lake_tbl <- function(.data, ..., .groups = NULL) {
  summarise.lake_tbl(.data, ..., .groups = .groups)
}

#' @export
group_by.lake_tbl <- function(.data, ..., .add = FALSE, .drop = TRUE) {
  result <- NextMethod()
  attr(result, "lake_source") <- attr(.data, "lake_source")
  class(result) <- unique(c("lake_tbl", class(result)))
  result
}

#' @export
ungroup.lake_tbl <- function(x, ...) {
  result <- NextMethod()
  attr(result, "lake_source") <- attr(x, "lake_source")
  class(result) <- unique(c("lake_tbl", class(result)))
  result
}

#' @export
arrange.lake_tbl <- function(.data, ..., .by_group = FALSE) {
  result <- NextMethod()
  attr(result, "lake_source") <- attr(.data, "lake_source")
  class(result) <- unique(c("lake_tbl", class(result)))
  result
}

#' @export
distinct.lake_tbl <- function(.data, ..., .keep_all = FALSE) {
  result <- NextMethod()
  attr(result, "lake_source") <- attr(.data, "lake_source")
  class(result) <- unique(c("lake_tbl", class(result)))
  result
}

#' @export
slice.lake_tbl <- function(.data, ..., .preserve = FALSE) {
  result <- NextMethod()
  attr(result, "lake_source") <- attr(.data, "lake_source")
  class(result) <- unique(c("lake_tbl", class(result)))
  result
}

#' @export
rename.lake_tbl <- function(.data, ...) {
  result <- NextMethod()
  attr(result, "lake_source") <- attr(.data, "lake_source")
  class(result) <- unique(c("lake_tbl", class(result)))
  result
}

#' @export
relocate.lake_tbl <- function(.data, ..., .before = NULL, .after = NULL) {
  result <- NextMethod()
  attr(result, "lake_source") <- attr(.data, "lake_source")
  class(result) <- unique(c("lake_tbl", class(result)))
  result
}

# Join operations - merge sources from both sides

#' @export
left_join.lake_tbl <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  result <- NextMethod()
  # Combine sources from both tables
  x_source <- attr(x, "lake_source")
  y_source <- attr(y, "lake_source")
  attr(result, "lake_source") <- unique(c(x_source, y_source))
  class(result) <- unique(c("lake_tbl", class(result)))
  result
}

#' @export
inner_join.lake_tbl <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  result <- NextMethod()
  x_source <- attr(x, "lake_source")
  y_source <- attr(y, "lake_source")
  attr(result, "lake_source") <- unique(c(x_source, y_source))
  class(result) <- unique(c("lake_tbl", class(result)))
  result
}

#' @export
right_join.lake_tbl <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  result <- NextMethod()
  x_source <- attr(x, "lake_source")
  y_source <- attr(y, "lake_source")
  attr(result, "lake_source") <- unique(c(x_source, y_source))
  class(result) <- unique(c("lake_tbl", class(result)))
  result
}

#' @export
full_join.lake_tbl <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  result <- NextMethod()
  x_source <- attr(x, "lake_source")
  y_source <- attr(y, "lake_source")
  attr(result, "lake_source") <- unique(c(x_source, y_source))
  class(result) <- unique(c("lake_tbl", class(result)))
  result
}

#' @export
semi_join.lake_tbl <- function(x, y, by = NULL, copy = FALSE, ...) {
  result <- NextMethod()
  x_source <- attr(x, "lake_source")
  y_source <- attr(y, "lake_source")
  attr(result, "lake_source") <- unique(c(x_source, y_source))
  class(result) <- unique(c("lake_tbl", class(result)))
  result
}

#' @export
anti_join.lake_tbl <- function(x, y, by = NULL, copy = FALSE, ...) {
  result <- NextMethod()
  x_source <- attr(x, "lake_source")
  y_source <- attr(y, "lake_source")
  attr(result, "lake_source") <- unique(c(x_source, y_source))
  class(result) <- unique(c("lake_tbl", class(result)))
  result
}

# Collect preserves lineage info differently

#' @export
collect.lake_tbl <- function(x, ...) {
  result <- NextMethod()
  # Transfer lake_source to lake_deps for collected data
  sources <- attr(x, "lake_source")
  if (!is.null(sources)) {
    attr(result, "lake_deps") <- sources
  }
  result
}

# Print method for lake_tbl

#' @export
print.lake_tbl <- function(x, ...) {
  sources <- attr(x, "lake_source")
  if (!is.null(sources)) {
    cat("# Lake table from:", paste(sources, collapse = ", "), "\n")
  }
  NextMethod()
}
