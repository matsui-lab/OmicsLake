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

  # Extract tracked dependencies - prefer new paired format
  lake_sources <- attr(.data, "lake_sources")
  deps <- NULL

  if (!is.null(lake_sources) && length(lake_sources) > 0) {
    # New format: list of list(name=..., ref=...)
    deps <- lake_sources
  } else {
    # Fallback to legacy format
    # Note: lake_source_ref may be a vector (from joins), so pair by index
    legacy_deps <- attr(.data, "lake_source")
    if (is.null(legacy_deps)) {
      legacy_deps <- attr(.data, "lake_deps")
    }
    if (!is.null(legacy_deps)) {
      # Convert to new format with ref, pairing by index if possible
      source_ref <- attr(.data, "lake_source_ref")
      if (is.null(source_ref)) source_ref <- "@latest"

      deps <- lapply(seq_along(legacy_deps), function(i) {
        dep_name <- legacy_deps[i]
        # Pair ref by index if source_ref is a vector of same length
        if (length(source_ref) == length(legacy_deps)) {
          ref_val <- source_ref[i]
        } else if (length(source_ref) == 1) {
          ref_val <- source_ref
        } else {
          ref_val <- "@latest"  # Safe fallback for mismatched lengths
        }
        list(name = dep_name, ref = ref_val)
      })
    }
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

# Helper to preserve lineage attributes (with paired name-ref storage)
.preserve_lake_attrs <- function(result, source_data) {
  # New format: lake_sources is a list of list(name=..., ref=...)
  attr(result, "lake_sources") <- attr(source_data, "lake_sources")
  # Legacy format: keep for backward compatibility
  attr(result, "lake_source") <- attr(source_data, "lake_source")
  attr(result, "lake_source_ref") <- attr(source_data, "lake_source_ref")
  class(result) <- unique(c("lake_tbl", class(result)))
  result
}

#' @export
filter.lake_tbl <- function(.data, ..., .preserve = FALSE) {
  result <- NextMethod()
  .preserve_lake_attrs(result, .data)
}

#' @export
select.lake_tbl <- function(.data, ...) {
  result <- NextMethod()
  .preserve_lake_attrs(result, .data)
}

#' @export
mutate.lake_tbl <- function(.data, ...) {
  result <- NextMethod()
  .preserve_lake_attrs(result, .data)
}

#' @export
summarise.lake_tbl <- function(.data, ..., .groups = NULL) {
  result <- NextMethod()
  .preserve_lake_attrs(result, .data)
}

#' @export
summarize.lake_tbl <- function(.data, ..., .groups = NULL) {
  summarise.lake_tbl(.data, ..., .groups = .groups)
}

#' @export
group_by.lake_tbl <- function(.data, ..., .add = FALSE, .drop = TRUE) {
  result <- NextMethod()
  .preserve_lake_attrs(result, .data)
}

#' @export
ungroup.lake_tbl <- function(x, ...) {
  result <- NextMethod()
  .preserve_lake_attrs(result, x)
}

#' @export
arrange.lake_tbl <- function(.data, ..., .by_group = FALSE) {
  result <- NextMethod()
  .preserve_lake_attrs(result, .data)
}

#' @export
distinct.lake_tbl <- function(.data, ..., .keep_all = FALSE) {
  result <- NextMethod()
  .preserve_lake_attrs(result, .data)
}

#' @export
slice.lake_tbl <- function(.data, ..., .preserve = FALSE) {
  result <- NextMethod()
  .preserve_lake_attrs(result, .data)
}

#' @export
rename.lake_tbl <- function(.data, ...) {
  result <- NextMethod()
  .preserve_lake_attrs(result, .data)
}

#' @export
relocate.lake_tbl <- function(.data, ..., .before = NULL, .after = NULL) {
  result <- NextMethod()
  .preserve_lake_attrs(result, .data)
}

# Join operations - merge sources from both sides
# Helper to merge lineage from two tables (with proper name-ref pairing)
.merge_lake_attrs <- function(result, x, y) {
  # New format: merge lake_sources lists
  x_sources <- attr(x, "lake_sources")
  y_sources <- attr(y, "lake_sources")

  # Combine and deduplicate by name (keep first occurrence)
  merged_sources <- c(x_sources, y_sources)
  if (length(merged_sources) > 0) {
    seen_names <- character(0)
    unique_sources <- list()
    for (src in merged_sources) {
      if (!is.null(src$name) && !(src$name %in% seen_names)) {
        seen_names <- c(seen_names, src$name)
        unique_sources <- c(unique_sources, list(src))
      }
    }
    attr(result, "lake_sources") <- unique_sources
  }

  # Legacy format: keep for backward compatibility
  x_source <- attr(x, "lake_source")
  y_source <- attr(y, "lake_source")
  x_ref <- attr(x, "lake_source_ref")
  y_ref <- attr(y, "lake_source_ref")

  attr(result, "lake_source") <- unique(c(x_source, y_source))
  if (!is.null(x_ref) || !is.null(y_ref)) {
    attr(result, "lake_source_ref") <- unique(c(x_ref, y_ref))
  }
  class(result) <- unique(c("lake_tbl", class(result)))
  result
}

#' @export
left_join.lake_tbl <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  result <- NextMethod()
  .merge_lake_attrs(result, x, y)
}

#' @export
inner_join.lake_tbl <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  result <- NextMethod()
  .merge_lake_attrs(result, x, y)
}

#' @export
right_join.lake_tbl <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  result <- NextMethod()
  .merge_lake_attrs(result, x, y)
}

#' @export
full_join.lake_tbl <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  result <- NextMethod()
  .merge_lake_attrs(result, x, y)
}

#' @export
semi_join.lake_tbl <- function(x, y, by = NULL, copy = FALSE, ...) {
  result <- NextMethod()
  .merge_lake_attrs(result, x, y)
}

#' @export
anti_join.lake_tbl <- function(x, y, by = NULL, copy = FALSE, ...) {
  result <- NextMethod()
  .merge_lake_attrs(result, x, y)
}

# Collect preserves lineage info differently

#' @export
collect.lake_tbl <- function(x, ...) {
  result <- NextMethod()
  # Transfer lake_sources (new paired format) to collected data
  lake_sources <- attr(x, "lake_sources")
  if (!is.null(lake_sources) && length(lake_sources) > 0) {
    attr(result, "lake_sources") <- lake_sources
  }
  # Also transfer legacy format for backward compatibility
  sources <- attr(x, "lake_source")
  source_ref <- attr(x, "lake_source_ref")
  if (!is.null(sources)) {
    attr(result, "lake_deps") <- sources
    if (!is.null(source_ref)) {
      attr(result, "lake_source_ref") <- source_ref
    }
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
