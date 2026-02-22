#' @title Function Wrapping for Lineage Tracking
#' @description Wrap existing functions to automatically track data lineage.
#' Enables non-invasive integration with existing analysis pipelines.
#'
#' @details
#' Function wrapping provides a way to add lineage tracking to existing code
#' without modifying the original functions. Wrapped functions automatically
#' record their inputs and outputs to the lineage graph.
#'
#' @examples
#' if (FALSE) {
#' lake <- Lake$new("project")
#'
#' # Wrap a function
#' tracked_normalize <- wrap_fn(normalize_data, lake, "normalized")
#'
#' # Use the wrapped function
#' result <- tracked_normalize(raw_data)
#' # Dependencies are automatically recorded
#'
#' # Or wrap and use inline
#' result <- lake |>
#'   wrap_call(normalize_data, raw_data, output = "normalized")
#' }
#'
#' @name wrap
NULL

#' Wrap a function with lineage tracking
#'
#' Creates a wrapped version of a function that automatically records
#' its execution in the lake's lineage graph.
#'
#' @param fn Function to wrap
#' @param lake Lake instance for lineage recording
#' @param output_name Name for the output in the lineage graph
#' @param input_names Optional character vector specifying which arguments
#'   should be recorded as dependencies. If NULL, attempts to detect
#'   lake-sourced data automatically.
#' @param save_output If TRUE, automatically saves the output to the lake
#' @return A wrapped function with the same signature as fn
#' @export
#' @examples
#' if (FALSE) {
#' lake <- Lake$new("project")
#' lake$put("raw_data", data.frame(x = 1:10))
#'
#' # Create a simple processing function
#' process <- function(data) {
#'   data$y <- data$x * 2
#'   data
#' }
#'
#' # Wrap it
#' tracked_process <- wrap_fn(process, lake, "processed_data")
#'
#' # Use the wrapped function
#' input <- lake$get("raw_data")
#' result <- tracked_process(input)
#'
#' # The lineage is automatically recorded
#' lake$tree("processed_data")
#' }
wrap_fn <- function(fn, lake, output_name, input_names = NULL, save_output = TRUE) {
  force(fn)
  force(lake)
  force(output_name)
  force(input_names)
  force(save_output)

  function(...) {
    args <- list(...)
    arg_names <- names(args)

    # Detect dependencies from arguments
    deps <- character(0)

    # Check for explicit input names
    if (!is.null(input_names)) {
      for (input_name in input_names) {
        if (input_name %in% arg_names) {
          val <- args[[input_name]]
          # If it's a string, assume it's a lake reference
          if (is.character(val) && length(val) == 1) {
            deps <- c(deps, val)
          }
        }
      }
    }

    # Check for lake_deps attribute on arguments
    for (arg in args) {
      arg_deps <- attr(arg, "lake_deps")
      if (!is.null(arg_deps)) {
        deps <- c(deps, arg_deps)
      }
      # Also check lake_source
      arg_source <- attr(arg, "lake_source")
      if (!is.null(arg_source)) {
        deps <- c(deps, arg_source)
      }
    }

    deps <- unique(deps)

    # Execute the function
    result <- do.call(fn, args)

    # Save to lake if requested
    if (save_output) {
      lake$put(output_name, result, depends_on = deps)
    }

    result
  }
}

#' Wrap a function call inline
#'
#' Execute a function call with lineage tracking, without creating
#' a persistent wrapper.
#'
#' @param lake Lake instance
#' @param fn Function to call
#' @param ... Arguments to pass to fn
#' @param output Name for the output in the lineage
#' @param save If TRUE, save the result to the lake
#' @return The result of calling fn
#' @export
#' @examples
#' if (FALSE) {
#' lake <- Lake$new("project")
#'
#' result <- wrap_call(lake, mean, c(1, 2, 3, NA), na.rm = TRUE,
#'                     output = "mean_result", save = FALSE)
#' }
wrap_call <- function(lake, fn, ..., output = NULL, save = TRUE) {
  args <- list(...)

  # Detect dependencies
  deps <- character(0)
  for (arg in args) {
    arg_deps <- attr(arg, "lake_deps")
    if (!is.null(arg_deps)) {
      deps <- c(deps, arg_deps)
    }
    arg_source <- attr(arg, "lake_source")
    if (!is.null(arg_source)) {
      deps <- c(deps, arg_source)
    }
  }
  deps <- unique(deps)

  # Execute
  result <- do.call(fn, args)

  # Save if requested
  if (save && !is.null(output)) {
    lake$put(output, result, depends_on = deps)
  }

  result
}

#' Mark data in lineage without storing
#'
#' Creates a node in the lineage graph without actually storing any data.
#' Useful for tracking external files or intermediate calculations.
#'
#' @param name Node name in the lineage graph
#' @param data Optional data to extract metadata from (not stored).
#'   Use a character scalar for file path/URL, or a named list with fields like
#'   \code{source_kind}, \code{source_id}, \code{etag}, \code{last_modified},
#'   \code{status_code}, \code{content_md5}, \code{response_md5}.
#' @param lake Lake instance (uses default if NULL)
#' @return Invisibly returns the data (for piping)
#' @export
#' @examples
#' if (FALSE) {
#' lake <- Lake$new("project")
#'
#' # Mark an external file in the lineage
#' mark("external_data.csv", lake = lake)
#'
#' # Mark with metadata extraction
#' large_matrix <- matrix(1:1000000, 1000, 1000)
#' mark("large_computation", large_matrix, lake)
#' # Only metadata is recorded, not the actual data
#' }
mark <- function(name, data = NULL, lake = NULL) {
  if (is.null(lake)) {
    lake <- lake()
  }

  source_meta <- .ol_marker_source_metadata(data = data, name = name)

  # Extract metadata without storing actual data
  metadata <- list(
    class = if (!is.null(data)) class(data) else "marker",
    type = if (!is.null(data)) typeof(data) else "unknown",
    dims = if (!is.null(data) && (is.data.frame(data) || is.matrix(data))) dim(data) else NULL,
    length = if (!is.null(data)) length(data) else 0,
    marked_at = Sys.time(),
    marked_at_utc = .ol_marker_now_utc()
  )
  metadata <- c(metadata, source_meta)

  # Store as a marker object
  state <- lake$.__enclos_env__$private$.state
  .ol_ensure_dependencies_table(state)

  # We can store the marker metadata as an object
  tryCatch({
    ol_save(paste0(".__marker__.", name), metadata,
            project = lake$.__enclos_env__$private$.project)
  }, error = function(e) {
    # Ignore if can't save - just for lineage
  })

  invisible(data)
}

.ol_marker_now_utc <- function() {
  format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%d %H:%M:%S UTC")
}

.ol_marker_empty_source_metadata <- function() {
  list(
    source_kind = "value",
    source_id = NA_character_,
    source_exists = NA,
    size_bytes = NA_real_,
    mtime_utc = NA_character_,
    content_md5 = NA_character_,
    hash_skipped = FALSE,
    etag = NA_character_,
    last_modified = NA_character_,
    status_code = NA_integer_,
    response_md5 = NA_character_
  )
}

.ol_marker_scalar_chr <- function(x, default = NA_character_) {
  if (is.null(x) || length(x) < 1 || is.na(x[[1]])) {
    return(default)
  }
  as.character(x[[1]])
}

.ol_marker_scalar_lgl <- function(x, default = NA) {
  if (is.null(x) || length(x) < 1 || is.na(x[[1]])) {
    return(default)
  }
  as.logical(x[[1]])
}

.ol_marker_scalar_num <- function(x, default = NA_real_) {
  if (is.null(x) || length(x) < 1 || is.na(x[[1]])) {
    return(default)
  }
  as.numeric(x[[1]])
}

.ol_marker_scalar_int <- function(x, default = NA_integer_) {
  if (is.null(x) || length(x) < 1 || is.na(x[[1]])) {
    return(default)
  }
  as.integer(x[[1]])
}

.ol_marker_file_metadata <- function(path, out = .ol_marker_empty_source_metadata()) {
  if (!(is.character(path) && length(path) == 1 && !is.na(path) && nzchar(path))) {
    return(out)
  }
  out$source_kind <- "file"
  out$source_id <- tryCatch(
    normalizePath(path, winslash = "/", mustWork = FALSE),
    error = function(e) path
  )
  exists <- file.exists(path)
  out$source_exists <- isTRUE(exists)
  if (!isTRUE(exists)) {
    return(out)
  }

  info <- tryCatch(file.info(path), error = function(e) NULL)
  if (!is.null(info) && nrow(info) > 0) {
    out$size_bytes <- as.numeric(info$size[[1]])
    mtime <- info$mtime[[1]]
    if (!is.na(mtime)) {
      out$mtime_utc <- format(as.POSIXct(mtime, tz = "UTC"), "%Y-%m-%d %H:%M:%S UTC")
    }
  }

  max_bytes <- getOption("ol.external.hash.max_bytes", 50 * 1024 * 1024)
  if (!is.numeric(max_bytes) || length(max_bytes) != 1 || is.na(max_bytes) || max_bytes < 0) {
    max_bytes <- 50 * 1024 * 1024
  }
  if (!is.na(out$size_bytes) && out$size_bytes > max_bytes) {
    out$hash_skipped <- TRUE
    return(out)
  }
  out$content_md5 <- tryCatch(
    as.character(unname(tools::md5sum(path))[1]),
    error = function(e) NA_character_
  )
  out
}

.ol_marker_source_metadata <- function(data, name = NULL) {
  out <- .ol_marker_empty_source_metadata()
  if (is.character(name) && length(name) == 1 && !is.na(name) && nzchar(name)) {
    if (grepl("^api:", name, ignore.case = TRUE)) {
      out$source_kind <- "api"
      out$source_id <- as.character(name)
    } else if (grepl("^url:", name, ignore.case = TRUE)) {
      out$source_kind <- "url"
      out$source_id <- as.character(name)
    } else if (grepl("^file:", name, ignore.case = TRUE)) {
      out$source_kind <- "file"
      out$source_id <- as.character(name)
    } else if (grepl("^external:", name, ignore.case = TRUE)) {
      out$source_kind <- "external"
      out$source_id <- as.character(name)
    }
  }

  if (is.null(data)) {
    return(out)
  }

  if (is.list(data) && !is.data.frame(data) && !is.null(names(data))) {
    source_kind <- .ol_marker_scalar_chr(data$source_kind, default = NA_character_)
    source_id <- .ol_marker_scalar_chr(data$source_id, default = NA_character_)
    if (is.na(source_id)) {
      source_id <- .ol_marker_scalar_chr(data$path, default = source_id)
    }
    if (is.na(source_id)) {
      source_id <- .ol_marker_scalar_chr(data$file, default = source_id)
    }
    if (is.na(source_id)) {
      source_id <- .ol_marker_scalar_chr(data$url, default = source_id)
    }
    if (is.na(source_id)) {
      source_id <- .ol_marker_scalar_chr(data$endpoint, default = source_id)
    }
    if (is.na(source_id)) {
      source_id <- .ol_marker_scalar_chr(data$uri, default = source_id)
    }
    if (!is.na(source_kind)) {
      out$source_kind <- source_kind
    }
    if (!is.na(source_id)) {
      out$source_id <- source_id
    }

    if (identical(out$source_kind, "value")) {
      if (is.character(out$source_id) && nzchar(out$source_id) && grepl("^(https?|ftp)://", out$source_id, ignore.case = TRUE)) {
        out$source_kind <- "url"
      } else if (is.character(out$source_id) && nzchar(out$source_id) && file.exists(out$source_id)) {
        out$source_kind <- "file"
      }
    }
    if (identical(out$source_kind, "file") && is.character(out$source_id) && nzchar(out$source_id)) {
      out <- .ol_marker_file_metadata(out$source_id, out = out)
    }

    source_exists <- .ol_marker_scalar_lgl(data$source_exists, default = NA)
    if (!is.na(source_exists)) {
      out$source_exists <- source_exists
    }
    size_bytes <- .ol_marker_scalar_num(data$size_bytes, default = NA_real_)
    if (!is.na(size_bytes)) {
      out$size_bytes <- size_bytes
    }
    mtime_utc <- .ol_marker_scalar_chr(data$mtime_utc, default = NA_character_)
    if (!is.na(mtime_utc)) {
      out$mtime_utc <- mtime_utc
    }
    content_md5 <- .ol_marker_scalar_chr(data$content_md5, default = NA_character_)
    if (!is.na(content_md5)) {
      out$content_md5 <- content_md5
    }
    hash_skipped <- .ol_marker_scalar_lgl(data$hash_skipped, default = NA)
    if (!is.na(hash_skipped)) {
      out$hash_skipped <- isTRUE(hash_skipped)
    }
    etag <- .ol_marker_scalar_chr(data$etag, default = NA_character_)
    if (!is.na(etag)) {
      out$etag <- etag
    }
    last_modified <- .ol_marker_scalar_chr(data$last_modified, default = NA_character_)
    if (!is.na(last_modified)) {
      out$last_modified <- last_modified
    }
    status_code <- .ol_marker_scalar_int(data$status_code, default = NA_integer_)
    if (!is.na(status_code)) {
      out$status_code <- status_code
    }
    response_md5 <- .ol_marker_scalar_chr(data$response_md5, default = NA_character_)
    if (!is.na(response_md5)) {
      out$response_md5 <- response_md5
    }
    return(out)
  }

  if (!(is.character(data) && length(data) == 1 && !is.na(data) && nzchar(data))) {
    return(out)
  }

  value <- as.character(data[[1]])
  if (grepl("^api:", value, ignore.case = TRUE)) {
    out$source_kind <- "api"
    out$source_id <- value
    return(out)
  }
  if (grepl("^(https?|ftp)://", value, ignore.case = TRUE)) {
    out$source_kind <- "url"
    out$source_id <- value
    return(out)
  }

  out <- .ol_marker_file_metadata(value, out = out)
  out
}

#' Create an explicit dependency link
#'
#' Manually creates a dependency relationship between two nodes in
#' the lineage graph.
#'
#' @param from Source node name (parent)
#' @param to Target node name (child)
#' @param lake Lake instance (uses default if NULL)
#' @param relationship Type of relationship (default: "linked")
#' @return Invisible TRUE on success
#' @export
#' @examples
#' if (FALSE) {
#' lake <- Lake$new("project")
#'
#' # Store some data
#' lake$put("source_data", df1)
#' lake$put("derived_data", df2)
#'
#' # Manually link them
#' link("source_data", "derived_data", lake)
#'
#' # Now lineage shows the connection
#' lake$tree("derived_data")
#' }
link <- function(from, to, lake = NULL, relationship = "linked") {
  if (is.null(lake)) {
    lake <- lake()
  }

  state <- lake$.__enclos_env__$private$.state
  .ol_ensure_dependencies_table(state)

  # Determine types
  from_type <- if (lake$.__enclos_env__$private$.is_object(from)) "object" else "table"
  to_type <- if (lake$.__enclos_env__$private$.is_object(to)) "object" else "table"

  # Record the dependency
  .ol_record_dependency(state, to, to_type, from, from_type, relationship)

  invisible(TRUE)
}

#' Unlink a dependency
#'
#' Removes a dependency relationship between two nodes.
#'
#' @param from Source node name
#' @param to Target node name
#' @param lake Lake instance (uses default if NULL)
#' @return Invisible TRUE on success
#' @export
unlink_dep <- function(from, to, lake = NULL) {
  if (is.null(lake)) {
    lake <- lake()
  }

  state <- lake$.__enclos_env__$private$.state
  conn <- state$conn
  ident <- .ol_sql_ident(conn, state, "__ol_dependencies")

  sql <- sprintf(
    "DELETE FROM %s WHERE parent_name = %s AND child_name = %s",
    ident,
    DBI::dbQuoteString(conn, from),
    DBI::dbQuoteString(conn, to)
  )

  DBI::dbExecute(conn, sql)
  invisible(TRUE)
}

#' Create a pipeline of tracked operations
#'
#' Defines a sequence of operations that will be tracked as a unit.
#'
#' @param lake Lake instance
#' @param name Name for this pipeline
#' @return A Pipeline object
#' @export
#' @examples
#' if (FALSE) {
#' lake <- Lake$new("project")
#'
#' pipeline <- create_pipeline(lake, "preprocessing")
#' pipeline$
#'   step("load", function() read.csv("data.csv"))$
#'   step("clean", function(data) na.omit(data))$
#'   step("normalize", function(data) scale(data))$
#'   run()
#'
#' # All steps are recorded in lineage
#' lake$tree("preprocessing.normalize")
#' }
create_pipeline <- function(lake, name) {
  Pipeline$new(lake, name)
}

#' @title Pipeline Class
#' @description Represents a sequence of tracked operations
#' @keywords internal
Pipeline <- R6::R6Class("Pipeline",
  public = list(
    initialize = function(lake, name) {
      private$.lake <- lake
      private$.name <- name
      private$.steps <- list()
      invisible(self)
    },

    #' Add a step to the pipeline
    step = function(name, fn) {
      private$.steps <- append(private$.steps, list(list(
        name = name,
        fn = fn
      )))
      self
    },

    #' Run the pipeline
    run = function(input = NULL) {
      result <- input
      prev_name <- NULL

      for (step in private$.steps) {
        step_name <- paste(private$.name, step$name, sep = ".")

        # Execute step
        if (is.null(result)) {
          result <- step$fn()
        } else {
          result <- step$fn(result)
        }

        # Record in lake with dependency on previous step
        deps <- if (!is.null(prev_name)) prev_name else character(0)
        private$.lake$put(step_name, result, depends_on = deps)

        prev_name <- step_name
      }

      result
    },

    print = function() {
      lines <- c(
        paste0("Pipeline: ", private$.name),
        paste0("Steps: ", length(private$.steps))
      )
      for (i in seq_along(private$.steps)) {
        lines <- c(lines, paste0("  ", i, ". ", private$.steps[[i]]$name))
      }
      writeLines(lines)
      invisible(self)
    }
  ),

  private = list(
    .lake = NULL,
    .name = NULL,
    .steps = list()
  )
)

#' Trace function calls for lineage
#'
#' Temporarily hooks into specified functions to track their calls.
#' More invasive than observe() but provides finer-grained control.
#'
#' @param functions Named list of functions to trace
#' @param expr Expression to evaluate while tracing
#' @param lake Optional lake to record to
#' @return List with result and call trace
#' @export
#' @examples
#' if (FALSE) {
#' trace_calls(
#'   list(read.csv = "input", write.csv = "output"),
#'   {
#'     data <- read.csv("file.csv")
#'     write.csv(data, "out.csv")
#'   }
#' )
#' }
trace_calls <- function(functions, expr, lake = NULL) {
  calls_env <- new.env(parent = emptyenv())
  calls_env$calls <- list()

  # Set up traces
  for (fn_name in names(functions)) {
    role <- functions[[fn_name]]

    tracer <- bquote({
      .record_trace_call(.(fn_name), .(role), match.call())
    })

    tryCatch({
      trace(fn_name, tracer, print = FALSE, where = parent.frame())
    }, error = function(e) {
      # Function might not be traceable
    })
  }

  # Helper to record calls
  .record_trace_call <- function(fn_name, role, call) {
    calls_env$calls <- append(calls_env$calls, list(list(
      fn = fn_name,
      role = role,
      call = call,
      time = Sys.time()
    )))
  }

  # Execute
  result <- tryCatch({
    eval(substitute(expr), envir = parent.frame())
  }, finally = {
    # Remove traces
    for (fn_name in names(functions)) {
      tryCatch({
        untrace(fn_name, where = parent.frame())
      }, error = function(e) {
        # Ignore
      })
    }
  })

  list(
    result = result,
    calls = calls_env$calls
  )
}
