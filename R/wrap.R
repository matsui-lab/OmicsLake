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
#' \dontrun{
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
#' \dontrun{
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

    # Attach lineage info to result
    attr(result, "lake_deps") <- deps
    attr(result, "lake_output") <- output_name

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
#' \dontrun{
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

  # Attach lineage
  attr(result, "lake_deps") <- deps

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
#' @param data Optional data to extract metadata from (not stored)
#' @param lake Lake instance (uses default if NULL)
#' @return Invisibly returns the data (for piping)
#' @export
#' @examples
#' \dontrun{
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

  # Extract metadata without storing actual data
  metadata <- list(
    class = if (!is.null(data)) class(data) else "marker",
    type = if (!is.null(data)) typeof(data) else "unknown",
    dims = if (!is.null(data) && (is.data.frame(data) || is.matrix(data))) dim(data) else NULL,
    length = if (!is.null(data)) length(data) else 0,
    marked_at = Sys.time()
  )

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
#' \dontrun{
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
#' \dontrun{
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
      cat("Pipeline:", private$.name, "\n")
      cat("Steps:", length(private$.steps), "\n")
      for (i in seq_along(private$.steps)) {
        cat("  ", i, ". ", private$.steps[[i]]$name, "\n")
      }
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
#' \dontrun{
#' trace_calls(
#'   list(read.csv = "input", write.csv = "output"),
#'   {
#'     data <- read.csv("file.csv")
#'     write.csv(data, "out.csv")
#'   }
#' )
#' }
trace_calls <- function(functions, expr, lake = NULL) {
  calls <- list()

  # Set up traces
  for (fn_name in names(functions)) {
    role <- functions[[fn_name]]

    tracer <- bquote({
      .record_trace_call(.(fn_name), .(role), match.call())
    })

    tryCatch({
      suppressMessages(trace(fn_name, tracer, print = FALSE, where = parent.frame()))
    }, error = function(e) {
      # Function might not be traceable
    })
  }

  # Helper to record calls
  .record_trace_call <- function(fn_name, role, call) {
    calls <<- append(calls, list(list(
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
        suppressMessages(untrace(fn_name, where = parent.frame()))
      }, error = function(e) {
        # Ignore
      })
    }
  })

  list(
    result = result,
    calls = calls
  )
}
