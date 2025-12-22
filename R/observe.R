#' @title Observation Mode
#' @description Track file I/O operations without modifying existing code.
#' Provides a non-invasive way to build lineage from existing workflows.
#'
#' @details
#' Observation mode works by temporarily hooking into R's file I/O functions
#' to track which files are read and written during code execution. This
#' allows building a lineage graph from existing scripts without any code changes.
#'
#' @examples
#' \dontrun{
#' # Observe an existing script
#' lineage <- observe({
#'   data <- read.csv("input.csv")
#'   result <- transform(data)
#'   write.csv(result, "output.csv")
#' })
#'
#' # View what was tracked
#' print(lineage)
#'
#' # Track and store in a lake
#' observe_to_lake({
#'   source("my_pipeline.R")
#' }, lake = my_lake)
#' }
#'
#' @name observe
NULL

# Global tracking environment
.observe_env <- new.env(parent = emptyenv())
.observe_env$active <- FALSE
.observe_env$reads <- character(0)
.observe_env$writes <- character(0)
.observe_env$depth <- 0

#' Start observation mode
#'
#' @keywords internal
.start_observe <- function() {
  .observe_env$active <- TRUE
  .observe_env$depth <- .observe_env$depth + 1
  if (.observe_env$depth == 1) {
    .observe_env$reads <- character(0)
    .observe_env$writes <- character(0)
  }
}

#' Stop observation mode
#'
#' @keywords internal
.stop_observe <- function() {
  .observe_env$depth <- max(0, .observe_env$depth - 1)
  if (.observe_env$depth == 0) {
    .observe_env$active <- FALSE
  }
}

#' Check if observation is active
#'
#' @keywords internal
.is_observing <- function() {
  .observe_env$active && .observe_env$depth > 0
}

#' Record a file read
#'
#' @keywords internal
.record_read <- function(path) {
  if (.is_observing()) {
    norm_path <- normalizePath(path, winslash = "/", mustWork = FALSE)
    .observe_env$reads <- unique(c(.observe_env$reads, norm_path))
  }
}

#' Record a file write
#'
#' @keywords internal
.record_write <- function(path) {
  if (.is_observing()) {
    norm_path <- normalizePath(path, winslash = "/", mustWork = FALSE)
    .observe_env$writes <- unique(c(.observe_env$writes, norm_path))
  }
}

#' Observe I/O operations in a code block
#'
#' Executes code while tracking file reads and writes to build a lineage graph.
#' This is useful for understanding data flow in existing scripts without
#' modifying them.
#'
#' @param expr Expression or code block to observe
#' @param track_functions Character vector of additional functions to track.
#'   Default tracks common I/O functions.
#' @return A list containing:
#'   \item{result}{The result of evaluating expr}
#'   \item{reads}{Character vector of files read}
#'   \item{writes}{Character vector of files written}
#'   \item{lineage}{Data frame of inferred dependencies}
#' @export
#' @examples
#' \dontrun{
#' result <- observe({
#'   data <- read.csv("data.csv")
#'   processed <- data[data$value > 0, ]
#'   write.csv(processed, "processed.csv")
#' })
#'
#' print(result$reads)   # "data.csv"
#' print(result$writes)  # "processed.csv"
#' print(result$lineage) # data.csv -> processed.csv
#' }
observe <- function(expr, track_functions = NULL) {
  # Start tracking
.start_observe()

  # Ensure cleanup on exit
  on.exit(.stop_observe(), add = TRUE)

  # Execute the expression
  result <- tryCatch({
    eval(substitute(expr), envir = parent.frame())
  }, error = function(e) {
    warning("Error during observation: ", conditionMessage(e))
    NULL
  })

  # Build lineage from tracked I/O
  reads <- .observe_env$reads
  writes <- .observe_env$writes

  # Create lineage data frame
  # Each write depends on all reads that occurred before it
  lineage <- if (length(writes) > 0 && length(reads) > 0) {
    expand.grid(
      parent = basename(reads),
      child = basename(writes),
      parent_path = reads,
      child_path = writes,
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      parent = character(0),
      child = character(0),
      parent_path = character(0),
      child_path = character(0),
      stringsAsFactors = FALSE
    )
  }

  structure(
    list(
      result = result,
      reads = reads,
      writes = writes,
      lineage = lineage
    ),
    class = "lake_observation"
  )
}

#' Print method for lake_observation
#'
#' @param x A lake_observation object
#' @param ... Additional arguments (ignored)
#' @export
print.lake_observation <- function(x, ...) {
  cat("Lake Observation\n")
  cat("================\n")
  cat("Files read:", length(x$reads), "\n")
  if (length(x$reads) > 0) {
    for (f in x$reads) {
      cat("  <- ", basename(f), "\n")
    }
  }
  cat("Files written:", length(x$writes), "\n")
  if (length(x$writes) > 0) {
    for (f in x$writes) {
      cat("  -> ", basename(f), "\n")
    }
  }
  if (nrow(x$lineage) > 0) {
    cat("Inferred lineage:\n")
    for (i in seq_len(nrow(x$lineage))) {
      cat("  ", x$lineage$parent[i], " -> ", x$lineage$child[i], "\n")
    }
  }
  invisible(x)
}

#' Observe and record to a Lake
#'
#' Executes code while tracking I/O, then records the lineage to a Lake.
#'
#' @param expr Expression to observe
#' @param lake Lake instance to record lineage to
#' @param prefix Prefix for file-based node names in the lake
#' @return The result of evaluating expr
#' @export
#' @examples
#' \dontrun{
#' lake <- Lake$new("my_project")
#'
#' observe_to_lake({
#'   source("analysis_pipeline.R")
#' }, lake = lake)
#'
#' # View the recorded lineage
#' lake$tree()
#' }
observe_to_lake <- function(expr, lake, prefix = "file:") {
  obs <- observe(substitute(expr))

  # Record reads as marker nodes
  for (path in obs$reads) {
    name <- paste0(prefix, basename(path))
    tryCatch({
      mark(name, path, lake)
    }, error = function(e) {
      # Ignore errors for marker nodes
    })
  }

  # Record writes with dependencies
  for (path in obs$writes) {
    name <- paste0(prefix, basename(path))
    deps <- paste0(prefix, basename(obs$reads))
    tryCatch({
      mark(name, path, lake)
      for (dep in deps) {
        link(dep, name, lake)
      }
    }, error = function(e) {
      # Ignore errors
    })
  }

  obs$result
}

#' Track a code block with explicit lake integration
#'
#' A simpler alternative to observe() that directly integrates with Lake.
#' Wraps code execution and records any lake operations.
#'
#' @param lake Lake instance
#' @param name Name for the output in the lineage
#' @param expr Expression to track
#' @return The result of the expression
#' @export
#' @examples
#' \dontrun{
#' lake <- Lake$new("project")
#'
#' result <- with_tracking(lake, "analysis_result", {
#'   data <- lake$get("raw_data")
#'   processed <- transform(data)
#'   processed
#' })
#'
#' # Result is automatically saved with tracked dependencies
#' lake$tree("analysis_result")
#' }
with_tracking <- function(lake, name, expr) {
  # Start tracking in the lake's tracker
  tracker <- lake$.__enclos_env__$private$.tracker
  tracker$start_write(name)

  result <- tryCatch({
    eval(substitute(expr), envir = parent.frame())
  }, finally = {
    # End tracking and get dependencies
    deps <- tracker$end_write()
  })

  # Save result with dependencies
  lake$put(name, result, depends_on = deps)

  result
}

#' Create an observed session
#'
#' Starts an observation session that tracks all Lake operations until stopped.
#'
#' @param lake Lake instance to observe
#' @return An ObserveSession object
#' @export
#' @examples
#' \dontrun{
#' lake <- Lake$new("project")
#' session <- observe_session(lake)
#'
#' # Do work...
#' lake$put("data1", df1)
#' lake$put("data2", df2, depends_on = "data1")
#'
#' # Stop and get summary
#' summary <- session$stop()
#' print(summary)
#' }
observe_session <- function(lake) {
  ObserveSession$new(lake)
}

#' @title Observation Session
#' @description Tracks Lake operations over time
#' @keywords internal
ObserveSession <- R6::R6Class("ObserveSession",
  public = list(
    initialize = function(lake) {
      private$.lake <- lake
      private$.start_time <- Sys.time()
      private$.operations <- list()
      private$.active <- TRUE
      invisible(self)
    },

    #' Record an operation
    record = function(op_type, name, details = list()) {
      if (private$.active) {
        private$.operations <- append(private$.operations, list(list(
          type = op_type,
          name = name,
          time = Sys.time(),
          details = details
        )))
      }
      invisible(self)
    },

    #' Stop the session
    stop = function() {
      private$.active <- FALSE
      private$.end_time <- Sys.time()

      list(
        start_time = private$.start_time,
        end_time = private$.end_time,
        duration = difftime(private$.end_time, private$.start_time, units = "secs"),
        operations = private$.operations,
        n_operations = length(private$.operations)
      )
    },

    #' Check if session is active
    is_active = function() {
      private$.active
    },

    print = function() {
      cat("ObserveSession\n")
      cat("  Active:", private$.active, "\n")
      cat("  Operations:", length(private$.operations), "\n")
      cat("  Started:", format(private$.start_time), "\n")
      invisible(self)
    }
  ),

  private = list(
    .lake = NULL,
    .start_time = NULL,
    .end_time = NULL,
    .operations = list(),
    .active = FALSE
  )
)
