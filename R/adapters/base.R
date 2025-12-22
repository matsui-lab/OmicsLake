#' @title Lake Adapter Base Class
#' @description Base class for type-specific data adapters.
#' Adapters provide specialized storage and retrieval for complex object types.
#'
#' @details
#' Adapters allow OmicsLake to store complex objects (like SummarizedExperiment
#' or Seurat objects) in a structured way that preserves all metadata and
#' allows partial queries.
#'
#' @examples
#' \dontrun{
#' # Register a custom adapter
#' my_adapter <- MyAdapter$new()
#' register_adapter(my_adapter)
#'
#' # Now Lake will use this adapter for matching types
#' lake$put("my_data", my_special_object)
#' }
#'
#' @importFrom R6 R6Class
#' @export
LakeAdapter <- R6::R6Class("LakeAdapter",
  public = list(

    #' @description Get the adapter name
    #' @return Character string
    name = function() {
      "base"
    },

    #' @description Check if this adapter can handle the given data

    #' @param data Data to check
    #' @return Logical indicating if adapter can handle this type
    can_handle = function(data) {
      FALSE
    },

    #' @description Get the priority of this adapter (higher = checked first)
    #' @return Numeric priority value
    priority = function() {
      0
    },

    #' @description Store data using this adapter
    #' @param lake Lake instance
    #' @param name Data name
    #' @param data Data to store
    #' @return Invisible TRUE on success
    put = function(lake, name, data) {
      stop("Not implemented: put()", call. = FALSE)
    },

    #' @description Retrieve data using this adapter
    #' @param lake Lake instance
    #' @param name Data name
    #' @param ref Version reference
    #' @return The retrieved data
    get = function(lake, name, ref = "@latest") {
      stop("Not implemented: get()", call. = FALSE)
    },

    #' @description List components stored for this data
    #' @param lake Lake instance
    #' @param name Data name
    #' @return Data frame of components
    components = function(lake, name) {
      data.frame(
        component = character(0),
        type = character(0),
        stringsAsFactors = FALSE
      )
    },

    #' @description Check if data exists
    #' @param lake Lake instance
    #' @param name Data name
    #' @return Logical
    exists = function(lake, name) {
      FALSE
    }
  )
)

# Global adapter registry
.adapter_registry <- new.env(parent = emptyenv())
.adapter_registry$adapters <- list()

#' Register a data adapter
#'
#' @param adapter An LakeAdapter instance
#' @export
register_adapter <- function(adapter) {
  if (!inherits(adapter, "LakeAdapter")) {
    stop("adapter must be a LakeAdapter instance", call. = FALSE)
  }
  .adapter_registry$adapters[[adapter$name()]] <- adapter
  # Sort by priority (descending)
  priorities <- sapply(.adapter_registry$adapters, function(a) a$priority())
  .adapter_registry$adapters <- .adapter_registry$adapters[order(priorities, decreasing = TRUE)]
  invisible(TRUE)
}

#' Get registered adapters
#'
#' @return List of registered adapters
#' @export
get_adapters <- function() {
  .adapter_registry$adapters
}

#' Find adapter for data
#'
#' @param data Data to find adapter for
#' @return Matching adapter or NULL
#' @keywords internal
find_adapter <- function(data) {
  for (adapter in .adapter_registry$adapters) {
    if (adapter$can_handle(data)) {
      return(adapter)
    }
  }
  NULL
}

#' Clear all registered adapters
#'
#' @keywords internal
clear_adapters <- function() {
  .adapter_registry$adapters <- list()
  invisible(TRUE)
}
