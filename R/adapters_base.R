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
#' if (FALSE) {
#'     # Register a custom adapter
#'     my_adapter <- MyAdapter$new()
#'     register_adapter(my_adapter)
#'
#'     # Now Lake will use this adapter for matching types
#'     lake$put("my_data", my_special_object)
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

        #' @description Get the priority of this adapter (higher = checked
        #' first)
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
        },

        #' @description List root names managed by this adapter
        #' @param lake Lake instance
        #' @return Character vector of names
        list_names = function(lake) {
            character(0)
        }
    )
)

# Global adapter registry
.adapter_registry <- new.env(parent = emptyenv())
.adapter_registry$adapters <- list()
.adapter_registry$autoload_attempted <- FALSE

.ol_try_register_adapter_class <- function(class_name) {
    if (!exists(class_name, inherits = TRUE)) {
        return(FALSE)
    }
    ctor <- get(class_name, inherits = TRUE)
    if (!inherits(ctor, "R6ClassGenerator")) {
        return(FALSE)
    }
    adapter <- tryCatch(
        ctor$new(),
        error = function(e) NULL
    )
    if (is.null(adapter) || !inherits(adapter, "LakeAdapter")) {
        return(FALSE)
    }
    register_adapter(adapter)
    TRUE
}

.ol_autoregister_builtin_adapters <- function() {
    if (isTRUE(.adapter_registry$autoload_attempted)) {
        return(invisible(FALSE))
    }
    .adapter_registry$autoload_attempted <- TRUE

    registered <- FALSE
    for (class_name in c(
        "SeuratAdapter",
        "SpatialExperimentAdapter",
        "RaggedExperimentAdapter",
        "VCFAdapter",
        "MethylationAdapter",
        "ATACAdapter",
        "ChIPAdapter",
        "MetabolomicsAdapter",
        "LipidomicsAdapter",
        "GlycomicsAdapter",
        "PhosphoproteomicsAdapter",
        "TranscriptomicsAdapter",
        "ProteomicsAdapter",
        "GenomicsAdapter",
        "EpigenomicsAdapter",
        "XCMSAdapter",
        "ChromatogramsAdapter",
        "QFeaturesAdapter",
        "MsExperimentAdapter",
        "SpectraAdapter",
        "MAEAdapter",
        "SCEAdapter",
        "SEAdapter"
    )) {
        ok <- tryCatch(
            .ol_try_register_adapter_class(class_name),
            error = function(e) FALSE
        )
        registered <- isTRUE(registered || ok)
    }

    if (!registered && length(.adapter_registry$adapters) == 0) {
        .adapter_registry$autoload_attempted <- FALSE
    }

    invisible(registered)
}

#' Register a data adapter
#'
#' @param adapter An LakeAdapter instance
#' @export
register_adapter <- function(adapter) {
    if (!inherits(adapter, "LakeAdapter")) {
        stop("adapter must be a LakeAdapter instance", call. = FALSE)
    }
    .adapter_registry$autoload_attempted <- TRUE
    .adapter_registry$adapters[[adapter$name()]] <- adapter
    # Sort by priority (descending)
    priorities <- vapply(.adapter_registry$adapters, function(a) a$priority(),
        numeric(1))
    .adapter_registry$adapters <- .adapter_registry$adapters[order(priorities,
        decreasing = TRUE)]
    invisible(TRUE)
}

#' Get registered adapters
#'
#' @return List of registered adapters
#' @export
get_adapters <- function() {
    if (length(.adapter_registry$adapters) == 0) {
        .ol_autoregister_builtin_adapters()
    }
    .adapter_registry$adapters
}

#' Find adapter for data
#'
#' @param data Data to find adapter for
#' @return Matching adapter or NULL
#' @keywords internal
find_adapter <- function(data) {
    adapters <- get_adapters()
    for (adapter in adapters) {
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
    # Reset so built-in adapters can be auto-registered on next access.
    .adapter_registry$autoload_attempted <- FALSE
    invisible(TRUE)
}
