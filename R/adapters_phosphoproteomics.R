#' @title Phosphoproteomics Adapter
#' @description Adapter for storing and retrieving phosphoproteomics-layer
#' objects.
#' @details
#' Supports explicit phosphoproteomics marker classes/metadata.
#' @importFrom R6 R6Class
#' @export
#' @return An R6 generator for a \code{LakeAdapter} subclass that
#'   serializes and restores objects of this omics layer.
#' @examples
#' adapter <- PhosphoproteomicsAdapter$new()
#' class(adapter)
PhosphoproteomicsAdapter <- .ol_create_serialized_adapter(
  class_name = "PhosphoproteomicsAdapter",
  adapter_name = "Phosphoproteomics",
  token = "phospho",
  priority = 101,
  can_handle_fn = function(data) {
    inherits(data, "PhosphoproteomicsLayer") ||
      identical(attr(data, "omics_layer"), "phosphoproteomics")
  },
  manifest_type = "Phosphoproteomics"
)

