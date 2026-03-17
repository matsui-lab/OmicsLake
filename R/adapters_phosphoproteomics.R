#' @title Phosphoproteomics Adapter
#' @description Adapter for storing and retrieving phosphoproteomics-layer
#' objects.
#' @details
#' Supports explicit phosphoproteomics marker classes/metadata.
#' @importFrom R6 R6Class
#' @export
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

