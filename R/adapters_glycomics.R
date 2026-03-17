#' @title Glycomics Adapter
#' @description Adapter for storing and retrieving glycomics-layer objects.
#' @details
#' Supports explicit glycomics marker classes/metadata.
#' @importFrom R6 R6Class
#' @export
GlycomicsAdapter <- .ol_create_serialized_adapter(
  class_name = "GlycomicsAdapter",
  adapter_name = "Glycomics",
  token = "glyco",
  priority = 102,
  can_handle_fn = function(data) {
    inherits(data, "GlycomicsLayer") ||
      identical(attr(data, "omics_layer"), "glycomics")
  },
  manifest_type = "Glycomics"
)

