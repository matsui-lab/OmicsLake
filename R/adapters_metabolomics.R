#' @title Metabolomics Adapter
#' @description Adapter for storing and retrieving metabolomics-layer objects.
#' @details
#' Supports explicit metabolomics marker classes/metadata.
#' @importFrom R6 R6Class
#' @export
MetabolomicsAdapter <- .ol_create_serialized_adapter(
  class_name = "MetabolomicsAdapter",
  adapter_name = "Metabolomics",
  token = "metabol",
  priority = 104,
  can_handle_fn = function(data) {
    inherits(data, "MetabolomicsLayer") ||
      identical(attr(data, "omics_layer"), "metabolomics")
  },
  manifest_type = "Metabolomics"
)

