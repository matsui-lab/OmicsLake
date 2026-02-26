#' @title Lipidomics Adapter
#' @description Adapter for storing and retrieving lipidomics-layer objects.
#' @details
#' Supports explicit lipidomics marker classes/metadata.
#' @importFrom R6 R6Class
#' @export
LipidomicsAdapter <- .ol_create_serialized_adapter(
  class_name = "LipidomicsAdapter",
  adapter_name = "Lipidomics",
  token = "lipid",
  priority = 103,
  can_handle_fn = function(data) {
    inherits(data, "LipidomicsLayer") ||
      identical(attr(data, "omics_layer"), "lipidomics")
  },
  manifest_type = "Lipidomics"
)

