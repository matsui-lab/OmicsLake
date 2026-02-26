#' @title ChIP Adapter
#' @description Adapter for storing and retrieving ChIP-layer objects.
#' @details
#' Supports explicit ChIP marker classes/metadata.
#' @importFrom R6 R6Class
#' @export
ChIPAdapter <- .ol_create_serialized_adapter(
  class_name = "ChIPAdapter",
  adapter_name = "ChIP",
  token = "chip",
  priority = 105,
  can_handle_fn = function(data) {
    inherits(data, "ChIPLayer") ||
      identical(attr(data, "omics_layer"), "chip")
  },
  manifest_type = "ChIP"
)

