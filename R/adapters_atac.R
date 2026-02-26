#' @title ATAC Adapter
#' @description Adapter for storing and retrieving ATAC-layer objects.
#' @details
#' Supports explicit ATAC marker classes/metadata and ChromatinAssay objects.
#' @importFrom R6 R6Class
#' @export
ATACAdapter <- .ol_create_serialized_adapter(
  class_name = "ATACAdapter",
  adapter_name = "ATAC",
  token = "atac",
  priority = 106,
  can_handle_fn = function(data) {
    inherits(data, c("ATACLayer", "ChromatinAssay")) ||
      identical(attr(data, "omics_layer"), "atac")
  },
  manifest_type = "ATAC"
)

