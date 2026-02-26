#' @title Seurat Adapter
#' @description Adapter for storing and retrieving Seurat objects.
#' @details
#' This adapter stores a full-fidelity serialized Seurat object and manifest.
#' @importFrom R6 R6Class
#' @export
SeuratAdapter <- .ol_create_serialized_adapter(
  class_name = "SeuratAdapter",
  adapter_name = "Seurat",
  token = "seurat",
  priority = 140,
  can_handle_fn = function(data) {
    inherits(data, "Seurat")
  },
  manifest_type = "Seurat"
)

