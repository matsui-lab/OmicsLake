#' @title Epigenomics Adapter
#' @description Adapter for storing and retrieving epigenomics-layer objects.
#' @details
#' This umbrella adapter is lower priority than specific adapters
#' (e.g. Methylation / ATAC / ChIP) and captures epigenomics-marked objects.
#' @importFrom R6 R6Class
#' @export
EpigenomicsAdapter <- .ol_create_serialized_adapter(
  class_name = "EpigenomicsAdapter",
  adapter_name = "Epigenomics",
  token = "epigen",
  priority = 95,
  can_handle_fn = function(data) {
    inherits(data, "EpigenomicsLayer") ||
      identical(attr(data, "omics_layer"), "epigenomics")
  },
  manifest_type = "Epigenomics"
)
